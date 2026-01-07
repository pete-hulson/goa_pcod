
#' Retrospective diagnostic plots for Stock Synthesis parameters (report:5)
#'
#' Generates retrospective plots for one or more Stock Synthesis model parameters
#' using the \code{PARAMETERS report:5} section from a sequence of Stock Synthesis
#' \code{.sso} report files (e.g., \code{Report_9.sso} through \code{Report_0.sso}).
#' Each peel is plotted as a function of years removed, allowing direct evaluation
#' of parameter stability and retrospective behavior.
#'
#' The function parses the parameter table using the explicit header row beginning
#' with \code{Num Label Value}, ensuring robust alignment with Stock Synthesis output
#' formats across versions. Parameter uncertainty is displayed using 95\% confidence
#' intervals, derived either from the explicit \code{Value±1.96*SD} columns when
#' available or computed from \code{Parm_StDev} when necessary.
#'
#' Optional normalization to the terminal model (\code{Report_0.sso}) is supported,
#' allowing parameters to be displayed as either ratios or differences relative to
#' the full-data estimate. When normalization is enabled, reference lines are drawn
#' at 1.0 (ratio) or 0.0 (difference) to aid interpretation.
#'
#' Multiple parameters may be plotted simultaneously and are displayed in separate
#' facets, with optional free y-axis scaling to accommodate parameters on different
#' scales. The x-axis is expressed as \emph{years removed} (negative peel), consistent
#' with standard Stock Synthesis retrospective diagnostics.
#'
#' @param report_dir Directory containing Stock Synthesis \code{Report_k.sso} files.
#' @param peel_range Integer vector of retrospective peels (e.g., \code{9:0}),
#'   where larger values indicate more years removed.
#' @param param_labels Character vector of parameter names exactly matching the
#'   \code{Label} column in \code{PARAMETERS report:5}.
#' @param normalize_to_report0 Logical; if \code{TRUE}, normalize parameter estimates
#'   to the corresponding value from \code{Report_0.sso}.
#' @param normalize_mode Character; either \code{"ratio"} (estimate / Report\_0) or
#'   \code{"diff"} (estimate − Report\_0).
#' @param eps Small numeric value used to avoid division by zero when normalizing ratios.
#' @param add_reference_line Logical; if \code{TRUE}, add a horizontal reference line
#'   at 1.0 (ratio) or 0.0 (difference) when normalization is enabled.
#' @param facet_scales Scaling option passed to \code{facet_wrap}; typically
#'   \code{"free_y"} or \code{"fixed"}.
#' @param show_ci Logical; if \code{TRUE}, display 95\% confidence intervals for
#'   parameter estimates.
#' @param title Optional plot title. If \code{NULL}, a descriptive default is used.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{plot}}{A \code{ggplot2} object containing the retrospective plot(s).}
#'     \item{\code{data}}{A tidy data frame containing parameter estimates, confidence
#'       intervals, peels, and labels used in the plot.}
#'   }
#'
#' @details
#' This function is intended for retrospective diagnostics of parameter stability
#' in Stock Synthesis assessments. It does not use derived quantities or forecast
#' outputs and is limited strictly to \code{PARAMETERS report:5}. The function
#' fails fast if parameter labels are not unique within a report or cannot be
#' located, ensuring SS-Strict behavior and reproducibility.
#'
#' @examples
#' \dontrun{
#' # List available parameter labels
#' labs <- list_parameter_labels("Report_0.sso")
#'
#' # Plot a single parameter with confidence intervals
#' plot_retro_parameters(
#'   report_dir = ".",
#'   peel_range = 9:0,
#'   param_labels = "LnQ_base_Srv(4)"
#' )
#'
#' # Plot multiple parameters, normalized to the terminal model
#' plot_retro_parameters(
#'   report_dir = ".",
#'   peel_range = 9:0,
#'   param_labels = c("LnQ_base_Srv(4)", "NatM_p_1_Fem_GP_1"),
#'   normalize_to_report0 = TRUE,
#'   normalize_mode = "ratio"
#' )
#' }


suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(tibble)
})

# --- Block finder (regex header match) ---
find_block_regex <- function(lines, header_regex) {
  idx <- which(str_detect(str_trim(lines), header_regex))
  if (length(idx) == 0) stop("Header not found (regex): ", header_regex)
  start <- idx[1] + 1

  next_idx <- which(seq_along(lines) > start &
                      str_detect(str_trim(lines), "^[A-Z0-9_().]+.*\\sreport:\\d+\\s*$"))
  end <- if (length(next_idx) == 0) length(lines) + 1 else next_idx[1]
  lines[start:(end - 1)]
}

# --- SS-Strict: parse PARAMETERS report:5 table using its header row ---
parse_parameters_report5_lines <- function(lines) {
  blk <- find_block_regex(lines, "^PARAMETERS\\s+report:5\\b")
  blk <- blk[nzchar(str_trim(blk))]
  blk_trim <- str_trim(blk)

  # Find header row containing Num, Label, Value
  hdr_idx <- which(
    str_detect(blk_trim, "\\bNum\\b") &
      str_detect(blk_trim, "\\bLabel\\b") &
      str_detect(blk_trim, "\\bValue\\b")
  )
  if (length(hdr_idx) == 0) {
    stop("Could not find PARAMETERS report:5 header row containing 'Num', 'Label', and 'Value'.")
  }

  hdr_line <- blk_trim[hdr_idx[1]]
  hdr <- str_split(hdr_line, "\\s+")[[1]]

  pos_num   <- match("Num", hdr)
  pos_label <- match("Label", hdr)
  pos_value <- match("Value", hdr)

  # SD column (prefer Parm_StDev)
  pos_sd <- match("Parm_StDev", hdr)
  if (is.na(pos_sd)) pos_sd <- match("StdDev", hdr)

  # Explicit CI columns if present
  pos_lo <- match("Value-1.96*SD", hdr)
  pos_hi <- match("Value+1.96*SD", hdr)

  if (any(is.na(c(pos_num, pos_label, pos_value)))) {
    stop("Header parsed but could not match one of: Num/Label/Value.")
  }

  dat <- blk[(hdr_idx[1] + 1):length(blk)]
  dat <- dat[nzchar(str_trim(dat))]
  dat <- dat[!str_starts(str_trim(dat), "#")]

  out <- lapply(dat, function(x) {
    p <- str_split(str_trim(x), "\\s+")[[1]]
    if (length(p) < pos_value) return(NULL)

    num_i <- suppressWarnings(as.integer(p[pos_num]))
    if (is.na(num_i)) return(NULL)

    label <- p[pos_label]
    value <- suppressWarnings(as.numeric(p[pos_value]))
    if (is.na(value)) return(NULL)

    sd <- NA_real_
    if (!is.na(pos_sd) && length(p) >= pos_sd) sd <- suppressWarnings(as.numeric(p[pos_sd]))

    lo <- NA_real_
    hi <- NA_real_
    if (!is.na(pos_lo) && length(p) >= pos_lo) lo <- suppressWarnings(as.numeric(p[pos_lo]))
    if (!is.na(pos_hi) && length(p) >= pos_hi) hi <- suppressWarnings(as.numeric(p[pos_hi]))

    # If explicit CI not present, compute from sd if available
    if ((is.na(lo) || is.na(hi)) && !is.na(sd)) {
      lo <- value - 1.96 * sd
      hi <- value + 1.96 * sd
    }

    tibble(Num = num_i, Label = label, Value = value, Parm_StDev = sd,
           CI_lo = lo, CI_hi = hi)
  }) |> bind_rows()

  if (nrow(out) == 0) stop("No parameter rows parsed from PARAMETERS report:5.")
  out
}

parse_parameters_report5_file <- function(report_file) {
  lines <- readLines(report_file, warn = FALSE)
  parse_parameters_report5_lines(lines)
}

# --- list unique parameter labels from Report_0 ---
list_parameter_labels <- function(report0_file = "Report_0.sso") {
  parse_parameters_report5_file(report0_file) |>
    distinct(Label) |>
    arrange(Label) |>
    pull(Label)
}

# ---- Plot retrospective parameter(s) with CI + facets ----
plot_retro_parameters <- function(
    report_dir = ".",
    peel_range = 9:0,
    param_labels,                      # character vector
    normalize_to_report0 = FALSE,
    normalize_mode = c("ratio", "diff"),
    eps = 1e-12,
    add_reference_line = TRUE,
    facet_scales = "free_y",           # "fixed" or "free_y"
    show_ci = TRUE,
    title = NULL
) {
  normalize_mode <- match.arg(normalize_mode)

  if (length(param_labels) < 1) stop("param_labels must contain at least one parameter label.")

  # Read requested parameters from each peel
  all <- lapply(peel_range, function(p) {
    f <- file.path(report_dir, paste0("Report_", p, ".sso"))
    if (!file.exists(f)) stop("Missing file: ", f)

    pars <- parse_parameters_report5_file(f)
    hit <- pars |> filter(Label %in% param_labels)

    missing <- setdiff(param_labels, unique(hit$Label))
    if (length(missing) > 0) {
      stop("Missing parameter(s) in ", f, ":\n  ",
           paste(missing, collapse = ", "),
           "\nTip: labs <- list_parameter_labels('Report_0.sso')")
    }

    # SS-Strict: ensure uniqueness per peel/label
    dup <- hit |> count(Label) |> filter(n > 1)
    if (nrow(dup) > 0) {
      stop("Non-unique label(s) in ", f, ":\n  ",
           paste(dup$Label, collapse = ", "))
    }

    hit |> mutate(Peel = p)
  }) |>
    bind_rows() |>
    select(Peel, Label, Value, Parm_StDev, CI_lo, CI_hi)

  # Normalize to Report_0 (optional), by Label
  if (normalize_to_report0) {
    base <- all |>
      filter(Peel == 0) |>
      select(Label, V0 = Value)

    all <- all |>
      left_join(base, by = "Label") |>
      mutate(
        Value = case_when(
          normalize_mode == "ratio" ~ Value / pmax(V0, eps),
          normalize_mode == "diff"  ~ Value - V0,
          TRUE ~ Value
        ),
        CI_lo = case_when(
          !show_ci ~ CI_lo,
          normalize_mode == "ratio" ~ CI_lo / pmax(V0, eps),
          normalize_mode == "diff"  ~ CI_lo - V0,
          TRUE ~ CI_lo
        ),
        CI_hi = case_when(
          !show_ci ~ CI_hi,
          normalize_mode == "ratio" ~ CI_hi / pmax(V0, eps),
          normalize_mode == "diff"  ~ CI_hi - V0,
          TRUE ~ CI_hi
        )
      ) |>
      select(-V0)
  }

  # X axis = -peel
  all <- all |>
    mutate(
      Peel = factor(Peel, levels = peel_range),
      Peel_x = -as.integer(as.character(Peel))
    ) |>
    arrange(Label, Peel_x)

  if (is.null(title)) {
    title <- if (!normalize_to_report0) {
      "Retrospective parameters"
    } else {
      paste0("Retrospective parameters (", normalize_mode, " to Report_0)")
    }
  }

  ylab <- if (!normalize_to_report0) {
    "Estimate"
  } else if (normalize_mode == "ratio") {
    "Estimate / Report_0"
  } else {
    "Estimate − Report_0"
  }

  p <- ggplot(all, aes(x = Peel_x, y = Value, group = 1)) +
    {if (show_ci) geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi), width = 0.15, linewidth = 0.5) } +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = sort(unique(all$Peel_x))) +
    facet_wrap(~ Label, scales = facet_scales) +
    labs(
      title = title,
      subtitle = "SS-Strict: PARAMETERS report:5; CI from Value±1.96*Parm_StDev or explicit Value±1.96*SD columns",
      x = "Peel (-Current year)",
      y = ylab
    ) +
    theme_bw() +
    theme(plot.title.position = "plot")

  if (normalize_to_report0 && add_reference_line) {
    if (normalize_mode == "ratio") {
      p <- p + geom_hline(yintercept = 1.0, linetype = "dashed", color = "black")
    } else {
      p <- p + geom_hline(yintercept = 0.0, linetype = "dashed", color = "black")
    }
  }

  list(plot = p, data = all)
}
