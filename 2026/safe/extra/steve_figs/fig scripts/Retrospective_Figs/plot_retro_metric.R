#' Retrospective diagnostic plots for Stock Synthesis derived quantities 
#'
#' Generates retrospective plots for Stock Synthesis derived quantity time series
#' (e.g., \code{SSB_YYYY} or \code{Recr_YYYY}) using the \code{DERIVED_QUANTITIES report:6}
#' section from a sequence of Stock Synthesis \code{.sso} report files (e.g.,
#' \code{Report_9.sso} through \code{Report_0.sso}). Each peel is truncated at its
#' terminal year (\code{current_year - peel}), consistent with standard retrospective
#' diagnostics.
#'
#' The plotted metric is identified by matching labels of the form
#' \code{<metric>_YYYY} within \code{DERIVED_QUANTITIES report:6}. Optional normalization
#' to the full-data model (\code{Report_0.sso}) is supported, allowing display as either
#' ratios or differences relative to the corresponding \code{Report_0} estimate by year.
#' When normalization is enabled, reference lines are drawn at 1.0 (ratio) or 0.0
#' (difference) to aid interpretation.
#'
#' The function also computes Mohn's rho for the chosen metric using overlapping years
#' across peels, constrained to each peel’s terminal year and (by default) the last
#' 10 years ending at \code{current_year}. Mohn's rho is reported on the plot in the
#' upper-right corner and returned as a numeric value.
#'
#' @param report_dir Directory containing Stock Synthesis \code{Report_k.sso} files.
#' @param peel_range Integer vector of retrospective peels (e.g., \code{9:0}),
#'   where larger values indicate more terminal years removed.
#' @param metric Character scalar indicating which derived quantity series to extract.
#'   Must match the label prefix used in \code{DERIVED_QUANTITIES report:6}
#'   (e.g., \code{"SSB"} for \code{SSB_YYYY}, \code{"Recr"} for \code{Recr_YYYY}).
#' @param years Integer vector of years to display on the x-axis. Each peel is still
#'   truncated at \code{current_year - peel}.
#' @param current_year Integer terminal year for \code{Report_0.sso}. Used to compute
#'   peel terminal years (\code{current_year - peel}) and the default Mohn’s rho window.
#' @param normalize_to_report0 Logical; if \code{TRUE}, normalize values to the
#'   corresponding \code{Report_0.sso} value by year.
#' @param normalize_mode Character; either \code{"ratio"} (value / Report\_0) or
#'   \code{"diff"} (value − Report\_0).
#' @param eps Small numeric value used to avoid division by zero when normalizing ratios.
#' @param show_mohn_rho Logical; if \code{TRUE}, compute and annotate Mohn’s rho.
#' @param rho_years Optional integer vector of years over which to compute Mohn’s rho.
#'   If \code{NULL}, defaults to the last 10 years ending at \code{current_year}.
#' @param title Optional plot title. If \code{NULL}, a descriptive default is used.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{\code{plot}}{A \code{ggplot2} object containing the retrospective plot.}
#'     \item{\code{data}}{A tidy data frame of extracted values by year and peel.}
#'     \item{\code{mohn_rho}}{Numeric Mohn’s rho value for the chosen metric (or \code{NA}).}
#'   }
#'
#' @details
#' This function is SS-Strict: values are taken only from \code{DERIVED_QUANTITIES report:6}
#' using labels matching \code{<metric>_YYYY}, and each peel is truncated at its terminal
#' year (\code{current_year - peel}). Mohn's rho is computed on raw (non-normalized)
#' values, consistent with standard reporting practice.
#'
#' @examples
#' \dontrun{
#' # Retrospective SSB with default rho window (last 10 years)
#' res <- plot_retro_metric(report_dir=".", peel_range=9:0, metric="SSB",
#'                          years=1977:2025, current_year=2025)
#' print(res$plot)
#' res$mohn_rho
#'
#' # Retrospective recruitment
#' res <- plot_retro_metric(report_dir=".", peel_range=9:0, metric="Recr",
#'                          years=1977:2025, current_year=2025)
#' print(res$plot)
#'
#' # Normalized differences to Report_0 and custom rho window
#' res <- plot_retro_metric(report_dir=".", peel_range=9:0, metric="SSB",
#'                          years=1977:2025, current_year=2025,
#'                          normalize_to_report0=TRUE, normalize_mode="diff",
#'                          rho_years=2016:2025)
#' print(res$plot)
#' }
#'
#' @export


suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(tibble)
})

# ---- SS-Strict helpers ----
find_block <- function(lines, header) {
  start <- which(str_trim(lines) == header)
  if (length(start) == 0) stop("Header not found: ", header)
  start <- start[1]

  next_idx <- which(seq_along(lines) > start &
                      str_detect(lines, "^[A-Z0-9_().]+.*\\sreport:\\d+\\s*$"))
  end <- if (length(next_idx) == 0) length(lines) + 1 else next_idx[1]
  lines[(start + 1):(end - 1)]
}

parse_derived_quantities <- function(lines) {
  blk <- find_block(lines, "DERIVED_QUANTITIES report:6")
  blk <- blk[nzchar(str_trim(blk))]

  lapply(blk, function(x) {
    p <- str_split(str_trim(x), "\\s+")[[1]]
    if (length(p) < 2) return(NULL)
    tibble(Label = p[1], Value = suppressWarnings(as.numeric(p[2])))
  }) |>
    bind_rows() |>
    filter(!is.na(Value))
}

extract_metric_timeseries <- function(report_file, metric = c("SSB", "Recr")) {
  metric <- match.arg(metric)
  lines <- readLines(report_file, warn = FALSE)
  dq <- parse_derived_quantities(lines)

  # Match e.g. SSB_YYYY or Recr_YYYY
  pat <- paste0("^", metric, "_\\d{4}$")

  ts <- dq |>
    filter(str_detect(Label, pat)) |>
    mutate(Year = as.integer(str_match(Label, paste0("^", metric, "_(\\d{4})$"))[,2])) |>
    select(Year, Value) |>
    arrange(Year)

  if (nrow(ts) == 0) stop("No ", metric, "_YYYY found in DERIVED_QUANTITIES for: ", report_file)
  ts
}

compute_mohn_rho <- function(all_ts, current_year, peel_range, rho_years) {
  # all_ts columns: Year, Peel, Value
  base <- all_ts |> filter(Peel == 0) |> select(Year, V0 = Value)

  contrib <- all_ts |>
    filter(Peel %in% peel_range, Peel != 0, Year %in% rho_years) |>
    mutate(TerminalYear = current_year - Peel) |>
    filter(Year <= TerminalYear) |>
    left_join(base, by = "Year") |>
    filter(!is.na(V0), V0 != 0) |>
    mutate(RelDiff = (Value - V0) / V0)

  if (nrow(contrib) == 0) return(NA_real_)
  mean(contrib$RelDiff, na.rm = TRUE)
}

# ---- Main function ----
plot_retro_metric <- function(
    report_dir = ".",
    peel_range = 9:0,                 # expects Report_9 ... Report_0
    metric = c("SSB", "Recr"),           # which derived quantity series
    years = 1977:2025,                # years to display (each peel truncated anyway)
    current_year = 2025,              # REQUIRED
    normalize_to_report0 = FALSE,
    normalize_mode = c("ratio", "diff"),
    eps = 1e-12,
    show_mohn_rho = TRUE,
    rho_years = NULL,                 # default = last 10 years ending at current_year
    title = NULL
) {
  metric <- match.arg(metric)
  normalize_mode <- match.arg(normalize_mode)

  if (is.null(rho_years)) rho_years <- (current_year - 9):current_year

  # ---- Read all peels, truncate each at terminal year ----
  all <- lapply(peel_range, function(p) {
    f <- file.path(report_dir, paste0("Report_", p, ".sso"))
    if (!file.exists(f)) stop("Missing file: ", f)

    ts <- extract_metric_timeseries(f, metric = metric) |>
      mutate(Peel = p)

    terminal_year <- current_year - p
    ts |>
      filter(Year <= terminal_year, Year %in% years) |>
      rename(Value = Value)
  }) |>
    bind_rows()

  # ---- Mohn's rho computed on raw (non-normalized) values ----
  rho <- NA_real_
  if (show_mohn_rho) {
    raw_all <- lapply(peel_range, function(p) {
      f <- file.path(report_dir, paste0("Report_", p, ".sso"))
      ts <- extract_metric_timeseries(f, metric = metric) |>
        mutate(Peel = p)
      terminal_year <- current_year - p
      ts |> filter(Year <= terminal_year, Year %in% rho_years)
    }) |> bind_rows()

    rho <- compute_mohn_rho(
      all_ts = raw_all,
      current_year = current_year,
      peel_range = peel_range,
      rho_years = rho_years
    )
  }

  # ---- Optional normalization to Report_0 by Year ----
  if (normalize_to_report0) {
    base <- all |> filter(Peel == 0) |> select(Year, V0 = Value)

    all <- all |>
      left_join(base, by = "Year") |>
      mutate(
        Value = case_when(
          normalize_mode == "ratio" ~ Value / pmax(V0, eps),
          normalize_mode == "diff"  ~ Value - V0,
          TRUE ~ Value
        )
      ) |>
      select(Year, Peel, Value)
  }

  all <- all |>
    mutate(Peel = factor(Peel, levels = peel_range)) |>
    arrange(as.integer(as.character(Peel)), Year)

  # Labels
  if (is.null(title)) {
    title <- if (!normalize_to_report0) {
      paste0("Retrospective ", metric, " (", metric, "_YYYY)")
    } else {
      paste0("Retrospective ", metric, " (", normalize_mode, " to Report_0)")
    }
  }

  ylab <- if (!normalize_to_report0) {
    metric
  } else if (normalize_mode == "ratio") {
    paste0(metric, " / ", metric, "(Report_0)")
  } else {
    paste0(metric, " − ", metric, "(Report_0)")
  }

  p <- ggplot(all, aes(x = Year, y = Value, color = Peel, group = Peel)) +
    geom_line(linewidth = 1) +
    labs(
      title = title,
      subtitle = paste0("SS-Strict: DERIVED_QUANTITIES report:6; each peel truncated at (",
                        current_year, " − peel)"),
      x = "Year",
      y = ylab,
      color = "Peel"
    ) +
    theme_bw() +
    theme(plot.title.position = "plot")

  # Reference lines for normalized plots
  if (normalize_to_report0 && normalize_mode == "ratio") {
    p <- p + geom_hline(yintercept = 1.0, linetype = "dashed", color = "black")
  }
  if (normalize_to_report0 && normalize_mode == "diff") {
    p <- p + geom_hline(yintercept = 0.0, linetype = "dashed", color = "black")
  }

  # Mohn's rho label (last 10 years by default)
  if (show_mohn_rho) {
    lab <- if (is.na(rho)) {
      paste0("Mohn's rho (", min(rho_years), "–", max(rho_years), "): NA")
    } else {
      sprintf("Mohn's rho (%d–%d): %.3f", min(rho_years), max(rho_years), rho)
    }
    p <- p + annotate("text",
                      x = Inf, y = Inf,
                      label = lab,
                      hjust = 1.05, vjust = 1.2,
                      size = 3.5)
  }

  list(plot = p, data = all, mohn_rho = rho)
}

# ---- Examples ----
# SSB figure (through 2025), with Mohn's rho last 10 yrs
# res <- plot_retro_metric(report_dir=".", peel_range=9:0, metric="SSB",
#                          years=1977:2025, current_year=2025)
# print(res$plot)
#
# Recruitment (R_YYYY) figure
# res <- plot_retro_metric(report_dir=".", peel_range=9:0, metric="Recr",
                          years=1977:2025, current_year=2025)
# print(res$plot)
#
# Normalized ratio to Report_0
# res <- plot_retro_metric(report_dir=".", peel_range=10:0, metric="Recr",
#                          years=1977:2025, current_year=2024,
#                          normalize_to_report0=TRUE, normalize_mode="diff")
# print(res$plot)
