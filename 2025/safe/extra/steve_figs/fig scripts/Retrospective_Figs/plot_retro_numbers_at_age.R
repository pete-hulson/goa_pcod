#' Retrospective diagnostics for Stock Synthesis numbers-at-age with Mohn's rho (report:35)
#'
#' Generates retrospective plots of Stock Synthesis numbers-at-age using the
#' \code{NUMBERS_AT_AGE report:35} section from a sequence of Stock Synthesis
#' \code{.sso} report files (e.g., \code{Report_9.sso} through \code{Report_0.sso}).
#' A single age is extracted and plotted across peels as a time series over \code{years}.
#'
#' The function enforces SS-Strict censoring so that points are not plotted beyond
#' the information available to a given peel. Specifically, for peel \code{p}, points
#' with \code{X >= current_year - p} are excluded (where \code{X} is the plotted year
#' value and \code{current_year} corresponds to the terminal year in \code{Report_0.sso}).
#' This prevents leakage of future information into retrospective peels.
#'
#' Optional normalization to the terminal (full-data) run (\code{Report_0.sso}) is supported,
#' allowing values to be displayed as ratios or differences relative to \code{Report_0}
#' for each plotted year. When normalized, reference lines are drawn at 1.0 (ratio) or
#' 0.0 (difference) if enabled.
#'
#' Mohn's rho is computed for the plotted series using overlapping years across peels,
#' constrained by the same censoring rule and (by default) the last 10 years ending at
#' \code{current_year}. Mohn's rho is computed on the raw (non-normalized) values, consistent
#' with common retrospective reporting practice, and is annotated in the upper-right of
#' the plot. The value is also returned.
#'
#' @param report_dir Directory containing Stock Synthesis \code{Report_k.sso} files.
#' @param peel_range Integer vector of peels (e.g., \code{9:0}), where larger values
#'   indicate more years removed.
#' @param years Integer vector of year values to plot on the x-axis.
#' @param age_to_plot Integer age to extract from \code{NUMBERS_AT_AGE report:35}.
#' @param current_year Integer terminal year for \code{Report_0.sso}. Used to censor peels via
#'   \code{X < current_year - peel}. Must be supplied (do not auto-calculate).
#' @param sex Integer sex code used in the SS report.
#' @param seas Integer season code to extract.
#' @param begmid Character indicating beginning/mid-year values (e.g., \code{"B"}).
#' @param area Integer area code to extract.
#' @param morph Integer morph code to extract.
#' @param normalize_to_report0 Logical; if \code{TRUE}, normalize to \code{Report_0.sso} values by year.
#' @param normalize_mode Character; either \code{"ratio"} (peel / Report\_0) or
#'   \code{"diff"} (peel − Report\_0).
#' @param eps Small numeric value used to avoid division by zero when normalizing ratios.
#' @param add_reference_line Logical; if \code{TRUE}, add a horizontal reference line
#'   at 1.0 (ratio) or 0.0 (difference) when normalization is enabled.
#' @param show_mohn_rho Logical; if \code{TRUE}, compute and annotate Mohn's rho.
#' @param rho_years Optional integer vector of years over which to compute Mohn's rho.
#'   If \code{NULL}, defaults to the last 10 years ending at \code{current_year}.
#' @param title Optional plot title. If \code{NULL}, a descriptive default is used.
#'
#' @return A list with:
#' \describe{
#'   \item{\code{plot}}{A \code{ggplot2} object containing the retrospective plot.}
#'   \item{\code{data}}{A tidy data frame of extracted values by peel and year.}
#'   \item{\code{mohn_rho}}{Numeric Mohn's rho value (or \code{NA}).}
#' }
#'
#' @details
#' This function assumes a helper function \code{parse_numbers_at_age()} is available in
#' the calling environment (as in your existing SS utilities) and that it returns a data
#' frame including columns \code{Year}, \code{Age}, and \code{N} after applying report:35
#' filters (\code{sex}, \code{seas}, \code{begmid}, \code{area}, \code{morph}).
#'
#' Mohn's rho is computed as the mean relative difference between each peel and the full-data
#' run (\code{Report_0.sso}) across \code{rho_years} and peels, subject to the censoring rule:
#' \deqn{\rho = mean\{ (N_p(X) - N_0(X)) / N_0(X) \}}
#'
#' @examples
#' \dontrun{
#' # Raw N-at-age-0 retrospectives (censored) with Mohn's rho (default last 10 years)
#' res <- plot_retro_numbers_at_age(
#'   report_dir   = ".",
#'   peel_range   = 9:0,
#'   years        = 2010:2025,
#'   age_to_plot  = 0,
#'   current_year   = 2025,
#'   sex          = 1,
#'   seas         = 1,
#'   begmid       = "B",
#'   area         = 1,
#'   morph        = 1
#' )
#' print(res$plot)
#' res$mohn_rho
#'
#' # Normalized ratio to Report_0 with a reference line at 1.0
#' res <- plot_retro_numbers_at_age(
#'   report_dir   = ".",
#'   peel_range   = 9:0,
#'   years        = 2010:2025,
#'   age_to_plot  = 0,
#'   current_year   = 2025,
#'   normalize_to_report0 = TRUE,
#'   normalize_mode = "ratio",
#'   add_reference_line = TRUE
#' )
#' print(res$plot)
#'
#' # Mohn's rho over a custom window
#' res <- plot_retro_numbers_at_age(
#'   report_dir   = ".",
#'   peel_range   = 9:0,
#'   years        = 2000:2025,
#'   age_to_plot  = 2,
#'   current_year   = 2025,
#'   show_mohn_rho = TRUE,
#'   rho_years     = 2016:2025
#' )
#' print(res$plot)
#' }
#'
#' @export
plot_retro_numbers_at_age <- function(
    report_dir = ".",
    peel_range = 9:0,
    years = 2010:2025,
    age_to_plot = 0L,
    current_year,
    sex = 1L,
    seas = 1L,
    begmid = "B",
    area = 1L,
    morph = 1L,
    normalize_to_report0 = FALSE,
    normalize_mode = c("ratio", "diff"),
    eps = 1e-12,
    add_reference_line = TRUE,
    show_mohn_rho = TRUE,
    rho_years = NULL,
    title = NULL
) {
  normalize_mode <- match.arg(normalize_mode)

  if (missing(current_year) || length(current_year) != 1 || is.na(current_year)) {
    stop("current_year is required (e.g., current_year=2025).")
  }
  current_year <- as.integer(current_year)
  years <- as.integer(years)

  if (is.null(rho_years)) rho_years <- (current_year - 10):current_year
  rho_years <- as.integer(rho_years)

  # ---- helper: compute Mohn's rho on raw values ----
  compute_mohn_rho_series <- function(df_raw, current_year, peel_range, rho_years, eps = 1e-12) {
    # df_raw must have columns: Peel (int), X (int), N_raw (numeric)
    base <- df_raw %>%
      dplyr::filter(Peel == 0, X %in% rho_years) %>%
      dplyr::select(X, N0 = N_raw)

    contrib <- df_raw %>%
      dplyr::filter(Peel %in% peel_range, Peel != 0, X %in% rho_years) %>%
      dplyr::mutate(TerminalX = current_year - Peel) %>%
      dplyr::filter(X < TerminalX) %>%                         # censor rule
      dplyr::left_join(base, by = "X") %>%
      dplyr::filter(!is.na(N0), abs(N0) > eps) %>%
      dplyr::mutate(RelDiff = (N_raw - N0) / N0)

    if (nrow(contrib) == 0) return(NA_real_)
    mean(contrib$RelDiff, na.rm = TRUE)
  }

  # ---- read one peel ----
  read_one <- function(peel) {
    f <- file.path(report_dir, paste0("Report_", peel, ".sso"))
    if (!file.exists(f)) stop("Missing file: ", f)
    lines <- readLines(f, warn = FALSE)

    # Requires parse_numbers_at_age(lines, years, sex, seas, begmid, area, morph)
    num <- parse_numbers_at_age(
      lines,
      years = years,
      sex = sex, seas = seas, begmid = begmid,
      area = area, morph = morph
    )

    if (nrow(num) == 0) {
      stop("No numbers-at-age parsed for ", f,
           ". Check filters: sex/seas/begmid/area/morph/years.")
    }

    terminal_x <- current_year - peel

    num %>%
      dplyr::filter(Age == age_to_plot, Year %in% years) %>%
      dplyr::transmute(
        Peel = as.integer(peel),
        X = as.integer(Year),
        N = as.numeric(N)
      ) %>%
      dplyr::filter(X < terminal_x)                            # censor rule
  }

  # ---- read all peels ----
  all <- lapply(peel_range, read_one) %>% dplyr::bind_rows()

  # raw copy for rho (rho computed on raw, regardless of normalization)
  all_raw <- all %>% dplyr::mutate(N_raw = N)

  # ---- Mohn's rho ----
  rho <- NA_real_
  if (show_mohn_rho) {
    rho <- compute_mohn_rho_series(
      df_raw = all_raw,
      current_year = current_year,
      peel_range = peel_range,
      rho_years = rho_years,
      eps = eps
    )
  }

  # ---- optional normalization to Report_0 (by X) ----
  if (normalize_to_report0) {
    base <- all %>% dplyr::filter(Peel == 0) %>% dplyr::select(X, N0 = N)

    if (nrow(base) == 0) {
      stop("Normalization requested, but Report_0 data missing after censoring.\n",
           "Try expanding years or verify current_year / peel_range.")
    }

    all <- all %>%
      dplyr::left_join(base, by = "X") %>%
      dplyr::mutate(
        N = dplyr::case_when(
          normalize_mode == "ratio" ~ N / pmax(N0, eps),
          normalize_mode == "diff"  ~ N - N0,
          TRUE ~ N
        )
      ) %>%
      dplyr::select(-N0)
  }

  # ---- labels ----
  if (is.null(title)) {
    if (!normalize_to_report0) {
      title <- paste0("Retrospective N-at-age ", age_to_plot,
                      " (censored: X < current_year - peel; current_year=", current_year, ")")
    } else {
      nm <- if (normalize_mode == "ratio") "ratio to Report_0" else "difference vs Report_0"
      title <- paste0("Retrospective N-at-age ", age_to_plot,
                      " (", nm, "; censored: X < current_year - peel; current_year=", current_year, ")")
    }
  }

  ylab <- if (!normalize_to_report0) {
    paste0("Numbers at age ", age_to_plot)
  } else if (normalize_mode == "ratio") {
    paste0("N(age ", age_to_plot, ") / N0(age ", age_to_plot, ")")
  } else {
    paste0("N(age ", age_to_plot, ") − N0(age ", age_to_plot, ")")
  }

  # ---- plot ----
  p <- ggplot(all, aes(x = X, y = N, color = factor(Peel), group = Peel)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = title,
      subtitle = paste0(
        "SS-Strict: NUMBERS_AT_AGE report:35 | sex=", sex, " seas=", seas, " begmid=", begmid,
        " area=", area, " morph=", morph, " | censor rule: X < current_year - peel"
      ),
      x = "Year (X)",
      y = ylab,
      color = "Peel"
    ) +
    theme_bw() +
    theme(legend.position = "right", plot.title.position = "plot")

  # reference lines for normalized plots
  if (normalize_to_report0 && add_reference_line) {
    if (normalize_mode == "ratio") {
      p <- p + geom_hline(yintercept = 1.0, linetype = "dashed", color = "black")
    } else {
      p <- p + geom_hline(yintercept = 0.0, linetype = "dashed", color = "black")
    }
  }

  # Mohn's rho annotation
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

