
#' Retrospective cohort diagnostics for Stock Synthesis numbers-at-age
#'
#' Generates retrospective diagnostic plots of cohort strength at a specified age
#' using the \code{NUMBERS_AT_AGE report:35} section from a sequence of Stock Synthesis
#' \code{.sso} report files (e.g., \code{Report_9.sso} through \code{Report_0.sso}).
#' Each cohort is plotted as a separate line across retrospective peels, allowing
#' evaluation of cohort stability as additional years of data are added.
#'
#' The x-axis is expressed as \emph{years removed} (negative peel), defined as
#' \code{-peel}, and the effective terminal year for each peel is
#' \code{current_year - peel}. Observations are censored such that cohort estimates
#' are only plotted when the cohort year is strictly less than the peel-specific
#' terminal year, preventing use of information not available to that retrospective
#' run.
#'
#' Numbers-at-age are parsed SS-Strictly using the explicit column structure of
#' \code{NUMBERS_AT_AGE report:35}, including filtering by area, sex, season,
#' morph, and beginning/mid-year timing. For a given cohort \code{C} and age
#' \code{A}, values are extracted from report year \code{C + A}.
#'
#' Optional normalization to the terminal model (\code{Report_0.sso}) is supported,
#' allowing cohort trajectories to be displayed as ratios or differences relative
#' to the full-data estimate. When normalization is enabled, reference lines are
#' drawn at 1.0 (ratio) or 0.0 (difference) to aid interpretation.
#'
#' @param report_dir Directory containing Stock Synthesis \code{Report_k.sso} files.
#' @param peel_range Integer vector of retrospective peels (e.g., \code{9:0}),
#'   where larger values indicate more terminal years removed.
#' @param cohorts Integer vector of cohort (year-class) identifiers to include.
#' @param age_to_plot Integer age to extract from \code{NUMBERS_AT_AGE report:35}.
#' @param current_year Integer terminal year corresponding to \code{Report_0.sso}.
#'   Used to compute peel terminal years (\code{current_year - peel}) and enforce
#'   cohort censoring.
#' @param sex Integer sex code used in the SS report (e.g., 1 = female).
#' @param seas Integer season code to extract from the SS report.
#' @param begmid Character flag indicating beginning or mid-year values
#'   (typically \code{"B"}).
#' @param area Integer area code to extract from the SS report.
#' @param morph Integer morph code to extract from the SS report.
#' @param normalize_to_report0 Logical; if \code{TRUE}, normalize cohort estimates
#'   to the corresponding value from \code{Report_0.sso}.
#' @param normalize_mode Character; either \code{"ratio"} (estimate / Report\_0) or
#'   \code{"diff"} (estimate − Report\_0).
#' @param eps Small numeric value used to avoid division by zero when normalizing ratios.
#' @param title Optional plot title. If \code{NULL}, a descriptive default is used.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{plot}}{A \code{ggplot2} object showing cohort trajectories over
#'       retrospective peels.}
#'     \item{\code{data}}{A tidy data frame containing cohort, peel, and
#'       numbers-at-age values used in the plot.}
#'   }
#'
#' @details
#' This function is intended for retrospective diagnostics of recruitment and cohort
#' strength in Stock Synthesis models. It uses only \code{NUMBERS_AT_AGE report:35}
#' and enforces SS-Strict censoring rules to prevent leakage of future information
#' into retrospective peels. The function fails fast if required report sections
#' are missing or if filtering criteria result in no parsable data.
#'
#' @examples
#' \dontrun{
#' # Raw cohort trajectories at age 0
#' res <- plot_retro_cohort_lines_over_peel(
#'   report_dir   = ".",
#'   peel_range   = 10:0,
#'   cohorts      = 2010:2025,
#'   age_to_plot  = 0,
#'   current_year = 2025
#' )
#' print(res$plot)
#'
#' # Normalized ratio to Report_0
#' res <- plot_retro_cohort_lines_over_peel(
#'   report_dir   = ".",
#'   peel_range   = 10:0,
#'   cohorts      = 2010:2025,
#'   age_to_plot  = 0,
#'   current_year = 2025,
#'   normalize_to_report0 = TRUE,
#'   normalize_mode = "ratio"
#' )
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

parse_numbers_at_age <- function(lines, years, sex, seas, begmid, area, morph) {
  blk <- find_block(lines, "NUMBERS_AT_AGE report:35")
  blk <- blk[blk != "" & !str_starts(str_trim(blk), "#")]

  hdr  <- str_split(str_trim(blk[1]), "\\s+")[[1]]
  ages <- as.integer(hdr[str_detect(hdr, "^\\d+$")])

  dat_lines <- blk[-1]

  out <- lapply(dat_lines, function(x) {
    parts <- str_split(str_trim(x), "\\s+")[[1]]
    if (length(parts) < 12 + length(ages)) return(NULL)

    # 1 Area, 3 Sex, 7 Morph, 8 Yr, 9 Seas, 11 Beg/Mid, then ages...
    area_i   <- suppressWarnings(as.integer(parts[1]))
    sex_i    <- suppressWarnings(as.integer(parts[3]))
    morph_i  <- suppressWarnings(as.integer(parts[7]))
    yr_i     <- suppressWarnings(as.integer(parts[8]))
    seas_i   <- suppressWarnings(as.integer(parts[9]))
    begmid_i <- parts[11]

    if (any(is.na(c(area_i, sex_i, morph_i, yr_i, seas_i)))) return(NULL)

    if (!(yr_i %in% years) ||
        area_i != area || sex_i != sex ||
        morph_i != morph || seas_i != seas ||
        begmid_i != begmid) return(NULL)

    vals <- suppressWarnings(as.numeric(tail(parts, length(ages))))
    if (anyNA(vals)) return(NULL)

    tibble(Year = yr_i, Age = ages, N = vals)
  })

  bind_rows(out)
}

# ---- Main function (cohort lines, x = -peel, optional normalization) ----
plot_retro_cohort_lines_over_peel <- function(
    report_dir = ".",
    peel_range = 9:0,
    cohorts = 2010:2025,
    age_to_plot = 0L,
    current_year,                 # REQUIRED (e.g., 2025)
    sex = 1L,
    seas = 1L,
    begmid = "B",
    area = 1L,
    morph = 1L,
    normalize_to_report0 = FALSE,
    normalize_mode = c("ratio", "diff"),
    eps = 1e-12,
    title = NULL
) {
  normalize_mode <- match.arg(normalize_mode)

  # For cohort C at age A, it appears in report YEAR = C + A
  years_needed <- sort(unique(as.integer(cohorts) + as.integer(age_to_plot)))

  read_one <- function(peel) {
    f <- file.path(report_dir, paste0("Report_", peel, ".sso"))
    if (!file.exists(f)) stop("Missing file: ", f)
    lines <- readLines(f, warn = FALSE)

    num <- parse_numbers_at_age(
      lines,
      years = years_needed,
      sex = sex, seas = seas, begmid = begmid,
      area = area, morph = morph
    )

    if (nrow(num) == 0) stop("No numbers-at-age parsed for: ", f)

    num %>%
      filter(Age == age_to_plot, Year %in% years_needed) %>%
      mutate(
        Cohort   = Year - Age,
        Peel     = peel,
        Peel_x   = -peel,
        PeelYear = current_year - peel
      ) %>%
      filter(Cohort %in% cohorts) %>%
      # censor: do not plot where cohort >= peel year
      filter(Cohort < PeelYear) %>%
      select(Peel, Peel_x, Cohort, N)
  }

  all <- lapply(peel_range, read_one) %>% bind_rows()

  # ---- Normalize to Report_0 (after censoring) ----
  if (normalize_to_report0) {
    base <- all %>% filter(Peel == 0) %>% select(Cohort, N0 = N)
    if (nrow(base) == 0) {
      stop("Normalization requested but Report_0 has no remaining points after censoring.\n",
           "Try fewer cohorts, different age, or confirm the censor rule you want.")
    }

    all <- all %>%
      left_join(base, by = "Cohort") %>%
      mutate(
        N = case_when(
          normalize_mode == "ratio" ~ N / pmax(N0, eps),
          normalize_mode == "diff"  ~ N - N0,
          TRUE ~ N
        )
      ) %>%
      select(-N0)
  }

  if (is.null(title)) {
    if (!normalize_to_report0) {
      title <- paste0("Retrospective N-at-age ", age_to_plot,
                      " by cohort over peel (current year = ", current_year, ")")
    } else {
      nm <- if (normalize_mode == "ratio") "ratio to Report_0" else "difference vs Report_0"
      title <- paste0("Retrospective N-at-age ", age_to_plot,
                      " by cohort over peel (", nm, "; current year = ", current_year, ")")
    }
  }

  ylab <- if (!normalize_to_report0) {
    paste0("Numbers at age ", age_to_plot)
  } else if (normalize_mode == "ratio") {
    paste0("N(age ", age_to_plot, ") / N0(age ", age_to_plot, ")")
  } else {
    paste0("N(age ", age_to_plot, ") - N0(age ", age_to_plot, ")")
  }

    p <- ggplot(all, aes(x = Peel_x, y = N,
                       group = factor(Cohort),
                       color = factor(Cohort))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = sort(unique(all$Peel_x))) +
    labs(
      title = title,
      subtitle = "peel_year = current_year - peel; censored where Cohort ≥ peel_year\nNUMBERS_AT_AGE report:35",
      x = "Peel (-Current year)",
      y = ylab,
      color = "Cohort"
    )

  # ---- Reference line for normalized plots ----
  if (normalize_to_report0 && normalize_mode == "ratio") {
    p <- p + geom_hline(yintercept = 1.0, linetype = "dashed", color = "black")
  }

  if (normalize_to_report0 && normalize_mode == "diff") {
    p <- p + geom_hline(yintercept = 0.0, linetype = "dashed", color = "black")
  }

  p <- p +
    theme_bw() +
    theme(plot.title.position = "plot")


  list(plot = p, data = all)
}