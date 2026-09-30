#' Difference in spawning biomass contributions by cohort between two Stock Synthesis models
#'
#' Computes and plots the difference in spawning stock biomass (SSB) contributions
#' by cohort between two Stock Synthesis model runs (e.g., a prior-year model and a
#' full updated model). Cohort-specific SSB contributions are reconstructed from
#' underlying numbers-at-age and fecundity components rather than taken from derived
#' quantities.
#'
#' Spawning biomass contributions are calculated as:
#' \deqn{SSB_{y,c} = \sum_a N_{y,a} \times Fecund_{y,a}}
#' where numbers-at-age (\code{N}) are extracted from
#' \code{NUMBERS_AT_AGE report:35} and fecundity-at-age is extracted from
#' \code{AGE_SELEX report:32} using the \code{fecund} factor. Contributions are then
#' aggregated by cohort, defined as \code{Year − Age}. Ages greater than or equal to
#' \code{plus_age} are combined into a single plus-group labeled
#' \dQuote{Age 10+ cohorts}.
#'
#' The function supports plotting either \code{Full − 2024} or \code{2024 − Full}
#' differences and produces a time series plot showing how cohort-specific SSB
#' contributions differ between the two model configurations over a specified set
#' of years.
#'
#' @param report_2024 Path to the Stock Synthesis \code{.sso} report file for the
#'   comparison (e.g., the 2024 assessment model).
#' @param report_full Path to the Stock Synthesis \code{.sso} report file for the
#'   full or updated assessment model.
#' @param years Integer vector of calendar years over which to compute and plot
#'   SSB differences.
#' @param plus_age Integer age defining the plus group; all ages greater than or
#'   equal to this value are aggregated and labeled as \dQuote{Age 10+ cohorts}.
#' @param delta Character specifying the direction of subtraction:
#'   \code{"full_minus_2024"} (default) or \code{"2024_minus_full"}.
#' @param title Optional plot title. If \code{NULL}, a descriptive default is used.
#'
#' @return A \code{ggplot2} object showing the difference in SSB contribution by cohort
#'   and year. Positive values indicate cohorts contributing more SSB in the model
#'   listed first in \code{delta}.
#'
#' @details
#' This function is SS-Strict and reconstructs spawning biomass directly from
#' model-internal components rather than relying on \code{DERIVED_QUANTITIES}.
#' Fecundity-at-age values are averaged across sexes if multiple rows are present,
#' and numbers-at-age are summed across sexes prior to computing cohort contributions.
#'
#' The plus group is visually emphasized in the plot using a black dashed line and
#' points, while individual cohorts are shown in color. A horizontal reference line
#' at zero is included to highlight the sign and magnitude of differences.
#'
#' @examples
#' \dontrun{
#' # Difference in cohort SSB contributions between 2024 and full models
#' p <- plot_delta_ssb_by_cohort(
#'   report_2024 = "Report_2024.sso",
#'   report_full = "Report_full.sso",
#'   years = 2020:2027,
#'   plus_age = 10,
#'   delta = "full_minus_2024"
#' )
#' print(p)
#'
#' # Reverse subtraction (2024 − Full)
#' p <- plot_delta_ssb_by_cohort(
#'   report_2024 = "Report_2024.sso",
#'   report_full = "Report_full.sso",
#'   years = 2020:2027,
#'   delta = "2024_minus_full"
#' )
#' print(p)
#' }
#'
#' @export

plot_delta_ssb_by_cohort <- function(report_2024,
                                    report_full,
                                    years = 2020:2027,
                                    plus_age = 10,
                                    delta = c("full_minus_2024","2024_minus_full"),
                                    title = NULL) {
  delta <- match.arg(delta)

  read_block <- function(path, header) {
    x <- readLines(path, warn = FALSE)
    i0 <- which(startsWith(trimws(x), header))
    if (!length(i0)) stop("Header not found: ", header)
    i0 <- i0[1] + 1
    i1 <- i0
    while (i1 <= length(x) && !grepl("^[A-Z0-9_().]+.*\\sreport:\\d+", trimws(x[i1]))) i1 <- i1 + 1
    x[i0:(i1-1)]
  }

  parse_age_header <- function(lines) {
    # AGE_SELEX has a "Factor ..." header; other tables have a numeric age header
    for (k in seq_along(lines)) {
      if (grepl("^\\s*Factor\\b", lines[k])) {
        toks <- strsplit(trimws(lines[k]), "\\s+")[[1]]
        ages <- as.integer(toks[grepl("^\\d+$", toks)])
        return(list(idx=k, ages=ages))
      }
    }
    for (k in seq_along(lines)) {
      toks <- strsplit(trimws(lines[k]), "\\s+")[[1]]
      ints <- toks[grepl("^\\d+$", toks)]
      if ("0" %in% ints && length(ints) >= 6) {
        return(list(idx=k, ages=as.integer(ints)))
      }
    }
    stop("Could not find an age header line.")
  }

  extract_fecund <- function(path, years_keep) {
    blk <- read_block(path, "AGE_SELEX report:32")
    blk <- blk[nzchar(trimws(blk))]
    h <- parse_age_header(blk)
    ages <- h$ages; nA <- length(ages)

    rows <- list()
    for (ln in blk[(h$idx+1):length(blk)]) {
      p <- strsplit(trimws(ln), "\\s+")[[1]]
      if (!length(p)) next
      if (tolower(p[1]) != "fecund") next
      yr <- as.integer(p[3])
      if (!(yr %in% years_keep)) next
      vals <- as.numeric(tail(p, nA))
      rows[[length(rows)+1]] <- data.frame(Year=yr, Age=ages, Fecund=vals)
    }
    dplyr::bind_rows(rows) |>
      dplyr::group_by(Year, Age) |>
      dplyr::summarise(Fecund=mean(Fecund), .groups="drop")
  }

  extract_numbers <- function(path, years_keep) {
    blk <- read_block(path, "NUMBERS_AT_AGE report:35")
    blk <- blk[nzchar(trimws(blk))]
    h <- parse_age_header(blk)
    ages <- h$ages; nA <- length(ages)

    rows <- list()
    for (ln in blk[(h$idx+1):length(blk)]) {
      s <- trimws(ln)
      if (!nzchar(s) || grepl("^#", s)) next
      p <- strsplit(s, "\\s+")[[1]]
      if (!("B" %in% p)) next  # both sexes
      yr_tok <- p[grepl("^\\d{4}$", p)][1]
      if (is.na(yr_tok)) next
      yr <- as.integer(yr_tok)
      if (!(yr %in% years_keep)) next
      vals <- suppressWarnings(as.numeric(tail(p, nA)))
      if (anyNA(vals)) next
      rows[[length(rows)+1]] <- data.frame(Year=yr, Age=ages, N=vals)
    }
    dplyr::bind_rows(rows) |>
      dplyr::group_by(Year, Age) |>
      dplyr::summarise(N=sum(N), .groups="drop")
  }

  ssb_by_cohort <- function(path) {
    fec <- extract_fecund(path, years)
    naa <- extract_numbers(path, years)
    df <- dplyr::inner_join(naa, fec, by=c("Year","Age")) |>
      dplyr::mutate(SSBcontrib = N * Fecund,
                    Cohort = dplyr::if_else(Age >= plus_age,
                                           "Age 10+ cohorts",
                                           as.character(Year - Age))) |>
      dplyr::group_by(Year, Cohort) |>
      dplyr::summarise(SSBcontrib = sum(SSBcontrib), .groups="drop")
    df
  }

  d24   <- ssb_by_cohort(report_2024) |> dplyr::rename(SSB_2024 = SSBcontrib)
  dfull <- ssb_by_cohort(report_full) |> dplyr::rename(SSB_full = SSBcontrib)

  d <- dplyr::full_join(dfull, d24, by=c("Year","Cohort")) |>
    dplyr::mutate(SSB_full = dplyr::coalesce(SSB_full, 0),
                  SSB_2024 = dplyr::coalesce(SSB_2024, 0),
                  Delta = if (delta=="full_minus_2024") SSB_full - SSB_2024 else SSB_2024 - SSB_full)

  # order cohorts with plus group first
  cohort_levels <- d |>
    dplyr::distinct(Cohort) |>
    dplyr::mutate(ord = dplyr::if_else(Cohort=="Age 10+ cohorts", -Inf, as.numeric(Cohort))) |>
    dplyr::arrange(ord) |>
    dplyr::pull(Cohort)

  d$Cohort <- factor(d$Cohort, levels=cohort_levels)

  if (is.null(title)) {
    title <- paste0("Difference in SSB by cohort: ",
                    ifelse(delta=="full_minus_2024","Full − 2024","2024 − Full"),
                    " (", min(years), "–", max(years), ")")
  }

  library(ggplot2)
  p <- ggplot(d, aes(x=Year, y=Delta, color=Cohort, group=Cohort)) +
    geom_hline(yintercept=0) +
    geom_line(linewidth=1) +
    geom_point(size=2) +
    # force plus group styling
    geom_line(data = subset(d, Cohort=="Age 10+ cohorts"),
              aes(x=Year, y=Delta, group=Cohort),
              color="black", linetype="dashed", linewidth=1.2, inherit.aes=FALSE) +
    geom_point(data = subset(d, Cohort=="Age 10+ cohorts"),
               aes(x=Year, y=Delta),
               color="black", size=2, inherit.aes=FALSE) +
    scale_x_continuous(breaks=years) +
    labs(title=title,
         x="Year",
         y="Δ SSB contribution (N × fecund units)",
         color="Cohort") +
    theme_bw() +
    theme(legend.position="right")

  return(p)
}

# Example:
# p <- plot_delta_ssb_by_cohort("Report_2024.sso", "Report_full.sso", years=2020:2027)
# print(p)
