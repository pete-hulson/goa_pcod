#' Plot F (derived from cohort survival) by year and cohort (Full model)
#'
#' SS-Strict:
#'   Z_{y,a} = ln(N_{y,a} / N_{y+1,a+1})
#'   F_{y,a} = Z_{y,a} - M
#'
#' Sources (from the .sso report file):
#'   - NUMBERS_AT_AGE report:35  (Both sexes row "B")
#'
#' Cohort definition:
#'   cohort = Year - Age
#'
#' Age 10+ handling:
#'   Excludes ages >= plus_age (e.g., 10+) so plus-group artifacts are removed.
#'
#' @param report_file Path to SS report (.sso), e.g. "Report_full.sso"
#' @param years Integer vector of years to plot (e.g., 2020:2027)
#' @param M Natural mortality (scalar), e.g. 0.485
#' @param plus_age Age that is a plus group (exclude ages >= plus_age); default 10
#' @param title Optional plot title
#' @return A ggplot object, plus invisibly returns the underlying data.frame
#'
#' @Example
#' library(dplyr); library(tidyr); library(ggplot2)
#' res <- plot_F_by_year_and_cohort("Report_full.sso", years = 2020:2027, M = 0.485, plus_age = 10)
#' head(res$data)

plot_F_by_year_and_cohort <- function(report_file,
                                      years = 2020:2027,
                                      M = 0.485,
                                      plus_age = 10,
                                      title = NULL) {

  # ---- helpers ----
  read_block <- function(path, header) {
    x <- readLines(path, warn = FALSE)
    i0 <- which(startsWith(trimws(x), header))
    if (!length(i0)) stop("Header not found: ", header)
    i0 <- i0[1] + 1
    i1 <- i0
    while (i1 <= length(x) && !grepl("^[A-Z0-9_().]+.*\\sreport:\\d+", trimws(x[i1]))) i1 <- i1 + 1
    blk <- x[i0:(i1-1)]
    blk[nzchar(trimws(blk))]
  }

  parse_age_header <- function(lines) {
    # NUMBERS_AT_AGE tables typically have a header line with many integers including 0
    for (k in seq_along(lines)) {
      toks <- strsplit(trimws(lines[k]), "\\s+")[[1]]
      ints <- toks[grepl("^\\d+$", toks)]
      if ("0" %in% ints && length(ints) >= 6) {
        return(list(idx = k, ages = as.integer(ints)))
      }
    }
    stop("Could not find age header in NUMBERS_AT_AGE block.")
  }

  # ---- parse NUMBERS_AT_AGE report:35 ----
  blk <- read_block(report_file, "NUMBERS_AT_AGE report:35")
  h <- parse_age_header(blk)
  ages_all <- h$ages
  nA <- length(ages_all)

  # Build a Year x Age table of N from "B" rows
  rows <- list()
  for (ln in blk[(h$idx+1):length(blk)]) {
    s <- trimws(ln)
    if (!nzchar(s) || grepl("^#", s)) next
    p <- strsplit(s, "\\s+")[[1]]
    if (!("B" %in% p)) next

    yr_tok <- p[grepl("^\\d{4}$", p)][1]
    if (is.na(yr_tok)) next
    yr <- as.integer(yr_tok)

    # keep a buffer year for transitions
    if (!(yr %in% c(years, max(years)+1))) next

    vals <- suppressWarnings(as.numeric(tail(p, nA)))
    if (anyNA(vals)) next

    # store only ages < plus_age
    keep <- ages_all < plus_age
    rows[[length(rows)+1]] <- data.frame(
      Year = yr,
      Age  = ages_all[keep],
      N    = vals[keep]
    )
  }

  dfN <- dplyr::bind_rows(rows) |>
    dplyr::group_by(Year, Age) |>
    dplyr::summarise(N = sum(N), .groups = "drop")

  # Convert to wide for simple lookup
  Nmat <- tidyr::pivot_wider(dfN, names_from = Age, values_from = N)
  Nmat <- dplyr::arrange(Nmat, Year)

  # helper to get N(y,a)
  getN <- function(y, a) {
    r <- Nmat[Nmat$Year == y, as.character(a)]
    if (nrow(r) == 0) return(NA_real_)
    as.numeric(r[[1]])
  }

  # ---- compute Z and F by year/cohort ----
  out <- list()
  for (y in years) {
    for (a in sort(unique(dfN$Age))) {
      # need y+1 and a+1 for transition
      if (!(y + 1 %in% c(years, max(years)+1))) next
      if ((a + 1) >= plus_age) next

      n1 <- getN(y, a)
      n2 <- getN(y + 1, a + 1)
      if (is.na(n1) || is.na(n2) || n1 <= 0 || n2 <= 0) next

      Z <- log(n1 / n2)
      F <- Z - M
      cohort <- y - a

      out[[length(out)+1]] <- data.frame(
        Year = y,
        Cohort = cohort,
        Age = a,
        Z = Z,
        F = F
      )
    }
  }

  dfF <- dplyr::bind_rows(out)

  # ---- plot ----
  if (is.null(title)) {
    title <- paste0("Fishing mortality (F) by year and cohort (", min(years), "–", max(years),
                    ")\nM = ", M, ", Age ", plus_age, "+ excluded")
  }

  p <- ggplot2::ggplot(dfF, ggplot2::aes(x = Year, y = F, group = Cohort, color = factor(Cohort))) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(breaks = years) +
    ggplot2::labs(title = title, x = "Year", y = "Fishing mortality F", color = "Cohort") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "right")

  print(p)
  invisible(list(plot = p, data = dfF))
}


