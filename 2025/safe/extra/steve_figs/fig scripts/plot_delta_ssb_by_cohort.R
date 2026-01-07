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
