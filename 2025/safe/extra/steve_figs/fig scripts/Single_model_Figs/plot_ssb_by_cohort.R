#' Plot spawning biomass by cohort (absolute or percent) from an SS .sso report
#'
#' SS-Strict sources in report file:
#'   - NUMBERS_AT_AGE report:35  (female, seas=1, Beg/Mid=B, area=1, morph=1)
#'   - AGE_SELEX report:32       (factor Fecund; fecundity = maturity * weight-at-age)
#'
#' Plus-group handling:
#'   - Age >= plus_age is treated as a plus group and plotted as "Age 10+ cohorts"
#'
#' @param report_file Path to SS report (.sso), e.g. "Report_full.sso"
#' @param years Integer vector, e.g. 2020:2027
#' @param mode "ssb" for absolute cohort contribution, "percent" for percent of total SSB
#' @param plus_age Plus-group age (default 10)
#' @param seas Sea to use (default 1)
#' @param sex Sex to use in both tables (default 1 = female)
#' @param begmid "B" or "M" for numbers-at-age (default "B")
#' @param area Area filter for numbers-at-age (default 1)
#' @param morph Morph filter for numbers-at-age (default 1)
#' @param title Optional plot title
#' @return list(plot=ggplot, data=data.frame)
#'
#' @example
#' library(ggplot2)
#' library(patchwork)
#' res1 <- plot_ssb_by_cohort("Report_full.sso", years=2020:2027, mode="ssb")
#' print(res1$plot)
#'
#' res2 <- plot_ssb_by_cohort("Report_full.sso", years=2020:2027, mode="percent")
#' print(res2$plot)
#' res1$plot/res2$plot+plot_layout(heights=c(2,2),guides="collect")
#' res1 <- plot_ssb_by_cohort("Report_full.sso", years=2025:2027, mode="ssb", title="2025 Model cohort contribution to SSB")
#' print(res1$plot)


plot_ssb_by_cohort <- function(report_file,
                               years = 2020:2027,
                               mode = c("ssb", "percent"),
                               plus_age = 10L,
                               seas = 1L,
                               sex = 1L,
                               begmid = "B",
                               area = 1L,
                               morph = 1L,
                               title = NULL,
                               Ylim=c(0,3500)) {
  mode <- match.arg(mode)

  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(ggplot2)
    library(tibble)
  })

  # ---------------------------
  # Helpers
  # ---------------------------
  find_block <- function(lines, header) {
    start <- which(str_trim(lines) == header)
    if (length(start) == 0) stop("Header not found: ", header)
    start <- start[1]

    next_idx <- which(seq_along(lines) > start &
                        str_detect(lines, "^[A-Z0-9_().]+.*\\sreport:\\d+\\s*$"))
    end <- if (length(next_idx) == 0) length(lines) + 1 else next_idx[1]
    lines[(start + 1):(end - 1)]
  }

  parse_fecund <- function(lines, years, seas, sex) {
    blk <- find_block(lines, "AGE_SELEX report:32")
    blk <- blk[blk != "" & !str_starts(str_trim(blk), "#")]

    hdr_i <- which(str_starts(str_trim(blk), "Factor"))[1]
    if (is.na(hdr_i)) stop("AGE_SELEX header line not found (Factor ...)")

    hdr  <- str_split(str_trim(blk[hdr_i]), "\\s+")[[1]]
    ages <- as.integer(hdr[str_detect(hdr, "^\\d+$")])

    dat_lines <- blk[(hdr_i + 1):length(blk)]
    out <- lapply(dat_lines, function(x) {
      parts <- str_split(str_trim(x), "\\s+")[[1]]
      if (length(parts) < 7 + length(ages)) return(NULL)
      if (tolower(parts[1]) != "fecund") return(NULL)

      yr    <- suppressWarnings(as.integer(parts[3]))
      seas_i <- suppressWarnings(as.integer(parts[4]))
      sex_i  <- suppressWarnings(as.integer(parts[5]))
      if (any(is.na(c(yr, seas_i, sex_i)))) return(NULL)
      if (!(yr %in% years) || seas_i != seas || sex_i != sex) return(NULL)

      vals <- suppressWarnings(as.numeric(tail(parts, length(ages))))
      if (anyNA(vals)) return(NULL)

      tibble(Year = yr, Age = ages, Fecund = vals)
    })
    bind_rows(out)
  }

  parse_numbers_at_age <- function(lines, years, sex, seas, begmid, area, morph) {
    blk <- find_block(lines, "NUMBERS_AT_AGE report:35")
    blk <- blk[blk != "" & !str_starts(str_trim(blk), "#")]

    hdr <- str_split(str_trim(blk[1]), "\\s+")[[1]]
    ages <- as.integer(hdr[str_detect(hdr, "^\\d+$")])

    dat_lines <- blk[-1]

    # Positions (1-indexed) per SS header:
    # 1 Area, 2 Bio_Pattern, 3 Sex, 4 BirthSeas, 5 Settlement, 6 Platoon, 7 Morph,
    # 8 Yr, 9 Seas, 10 Time, 11 Beg/Mid, 12 Era, then ages...
    out <- lapply(dat_lines, function(x) {
      parts <- str_split(str_trim(x), "\\s+")[[1]]
      if (length(parts) < 12 + length(ages)) return(NULL)

      area_i   <- suppressWarnings(as.integer(parts[1]))
      sex_i    <- suppressWarnings(as.integer(parts[3]))
      morph_i  <- suppressWarnings(as.integer(parts[7]))
      yr_i     <- suppressWarnings(as.integer(parts[8]))
      seas_i   <- suppressWarnings(as.integer(parts[9]))
      begmid_i <- parts[11]

      if (any(is.na(c(area_i, sex_i, morph_i, yr_i, seas_i)))) return(NULL)
      if (!(yr_i %in% years) || area_i != area || sex_i != sex ||
          morph_i != morph || seas_i != seas || begmid_i != begmid) return(NULL)

      vals <- suppressWarnings(as.numeric(tail(parts, length(ages))))
      if (anyNA(vals)) return(NULL)

      tibble(Year = yr_i, Age = ages, N = vals)
    })
    bind_rows(out)
  }

  # ---------------------------
  # Parse inputs
  # ---------------------------
  lines <- readLines(report_file, warn = FALSE)

  fec <- parse_fecund(lines, years = years, seas = seas, sex = sex)
  num <- parse_numbers_at_age(lines, years = years, sex = sex, seas = seas,
                              begmid = begmid, area = area, morph = morph)

  if (nrow(fec) == 0) stop("No fecundity rows parsed. Check AGE_SELEX filters/format.")
  if (nrow(num) == 0) stop("No numbers-at-age rows parsed. Check NUMBERS_AT_AGE filters/format.")

  # ---------------------------
  # Compute cohort contributions
  # ---------------------------
  dat <- num %>%
    inner_join(fec, by = c("Year", "Age")) %>%
    mutate(
      Cohort_plot = if_else(Age >= plus_age, "Age 10+ cohorts", as.character(Year - Age)),
      SSB_contrib = N * Fecund
    ) %>%
    group_by(Year, Cohort_plot) %>%
    summarise(SSB_contrib = sum(SSB_contrib), .groups = "drop")

  if (mode == "percent") {
    dat <- dat %>%
      group_by(Year) %>%
      mutate(SSB_total = sum(SSB_contrib),
             Value = if_else(SSB_total > 0, 100 * SSB_contrib / SSB_total, 0)) %>%
      ungroup()
    ylab <- "Percent of total SSB"
  } else {
    dat <- dat %>% mutate(Value = SSB_contrib)
    ylab <- "SSB contribution (t)"
  }

  # Order cohorts: Age 10+ first, then oldest -> youngest
  cohort_levels <- dat %>%
    distinct(Cohort_plot) %>%
    mutate(sort_key = if_else(Cohort_plot == "Age 10+ cohorts", -1e9, as.numeric(Cohort_plot))) %>%
    arrange(sort_key) %>%
    pull(Cohort_plot)

  dat$Cohort_plot <- factor(dat$Cohort_plot, levels = cohort_levels)

  # Colors: distinct, non-repeating, force Age 10+ black
  n_coh <- length(cohort_levels)
  pal <- grDevices::hcl.colors(n_coh, palette = "Dynamic")
  names(pal) <- cohort_levels
  pal["Age 10+ cohorts"] <- "black"

  # Linetype: Age 10+ dashed
  lt <- rep("solid", n_coh)
  names(lt) <- cohort_levels
  lt["Age 10+ cohorts"] <- "dashed"

  if (is.null(title)) {
    title <- if (mode == "percent") {
      "Percent contribution to spawning biomass by cohort"
    } else {
      "Contribution of each cohort to SSB by year"
    }
  }

  p <- ggplot(dat, aes(x = Year, y = Value, color = Cohort_plot, linetype = Cohort_plot, group = Cohort_plot)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = years) +
    scale_color_manual(values = pal, name = "Cohort") +
    scale_linetype_manual(values = lt, name = "Cohort") + ylim(Ylim) +
    labs(
      title = title,
      subtitle = paste0(min(years), "–", max(years), " (Age ", plus_age, "+ treated as plus group)"),
      x = "Year",
      y = ylab
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      plot.title.position = "plot"
    )

  list(plot = p, data = dat)
}

