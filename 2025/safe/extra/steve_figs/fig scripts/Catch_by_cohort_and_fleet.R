#' Plot catch by cohort and fleet (lines), toggling between numbers and biomass
#'
#' SS-Strict sources (from Report_full.sso):
#'   - Numbers:  CATCH_AT_AGE report:40   (Catch numbers at age by fleet/year)
#'   - Bodywt:   AGE_SELEX report:32      (factor = bodywt, kg/fish, by fleet/year)
#'
#' Cohort definition:
#'   cohort = Year - Age, except Age==plus_age is treated as a plus-group cohort ("Age 10+ cohorts")
#'
#' @param report_file Path to SS .sso report file (e.g., "Report_full.sso")
#' @param years Integer vector of years to plot (e.g., 2020:2027)
#' @param fleets Integer vector of fleets to include (default 1:3)
#' @param fleet_names Named character vector mapping fleet IDs to labels
#' @param plus_age Integer plus-group age (default 10)
#' @param metric "biomass" (metric tons) or "numbers" (as reported in CATCH_AT_AGE)
#' @param numbers_scale Multiply numbers by this scalar for display (e.g., 1 for "numbers",
#'        1000 if report is in thousands and you want individuals, etc.)
#' @param title Optional plot title (character)
#' @return A ggplot object
#'
plot_catch_by_cohort_fleet <- function(report_file,
                                       years = 2020:2027,
                                       fleets = c(1,2,3),
                                       fleet_names = c(`1`="Trawl", `2`="Longline", `3`="Pot"),
                                       plus_age = 10,
                                       metric = c("biomass","numbers"),
                                       numbers_scale = 1,
                                       title = NULL,
                                       Ylim =c(0,1500)) {

  metric <- match.arg(metric)

  # -------- helpers --------
  read_report_block <- function(path, header) {
    x <- readLines(path, warn = FALSE)
    start <- which(trimws(x) == header)
    if (length(start) == 0) stop("Header not found: ", header)
    start <- start[1] + 1

    # End at next "XXXX report:n" line or EOF
    end <- start
    while (end <= length(x) &&
           !grepl("^[A-Z0-9_().]+.*\\sreport:\\d+", trimws(x[end]))) {
      end <- end + 1
    }
    x[start:(end-1)]
  }

  parse_age_header <- function(line) {
    toks <- strsplit(line, "\\s+")[[1]]
    toks <- toks[nzchar(toks)]
    as.integer(toks[grepl("^\\d+$", toks)])
  }

  # -------- parse CATCH_AT_AGE report:40 (numbers) --------
  catch_blk <- read_report_block(report_file, "CATCH_AT_AGE report:40")
  # First non-empty line is header with ages
  catch_blk <- catch_blk[nzchar(trimws(catch_blk))]
  age_line <- catch_blk[1]
  ages <- parse_age_header(age_line)
  if (length(ages) == 0) stop("Could not parse ages from CATCH_AT_AGE header line.")
  nA <- length(ages)

  # data lines
  catch_dat <- catch_blk[-1]
  catch_rows <- lapply(catch_dat, function(s) {
    p <- strsplit(trimws(s), "\\s+")[[1]]
    if (length(p) < (nA + 3)) return(NULL)
    # Fleet is second token in SS CATCH_AT_AGE tables
    if (!grepl("^\\d+$", p[2])) return(NULL)
    fl <- as.integer(p[2])
    if (!(fl %in% fleets)) return(NULL)

    yr_tok <- p[grepl("^\\d{4}$", p)][1]
    if (is.na(yr_tok)) return(NULL)
    yr <- as.integer(yr_tok)
    if (!(yr %in% years)) return(NULL)

    tail <- p[(length(p)-nA+1):length(p)]
    suppressWarnings(vals <- as.numeric(tail))
    if (anyNA(vals)) return(NULL)

    data.frame(Fleet = fl, Year = yr, Age = ages, CatchN = vals)
  })
  catchN <- do.call(rbind, catch_rows)
  if (is.null(catchN) || nrow(catchN) == 0) stop("No catch-at-age rows parsed. Check report file.")

  # -------- parse AGE_SELEX report:32 (bodywt) when needed --------
  if (metric == "biomass") {
    selex_blk <- read_report_block(report_file, "AGE_SELEX report:32")
    selex_blk <- selex_blk[nzchar(trimws(selex_blk))]
    # Find header starting with "Factor"
    hdr_i <- which(grepl("^Factor\\b", selex_blk))
    if (length(hdr_i) == 0) stop("Could not find AGE_SELEX header line beginning with 'Factor'.")
    hdr_i <- hdr_i[1]
    selex_hdr <- selex_blk[hdr_i]
    selex_ages <- parse_age_header(selex_hdr)
    if (length(selex_ages) != nA || any(selex_ages != ages)) {
      # Not fatal; but SS typically matches age structure. We'll still proceed using selex_ages.
      ages_wt <- selex_ages
    } else {
      ages_wt <- ages
    }
    nAw <- length(ages_wt)

    selex_lines <- selex_blk[(hdr_i+1):length(selex_blk)]
    bodywt_rows <- lapply(selex_lines, function(s) {
      p <- strsplit(trimws(s), "\\s+")[[1]]
      if (length(p) < (nAw + 3)) return(NULL)
      if (tolower(p[1]) != "bodywt") return(NULL)
      fl <- suppressWarnings(as.integer(p[2]))
      if (is.na(fl) || !(fl %in% fleets)) return(NULL)
      yr <- suppressWarnings(as.integer(p[3]))
      if (is.na(yr) || !(yr %in% years)) return(NULL)

      tail <- p[(length(p)-nAw+1):length(p)]
      suppressWarnings(vals <- as.numeric(tail))
      if (anyNA(vals)) return(NULL)

      data.frame(Fleet = fl, Year = yr, Age = ages_wt, BodyWt_kg = vals)
    })
    bodywt <- do.call(rbind, bodywt_rows)
    if (is.null(bodywt) || nrow(bodywt) == 0) stop("No bodywt rows parsed for requested years/fleets.")
  }

  # -------- compute cohort series --------
  library(dplyr)
  library(ggplot2)

  df <- catchN %>%
    mutate(Cohort = ifelse(Age >= plus_age, "Age 10+ cohorts", as.character(Year - Age)),
           FleetLab = unname(fleet_names[as.character(Fleet)]),
           # ensure plus-age is treated as plus group in label only; we already binned by Age>=plus_age
           Value = CatchN * numbers_scale)

  if (metric == "biomass") {
    df <- df %>%
      left_join(bodywt, by = c("Fleet","Year","Age")) %>%
      mutate(Value = (CatchN * BodyWt_kg) / 1000) # metric tons
  }

  # Aggregate across ages into cohort within fleet-year
  dfC <- df %>%
    group_by(FleetLab, Year, Cohort) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  # Put plus group first in legend
  cohort_levels <- dfC %>%
    distinct(Cohort) %>%
    mutate(ord = ifelse(Cohort == "Age 10+ cohorts", -Inf, as.numeric(Cohort))) %>%
    arrange(ord) %>%
    pull(Cohort)

  dfC$Cohort <- factor(dfC$Cohort, levels = cohort_levels)

  ylab <- if (metric == "biomass") "Catch biomass (metric tons)" else "Catch (numbers)"
  if (metric == "numbers" && numbers_scale != 1) {
    ylab <- paste0(ylab, " × ", numbers_scale)
  }
  if (is.null(title)) {
    title <- paste0("Catch ", ifelse(metric=="biomass","biomass","numbers"),
                    " by cohort and fleet (", min(years), "–", max(years), ")")
  }

  p <- ggplot(dfC, aes(x = Year, y = Value, group = Cohort, color = Cohort)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # make plus-group black dashed
    geom_line(data = dfC %>% filter(Cohort == "Age 10+ cohorts"),
              aes(x = Year, y = Value, group = Cohort),
              color = "black", linetype = "dashed", linewidth = 1.1, inherit.aes = FALSE) +
    geom_point(data = dfC %>% filter(Cohort == "Age 10+ cohorts"),
               aes(x = Year, y = Value),
               color = "black", size = 2, inherit.aes = FALSE) +
    facet_wrap(~FleetLab, nrow = 1, scales = "free_y") +
    labs(title = title, x = "Year", y = ylab, color = "Cohort") +ylim(Ylim) +
    theme_bw() +
    theme(legend.position = "right")

  return(p)
}

# ---------------- Example usage ----------------
# library(ggplot2)
# library(patchwork)
#
# p1 <- plot_catch_by_cohort_fleet("Report_full.sso", years=2020:2027, metric="biomass")
# p2 <- plot_catch_by_cohort_fleet("Report_full.sso", years=2020:2027, metric="numbers", numbers_scale=1)
# p1/p2+plot_layout(heights=c(2,2),guides="collect")
#
# p24 <- plot_catch_by_cohort_fleet("Report_2024.sso", years=2020:2027, metric="numbers", numbers_scale=1,title="2024 Model",Ylim=(0,1500))
# p25 <- plot_catch_by_cohort_fleet("Report_full.sso", years=2020:2027, metric="numbers", numbers_scale=1,title="2025 Model",Ylim=(0,1500))
# p24/p25+plot_layout(heights=c(2,2),guides="collect")