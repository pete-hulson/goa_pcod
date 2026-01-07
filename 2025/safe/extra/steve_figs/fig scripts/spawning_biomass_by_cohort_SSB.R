# Cohort contributions to SSB by year (LINES) — Full model, 2020–2027
# SS-Strict sources in Report_full.sso:
#   - NUMBERS_AT_AGE report:35 (female, seas=1, Beg/Mid=B, area=1, morph=1)
#   - AGE_SELEX report:32      (Fecund = maturity * weight-at-age; female, seas=1)
#
# Key fix:
#   - Age 10 is a plus group (10+). Do NOT assign Age 10 to a birth-year cohort.
#     Instead aggregate all Age 10 contributions as "Age 10+ cohorts".
# Styling:
#   - "Age 10+ cohorts" line is BLACK and DASHED.
#
# Dependencies: tidyverse (dplyr, stringr, ggplot2)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(readr)
})

report_path  <- "Report_full.sso"  # <-- set your path
years_target <- 2020:2027
plus_age     <- 10L  # Age 10 column represents 10+

# ---------------------------
# Helpers
# ---------------------------
read_lines_file <- function(path) readLines(path, warn = FALSE)

find_block <- function(lines, header) {
  start <- which(str_trim(lines) == header)
  if (length(start) == 0) stop("Header not found: ", header)
  start <- start[1]

  # Next report header looks like "... report:<n>"
  next_idx <- which(seq_along(lines) > start &
                      str_detect(lines, "^[A-Z0-9_().]+.*\\sreport:\\d+\\s*$"))
  end <- if (length(next_idx) == 0) length(lines) + 1 else next_idx[1]

  lines[(start + 1):(end - 1)]
}

# ---------------------------
# Parse AGE_SELEX report:32 -> fecundity-at-age by year
# ---------------------------
parse_fecund <- function(lines, years = 2020:2027, seas = 1, sex = 1) {
  blk <- find_block(lines, "AGE_SELEX report:32")

  hdr_i <- which(str_starts(str_trim(blk), "Factor"))[1]
  if (is.na(hdr_i)) stop("AGE_SELEX header line not found (Factor ...)")

  hdr  <- str_split(str_trim(blk[hdr_i]), "\\s+")[[1]]
  ages <- as.integer(hdr[str_detect(hdr, "^\\d+$")])

  dat_lines <- blk[(hdr_i + 1):length(blk)]
  dat_lines <- dat_lines[dat_lines != "" & !str_starts(str_trim(dat_lines), "#")]

  out <- lapply(dat_lines, function(x) {
    parts <- str_split(str_trim(x), "\\s+")[[1]]
    if (length(parts) < 7 + length(ages)) return(NULL)
    if (parts[1] != "Fecund") return(NULL)

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

# ---------------------------
# Parse NUMBERS_AT_AGE report:35 -> numbers-at-age by year
# ---------------------------
parse_numbers_at_age <- function(lines, years = 2020:2027,
                                 sex = 1, seas = 1, begmid = "B",
                                 area = 1, morph = 1) {
  blk <- find_block(lines, "NUMBERS_AT_AGE report:35")
  hdr <- str_split(str_trim(blk[1]), "\\s+")[[1]]
  ages <- as.integer(hdr[str_detect(hdr, "^\\d+$")])

  dat_lines <- blk[-1]
  dat_lines <- dat_lines[dat_lines != "" & !str_starts(str_trim(dat_lines), "#")]

  # Positions (1-indexed) per SS header:
  # 1 Area, 2 Bio_Pattern, 3 Sex, 4 BirthSeas, 5 Settlement, 6 Platoon, 7 Morph,
  # 8 Yr, 9 Seas, 10 Time, 11 Beg/Mid, 12 Era, then ages...
  out <- lapply(dat_lines, function(x) {
    parts <- str_split(str_trim(x), "\\s+")[[1]]
    if (length(parts) < 12 + length(ages)) return(NULL)

    area_i  <- suppressWarnings(as.integer(parts[1]))
    sex_i   <- suppressWarnings(as.integer(parts[3]))
    morph_i <- suppressWarnings(as.integer(parts[7]))
    yr_i    <- suppressWarnings(as.integer(parts[8]))
    seas_i  <- suppressWarnings(as.integer(parts[9]))
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
# Main
# ---------------------------
lines <- read_lines_file(report_path)

fec <- parse_fecund(lines, years = years_target, seas = 1, sex = 1)
num <- parse_numbers_at_age(lines, years = years_target, sex = 1, seas = 1, begmid = "B", area = 1, morph = 1)

if (nrow(fec) == 0) stop("No fecundity rows parsed. Check AGE_SELEX report:32 filters/format.")
if (nrow(num) == 0) stop("No numbers-at-age rows parsed. Check NUMBERS_AT_AGE report:35 filters/format.")

# Cohort SSB contributions (absolute)
plot_dat <- num %>%
  inner_join(fec, by = c("Year", "Age")) %>%
  mutate(
    Cohort = if_else(Age >= plus_age, "Age 10+ cohorts", as.character(Year - Age)),
    SSB_contrib = N * Fecund
  ) %>%
  group_by(Year, Cohort) %>%
  summarize(SSB_contrib = sum(SSB_contrib), .groups = "drop")

# Order cohorts: Age 10+ first, then oldest -> youngest
cohort_levels <- plot_dat %>%
  distinct(Cohort) %>%
  mutate(sort_key = if_else(Cohort == "Age 10+ cohorts", -1e9, as.numeric(Cohort))) %>%
  arrange(sort_key) %>%
  pull(Cohort)

plot_dat$Cohort <- factor(plot_dat$Cohort, levels = cohort_levels)

# Colors: distinct for cohorts, force Age 10+ to black
n_coh <- length(cohort_levels)
pal <- grDevices::hcl.colors(n_coh, palette = "Dynamic")
names(pal) <- cohort_levels
pal["Age 10+ cohorts"] <- "black"

# Linetypes: Age 10+ dashed, others solid
lt <- rep("solid", n_coh)
names(lt) <- cohort_levels
lt["Age 10+ cohorts"] <- "dashed"

# Plot
ggplot(plot_dat, aes(x = Year, y = SSB_contrib, color = Cohort, linetype = Cohort, group = Cohort)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = years_target) +
  scale_color_manual(values = pal, name = "Cohort") +
  scale_linetype_manual(values = lt, name = "Cohort") +
  labs(
    title = "Contribution of each cohort to SSB by year (Full model)",
    subtitle = "2020–2027 (Age 10+ treated as plus group)",
    x = "Year",
    y = "SSB contribution"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title.position = "plot"
  )
