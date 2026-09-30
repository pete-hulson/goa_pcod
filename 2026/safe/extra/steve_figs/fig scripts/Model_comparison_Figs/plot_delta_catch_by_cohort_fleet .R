#' Plot differences in catch by cohort and fleet between two SS runs
#'
#' SS-Strict sources:
#'   - Catch numbers-at-age: CATCH_AT_AGE report:40
#'   - Body weight-at-age:   AGE_SELEX report:32, factor=bodywt (kg/fish)  [only if metric="biomass"]
#'
#' Cohort definition:
#'   cohort = Year - Age, except Age>=plus_age is pooled into "Age 10+ cohorts"
#'
#' Output is a DELTA plot:
#'   Delta_{y,c,f} = RunB - RunA   (default: Full - 2024)
#'
#' @param runA Path to report file for baseline run (e.g., 2024 model)
#' @param runB Path to report file for comparison run (e.g., full model)
#' @param years Integer vector of years to include (e.g., 2020:2027)
#' @param fleets Integer vector of fleets to include (default 1:3)
#' @param fleet_names Named character vector mapping fleet IDs to labels
#' @param plus_age Plus-group age (default 10)
#' @param metric "biomass" (metric tons) or "numbers" (as reported)
#' @param direction "B_minus_A" or "A_minus_B"
#' @param numbers_scale Multiply numbers by this scalar for display
#' @param facet "fleet" (facet by fleet) or "none" (single panel with fleet in linetype)
#' @param title Optional title
#' @return ggplot object
#'
#' @examples
#' library(dplyr); library(ggplot2)
#' Full - 2024 differences in catch biomass by cohort & fleet
#' p_bio <- plot_delta_catch_by_cohort_fleet(
#'   runA="Report_2024.sso",
#'   runB="Report_full.sso",
#'   years=2020:2027,
#'   metric="biomass",
#'   direction="B_minus_A",
#'   facet="fleet",
#'   title="Model 2025 - Model 2024 catch by cohort"
#' )
#' print(p_bio)
#'
#' # Full - 2024 differences in catch numbers by cohort & fleet
#' p_num <- plot_delta_catch_by_cohort_fleet(
#'   runA="Report_2024.sso",
#'   runB="Report_full.sso",
#'   years=2020:2027,
#'   metric="numbers",
#'   direction="B_minus_A",
#'   numbers_scale=1,
#'   facet="fleet"
#' )
#' print(p_num)

plot_delta_catch_by_cohort_fleet <- function(runA,
                                            runB,
                                            years = 2020:2027,
                                            fleets = c(1,2,3),
                                            fleet_names = c(`1`="Trawl", `2`="Longline", `3`="Pot"),
                                            plus_age = 10,
                                            metric = c("biomass","numbers"),
                                            direction = c("B_minus_A","A_minus_B"),
                                            numbers_scale = 1,
                                            facet = c("fleet","none"),
                                            title = NULL) {

  metric <- match.arg(metric)
  direction <- match.arg(direction)
  facet <- match.arg(facet)

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
    # For CATCH_AT_AGE this is typically the first non-empty line with many ints incl 0
    for (k in seq_along(lines)) {
      toks <- strsplit(trimws(lines[k]), "\\s+")[[1]]
      ints <- toks[grepl("^\\d+$", toks)]
      if ("0" %in% ints && length(ints) >= 6) {
        return(list(idx = k, ages = as.integer(ints)))
      }
    }
    stop("Could not find age header line.")
  }

  parse_selex_age_header <- function(lines) {
    for (k in seq_along(lines)) {
      if (grepl("^\\s*Factor\\b", lines[k])) {
        toks <- strsplit(trimws(lines[k]), "\\s+")[[1]]
        ages <- as.integer(toks[grepl("^\\d+$", toks)])
        return(list(idx=k, ages=ages))
      }
    }
    stop("Could not find AGE_SELEX header line beginning with 'Factor'.")
  }

  extract_catchN <- function(path) {
    blk <- read_block(path, "CATCH_AT_AGE report:40")
    blk <- blk[nzchar(trimws(blk))]
    h <- parse_age_header(blk)
    ages <- h$ages; nA <- length(ages)

    rows <- list()
    for (ln in blk[(h$idx+1):length(blk)]) {
      p <- strsplit(trimws(ln), "\\s+")[[1]]
      if (length(p) < (nA + 3)) next
      if (!grepl("^\\d+$", p[2])) next
      fl <- as.integer(p[2])
      if (!(fl %in% fleets)) next
      yr_tok <- p[grepl("^\\d{4}$", p)][1]
      if (is.na(yr_tok)) next
      yr <- as.integer(yr_tok)
      if (!(yr %in% years)) next
      vals <- suppressWarnings(as.numeric(tail(p, nA)))
      if (anyNA(vals)) next
      rows[[length(rows)+1]] <- data.frame(Fleet=fl, Year=yr, Age=ages, CatchN=vals)
    }
    dplyr::bind_rows(rows) |>
      dplyr::group_by(Fleet, Year, Age) |>
      dplyr::summarise(CatchN=sum(CatchN), .groups="drop")
  }

  extract_bodywt <- function(path, ages_ref) {
    blk <- read_block(path, "AGE_SELEX report:32")
    blk <- blk[nzchar(trimws(blk))]
    h <- parse_selex_age_header(blk)
    ages <- h$ages; nA <- length(ages)

    # Prefer exact match; otherwise we'll still parse but join on Age.
    rows <- list()
    for (ln in blk[(h$idx+1):length(blk)]) {
      p <- strsplit(trimws(ln), "\\s+")[[1]]
      if (length(p) < (nA + 3)) next
      if (tolower(p[1]) != "bodywt") next
      fl <- suppressWarnings(as.integer(p[2]))
      if (is.na(fl) || !(fl %in% fleets)) next
      yr <- suppressWarnings(as.integer(p[3]))
      if (is.na(yr) || !(yr %in% years)) next
      vals <- suppressWarnings(as.numeric(tail(p, nA)))
      if (anyNA(vals)) next
      rows[[length(rows)+1]] <- data.frame(Fleet=fl, Year=yr, Age=ages, BodyWt_kg=vals)
    }
    dplyr::bind_rows(rows) |>
      dplyr::group_by(Fleet, Year, Age) |>
      dplyr::summarise(BodyWt_kg=mean(BodyWt_kg), .groups="drop")
  }

  to_cohort <- function(df) {
    df |>
      dplyr::mutate(Cohort = dplyr::if_else(Age >= plus_age,
                                           "Age 10+ cohorts",
                                           as.character(Year - Age)),
                    FleetLab = unname(fleet_names[as.character(Fleet)]))
  }

  # ----- Run A -----
  A <- extract_catchN(runA) |> to_cohort()
  if (metric == "biomass") {
    wtA <- extract_bodywt(runA, ages_ref = sort(unique(A$Age)))
    A <- A |>
      dplyr::left_join(wtA, by=c("Fleet","Year","Age")) |>
      dplyr::mutate(Value = (CatchN * BodyWt_kg) / 1000)  # mt
  } else {
    A <- A |> dplyr::mutate(Value = CatchN * numbers_scale)
  }
  A <- A |>
    dplyr::group_by(FleetLab, Year, Cohort) |>
    dplyr::summarise(ValueA = sum(Value, na.rm=TRUE), .groups="drop")

  # ----- Run B -----
  B <- extract_catchN(runB) |> to_cohort()
  if (metric == "biomass") {
    wtB <- extract_bodywt(runB, ages_ref = sort(unique(B$Age)))
    B <- B |>
      dplyr::left_join(wtB, by=c("Fleet","Year","Age")) |>
      dplyr::mutate(Value = (CatchN * BodyWt_kg) / 1000)  # mt
  } else {
    B <- B |> dplyr::mutate(Value = CatchN * numbers_scale)
  }
  B <- B |>
    dplyr::group_by(FleetLab, Year, Cohort) |>
    dplyr::summarise(ValueB = sum(Value, na.rm=TRUE), .groups="drop")

  # ----- Delta -----
  d <- dplyr::full_join(A, B, by=c("FleetLab","Year","Cohort")) |>
    dplyr::mutate(ValueA = dplyr::coalesce(ValueA, 0),
                  ValueB = dplyr::coalesce(ValueB, 0),
                  Delta  = if (direction=="B_minus_A") ValueB - ValueA else ValueA - ValueB)

  # Cohort ordering (plus first)
  cohort_levels <- d |>
    dplyr::distinct(Cohort) |>
    dplyr::mutate(ord = dplyr::if_else(Cohort=="Age 10+ cohorts", -Inf, as.numeric(Cohort))) |>
    dplyr::arrange(ord) |>
    dplyr::pull(Cohort)
  d$Cohort <- factor(d$Cohort, levels=cohort_levels)

  ylab <- if (metric=="biomass") "Δ Catch biomass (mt)" else "Δ Catch (numbers)"
  if (metric=="numbers" && numbers_scale != 1) ylab <- paste0(ylab, " × ", numbers_scale)

  if (is.null(title)) {
    title <- paste0("Δ catch by cohort",
                    " (", ifelse(metric=="biomass","biomass","numbers"), "), ",
                    ifelse(direction=="B_minus_A","RunB − RunA","RunA − RunB"),
                    " (", min(years), "–", max(years), ")")
  }

  library(ggplot2)

  p <- ggplot(d, aes(x=Year, y=Delta, color=Cohort, group=Cohort)) +
    geom_hline(yintercept=0) +
    geom_line(linewidth=1) +
    geom_point(size=2) +
    # plus-group styling
    geom_line(data = subset(d, Cohort=="Age 10+ cohorts"),
              aes(x=Year, y=Delta, group=Cohort),
              color="black", linetype="dashed", linewidth=1.2, inherit.aes=FALSE) +
    geom_point(data = subset(d, Cohort=="Age 10+ cohorts"),
               aes(x=Year, y=Delta),
               color="black", size=2, inherit.aes=FALSE) +
    scale_x_continuous(breaks=years) +
    labs(title=title, x="Year", y=ylab, color="Cohort") +
    theme_bw() +
    theme(legend.position="right")

  if (facet == "fleet") {
    p <- p + facet_wrap(~FleetLab, nrow=1, scales="free_y")
  } else {
    # optional: encode fleet with linetype if not faceting
    p <- p + aes(linetype=FleetLab) + labs(linetype="Fleet")
  }

  return(p)
}

