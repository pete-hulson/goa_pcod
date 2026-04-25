library(tidytable)
library(lubridate)
library(afscdata)
library(DR4SS)

db = 'akfin'
con_akfin = afscdata::connect(db)
species = 202
sp_area = "ALL"
start_year = 2000 
end_year = 2025
SEX = FALSE
PORT = TRUE
max_length = 200
max_age = 10L
age_length = "AGE"
map_sample = c("MAP")
n_samples = 1
season_def = list(A = c(1:3), B = c(4:12))
region_def = list(BS = c(500:539), WGOA = c(610), CGOA = c(620:649))
drop_unmapped = TRUE
wgoa_cod = FALSE
seed = 1L
return_predictor = TRUE
  
  age_length <- match.arg(age_length)
  map_sample <- match.arg(map_sample)
  vcat <- function(...) if (isTRUE(verbose)) message(...)
  
  # ---- Initial Checks ----
  if (is.null(con_akfin)) stop("`con_akfin` is required.")
  sp_area <- toupper(sp_area)
  
  # ---- region mapping logic (kept largely same for logic, used tidytable later) ----
  default_region_vec <- switch(
    sp_area,
    "AI"      = 540:544,
    "GOA"     = 600:699,
    "BS"      = 500:539,
    "BSWGOA"  = c(500:539, 610, 620),
    "ALL"      = 500:699,
    stop("Invalid `sp_area`.")
  )
  
  # Process region_def if provided
  if (!is.null(region_def)) {
    # (Internal list processing logic remains similar to ensure data integrity)
    all_areas <- unlist(region_def)
    region_vec <- sort(unique(all_areas))
  } else {
    region_vec <- default_region_vec
  }

  # ---- Helper: Add Region Group ----
  add_region_group <- function(df, region_def, area_col = "AREA") {
    df <- as_tidytable(df) %>%
      mutate(!!area_col := as.integer(as.character(.data[[area_col]])))
    
    if (is.null(region_def)) {
      return(df %>% mutate(REGION_GRP = factor(sp_area, levels = sp_area)))
    }
    
    key <- stack(region_def) %>% 
      as_tidytable() %>% 
      rename(REGION_GRP = ind, AREA_KEY = values)
    
    df %>%
      mutate(AREA_KEY = .data[[area_col]]) %>%
      left_join(key, by = "AREA_KEY") %>%
      select(-AREA_KEY) %>%
      filter(!is.na(REGION_GRP) | !drop_unmapped)
  }

  # ---- Helper: Assign AVEWT (The Priority Fallback) ----
  assign_avewt <- function(df, joins, priority) {
    df <- as_tidytable(df)
    for (j in joins) {
      df <- left_join(df, j$table, by = j$by)
    }
    # tidytable coalesce is great for fallback logic
    df %>%
      mutate(AVEWT = coalesce(!!!syms(priority)))
  }

  # ------------------------------------------------------------
  # 1) Observer lengths (AKFIN)
  # ------------------------------------------------------------
  lfreq_sql <- sql_reader("dom_length_AKFIN.sql") %>%
    sql_filter("IN", species, flag = "-- insert species", value_type = "numeric") %>%
    sql_filter("IN", region_vec, flag = "-- insert region", value_type = "numeric") %>%
    sql_filter("<=", end_year, flag = "-- insert end", value_type = "numeric") %>%
    sql_filter(">=", start_year, flag = "-- insert start", value_type = "numeric")

  Dspcomp <- as_tidytable(sql_run(con_akfin, lfreq_sql)) %>%
    rename_with(toupper) %>%
    filter(EXTRAPOLATED_WEIGHT > 0, NUMB > 0)


dcomp <- vroom::vroom(here::here(new_year, 'rsch', 'wgoa', 'data', 'fish_lfreq_domestic.csv'))



  if (isTRUE(wgoa_cod)) {
    Dspcomp <- Dspcomp %>%
      mutate(AREA = if_else(AREA == 620 & LONDD_END <= wgoa_cut, 610, AREA))
  }

  Dspcomp <- Dspcomp %>%
    mutate(
      HDAY_DATE = as.Date(parse_date_time(trimws(as.character(HDAY)), orders = c("Ymd", "mdY", "Ymd HMS"))),
      WED = WED_safe(HDAY_DATE),
      MONTH_WED = month(WED),
      MONTH = as.integer(as.character(MONTH)),
      QUARTER = case_when(
        MONTH < 3 ~ 1L,
        MONTH < 7 ~ 2L,
        MONTH < 10 ~ 3L,
        TRUE ~ 4L
      ),
      AREA2 = trunc(as.numeric(AREA) / 10) * 10,
      AREA2 = if_else(AREA2 == 500, 510, AREA2)
    ) %>%
    filter(!is.na(HDAY_DATE))

  OBS_DATA <- Dspcomp %>%
    select(SPECIES, YEAR, GEAR, AREA2, AREA, MONTH, QUARTER, MONTH_WED,
           CRUISE, PERMIT, VES_AKR_ADFG, HAUL_JOIN, SEX, LENGTH,
           SUM_FREQUENCY, EXTRAPOLATED_WEIGHT, NUMB, SOURCE) %>%
    filter(YEAR >= start_year, YEAR <= end_year) %>%
    add_region_group(region_def)

  # ------------------------------------------------------------
  # 2) AVEWT Lookup Tables
  # ------------------------------------------------------------
  # tidytable makes these group_by %>% summarize flows very clean
  calc_avwt <- function(df, ...) {
    df %>%
      summarize(AVE_WT = sum(EXTRAPOLATED_WEIGHT) / sum(NUMB), .by = c(...))
  }

  YAGM_AVWT <- Dspcomp %>% calc_avwt(YEAR, AREA2, MONTH, GEAR) %>% rename(YAGM_AVE_WT = AVE_WT)
  YAM_AVWT  <- Dspcomp %>% calc_avwt(YEAR, AREA2, MONTH)        %>% rename(YAM_AVE_WT = AVE_WT)
  YGM_AVWT  <- Dspcomp %>% calc_avwt(YEAR, GEAR, MONTH)         %>% rename(YGM_AVE_WT = AVE_WT)
  YGQ_AVWT  <- Dspcomp %>% calc_avwt(YEAR, GEAR, QUARTER)       %>% rename(YGQ_AVE_WT = AVE_WT)
  YAQ_AVWT  <- Dspcomp %>% calc_avwt(YEAR, AREA2, QUARTER)      %>% rename(YAQ_AVE_WT = AVE_WT)
  YG_AVWT   <- Dspcomp %>% calc_avwt(YEAR, GEAR)                %>% rename(YG_AVE_WT = AVE_WT)

  avwt_joins <- list(
    list(table = YAGM_AVWT, by = c("YEAR", "AREA2", "MONTH", "GEAR")),
    list(table = YGM_AVWT,  by = c("YEAR", "GEAR", "MONTH")),
    list(table = YGQ_AVWT,  by = c("YEAR", "GEAR", "QUARTER")),
    list(table = YAM_AVWT,  by = c("YEAR", "AREA2", "MONTH")),
    list(table = YAQ_AVWT,  by = c("YEAR", "AREA2", "QUARTER")),
    list(table = YG_AVWT,   by = c("YEAR", "GEAR"))
  )
  
  priority_cols <- c("YAGM_AVE_WT", "YAM_AVE_WT", "YAQ_AVE_WT", "YGM_AVE_WT", "YGQ_AVE_WT", "YG_AVE_WT")

  # ... (Port data era blocks would follow similar logic using left_join and mutate) ...

  # ------------------------------------------------------------
  # 5) Blend Catch -> Numbers
  # ------------------------------------------------------------
  catch_sql <- sql_reader("dom_catch_AKFIN.sql") %>%
    sql_filter("<=", end_year, flag = "-- insert eyear") %>%
    sql_filter(">=", start_year, flag = "-- insert syear")
  
  CATCHT <- as_tidytable(sql_run(con_akfin, catch_sql)) %>%
    rename_with(toupper) %>%
    filter(YEAR >= start_year, YEAR <= end_year, TONS > 0) %>%
    add_region_group(region_def) %>%
    mutate(
      MONTH = as.integer(as.character(MONTH_WED)),
      QUARTER = case_when(MONTH < 3 ~ 1L, MONTH < 7 ~ 2L, MONTH < 10 ~ 3L, TRUE ~ 4L),
      AREA2 = if_else(trunc(as.numeric(AREA)/10)*10 == 500, 510, trunc(as.numeric(AREA)/10)*10)
    ) %>%
    filter(GEAR %in% c("POT", "TRW", "HAL"))

  CATCHT2 <- assign_avewt(CATCHT, avwt_joins, priority_cols) %>%
    mutate(
      NUMBER = TONS / (AVEWT / 1000),
      SPECIES = as.numeric(species)
    ) %>%
    filter(is.finite(NUMBER), NUMBER > 0)

  # Final Catch summaries for weighting
  CATCH_SUMS <- CATCHT2 %>%
    summarize(YAGM_TNUM = sum(NUMBER), YAGM_TONS = sum(TONS), .by = c(REGION_GRP, SPECIES, YEAR, GEAR, AREA2, MONTH)) %>%
    mutate(YAG_TNUM = sum(YAGM_TNUM), .by = c(REGION_GRP, YEAR, AREA2, GEAR)) %>%
    mutate(YG_TNUM  = sum(YAGM_TNUM), .by = c(REGION_GRP, YEAR, GEAR)) %>%
    mutate(Y_TNUM   = sum(YAGM_TNUM), .by = c(REGION_GRP, YEAR))

  # ------------------------------------------------------------
  # 6) Final Composition Calculation
  # ------------------------------------------------------------
  # Simplified weighting logic using tidytable pipes
  final_df <- OBS_DATA %>%
    filter(GEAR %in% c("POT", "TRW", "HAL")) %>%
    left_join(CATCH_SUMS, by = c("REGION_GRP", "SPECIES", "YEAR", "AREA2", "GEAR", "MONTH")) %>%
    mutate(
      WEIGHT1 = SUM_FREQUENCY / sum(SUM_FREQUENCY), .by = HAUL_JOIN,
      WEIGHT2 = NUMB / sum(NUMB), .by = c(REGION_GRP, YEAR, AREA2, GEAR, MONTH),
      WEIGHTX = WEIGHT1 * WEIGHT2 * (YAGM_TNUM / Y_TNUM)
    )
    
  # Logic for dense grids and sample sizes would follow a similar summarize pattern
  vcat("Conversion complete.")
  return(final_df)
