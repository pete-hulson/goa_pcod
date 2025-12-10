#' function to produce standard tables for safe document
#' 
#' @param new_year current assessment year (default = NULL)
#' @param rec_mdl recommended model name (default = NULL)
#' @param prev_mdl previous accepted model (default = NULL)

safe_tbls <- function(new_year = NULL,
                      rec_mdl = NULL,
                      prev_mdl = NULL){
  
  # set up directory to plop tables into
  if (!dir.exists(here::here(new_year, "output", "safe_tables"))) {
    dir.create(here::here(new_year, "output", "safe_tables"), recursive = TRUE)
  }
  if (!dir.exists(here::here(new_year, "output", "web", "model_data"))) {
    dir.create(here::here(new_year, "output", "web", "model_data"), recursive = TRUE)
  }
  if (!dir.exists(here::here(new_year, "output", "web", "model_results"))) {
    dir.create(here::here(new_year, "output", "web", "model_results"), recursive = TRUE)
  }
  
  cat("\u231b", crayon::blue("Gathering table data..."), "\n")
  
  # read in/query needed results/etc ----
  
  # note: tables that need to be transferred over from year-to-year:
  # 1 - old_abc_tac
  # 2 - old_ref_pts
  # 3 - ageing error resuults
  # 4 - old apport
  
  ## from data queries ----
  ### catch data ----
  fed_raw <- vroom::vroom(here::here(new_year, "data", "raw", "fish_catch_data.csv"), 
                          progress = FALSE, 
                          show_col_types = FALSE)
  adfg_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'adfg_catch.csv'), 
                           progress = FALSE, 
                           show_col_types = FALSE)
  ### survey indices ----
  bts_raw <- vroom::vroom(here::here(new_year, "data", "raw", "twl_srvy_index.csv"), 
                          progress = FALSE, 
                          show_col_types = FALSE)
  bts_age_raw <- vroom::vroom(here::here(new_year, "data", "raw", "twl_srvy_apop.csv"), 
                              progress = FALSE, 
                              show_col_types = FALSE)
  bts_len_raw <- vroom::vroom(here::here(new_year, "data", "raw", "twl_srvy_lpop.csv"), 
                              progress = FALSE, 
                              show_col_types = FALSE)
  
  lls_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'lls_rpn_geoarea_data.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE) 
  lls_len_raw <- vroom::vroom(here::here(new_year, 'data', 'raw', 'lls_rpn_length_data.csv'), 
                              progress = FALSE, 
                              show_col_types = FALSE) 
  
  ### nontarget catch ----
  nontarg <- SimDesign::quiet(vroom::vroom(here::here(new_year, 'data', 'output', 'nontarget_catch.csv'), delim = ',', 
                                           progress = FALSE, 
                                           show_col_types = FALSE))
  ### prohib species catch ----
  psc <- SimDesign::quiet(vroom::vroom(here::here(new_year, 'data', 'output', 'psc_catch.csv'), delim = ',', 
                                       progress = FALSE, 
                                       show_col_types = FALSE))
  ### specs ----
  specs <- SimDesign::quiet(vroom::vroom(here::here(new_year, 'data', 'raw', 'specs.csv'), delim = ',', 
                                         progress = FALSE, 
                                         show_col_types = FALSE))
  ### bycatch ----
  bycatch <- vroom::vroom(here::here(new_year, 'data', 'raw', 'bycatch.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE)  
  
  ## from model runs ----
  ### ageing error ----
  ae_res <- SimDesign::quiet(vroom::vroom(here::here(new_year, 'output', 'ageing_error', 'agerr_res', 'Pcod_SS3_format_Reader1.csv'), delim = ',', 
                                          progress = FALSE, 
                                          show_col_types = FALSE))
  ### ageing bias ----
  bias_res <- SimDesign::quiet(vroom::vroom(here::here(new_year, 'output', 'ageing_error', 'agebias_res', 'Pcod_SS3_format_Reader1.csv'), delim = ',', 
                                     progress = FALSE, 
                                     show_col_types = FALSE))
  ### mscen results  ----
  # current model
  load(here::here(new_year, "output", "mscen", "mgmnt_scen_rec.RData"))
  mscen_curr <- mscen
  curr_2yr <- mscen_curr$Two_year
  # previous model
  load(here::here(new_year - 1, "output", "mscen", "mgmnt_scen_rec.RData"))
  mscen_prev <- mscen
  prev_2yr <- mscen_prev$Two_year
  ### jitter results  ----
  load(here::here(new_year, "output", "jitter", "jitt_res.RData"))
  ### apportionment ----
  load(here::here(new_year, "output", "apport", "apport.Rdata"))
  apport_curr <- apport_out
  ### f@ofl ----
  load(here::here(new_year, "output", "fofl_prev", "fofl_prev.Rdata"))
  ### model output ----
  mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl),
                             verbose = FALSE,
                             printstats = FALSE)
  prev_mdl_res <- r4ss::SS_output(dir = here::here(new_year - 1, "mgmt", prev_mdl),
                                  verbose = FALSE,
                                  printstats = FALSE)
  
  # note for future: code up how to pull in alternative models investigated
  # alternate model results
  # base_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1b"),
  #                                 verbose = FALSE,
  #                                 printstats = FALSE)
  # res_19_1c <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1c"),
  #                              verbose = FALSE,
  #                              printstats = FALSE)
  # res_19_1d <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1d"),
  #                              verbose = FALSE,
  #                              printstats = FALSE)
  # res_19_1e <- r4ss::SS_output(dir = here::here(new_year, "mgmt", "19.1e"),
  #                              verbose = FALSE,
  #                              printstats = FALSE)
  
  # model data
  mdl_data <- r4ss::SS_readdat_3.30(here::here(new_year, "mgmt", rec_mdl, list.files(here::here(new_year, "mgmt", rec_mdl), "GOAPcod")),
                                    verbose = FALSE)

  ## historical tables ----
  ### historical abc/tac/etc (fixed, does not update) ----
  old_abc <- vroom::vroom(here::here(new_year, 'data', 'historical', 'old_abc_tac.csv'), 
                          progress = FALSE, 
                          show_col_types = FALSE)
  
  
  # note: for the following files, if it is the first time loading them then the results
  # from the previous assessment get added and written out, if not, they are just read in
  
  ### historical ref pts ----
  if(lubridate::year(file.info(here::here(new_year, 'data', 'historical', 'old_ref_pts.csv'))$mtime) != new_year){
    old_ref_pts <- read.csv(here::here(new_year, 'data', 'historical', 'old_ref_pts.csv'))
    old_ref_pts %>% 
      tidytable::bind_rows(data.frame(Year = prev_2yr$Yr[1],
                                      sb100 = round(prev_2yr$SB100[1], digits = 0),
                                      sb40 = round(prev_2yr$SB40[1], digits = 0),
                                      f40 = round(prev_2yr$F40[1], digits = 2),
                                      ofl = round(prev_2yr$C_OFL[1], digits = 0),
                                      abc = round(prev_2yr$C_ABC[1], digits = 0))) -> old_ref_pts
    vroom::vroom_write(old_ref_pts,
                       file = here::here(new_year, 'data', 'historical', 'old_ref_pts.csv'),
                       delim = ',')
  } else{
    old_ref_pts <- vroom::vroom(here::here(new_year, 'data', 'historical', 'old_ref_pts.csv'), 
                                progress = FALSE, 
                                show_col_types = FALSE)
  }

  ### historical apport ----
  if(lubridate::year(file.info(here::here(new_year, 'data', 'historical', 'old_apport.csv'))$mtime) != new_year){
    old_apport <- read.csv(here::here(new_year, 'data', 'historical', 'old_apport.csv'))
    load(here::here(new_year - 1, "output", "apport", "apport.Rdata"))
    apport_prev <- apport_out
    old_apport %>% 
      tidytable::bind_rows(apport_prev$proportion_biomass_by_strata %>% 
                             tidytable::filter(year == max(year)) %>% 
                             tidytable::pivot_longer(., cols = c('central', 'eastern', 'western'), names_to = "region", values_to = "apport") %>% 
                             tidytable::select(region, apport) %>% 
                             tidytable::mutate(apport = round(apport, digits = 3),
                                               diff = case_when(region == 'western' ~ 1 - sum(apport),
                                                                .default = 0)) %>%
                             tidytable::mutate(apport_corr = apport + diff) %>%  # if rounding error happens, add to wgoa
                             tidytable::select(region, apport_corr) %>% 
                             tidytable::mutate(apport = apport_corr * 100) %>% 
                             tidytable::select(-apport_corr) %>% 
                             tidytable::pivot_wider(names_from = region, values_from = apport) %>% 
                             tidytable::rename(Western = western, Central = central, Eastern = eastern) %>% 
                             tidytable::mutate(Year.s. = new_year)) %>% 
      tidytable::rename('Year(s)' = Year.s.) -> old_apport
    vroom::vroom_write(old_apport,
                       file = here::here(new_year, 'data', 'historical', 'old_apport.csv'),
                       delim = ',')

  } else{
    old_apport <- vroom::vroom(here::here(new_year, 'data', 'historical', 'old_apport.csv'), 
                               progress = FALSE, 
                               show_col_types = FALSE)
  }

  ### historical model ssb ----
  if(lubridate::year(file.info(here::here(new_year, 'data', 'historical', 'hist_mdls.csv'))$mtime) != new_year){
    hist_mdls <- read.csv(here::here(new_year, 'data', 'historical', 'hist_mdls.csv'))
    prev_mdl_res$derived_quants %>% 
      tidytable::select(Label, Value, StdDev) %>% 
      tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
      tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
      tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
      tidytable::filter(Year <= new_year - 1) %>% 
      tidytable::mutate(test = Value / 2) %>% 
      tidytable::select(Year, test) -> t1
    colnames(t1) = c("year", paste0('a', new_year - 1))
    hist_mdls %>% 
      tidytable::right_join(t1) -> hist_mdls
    vroom::vroom_write(hist_mdls,
                       file = here::here(new_year, 'data', 'historical', 'hist_mdls.csv'),
                       delim = ',')
  } else{
    hist_mdls <- vroom::vroom(here::here(new_year, 'data', 'historical', 'hist_mdls.csv'), 
                               progress = FALSE, 
                               show_col_types = FALSE)
  }

  # catch by gear type and jurisdiction ----
  
  # get federal catch by gear
  fed_raw %>% 
    tidytable::select(year, 
                      zone = fmp_subarea,
                      species_group = species_group_code, 
                      gear = fmp_gear,
                      type = retained_or_discarded, 
                      week_end_date, 
                      weight_posted,
                      state_flag = akr_state_fishery_flag) %>%  
    tidytable::mutate(state_flag = replace_na(state_flag, "N")) %>% 
    filter(state_flag != "Y") %>% 
    tidytable::mutate(gear_desc = case_when(gear == "TRW" ~ "fed_trawl",
                                            gear == "HAL" ~ "fed_longline",
                                            gear == "POT" ~ "fed_pot",
                                            gear %in% c("JIG", "OTH", "GLN") ~ "fed_other")) %>% 
    tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
    tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
    tidytable::select(year, fed_trawl, fed_longline, fed_pot, fed_other) %>% 
    tidytable::mutate(fed_other = replace_na(fed_other, 0)) %>% 
    tidytable::mutate(fed_tot = fed_trawl + fed_longline + fed_pot + fed_other) %>% 
    # get state catch by gear
    tidytable::left_join(adfg_raw %>% 
                           tidytable::select(year = akfin_year,
                                             gear = fmp_gear,
                                             weight_posted = catch_mt) %>% 
                           tidytable::mutate(gear_desc = case_when(gear == "HAL" ~ "st_longline",
                                                                   gear == "POT" ~ "st_pot",
                                                                   gear %in% c("JIG", "OTH", "GLN") ~ "st_other")) %>% 
                           tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
                           tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
                           tidytable::select(year, st_longline, st_pot, st_other) %>% 
                           tidytable::mutate(st_longline = replace_na(st_longline, 0)) %>% 
                           tidytable::mutate(st_tot = st_longline + st_pot + st_other) %>% 
                           tidytable::bind_rows(fed_raw %>% 
                                                  tidytable::select(year, 
                                                                    zone = fmp_subarea,
                                                                    species_group = species_group_code, 
                                                                    gear = fmp_gear,
                                                                    type = retained_or_discarded, 
                                                                    week_end_date, 
                                                                    weight_posted,
                                                                    state_flag = akr_state_fishery_flag) %>%  
                                                  tidytable::mutate(state_flag = replace_na(state_flag, "N")) %>% 
                                                  filter(state_flag == "Y") %>% 
                                                  tidytable::mutate(gear_desc = case_when(gear == "HAL" ~ "st_longline",
                                                                                          gear == "POT" ~ "st_pot",
                                                                                          gear %in% c("JIG", "OTH", "GLN") ~ "st_other"))  %>% 
                                                  tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, gear_desc)) %>% 
                                                  tidytable::pivot_wider(names_from = gear_desc, values_from = catch) %>% 
                                                  tidytable::select(year, st_longline, st_pot, st_other) %>% 
                                                  tidytable::mutate(st_other = replace_na(st_other, 0)) %>% 
                                                  tidytable::mutate(st_tot = st_longline + st_pot + st_other))) %>% 
    base::replace(is.na(.), 0) %>% 
    tidytable::mutate(tot = fed_tot + st_tot) %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) -> juris_gr_tbl
  
  vroom::vroom_write(juris_gr_tbl, here::here(new_year, "output", "safe_tables", 'tbl1_juris_catch.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Catch by jurisdiction table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # catch-tac-abc-ofl-ghl ----
  old_abc %>% 
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
    tidytable::mutate(ofl = case_when(ofl == "     NA" ~ "-",
                                      .default = ofl),
                      ghl = case_when(ghl == "    NA" ~ "-",
                                      .default = ghl)) %>% 
    tidytable::bind_rows(specs %>% 
                           tidytable::arrange(year) %>% 
                           tidytable::filter(area_label == 'GOA',
                                             year <= new_year) %>% 
                           tidytable::select(year, tac = total_allowable_catch, abc = acceptable_biological_catch, ofl = overfishing_level) %>% 
                           tidytable::mutate(ghl = case_when(year != 2020 ~ abc - tac,
                                                             year == 2020 ~ 2537)) %>% 
                           tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
                           tidytable::left_join(juris_gr_tbl %>% 
                                                  tidytable::select(year, catch = tot))) %>%
    tidytable::rename(Year = year, Catch = catch, TAC = tac, ABC = abc, OFL = ofl, GHL = ghl) -> tac_abc_tbl
  
  vroom::vroom_write(tac_abc_tbl, here::here(new_year, "output", "safe_tables", 'tbl2_tac_abc.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Catch/ABC/GHL history table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # apportionment table ----
  
  # recommended apportionment (with rounded error check)
  apport_curr$proportion_biomass_by_strata %>% 
    tidytable::filter(year == max(year)) %>% 
    tidytable::pivot_longer(., cols = c('central', 'eastern', 'western'), names_to = "region", values_to = "apport") %>% 
    tidytable::select(region, apport) %>% 
    tidytable::mutate(apport = round(apport, digits = 3),
                      diff = case_when(region == 'western' ~ 1 - sum(apport),
                                       .default = 0)) %>%
    tidytable::mutate(apport_corr = apport + diff) %>%  # if rounding error happens, add to wgoa
    tidytable::select(region, apport_corr) %>% 
    tidytable::rename(apport = 'apport_corr') %>% 
    tidytable::mutate(ABC_yr1 = round(apport * curr_2yr$C_ABC[1], digits = 0),
                      ABC_yr2 = round(apport * curr_2yr$C_ABC[2], digits = 0)) %>% 
    tidytable::mutate(diff_y1 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[1], digits = 0) - sum(ABC_yr1),
                                          .default = 0),
                      diff_y2 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[2], digits = 0) - sum(ABC_yr2),
                                          .default = 0)) %>%
    tidytable::mutate(y1_corr = ABC_yr1 + diff_y1,
                      y2_corr = ABC_yr2 + diff_y2) %>%  # if rounding error happens, add to wgoa
    tidytable::select(-c(ABC_yr1, ABC_yr2, diff_y1, diff_y2)) %>% 
    tidytable::rename(ABC_yr1 = 'y1_corr',
                      ABC_yr2 = 'y2_corr') -> abc_apport

  # add to apport table
  apport_tbl <- old_apport %>% 
    tidytable::bind_rows(data.table(`Year(s)` = new_year + 1,
                                    Western = abc_apport$apport[which(abc_apport$region == 'western')] * 100,
                                    Central = abc_apport$apport[which(abc_apport$region == 'central')] * 100,
                                    Eastern = abc_apport$apport[which(abc_apport$region == 'eastern')] * 100))
  
  vroom::vroom_write(apport_tbl, here::here(new_year, "output", "safe_tables", 'tbl3_apport.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Apportionment table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # retained/discarded table ----
  fed_raw %>% 
    tidytable::select(year,
                      type = retained_or_discarded,
                      weight_posted) %>% 
    tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year, type)) %>%  
    tidytable::pivot_wider(names_from = type, values_from = catch) %>% 
    tidytable::left_join(adfg_raw %>% 
                           tidytable::select(year = akfin_year,
                                             weight_posted = catch_mt) %>% 
                           tidytable::summarise(catch = round(sum(weight_posted)), .by = c(year))) %>% 
    base::replace(is.na(.), 0) %>% 
    tidytable::mutate(retained = R + catch) %>% 
    tidytable::rename(discarded = D) %>% 
    tidytable::select(year, discarded, retained) %>% 
    tidytable::mutate(total = discarded + retained) %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
    tidytable::rename(Year = year, Discarded = discarded, Retained = retained, Total = total) -> dr_tbl
  
  vroom::vroom_write(dr_tbl, here::here(new_year, "output", "safe_tables", 'tbl4_dr.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Retained/discarded table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # bycatch table ----
  bycatch %>% 
    tidytable::filter(catch > 0,
                      !(species_name %in% c("sculpin, general",
                                            "sculpin, great",
                                            "sculpin, other large",
                                            "sculpin, yellow irish lord",
                                            "halibut, Pacific",
                                            "groundfish, general"))) %>% 
    tidytable::mutate(Stock = case_when(species_name %in% c("flounder, Alaska plaice",
                                                            "sole, butter",
                                                            "sole, English",
                                                            "sole, sand",
                                                            "flounder, starry",
                                                            "sole, yellowfin",
                                                            "sole, rock",
                                                            "flounder, general",
                                                            "sole, petrale") ~ "Shallow-water flatfish",
                                        species_name %in% c("sole, dover",
                                                            "turbot, Greenland",
                                                            "Kamchatka flounder") ~ "Deep-water flatfish",
                                        species_name %in% c("rockfish, canary",
                                                            "rockfish, yelloweye (red snapper)",
                                                            "rockfish, quillback",
                                                            "rockfish, copper",
                                                            "rockfish, rosethorn",
                                                            "rockfish, china",
                                                            "rockfish, tiger") ~ "Demersal shelf rockfish",
                                        species_name %in% c("rockfish, harlequin",
                                                            "rockfish, other",
                                                            "rockfish, redbanded",
                                                            "rockfish, redstripe",
                                                            "rockfish, sharpchin",
                                                            "rockfish, silvergray",
                                                            "rockfish, widow",
                                                            "rockfish, yellowtail") ~ "Other rockfish",
                                        species_name %in% c("Pacific sleeper shark",
                                                            "shark, other",
                                                            "shark, salmon",
                                                            "shark, spiny dogfish") ~ "Skarks",
                                        species_name %in% c("skate, Alaskan",
                                                            "skate, Aleutian",
                                                            "skate, big",
                                                            "skate, longnose",
                                                            "skate, other") ~ "Skates",
                                        species_name == "flounder, arrowtooth" ~ "Arrowtooth flounder",
                                        species_name == "greenling, atka mackerel" ~ "Atka mackerel",
                                        species_name == "octopus, North Pacific" ~ "Octopus",
                                        species_name == "perch, Pacific ocean" ~ "Pacific Ocean perch",
                                        species_name == "pollock, walleye" ~ "Walleye pollock",
                                        species_name == "rockfish, dusky" ~ "Dusky rockfish",
                                        species_name == "rockfish, northern" ~ "Northern rockfish",
                                        species_name == "rockfish, rougheye" ~ "Rougheye and Blackspotted rockfish",
                                        species_name == "rockfish, shortraker" ~ "Shortraker rockfish",
                                        species_name == "rockfish, thornyhead (idiots)" ~ "Thornyheads",
                                        species_name == "sablefish (blackcod)" ~ "Sablefish",
                                        species_name == "sole, flathead" ~ "Flathead sole",
                                        species_name == "sole, rex" ~ "Rex sole",
                                        .default = species_name)) %>% 
    tidytable::summarise(catch = round(sum(catch), digits = 1), .by = c(year, Stock, retained_or_discarded)) %>% 
    tidytable::bind_rows(bycatch %>% 
                           tidytable::summarise(catch = round(sum(catch), digits = 1), .by = c(year, retained_or_discarded)) %>% 
                           tidytable::mutate(Stock = "Total")) %>% 
    tidytable::pivot_wider(names_from = c(year, retained_or_discarded), values_from = catch) %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) -> bycatch_tbl
  
  vroom::vroom_write(bycatch_tbl, here::here(new_year, "output", "safe_tables", 'tbl5_bycatch.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Bycatch table"), crayon::green$underline$bold$italic("DONE"), "\n")
   
  
  # prohib species table ----
  psc %>% 
    tidytable::pivot_longer(cols = as.character(seq(min(as.numeric(colnames(psc)[which(colnames(psc) != "species")])),
                                                    max(as.numeric(colnames(psc)[which(colnames(psc) != "species")]))))) %>% 
    tidytable::mutate(name = as.numeric(name),
                      value = format(round(value, digits = 0), big.mark = ",")) %>% 
    tidytable::pivot_wider(names_from = name, values_from = value) %>% 
    tidytable::rename(Species = species) -> psc_table
  
  vroom::vroom_write(psc_table, here::here(new_year, "output", "safe_tables", 'tbl6_psc.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("PSC table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # non-target table ----
  # note for future: will need to figure out how to filter out those not included/confidential
  
  nontarg %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~round(., digits = 2)),
                      across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) %>% 
    tidytable::rename("Species Group" = species) -> nontarg_tbl
  
  vroom::vroom_write(nontarg_tbl, here::here(new_year, "output", "safe_tables", 'tbl7_nontarget.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Non-target table"), crayon::green$underline$bold$italic("DONE"), "\n")
  

  # cod catch in other fisheries ----
  
  # catch by fishery
  fed_raw %>% 
    tidytable::filter(year >= new_year - 4,
                      trip_target_name != "Other Species") %>% 
    tidytable::summarise(catch = format(round(sum(weight_posted), digits = 0), big.mark = ","), .by = c(year, trip_target_name)) %>% 
    tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
    # add avg column
    tidytable::left_join(fed_raw %>% 
                           tidytable::filter(year >= new_year - 4) %>% 
                           tidytable::summarise(catch = sum(weight_posted), .by = c(year, trip_target_name)) %>% 
                           tidytable::summarise(Average = round(mean(catch), digits = 0), .by = c(trip_target_name))) %>% 
    tidytable::arrange(-Average) %>%
    tidytable::mutate(Average = format(Average, big.mark = ",")) %>% 
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) %>% 
    # add grand total line
    tidytable::bind_rows(fed_raw %>% 
                           tidytable::filter(year >= new_year - 4) %>% 
                           tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                           tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                           tidytable::mutate(trip_target_name = 'Grand Total') %>% 
                           tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
                           tidytable::left_join(fed_raw %>% 
                                                  tidytable::filter(year >= new_year - 4) %>% 
                                                  tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                                                  tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                                                  tidytable::summarise(Average = round(mean(catch), digits = 0)) %>% 
                                                  tidytable::mutate(trip_target_name = 'Grand Total')) %>% 
                           tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))) %>% 
    # add non-pcod trip total line
    tidytable::bind_rows(fed_raw %>% 
                           tidytable::filter(year >= new_year - 4,
                                             trip_target_name != 'Pacific Cod') %>% 
                           tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                           tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                           tidytable::mutate(trip_target_name = 'Non-Pacific cod trip target total') %>% 
                           tidytable::pivot_wider(names_from = year, values_from = catch) %>% 
                           tidytable::left_join(fed_raw %>% 
                                                  tidytable::filter(year >= new_year - 4,
                                                                    trip_target_name != 'Pacific Cod') %>% 
                                                  tidytable::summarise(catch = round(sum(weight_posted), digits = 0), .by = c(year, trip_target_name)) %>% 
                                                  tidytable::summarise(catch = sum(catch), .by = c(year)) %>% 
                                                  tidytable::summarise(Average = round(mean(catch), digits = 0)) %>% 
                                                  tidytable::mutate(trip_target_name = 'Non-Pacific cod trip target total')) %>% 
                           tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))) %>% 
    tidytable::rename("Trip Target" = trip_target_name) -> targ_ctch
  
  vroom::vroom_write(targ_ctch, here::here(new_year, "output", "safe_tables", 'tbl8_catch_by_targ.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Catch by target table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # survey indices ----
  
  lls_raw %>% 
    tidytable::filter(year >= 1990) %>% 
    tidytable::summarise(RPN = sum(rpn),
                         RPN_var = sum(rpn_var), .by = year) %>% 
    tidytable::mutate(RPN = paste0(format(round(RPN, digits = 0), big.mark = ","), " (", round(100 * sqrt(RPN_var) / RPN, digits = 1),"%)")) %>% 
    tidytable::select(-RPN_var) %>% 
    tidytable::left_join(bts_raw %>% 
                           tidytable::filter(strata == 99903) %>% 
                           tidytable::summarise(Biomass = sum(biom),
                                                biom_var = sum(biom_var),
                                                Abundance = sum(num),
                                                num_var = sum(num_var), .by = year) %>% 
                           tidytable::mutate(Biomass = paste0(format(round(Biomass, digits = 0), big.mark = ","), " (", round(100 * sqrt(biom_var) / Biomass, digits = 1), "%)"),
                                             Abundance = paste0(format(round(Abundance / 1000, digits = 0), big.mark = ","), " (", round(100 * sqrt(num_var) / Abundance, digits = 1), "%)")) %>% 
                           tidytable::select(-biom_var, -num_var)) %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-")))  %>% 
    tidytable::rename(Year = year, "Biomass (t)" = Biomass) -> srv_indx_tbl
  
  vroom::vroom_write(srv_indx_tbl, here::here(new_year, "output", "safe_tables", 'tbl10_surv_indx.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Survey index table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # outside model parameter table ----
  data.frame(Name = "Ageing error SD at age-0", Value = round(ae_res$`Age 0`[3], digits = 2)) %>% 
    tidytable::bind_rows(data.frame(Name = "Ageing error SD at age-10", Value = 10 * round(ae_res$`Age 0`[3], digits = 3))) %>% 
    tidytable::bind_rows(data.frame(Name = "Ageing bias at age-0", Value = round(bias_res$`Age 1`[5] - 0.5, digits = 2))) %>% 
    tidytable::bind_rows(data.frame(Name = "Ageing bias at age-10", Value = round(bias_res$`Age 10`[5] - 10.5, digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$parameters %>% 
                           tidytable::select(Label, Value) %>% 
                           tidytable::filter(Label %in% c("Wtlen_1_Fem_GP_1",
                                                          "Wtlen_2_Fem_GP_1")) %>% 
                           tidytable::mutate(Name = c("Weight-length coefficient",
                                                      "Weight-length exponent")) %>% 
                           tidytable::mutate(Value = case_when(Value > 0.001 ~ round(Value, digits = 2),
                                                               .default = Value)) %>% 
                           tidytable::mutate(Value = case_when(Value < 0.001 ~ scales::scientific(Value, digits = 2),
                                                               .default = as.character(Value))) %>% 
                           tidytable::select(Name, Value)) %>% 
    tidytable::bind_rows(mdl_res$parameters %>% 
                           tidytable::select(Label, Value) %>% 
                           tidytable::filter(Label %in% c("Mat50%_Fem_GP_1",
                                                          "Mat_slope_Fem_GP_1")) %>% 
                           tidytable::mutate(Name = c("Length at 50% maturity",
                                                      "Slope of maturity")) %>% 
                           tidytable::mutate(Value = round(Value, digits = 2))%>% 
                           tidytable::select(Name, Value)) %>% 
    tidytable::rename(Parameter = Name) -> outside_param_tbl
  
  vroom::vroom_write(outside_param_tbl, here::here(new_year, "output", "safe_tables", 'tbl11_outside_param.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Outside model parameter table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # number of parameters table ----
  
  # set up helper fcns
  sing_param_table <- function(par_res, pattern, par_name){
    par_res %>% 
      tidytable::filter(Phase > 0) %>% 
      tidytable::select(Label, Value) %>% 
      tidytable::slice(grep(pattern, Label, perl = TRUE)) %>% 
      tidytable::mutate(!!par_name := length(Label)) %>% 
      tidytable::distinct(!!par_name) %>% 
      tidytable::pivot_longer(names_to = 'Parameter', values_to = 'Number')
  }
  
  dub_param_table <- function(par_res, pattern1, pattern2, par_name1, par_name2){
    par_res %>% 
      tidytable::filter(Phase > 0) %>% 
      tidytable::select(Label, Value) %>% 
      tidytable::slice(grep(pattern1, Label, perl = TRUE)) %>% 
      tidytable::mutate(!!par_name1 := length(Label) - length(grep(pattern2, Label, perl = TRUE)),
                        !!par_name2 := length(grep(pattern2, Label, perl = TRUE))) %>% 
      tidytable::distinct(!!par_name1, !!par_name2) %>% 
      tidytable::pivot_longer(names_to = 'Parameter', values_to = 'Number')
  }
  
  # get table
  # early init devs
  data.frame(Parameter = "Recruitment/Initial abundance", Number = "--") %>% 
    tidytable::bind_rows(sing_param_table(par_res = mdl_res$parameters, 
                                          pattern = "Early_Init", 
                                          par_name = 'Early Init Devs')) %>% 
    # early rec dev
    tidytable::bind_rows(sing_param_table(par_res = mdl_res$parameters, 
                                          pattern = "Early_RecrDev", 
                                          par_name = 'Early Rec Dev')) %>% 
    # main rec devs
    tidytable::bind_rows(sing_param_table(par_res = mdl_res$parameters, 
                                          pattern = "Main_RecrDev", 
                                          par_name = 'Main Rec Dev')) %>% 
    # r_zero
    tidytable::bind_rows(sing_param_table(par_res = mdl_res$parameters, 
                                          pattern = "SR_LN", 
                                          par_name = 'log(mean recruitment)')) %>% 
    # 76 regime
    tidytable::bind_rows(sing_param_table(par_res = mdl_res$parameters, 
                                          pattern = "SR_regime", 
                                          par_name = '1976 R reg.')) %>% 
    tidytable::bind_rows(data.frame(Parameter = "Biology", Number = "--")) %>% 
    # m and growth
    tidytable::bind_rows(dub_param_table(par_res = mdl_res$parameters,
                                         pattern1 = "Fem_GP_1",
                                         pattern2 = "NatM", 
                                         par_name1 = 'Growth (LVB)', 
                                         par_name2 = 'Natural Mortality')) %>% 
    tidytable::bind_rows(data.frame(Parameter = "Catchability/Selectivity", Number = "--")) %>% 
    # catchabilities
    tidytable::bind_rows(dub_param_table(par_res = mdl_res$parameters,
                                         pattern1 = "LnQ",
                                         pattern2 = "LLSrv", 
                                         par_name1 = 'Survey catchability (trawl)', 
                                         par_name2 = 'Survey catchability (longline)')) %>% 
    # survey selex
    tidytable::bind_rows(dub_param_table(par_res = mdl_res$parameters,
                                         pattern1 = "(?=.*Size_DblN)(?=.*Srv)",
                                         pattern2 = "(?=.*Size_DblN)(?=.*LLSrv)", 
                                         par_name1 = 'Trawl survey selex', 
                                         par_name2 = 'Longline survey selex')) %>% 
    # trawl fishery selex
    tidytable::bind_rows(dub_param_table(par_res = mdl_res$parameters,
                                         pattern1 = "FshTrawl",
                                         pattern2 = "DEVmult", 
                                         par_name1 = 'Trawl fishery selex', 
                                         par_name2 = 'Trawl fishery selex devs')) %>% 
    # longline fishery selex
    tidytable::bind_rows(dub_param_table(par_res = mdl_res$parameters,
                                         pattern1 = "FshLL",
                                         pattern2 = "DEVmult", 
                                         par_name1 = 'Longline fishery selex', 
                                         par_name2 = 'Longline fishery selex devs')) %>% 
    # pot fishery selex
    tidytable::bind_rows(sing_param_table(par_res = mdl_res$parameters, 
                                          pattern = "FshPot", 
                                          par_name = 'Pot fishery selex')) -> param_tbl

  # add total # params line
  param_tbl %>% 
    tidytable::bind_rows(param_tbl %>% 
                           tidytable::filter(Number != "--") %>% 
                           tidytable::summarise(Number = sum(as.numeric(Number), na.rm = TRUE)) %>% 
                           tidytable::mutate(Parameter = 'Total')) -> param_tbl

  vroom::vroom_write(param_tbl, here::here(new_year, "output", "safe_tables", 'tbl12_param.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("# of parameters table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # model compare likelihood table ----
  
  # helper function
  get_likes <- function(mdl_res){
    mdl_res$likelihoods_used %>% 
      tidytable::mutate(Component = rownames(.)) %>% 
      tidytable::select(Component, Value = values) %>% 
      tidytable::mutate(Value = round(Value, digits = 2)) %>% 
      tidytable::filter(Component == "TOTAL") %>% 
      tidytable::mutate(Component = "Total negative log-likelihood") %>% 
      tidytable::bind_rows(data.frame(Component = "Survey indices", 
                                      Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Survey')], digits = 2))) %>% 
      tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                             tidytable::filter(Label == "Surv_like") %>% 
                             tidytable::select(-Label) %>% 
                             tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                             tidytable::filter(Value != 0,
                                               Component != "ALL") %>% 
                             tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                             tidytable::mutate(Component = c("Bottom trawl survey index", 
                                                             "Longline survey index"))) %>% 
      tidytable::bind_rows(data.frame(Component = "Length composition", 
                                      Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Length_comp')], digits = 2))) %>% 
      tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                             tidytable::filter(Label == "Length_like") %>% 
                             tidytable::select(-Label) %>% 
                             tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                             tidytable::filter(Value != 0,
                                               Component != "ALL") %>% 
                             tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                             tidytable::mutate(Component = c("Trawl fishery length composition", 
                                                             "Longline fishery length composition",
                                                             "Pot fishery length composition",
                                                             "Bottom trawl survey length composition",
                                                             "Longline survey length composition"))) %>% 
      tidytable::bind_rows(data.frame(Component = "Conditional age-at-length", 
                                      Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Age_comp')], digits = 2))) %>% 
      tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                             tidytable::filter(Label == "Age_like") %>% 
                             tidytable::select(-Label) %>% 
                             tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                             tidytable::filter(Value != 0,
                                               Component != "ALL") %>% 
                             tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                             tidytable::mutate(Component = c("Trawl fishery CAAL", 
                                                             "Longline fishery CAAL",
                                                             "Pot fishery CAAL",
                                                             "Bottom trawl survey CAAL"))) %>% 
      tidytable::bind_rows(data.frame(Component = "Parameter deviations and priors", 
                                      Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Recruitment')] +
                                                      mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'InitEQ_Regime')] +
                                                      mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Parm_priors')] +
                                                      mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Parm_devs')], digits = 2))) %>% 
      tidytable::bind_rows(mdl_res$likelihoods_used %>% 
                             tidytable::mutate(Component = rownames(.)) %>% 
                             tidytable::select(Component, Value = values) %>% 
                             tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                             tidytable::filter(Component %in% c("Recruitment", "InitEQ_Regime", "Parm_priors", "Parm_devs")) %>% 
                             tidytable::mutate(Component = c("Recruitment deviations",
                                                             "Initial Regime (InitEQ_Regime)",
                                                             "Parameter priors",
                                                             "Selectivity deviations"))) %>% 
      tidytable::bind_rows(mdl_res$len_comp_fit_table %>% 
                             tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                             tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                             tidytable::summarise(Value = round(mean(ratio_ess), digits = 2)) %>% 
                             tidytable::mutate(Component = "Length composition mean ISS/ESS")) %>% 
      tidytable::bind_rows(mdl_res$len_comp_fit_table %>% 
                             tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                             tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                             tidytable::summarise(Value = round(mean(ratio_ess), digits = 2), .by = Fleet) %>% 
                             tidytable::mutate(Component = case_when(Fleet == 1 ~ "Trawl fishery",
                                                                     Fleet == 2 ~ "Longline fishery",
                                                                     Fleet == 3 ~ "Pot fishery",
                                                                     Fleet == 4 ~ "Bottom trawl survey",
                                                                     Fleet == 5 ~ "Longline survey")) %>% 
                             tidytable::select(Component, Value)) %>% 
      tidytable::bind_rows(mdl_res$age_comp_fit_table %>% 
                             tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                             tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                             tidytable::summarise(Value = round(mean(ratio_ess), digits = 2)) %>% 
                             tidytable::mutate(Component = "Conditional age-at-length mean ISS/ESS")) %>% 
      tidytable::bind_rows(mdl_res$age_comp_fit_table %>% 
                             tidytable::select(Fleet, Yr, Nsamp_in, effN) %>% 
                             tidytable::mutate(ratio_ess = Nsamp_in / effN) %>% 
                             tidytable::summarise(Value = round(mean(ratio_ess), digits = 2), .by = Fleet) %>% 
                             tidytable::mutate(Component = case_when(Fleet == 1 ~ "Trawl fishery",
                                                                     Fleet == 2 ~ "Longline fishery",
                                                                     Fleet == 3 ~ "Pot fishery",
                                                                     Fleet == 4 ~ "Bottom trawl survey")) %>% 
                             tidytable::select(Component, Value))
  }
  
  
  like_comp <- get_likes(prev_mdl_res) %>% 
    tidytable::rename("24.0-24" = Value) %>% 
    # tidytable::bind_cols(get_likes(base_mdl_res) %>% 
    #                        tidytable::select(-Component) %>% 
    #                        tidytable::rename("19.1b" = Value)) %>% 
    # tidytable::bind_cols(get_likes(res_19_1c) %>% 
    #                        tidytable::select(-Component) %>% 
    #                        tidytable::rename("19.1c" = Value)) %>% 
    # tidytable::bind_cols(get_likes(res_19_1d) %>% 
    #                        tidytable::select(-Component) %>% 
    #                        tidytable::rename("19.1d" = Value)) %>% 
    # tidytable::bind_cols(get_likes(res_19_1e) %>% 
    #                        tidytable::select(-Component) %>% 
    #                        tidytable::rename("19.1e" = Value)) %>% 
    tidytable::bind_cols(get_likes(mdl_res) %>% 
                           tidytable::select(-Component) %>% 
                           tidytable::rename("24.0" = Value))
  
  vroom::vroom_write(like_comp,
                     here::here(new_year, 'output', 'safe_tables', 'tbl13_like_comp.csv'),
                     delim = ",")

  # rec model likelihood table ----

  mdl_res$likelihoods_used %>% 
    tidytable::mutate(Component = rownames(.)) %>% 
    tidytable::select(Component, Value = values) %>% 
    tidytable::mutate(Value = round(Value, digits = 2)) %>% 
    tidytable::filter(Component == "TOTAL") %>% 
    tidytable::mutate(Component = "Total negative log-likelihood") %>% 
    tidytable::bind_rows(data.frame(Component = "Survey indices", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Survey')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                           tidytable::filter(Label == "Surv_like") %>% 
                           tidytable::select(-Label) %>% 
                           tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                           tidytable::filter(Value != 0,
                                             Component != "ALL") %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::mutate(Component = c("Bottom trawl survey index", 
                                                           "Longline survey index"))) %>% 
    tidytable::bind_rows(data.frame(Component = "Length composition", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Length_comp')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                           tidytable::filter(Label == "Length_like") %>% 
                           tidytable::select(-Label) %>% 
                           tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                           tidytable::filter(Value != 0,
                                             Component != "ALL") %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::mutate(Component = c("Trawl fishery length composition", 
                                                           "Longline fishery length composition",
                                                           "Pot fishery length composition",
                                                           "Bottom trawl survey length composition",
                                                           "Longline survey length composition"))) %>% 
    tidytable::bind_rows(data.frame(Component = "Conditional age-at-length", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Age_comp')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_by_fleet %>% 
                           tidytable::filter(Label == "Age_like") %>% 
                           tidytable::select(-Label) %>% 
                           tidytable::pivot_longer(names_to = "Component", values_to = "Value") %>% 
                           tidytable::filter(Value != 0,
                                             Component != "ALL") %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::mutate(Component = c("Trawl fishery CAAL", 
                                                           "Longline fishery CAAL",
                                                           "Pot fishery CAAL",
                                                           "Bottom trawl survey CAAL"))) %>% 
    tidytable::bind_rows(data.frame(Component = "Parameter deviations and priors", 
                                    Value = round(mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Recruitment')] +
                                                    mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'InitEQ_Regime')] +
                                                    mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Parm_priors')] +
                                                    mdl_res$likelihoods_used$values[which(rownames(mdl_res$likelihoods_used) == 'Parm_devs')], digits = 2))) %>% 
    tidytable::bind_rows(mdl_res$likelihoods_used %>% 
                           tidytable::mutate(Component = rownames(.)) %>% 
                           tidytable::select(Component, Value = values) %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::filter(Component %in% c("Recruitment", "InitEQ_Regime", "Parm_priors", "Parm_devs")) %>% 
                           tidytable::mutate(Component = c("Recruitment deviations",
                                                           "Initial Regime (InitEQ_Regime)",
                                                           "Parameter priors",
                                                           "Selectivity deviations"))) -> likes_tbl
  vroom::vroom_write(likes_tbl, here::here(new_year, "output", "safe_tables", 'tbl999_rcmd_likes.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Likelihood table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # key parameter table ----
  data.frame(Name = "Biology", Value = "--", SD = "--") %>% 
    tidytable::bind_rows(mdl_res$parameters %>% 
                           tidytable::select(Label, Value, Parm_StDev) %>% 
                           tidytable::filter(Label %in% c("L_at_Amin_Fem_GP_1", 
                                                          "L_at_Amax_Fem_GP_1", 
                                                          "VonBert_K_Fem_GP_1", 
                                                          "SD_young_Fem_GP_1",
                                                          "SD_old_Fem_GP_1")) %>% 
                           tidytable::mutate(Name = c("Beginning of year length at age-1 (cm)",
                                                      "Beginning of year length at age-10 (cm)",
                                                      "Growth rate",
                                                      "SD in length-at-age for age-1",
                                                      "SD in length-at-age for age-10")) %>% 
                           tidytable::mutate(Value = case_when(Value > 0.001 ~ round(Value, digits = 2),
                                                               .default = Value)) %>% 
                           tidytable::mutate(Value = case_when(Value < 0.001 ~ scales::scientific(Value, digits = 2),
                                                               .default = as.character(Value))) %>% 
                           tidytable::mutate(SD = case_when(!is.na(Parm_StDev) ~ round(Parm_StDev, digits = 3),
                                                            .default = Parm_StDev)) %>% 
                           tidytable::select(Name, Value, SD) %>% 
                           tidytable::arrange(Name)) %>% 
    tidytable::bind_rows(mdl_res$parameters %>% 
                           tidytable::select(Label, Value, Parm_StDev) %>% 
                           tidytable::filter(Label %in% c("NatM_uniform_Fem_GP_1",
                                                          "NatM_uniform_Fem_GP_1_BLK4repl_2014")) %>% 
                           tidytable::mutate(Name = c("Natural mortality (all years)",
                                                      "Natural mortality (2014-2016)")) %>% 
                           tidytable::mutate(Value = round(Value, digits = 2))%>% 
                           tidytable::mutate(SD = round(Parm_StDev, digits = 3)) %>% 
                           tidytable::select(Name, Value, SD) %>% 
                           tidytable::arrange(Name)) %>% 
    tidytable::bind_rows(data.frame(Name = "Recruitment/Abundance", Value = "--", SD = "--")) %>% 
    tidytable::bind_rows(mdl_res$parameters %>% 
                           tidytable::select(Label, Value, Parm_StDev) %>% 
                           tidytable::filter(Label %in% c("SR_LN(R0)",
                                                          "SR_regime_BLK5add_1976")) %>% 
                           tidytable::mutate(Name = c("log(mean recruitment)",
                                                      "1976 Regime adjustment")) %>% 
                           tidytable::mutate(Value = round(Value, digits = 2)) %>% 
                           tidytable::mutate(SD = case_when(!is.na(Parm_StDev) ~ round(Parm_StDev, digits = 3),
                                                            .default = Parm_StDev)) %>% 
                           tidytable::select(Name, Value, SD) %>% 
                           tidytable::arrange(-Name)) %>% 
    tidytable::bind_rows(data.frame(Name = "Survey catchability", Value = "--", SD = "--")) %>% 
    tidytable::bind_rows(mdl_res$parameters %>% 
                           tidytable::select(Label, Value, Parm_StDev) %>% 
                           tidytable::filter(Label %in% c("LnQ_base_Srv(4)",
                                                          "LnQ_base_LLSrv(5)",
                                                          "LnQ_base_LLSrv(5)_ENV_mult")) %>% 
                           tidytable::mutate(Name = c("Bottom trawl survey",
                                                      "Longline survey",
                                                      "Longline survey environmental coefficient")) %>% 
                           tidytable::mutate(Value = case_when(Name %in% c("Bottom trawl survey", "Longline survey") ~ round(exp(Value), digits = 2),
                                                               .default = round(Value, digits = 2))) %>% 
                           tidytable::mutate(SD = case_when(Name %in% c("Bottom trawl survey", "Longline survey") ~ round(Value * Parm_StDev, digits = 3),
                                                            .default = round(Parm_StDev, digits = 3))) %>% 
                           tidytable::select(Name, Value, SD) %>% 
                           tidytable::arrange(Name)) -> key_param_tbl
  
  vroom::vroom_write(key_param_tbl, here::here(new_year, "output", "safe_tables", 'tbl14_key_param.csv'), delim = ",")

  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Key parameter table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # ssb prev mdl comp table ----
  
  prev_mdl_res$derived_quants %>% 
    tidytable::select(Label, Value, StdDev) %>% 
    tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
    tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
    tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
    tidytable::filter(Year <= new_year) %>% 
    tidytable::mutate("Previous Sp.Bio" = round(Value / 2, digits = 0),
                      "Previous SD[Sp.Bio]" = round(StdDev / 2, digits = 0)) %>%
    tidytable::select(Year, "Previous Sp.Bio", "Previous SD[Sp.Bio]") %>% 
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
    tidytable::left_join(prev_mdl_res$timeseries %>% 
                           tidytable::select(Yr, Bio_all) %>% 
                           tidytable::filter(Yr <= new_year) %>% 
                           tidytable::select(Year = Yr, "Previous Tot.Bio." = Bio_all) %>% 
                           tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ",")))) %>% 
    tidytable::full_join(mdl_res$derived_quants %>% 
                           tidytable::select(Label, Value, StdDev) %>% 
                           tidytable::slice(grep("SSB", Label, perl = TRUE)) %>% 
                           tidytable::filter(!(Label %in% c("SSB_Virgin", "SSB_Initial", "SSB_unfished", "SSB_Btgt", "SSB_SPR", "SSB_MSY", "B_MSY/SSB_unfished"))) %>% 
                           tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
                           tidytable::filter(Year <= new_year + 1) %>% 
                           tidytable::mutate("Current Sp.Bio" = round(Value / 2, digits = 0),
                                             "Current SD[Sp.Bio]" = round(StdDev / 2, digits = 0)) %>%
                           tidytable::select(Year, "Current Sp.Bio", "Current SD[Sp.Bio]") %>% 
                           tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))) %>% 
                           tidytable::left_join(mdl_res$timeseries %>% 
                                                  tidytable::select(Yr, Bio_all) %>% 
                                                  tidytable::filter(Yr <= new_year + 1) %>% 
                                                  tidytable::select(Year = Yr, " Current Tot.Bio." = Bio_all) %>% 
                                                  tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~format(., big.mark = ","))))) %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) -> bio_comp_tbl
  
  vroom::vroom_write(bio_comp_tbl, here::here(new_year, "output", "safe_tables", 'tbl15_bio_comp.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Model biomass comparison table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # rec prev mdl comp table ----
  
  prev_mdl_res$derived_quants %>% 
    tidytable::select(Label, Value, StdDev) %>% 
    tidytable::slice(grep("Recr", Label, perl = TRUE)) %>% 
    tidytable::filter(!(Label %in% c("Recr_Virgin", "Recr_Initial", "Recr_unfished"))) %>% 
    tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
    tidytable::filter(Year <= new_year - 1) %>% 
    tidytable::mutate("Previous Recruitment" = round(Value / 1000000, digits = 2),
                      "Previous SD[Rec]" = round(StdDev / 1000000, digits = 2)) %>%
    tidytable::select(Year, "Previous Recruitment", "Previous SD[Rec]") %>% 
    tidytable::full_join(mdl_res$derived_quants %>% 
                           tidytable::select(Label, Value, StdDev) %>% 
                           tidytable::slice(grep("Recr", Label, perl = TRUE)) %>% 
                           tidytable::filter(!(Label %in% c("Recr_Virgin", "Recr_Initial", "Recr_unfished"))) %>% 
                           tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
                           tidytable::filter(Year <= new_year) %>% 
                           tidytable::mutate("Current Recruitment" = round(Value / 1000000, digits = 2),
                                             "Current SD[Rec]" = round(StdDev / 1000000, digits = 2)) %>%
                           tidytable::select(Year, "Current Recruitment", "Current SD[Rec]")) -> rec_comp_tbl
  
  rec_comp_tbl %>% 
    tidytable::bind_rows(data.frame(Year = "Mean 1977 - (Final year - 2)", 
                                    rec_comp_tbl %>% tidytable::summarise(prev_rec = round(mean(.$"Previous Recruitment"[which(.$Year <= new_year - 3)]), digits = 2)),
                                    sd_prev = "",
                                    rec_comp_tbl %>% tidytable::summarise(curr_rec = round(mean(.$"Current Recruitment"[which(.$Year <= new_year - 2)]), digits = 2)),
                                    sd_curr = "") %>% 
                           tidytable::rename("Previous Recruitment" = prev_rec, "Previous SD[Rec]" = sd_prev, "Current Recruitment" = curr_rec, "Current SD[Rec]" = sd_curr)) %>%
    tidytable::mutate(across(.cols = names(.)[2:length(names(.))], ~replace(., is.na(.), "-"))) -> rec_comp_tbl
  
  vroom::vroom_write(rec_comp_tbl, here::here(new_year, "output", "safe_tables", 'tbl16_rec_comp.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Model recruitment comparison table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # F table ----
  
  mdl_res$derived_quants %>% 
    tidytable::select(Label, Value, StdDev) %>% 
    tidytable::slice(grep("F_", Label, perl = TRUE)) %>% 
    tidytable::filter(!(Label %in% c("annF_Btgt", "annF_SPR", "annF_MSY"))) %>% 
    tidytable::mutate(Year = as.numeric(substr(Label, start = nchar(Label) - 3, stop = nchar(Label)))) %>% 
    tidytable::filter(Year <= new_year) %>% 
    tidytable::mutate("Sum Apical F" = round(Value, digits = 3),
                      "SD[F]" = round(StdDev, digits = 3)) %>%
    tidytable::select(Year, "Sum Apical F", "SD[F]") %>% 
    tidytable::left_join(mdl_res$timeseries %>% 
                           tidytable::select(Yr, Bio_all) %>% 
                           tidytable::filter(Yr <= new_year) %>% 
                           tidytable::select(Year = Yr, totbiom = Bio_all) %>% 
                           tidytable::left_join(mdl_res$timeseries %>% 
                                                  tidytable::select(Year = Yr, c1 = "obs_cat:_1", c2 = "obs_cat:_2", c3 = "obs_cat:_3") %>% 
                                                  tidytable::mutate(totcatch = c1 + c2 + c3) %>% 
                                                  tidytable::select(Year, totcatch)) %>% 
                           tidytable::filter(Year <= new_year,
                                             Year >= 1977) %>% 
                           tidytable::mutate("Total Exploitation" = round(totcatch / totbiom, digits = 3)) %>% 
                           tidytable::select(-totcatch, -totbiom)) -> F_tbl
  
  vroom::vroom_write(F_tbl, here::here(new_year, "output", "safe_tables", 'tbl17_F.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("F estimate table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # hist ref pts table ----
  old_ref_pts %>% 
    tidytable::mutate(across(.cols = names(.)[c(2:3, 5:6)], ~format(., big.mark = ","))) %>% 
    tidytable::bind_rows(data.frame(Year = curr_2yr$Yr[1],
                                    sb100 = format(round(curr_2yr$SB100[1], digits = 0), big.mark = ","),
                                    sb40 = format(round(0.4 * curr_2yr$SB100[1], digits = 0), big.mark = ","),
                                    f40 = round(curr_2yr$F40[1], digits = 2),
                                    ofl = format(round(curr_2yr$C_OFL[1], digits = 0), big.mark = ","),
                                    abc = format(round(curr_2yr$C_ABC[1], digits = 0), big.mark = ","))) -> ref_pts_tbl
  
  vroom::vroom_write(ref_pts_tbl, here::here(new_year, "output", "safe_tables", 'tbl18_hist_ref_pts.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Historical reference point table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  # mscen table ----
  data.table::data.table(Year = "Catch", 
                         "Scenario 1" = "-", 
                         "Scenario 2" = "-", 
                         "Scenario 3" = "-", 
                         "Scenario 4" = "-", 
                         "Scenario 5" = "-", 
                         "Scenario 6" = "-", 
                         "Scenario 7" = "-") %>% 
    tidytable::bind_rows(mscen$mscen_catch) %>% 
    tidytable::bind_rows(data.table::data.table(Year = "F", 
                                                "Scenario 1" = "-", 
                                                "Scenario 2" = "-", 
                                                "Scenario 3" = "-", 
                                                "Scenario 4" = "-", 
                                                "Scenario 5" = "-", 
                                                "Scenario 6" = "-", 
                                                "Scenario 7" = "-") %>% 
                           tidytable::bind_rows(mscen$mscen_f)) %>% 
    tidytable::bind_rows(data.table::data.table(Year = "SSB", 
                                                "Scenario 1" = "-", 
                                                "Scenario 2" = "-", 
                                                "Scenario 3" = "-", 
                                                "Scenario 4" = "-", 
                                                "Scenario 5" = "-", 
                                                "Scenario 6" = "-", 
                                                "Scenario 7" = "-") %>% 
                           tidytable::bind_rows(mscen$mscen_ssb)) -> mscen_tbl
  
  vroom::vroom_write(mscen_tbl, here::here(new_year, "output", "safe_tables", 'tbl19_mscen.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Management scenario table"), crayon::green$underline$bold$italic("DONE"), "\n")
  

  # in-text tables ----
  
  ## exec summ table ----
  exec_summ_tbl <- data.table::data.table(Quantity = c("M (natural mortality rate)", 
                                                       "Tier", 
                                                       "Projected total (age 0+) biomass (t)",
                                                       "Female spawning biomass(t)",
                                                       "Projected",
                                                       "",
                                                       "B100%",
                                                       "B40%",
                                                       "B35%", 
                                                       "FOFL", 
                                                       "maxFABC",
                                                       "FABC",
                                                       "OFL (t)",
                                                       "maxABC (t)", 
                                                       "ABC (t)"),
                                          prev1 = c(paste0(round(prev_mdl_res$parameters$Value[which(prev_mdl_res$parameters$Label == "NatM_uniform_Fem_GP_1" )], digits = 2), "*"), 
                                                    ifelse(prev_2yr$SSB_PER[1] > 0.4, "3a", "3b"), 
                                                    format(round(prev_mdl_res$timeseries$Bio_all[which(prev_mdl_res$timeseries$Yr == new_year)], digits = 0), big.mark = ","),
                                                    "", 
                                                    format(round(prev_2yr$SSB[1], digits = 0), big.mark = ","),
                                                    "", 
                                                    format(round(prev_2yr$SB100[1], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$SB40[1], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$SB35[1], digits = 0), big.mark = ","),
                                                    round(prev_2yr$F35[1], digits = 2),
                                                    round(prev_2yr$F40[1], digits = 2),
                                                    round(prev_2yr$F40[1], digits = 2),
                                                    format(round(prev_2yr$C_OFL[1], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$C_ABC[1], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$C_ABC[1], digits = 0), big.mark = ",")),
                                          prev2 = c(paste0(round(prev_mdl_res$parameters$Value[which(prev_mdl_res$parameters$Label == "NatM_uniform_Fem_GP_1" )], digits = 2), "*"), 
                                                    ifelse(prev_2yr$SSB_PER[2] > 0.4, "3a", "3b"), 
                                                    format(round(prev_mdl_res$timeseries$Bio_all[which(prev_mdl_res$timeseries$Yr == new_year + 1)], digits = 0), big.mark = ","),
                                                    "", 
                                                    format(round(prev_2yr$SSB[2], digits = 0), big.mark = ","),
                                                    "", 
                                                    format(round(prev_2yr$SB100[2], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$SB40[2], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$SB35[2], digits = 0), big.mark = ","),
                                                    round(prev_2yr$F35[2], digits = 2),
                                                    round(prev_2yr$F40[2], digits = 2),
                                                    round(prev_2yr$F40[2], digits = 2),
                                                    format(round(prev_2yr$C_OFL[2], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$C_ABC[2], digits = 0), big.mark = ","),
                                                    format(round(prev_2yr$C_ABC[2], digits = 0), big.mark = ",")),
                                          curr1 = c(paste0(round(mdl_res$parameters$Value[which(mdl_res$parameters$Label == "NatM_uniform_Fem_GP_1" )], digits = 2), "*"), 
                                                    ifelse(curr_2yr$SSB_PER[1] > 0.4, "3a", "3b"), 
                                                    format(round(mdl_res$timeseries$Bio_all[which(mdl_res$timeseries$Yr == new_year + 1)], digits = 0), big.mark = ","),
                                                    "", 
                                                    format(round(curr_2yr$SSB[1], digits = 0), big.mark = ","),
                                                    "", 
                                                    format(round(curr_2yr$SB100[1], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$SB40[1], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$SB35[1], digits = 0), big.mark = ","),
                                                    round(curr_2yr$F35[1], digits = 2),
                                                    round(curr_2yr$F40[1], digits = 2),
                                                    round(curr_2yr$F40[1], digits = 2),
                                                    format(round(curr_2yr$C_OFL[1], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$C_ABC[1], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$C_ABC[1], digits = 0), big.mark = ",")),
                                          curr2 = c(paste0(round(mdl_res$parameters$Value[which(mdl_res$parameters$Label == "NatM_uniform_Fem_GP_1" )], digits = 2), "*"),
                                                    ifelse(curr_2yr$SSB_PER[2] > 0.4, "3a", "3b"),
                                                    format(round(mdl_res$timeseries$Bio_all[which(mdl_res$timeseries$Yr == new_year + 2)], digits = 0), big.mark = ","),
                                                    "",
                                                    format(round(curr_2yr$SSB[2], digits = 0), big.mark = ","),
                                                    "",
                                                    format(round(curr_2yr$SB100[2], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$SB40[2], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$SB35[2], digits = 0), big.mark = ","),
                                                    round(curr_2yr$F35[2], digits = 2),
                                                    round(curr_2yr$F40[2], digits = 2),
                                                    round(curr_2yr$F40[2], digits = 2),
                                                    format(round(curr_2yr$C_OFL[2], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$C_ABC[2], digits = 0), big.mark = ","),
                                                    format(round(curr_2yr$C_ABC[2], digits = 0), big.mark = ",")))
  
  vroom::vroom_write(exec_summ_tbl, here::here(new_year, "output", "safe_tables", 'exec_summ_tbl.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Executive summary table"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  
  ## apportionment table ----
  apport_curr$proportion_biomass_by_strata %>% 
    tidytable::filter(year == max(year)) %>% 
    tidytable::pivot_longer(., cols = c('central', 'eastern', 'western'), names_to = "region", values_to = "apport") %>% 
    tidytable::select(region, apport) %>% 
    tidytable::mutate(apport = round(apport, digits = 3),
                      diff = case_when(region == 'western' ~ 1 - sum(apport),
                                       .default = 0)) %>%
    tidytable::mutate(apport_corr = apport + diff) %>%  # if rounding error happens, add to wgoa
    tidytable::select(region, apport_corr) %>% 
    tidytable::rename(apport = 'apport_corr') %>% 
    tidytable::mutate(ABC_yr1 = round(apport * curr_2yr$C_ABC[1], digits = 0),
                      ABC_yr2 = round(apport * curr_2yr$C_ABC[2], digits = 0)) %>% 
    tidytable::mutate(diff_y1 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[1], digits = 0) - sum(ABC_yr1),
                                          .default = 0),
                      diff_y2 = case_when(region == 'western' ~ round(curr_2yr$C_ABC[2], digits = 0) - sum(ABC_yr2),
                                          .default = 0)) %>%
    tidytable::mutate(y1_corr = ABC_yr1 + diff_y1,
                      y2_corr = ABC_yr2 + diff_y2) %>%  # if rounding error happens, add to wgoa
    tidytable::select(-c(ABC_yr1, ABC_yr2, diff_y1, diff_y2)) %>% 
    tidytable::rename(ABC_yr1 = 'y1_corr',
                      ABC_yr2 = 'y2_corr') -> abc_apport
  
  apport_tbl <- data.table::data.table(" " = c("Area apportionment", 
                                               paste(new_year + 1, "ABC"), 
                                               paste(new_year + 2, "ABC")),
                                       Western = c(paste0(abc_apport$apport[which(abc_apport$region == 'western')] * 100, "%"),
                                                   format(abc_apport$ABC_yr1[which(abc_apport$region == 'western')], big.mark = ","),
                                                   format(abc_apport$ABC_yr2[which(abc_apport$region == 'western')], big.mark = ",")),
                                       Central = c(paste0(abc_apport$apport[which(abc_apport$region == 'central')] * 100, "%"),
                                                   format(abc_apport$ABC_yr1[which(abc_apport$region == 'central')], big.mark = ","),
                                                   format(abc_apport$ABC_yr2[which(abc_apport$region == 'central')], big.mark = ",")),
                                       Eastern = c(paste0(abc_apport$apport[which(abc_apport$region == 'eastern')] * 100, "%"),
                                                   format(abc_apport$ABC_yr1[which(abc_apport$region == 'eastern')], big.mark = ","),
                                                   format(abc_apport$ABC_yr2[which(abc_apport$region == 'eastern')], big.mark = ",")),
                                       Total = c(paste0(sum(abc_apport$apport) * 100, "%"),
                                                 format(sum(abc_apport$ABC_yr1), big.mark = ","),
                                                 format(sum(abc_apport$ABC_yr2), big.mark = ",")))
  
  vroom::vroom_write(apport_tbl, here::here(new_year, "output", "safe_tables", 'intext_abc_apport.csv'), delim = ",")
  
  ## new data table ----
  # work on this some other time
  
  ## reference points ----
  data.table::data.table("Reference Point:" = "Spawning Biomass:", 
                         B35 = paste(format(round(curr_2yr$SB35[1], digits = 0), big.mark = ","), "t"),
                         B40 = paste(format(round(curr_2yr$SB40[2], digits = 0), big.mark = ","), "t"),
                         B100 = paste(format(round(curr_2yr$SB100[1], digits = 0), big.mark = ","), "t")) -> ref_pts_intext
  
  vroom::vroom_write(ref_pts_intext, here::here(new_year, "output", "safe_tables", 'intext_ref_pts.csv'), delim = ",")
  
  ## abc/ofl table ----
  data.table::data.table(Units = c("Harvest amount",
                                   "Harvest amount",
                                   "Fishing mortality rate",
                                   "Fishing mortality rate"),
                         Year = c(new_year + 1,
                                  new_year + 2,
                                  new_year + 1,
                                  new_year + 2),
                         "Overfishing Level (OFL)" = c(format(round(curr_2yr$C_OFL[1], digits = 0), big.mark = ","),
                                                       format(round(curr_2yr$C_OFL[2], digits = 0), big.mark = ","),
                                                       round(curr_2yr$F35[1], digits = 2),
                                                       round(curr_2yr$F35[2], digits = 2)),
                         "Maximum Permissible ABC" = c(format(round(curr_2yr$C_ABC[1], digits = 0), big.mark = ","),
                                                       format(round(curr_2yr$C_ABC[2], digits = 0), big.mark = ","),
                                                       round(curr_2yr$F40[1], digits = 2),
                                                       round(curr_2yr$F40[2], digits = 2))) -> abc_intext
  
  vroom::vroom_write(abc_intext, here::here(new_year, "output", "safe_tables", 'intext_abc_ofl.csv'), delim = ",")
  
  ## jitter results ----
  # get diff with mle estimates
  jitt_res$likelihoods[jitt_res$likelihoods$Label == 'TOTAL',] %>% 
    select(-Label) %>% 
    tidytable::pivot_longer() %>% 
    tidytable::mutate(diff = value - mdl_res$likelihoods_used$values[1]) -> mle_diff

  # put together table
  jitt_tbl <- data.table(conv = length(jitt_res$maxgrad[which(jitt_res$maxgrad <= 0.001)]),
                         mle = length(mle_diff$diff[which(mle_diff$diff == 0)]) / 50)
  
  vroom::vroom_write(jitt_tbl, here::here(new_year, "output", "safe_tables", 'intext_jitter.csv'), delim = ",")
  
  ## llq results ----
  
  
  
  ## f with prev catch @ ofl ----
  fofl_prev$derived_quants %>% 
    tidytable::filter(Label == paste0("F_", new_year - 1)) %>% 
    tidytable::select(param = Label, value = Value) -> f_ofl_val
  
  vroom::vroom_write(f_ofl_val, here::here(new_year, "output", "safe_tables", 'intext_f_ofl.csv'), delim = ",")

  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("In-text tables"), crayon::green$underline$bold$italic("DONE"), "\n")
  
  # get results for website ----
  
  ## survey age numbers ----
  srv_age <- bts_age_raw %>% 
    tidytable::mutate(age = case_when(age > 10 ~ 10,
                                      .default = age)) %>% 
    tidytable::mutate(names = paste0("age", age)) %>% 
    tidytable::summarise(apop = sum(num), .by = c(year, names)) %>% 
    tidytable::pivot_wider(names_from = names,
                           values_from = apop) %>% 
    tidytable::mutate(units = "Numbers",
                      survey = "Bottom trawl",
                      across(.cols = names(.)[2:length(names(.))], ~replace_na(., 0))) %>% 
    tidytable::select(year, survey, units, paste0("age", seq(1, 10)))
  
  vroom::vroom_write(srv_age, here::here(new_year, "output", "web", "model_data", 'survey_age_numbers.csv'), delim = ",")
  

  ## survey length numbers ----
  srv_len <- tidytable::expand_grid(length = seq(min(bts_len_raw$length, lls_len_raw$length), 104)) %>% 
    tidytable::left_join(bts_len_raw %>% 
                           tidytable::mutate(length = case_when(length > 104 ~ 104,
                                                                .default = length)) %>% 
                           tidytable::summarise(lpop = sum(num), .by = c(year, length))) %>% 
    tidytable::mutate(names = paste(length, "cm")) %>% 
    tidytable::select(-length) %>% 
    tidytable::pivot_wider(names_from = names,
                           values_from = lpop) %>% 
    tidytable::mutate(units = "Numbers",
                      survey = "Bottom trawl") %>% 
    tidytable::bind_rows(tidytable::expand_grid(length = seq(min(bts_len_raw$length, lls_len_raw$length), 104)) %>% 
                           tidytable::left_join(lls_len_raw %>% 
                                                  tidytable::mutate(length = case_when(length > 104 ~ 104,
                                                                                       .default = length)) %>% 
                                                  tidytable::summarise(lrpn = sum(rpn), .by = c(year, length))) %>% 
                           tidytable::mutate(names = paste(length, "cm")) %>% 
                           tidytable::filter(year >= 1990,
                                             !is.na(year)) %>% 
                           tidytable::select(-length) %>% 
                           tidytable::pivot_wider(names_from = names,
                                                  values_from = lrpn) %>% 
                           tidytable::mutate(units = "RPN",
                                             survey = "Longline")) %>% 
    tidytable::select(year, survey, units, paste(seq(min(bts_len_raw$length, lls_len_raw$length), 104), "cm")) %>% 
    tidytable::mutate(across(.cols = names(.)[4:length(names(.))], ~replace_na(., 0)))
    
  vroom::vroom_write(srv_len, here::here(new_year, "output", "web", "model_data", 'survey_length_numbers.csv'), delim = ",")
  
  ## pred numbers at age ----
  natage <- mdl_res$natage %>% 
    tidytable::filter(Yr >= 1977,
                      `Beg/Mid` == 'B') %>% 
    tidytable::select(-c(Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, `Beg/Mid`, Era))
  
  vroom::vroom_write(natage, here::here(new_year, "output", "web", "model_results", 'predicted_numbers_at_age.csv'), delim = ",")
  
  ## pred numbers at length ----
  natlen <- mdl_res$natlen %>% 
    tidytable::filter(Yr >= 1977,
                      `Beg/Mid` == 'B') %>% 
    tidytable::select(-c(Area, Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph, Seas, Time, `Beg/Mid`, Era))
  
  vroom::vroom_write(natlen, here::here(new_year, "output", "web", "model_results", 'predicted_numbers_at_length.csv'), delim = ",")
  
  ## parameters ----
  parms <- mdl_res$parameters %>% 
    tidytable::select(Label, Value, Parm_StDev)
  
  vroom::vroom_write(parms, here::here(new_year, "output", "web", "model_results", 'model_parameters.csv'), delim = ",")
  
  # print message when done
  cat(crayon::green$bold("\u2713"), crayon::blue("Website tables"), crayon::green$underline$bold$italic("DONE"), "\n")
  

  cat(crayon::green$bold("\u2713"), crayon::blue("All tables"), crayon::green$underline$bold$italic("DONE"), "\n")
  
}
