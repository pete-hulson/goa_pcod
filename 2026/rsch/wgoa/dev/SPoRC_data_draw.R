# code to pull data and compile within SPoRC package framework for AK PAcific cod

# load libraries ----

library(DR4SS)
library(afscdata)
library(tidyverse)

# notes: ----
# there are two formats: (1) includes only 610 in the wgoa, (2) includes both 610 and 620 in wgoa

season <- list(A = c(1:3), B = c(4:12))
region_1 <- list(BS = c(500:539), WGOA = c(610), CGOA = c(620:649))
region_2 <- list(BS = c(500:539), WGOA = c(610, 620), CGOA = c(630:649))


db = 'akfin'
conn = afscdata::connect(db)  
twl_srvy = 47
srv_sp = 21720
fsh_sp_code = 202
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))


# fishery data ----

## catch ----

.catch_1 <- DR4SS::get_blend_catch_region(conn,
                                          species = 202,
                                          region_def = region_1,
                                          season_def = season,
                                          start_year = 1990,
                                          end_year = 2025,
                                          wgoa_cod = FALSE)

catch_1 <- DR4SS::build_catch_season_from_blend(blend_catch = .catch_1,
                                                by_gear = FALSE,
                                                complete_grid = TRUE)

.catch_2 <- DR4SS::get_blend_catch_region(conn,
                                          species = 202,
                                          region_def = region_2,
                                          season_def = season,
                                          start_year = 1990,
                                          end_year = 2025,
                                          wgoa_cod = FALSE)

catch_2 <- DR4SS::build_catch_season_from_blend(blend_catch = .catch_2,
                                                by_gear = FALSE,
                                                complete_grid = TRUE)

## age comp ----


acomp <- DR4SS::LENGTH_AGE_BY_CATCH(conn, 
                                    species = 202, 
                                    sp_area = "ALL", 
                                    start_year = 2000, 
                                    end_year = 2025,
                                    SEX = FALSE, 
                                    PORT = TRUE,
                                    max_length = 200,
                                    max_age = 10L,
                                    age_length = "AGE",
                                    map_sample = c("MAP"),
                                    n_samples = 1,
                                    season_def = season,
                                    region_def = region_1,
                                    drop_unmapped = TRUE,
                                    wgoa_cod = FALSE,
                                    seed = 1L,
                                    return_predictor = TRUE)

ACOMP<-format_age_to_ss_block(acomp,
                              bins = 0:10,
                              sexed = TRUE,
                              file = NULL)




