## data for SPORK Matt

library(DR4SS)
conn=db_connect()
con_akfin=conn$akfin
con_afsc=conn$afsc

season_q <-list(A=c(1:3),B=c(4:12))
region_q <-list(BS=c(500:539),WGOA=c(610,620),CGOA=c(630:649))


## Domestic catch (note that foreign catch by region will need to come from the stock assessments)
catch<-get_blend_catch_region(con_akfin,
                            species=202,
                            region_def=region_q,
                            season_def=season_q,
                            start_year=1990,
                            end_year=2025,
                            wgoa_cod=FALSE
                            )


CATCH<-build_catch_season_from_blend(blend_catch=catch,
                                         by_gear = FALSE,
                                         complete_grid = TRUE)

## Domestic age comps
acomp <- LENGTH_AGE_BY_CATCH(
  con_akfin=con_akfin, 
  species=202, 
  sp_area="ALL", 
  start_year=2000, 
  end_year=2025,
  SEX=FALSE, 
  PORT=TRUE,
  max_length=200,
  max_age = 10L,
  age_length="AGE",
  map_sample = c("MAP"),
  n_samples = 1,
  season_def = season_q,
  region_def = region_q ,
  drop_unmapped = TRUE,
  wgoa_cod=FALSE,
  seed=1L,
  return_predictor = TRUE
  )

ACOMP<-format_age_to_ss_block(acomp,
                               bins = 0:10,
                               sexed = TRUE,
                               file = NULL)

## Domestic length comps
lcomp <- LENGTH_AGE_BY_CATCH(
  con_akfin=con_akfin, 
  species=202, 
  sp_area="ALL", 
  start_year=1990, 
  end_year=2025,
  SEX=FALSE, 
  PORT=TRUE,
  max_length=200,
  max_age = 10L,
  age_length="LENGTH",
  map_sample = c("MAP"),
  n_samples = 1,
  season_def = season_q,
  region_def = region_q ,
  drop_unmapped = TRUE,
  wgoa_cod=FALSE,
  seed=1L,
  return_predictor = TRUE
  )

LCOMP <- format_comp_to_ss_block(
  data = lcomp,
  bins = seq(4.5, 119.5, by = 5),
  bin_col = "LENGTH",
  comp_type = "len",
  bin_fun=BIN_LEN_DATA,
  sexed = FALSE
)

##Conditional age at length  
CAAL<-fishery_cond_age_length(con_akfin=con_akfin,
                                    species=202,
                                    start_year=2000,
                                    end_year=2025,
                                    season_def=season_q,
                                    region_def=region_q,
                                    max_age=10,
                                    len_bins=seq(4.5,119.5,5),
                                    drop_unmapped = TRUE,
                                    one_fleet = TRUE,
                                    wgoa_cod=FALSE,
                                    wt = 1,
                                    ageerr = 1,
                                    by_sex = FALSE)


##Foreign length comps
f_len<-foreign_length_by_catch(con_akfin=con_akfin,
                                  species =202,
                                  for_species_catch ="PACIFIC COD",
                                  start_year=1977,
                                  end_year=1989,
                                  SEX = FALSE,
                                  season_def = season_q,
                                  region_def = region_q,
                                  drop_unmapped = TRUE,
                                  wgoa_cod =FALSE,
                                  verbose=TRUE
                                  )

FLCOMP <- format_comp_to_ss_block(
  data = f_len,
  bins = seq(4.5, 119.5, by = 5),
  bin_col = "LENGTH",
  comp_type = "len",
  bin_fun=BIN_LEN_DATA,
  sexed = FALSE
)




