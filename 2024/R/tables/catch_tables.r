# Get catch tables for SAFE

## Load libraries
library(magrittr)
library(tidyr)
library(dplyr)

## Open up data base connections
afsc_user = "hulsonp"   ## enter afsc username
afsc_pass = "Liam1Fin2Bri3<3!" ## enter afsc password
akfin_user = "phulson"  ## enter AKFIN username
akfin_pass = "$blwins1" ## enter AKFIN password

AFSC = odbcConnect("AFSC", 
                   afsc_user, 
                   afsc_pass, 
                   believeNRows=FALSE)
CHINA = odbcConnect("AKFIN", 
                    akfin_user, 
                    akfin_pass, 
                    believeNRows=FALSE)


## ----- get REGION catch -----

test <- paste("SELECT SUM(COUNCIL.COMPREHENSIVE_BLEND_CA.WEIGHT_POSTED)AS TONS,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA AS ZONE,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR AS GEAR,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED AS TYPE,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR AS YEAR,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.AKR_STATE_FISHERY_FLAG AS STATE,\n ",
              "TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM') AS MONTH,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE AS SPECIES_GROUP\n ",
              "FROM COUNCIL.COMPREHENSIVE_BLEND_CA\n ",
              "WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA in (",fsh_sp_area,")\n ",
              "AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR <= ",new_year,"\n ",
              "AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",fsh_sp_label,")\n ",
              "GROUP BY COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_GEAR,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.RETAINED_OR_DISCARDED,\n ",
              "COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR,\n ", 
              "COUNCIL.COMPREHENSIVE_BLEND_CA.AKR_STATE_FISHERY_FLAG,\n ",
              "TO_CHAR(COUNCIL.COMPREHENSIVE_BLEND_CA.WEEK_END_DATE,'MM')", sep="")


CATCH <- sqlQuery(CHINA, test)

CATCH %>% 
  rename_all(tolower) %>% 
  group_by(gear, year, state) %>% 
  summarise(catch = sum(tons)) %>% 
  pivot_wider(names_from = c(state, gear), values_from = catch) %>% 
  mutate_all(~replace_na(., 0)) -> catch
  
  
write.csv(catch, here::here("catch.csv"))
write.csv(CATCH, here::here("CATCHall.csv"))


test <- paste("SELECT * \n ",
              "FROM COUNCIL.COMPREHENSIVE_BLEND_CA\n ",
              "WHERE COUNCIL.COMPREHENSIVE_BLEND_CA.FMP_SUBAREA in (",fsh_sp_area,")\n ",
              "AND COUNCIL.COMPREHENSIVE_BLEND_CA.YEAR = ",1997,"\n ",
              "AND COUNCIL.COMPREHENSIVE_BLEND_CA.SPECIES_GROUP_CODE in (",fsh_sp_label,") ", sep="")


CATCHfull <- sqlQuery(CHINA, test)

write.csv(CATCHfull, here::here("catchfull.csv"))





