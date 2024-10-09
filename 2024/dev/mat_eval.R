
# get connected
db = 'afsc'
conn = afscdata::connect(db)

# domestic fishery age data ----
dplyr::tbl(conn, dplyr::sql('obsint.debriefed_age')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('obsint.debriefed_haul')),
                    by = c('HAUL_JOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(year = year.y,
                area = nmfs_area.x,
                haul_date,
                gear,
                species,
                sex,
                length,
                age,
                maturity_code,
                maturity_description,
                sample_system) %>% 
  dplyr::filter(area >= 600,
                area <= 699,
                area != 670,
                species == 202,
                sex == 'F',
                !is.na(maturity_code),
                year > 2006) -> afsc_age

dplyr::collect(afsc_age) %>% 
  dplyr::mutate(month = month(haul_date)) %>% 
  dplyr::select(year, month, species, sex, length, age, maturity_code, maturity_description, sample_system) -> mat_dat

vroom::vroom_write(mat_dat, here::here(2024, 'data', 'maturity', 'obs_mat.csv'), delim = ',')

sizeMat::gonad_mature(mat_dat, 
                      varNames = c("length", "maturity_description"),
                      inmName = "Immature",
                      matName = c("Pre Spawn","Spawning","Spent","Developing","Resting" ),
                      method = "bayes",
                      niter = 999)

plot(my_ogive_fq, xlab = "Total length (cm)", ylab = "Proportion mature", col = c("blue", "red"))

mat_dat %>% 
  arrange(length)

my_ogive_fq_spr = sizeMat::gonad_mature(dat, 
                                        varNames = c("length", "maturity_description"),
                                        inmName = "Immature",
                                        matName = c("Pre Spawn","Spawning","Spent","Developing","Resting" ),
                                        method = "bayes",
                                        niter = 999)

plot(my_ogive_fq_spr, xlab = "Total length (cm)", ylab = "Proportion mature", col = c("blue", "red"))
