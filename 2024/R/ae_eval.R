
library(afscdata)
conn = afscdata::connect('akfin') 
curr_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# query trawl survey age specimen data ----
twl_srvy = c(47, 98)
srv_sp = 21720
cruise_samp = 200301
vessel = c(88, 89, 147)

dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                    by = c('CRUISEJOIN')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
                    by = c('HAULJOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(survey_definition_id,
                species_code,
                cruise,
                vessel_id,
                haul,
                specimen_id,
                sex,
                length_mm,
                weight_g,
                age) %>% 
  dplyr::filter(survey_definition_id %in% twl_srvy,
                species_code %in% srv_sp,
                cruise %in% cruise_samp,
                vessel_id %in% vessel) %>% 
  dplyr::rename(survey = survey_definition_id,
                specimen = specimen_id,
                vessel = vessel_id,
                length = length_mm,
                weight = weight_g,
                original_age = age) -> twl_q

dplyr::collect(twl_q) %>% 
  tidytable::filter(original_age > 0) %>% 
  tidytable::mutate(length = length / 10,
                    weight = weight / 1000) -> spec_dat


# get reread data ----
vroom::vroom(here::here(curr_year, 'data', 'ageing_error', 'bias', '88200301202R.csv'), delim = ',') %>% 
  tidytable::bind_rows(vroom::vroom(here::here(curr_year, 'data', 'ageing_error', 'bias', '89200301202R.csv'), delim = ',')) %>% 
  tidytable::bind_rows(vroom::vroom(here::here(curr_year, 'data', 'ageing_error', 'bias', '147200301202R.csv'), delim = ',')) %>% 
  tidytable::filter(age > 0) %>% 
  tidytable::select(vessel, cruise, haul, sex, length, specimen, weight, age) %>% 
  tidytable::mutate(length = length / 10,
                    weight = weight / 1000) %>% 
  tidytable::rename(reread_age = age) -> reread_dat

# plot reread vs original read ----
spec_dat %>% 
  tidytable::left_join(reread_dat) %>% 
  tidytable::drop_na(reread_age) %>% 
  tidytable::summarise(num_reread = .N,
                       .by = c(reread_age, original_age))


tidytable::expand_grid(reread_age = sort(unique(reread_dat$reread_age)),
                       original_age = sort(unique(spec_dat$original_age))) %>% 
  tidytable::left_join(spec_dat %>% 
                         tidytable::left_join(reread_dat) %>% 
                         tidytable::drop_na(reread_age) %>% 
                         tidytable::summarise(num_reread = .N,
                                              .by = c(original_age, reread_age))) %>% 
  tidytable::mutate(num_reread = replace_na(num_reread, 0)) %>% 
  tidytable::mutate(tot = sum(num_reread), .by = reread_age) %>% 
  tidytable::mutate(prop_reread = num_reread / tot) %>% 
  tidytable::select(original_age, reread_age, prop_reread) -> plot_dat

ggplot(plot_dat %>% filter(original_age == 1), aes(x = reread_age, y = prop_reread)) + 
  geom_col()






spec_dat %>% 
  tidytable::left_join(reread_dat) %>% 
  tidytable::drop_na(reread_age) %>% 
  tidytable::summarise(mean_og = mean(original_age),
                       n = .N,
                       uci = quantile(original_age, probs = 0.975),
                       lci = quantile(original_age, probs = 0.025),
                       .by = reread_age) -> dat

ggplot(dat, aes(x = reread_age, y = mean_og)) +
  geom_abline(a = 0, b = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.2) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  labs(x = "'True' age (2018 read)", y = "Pre-2007 age")






spec_dat %>% 
  arrange(-original_age)





















reread <- vroom::vroom(here::here(curr_year, 'data', 'ageing_error', 'rereads.csv'), delim = ',')

reread %>% 
  tidytable::uncount(n) %>% 
  tidytable::summarise(mean_og = mean(original),
                       n = .N,
                       uci = quantile(original, probs = 0.975),
                       lci = quantile(original, probs = 0.025),
                       .by = reread) %>% 
  tidytable::mutate(bias = mean_og - reread) %>% 
  tidytable::rename(true_age = reread) %>% 
  tidytable::filter(true_age <= 7) -> dat


ggplot(dat, aes(x = true_age, y = bias)) +
  geom_point() +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) 


dat %>% 
  tidytable::mutate(est_bias = -0.274 + 0.249 * original_age)



# steve code
data2<-AgeingError:::CreateData("dataREREADS.dat", NDataSet = 1, verbose = TRUE, EchoFile = "data_echo.out")

PCOD2_spc <- AgeingError:::CreateSpecs("control3.spc", DataSpecs = data2,verbose = TRUE)


PCOD2_mod <- AgeingError::DoApplyAgeError(
  Species = "Pcod",
  DataSpecs = data2,
  ModelSpecsInp = PCOD2_spc,
  AprobWght = 1e-06,
  SlopeWght = 0.01,
  SaveDir = "ResultsREREADS",
  verbose = TRUE
)



PCOD2_out <- AgeingError:::ProcessResults(Species = "Pcod", SaveDir = "ResultsREREADS", CalcEff = TRUE, verbose = FALSE)