
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
  tidytable::filter(true_age <= 8) -> dat


ggplot(dat, aes(x = true_age, y = mean_og)) +
  geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci), width=.2) +
  labs(x = "'True' age (2018 read)", y = "Pre-2007 age") +
  theme_bw() +
  geom_abline(slope = 1, color = "grey", linewidth = 1.25) +
  scale_x_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  scale_y_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  geom_segment(aes(x = 1, y = 1.286, xend = 10, yend = 12))
  



reread %>% 
  tidytable::uncount(n) %>% 
  tidytable::summarise(mean_og = mean(original),
                       n = .N,
                       uci = quantile(original, probs = 0.975),
                       lci = quantile(original, probs = 0.025),
                       .by = reread) %>% 
  tidytable::mutate(bias = mean_og - reread) %>% 
  tidytable::rename(true_age = reread) %>% 
  tidytable::filter(true_age <= 8) -> dat


ggplot(reread, aes(x = reread, y = original, size = n)) +
  geom_abline(slope = 1, color = "grey", linewidth = 1.25) +
  geom_point(alpha = 0.5) +
  labs(x = "'True' age (2018 read)", y = "Pre-2007 age") +
  geom_segment(aes(x = 1, y = 1.286, xend = 8.74, yend = 10.5), size = 1, color = "blue") +
  theme_bw(base_size = 15) +
  theme(legend.position = 'top') +
  scale_x_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  scale_y_continuous(breaks = seq(1,10), labels = seq(1,10)) +
  theme(panel.grid.minor = element_blank()) -> ae_plot


suppressWarnings(ggplot2::ggsave(ae_plot,
                                 file = here::here(new_year, "plots", 'other','age_bias.png'),
                                 width = 6.5, height = 5.5, unit = 'in', dpi = 520))










  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) 

  
  0.286
  
  2
  

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




# get ageing error stats ----
## format reader-tester data ----
vroom::vroom(here::here(new_year, 'data', 'ageing_error', 'reader_tester.csv'), delim = ',') %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species == 202,
                    region != 'AI') %>% 
  tidytable::mutate(year = as.numeric(substr(date_read, start = nchar(date_read) - 3, stop = nchar(date_read)))) %>% 
  tidytable::select(region, year, read_age, test_age) %>% 
  filter(year >= 2000) %>%  
  tidytable::summarise(n = .N, .by = c(read_age, test_age)) -> r_t

c("Range_of_ages",
  paste(min(c(r_t$read_age, r_t$test_age)), max(c(r_t$read_age, r_t$test_age))),
  "Data_set_1",
  paste(nrow(r_t), "# number of lines"),
  "2 # number of readers",
  "0 10 1 # minus group; plus group; reference age",
  "1 2 # which readers",
  "",
  paste(r_t$n, r_t$read_age, r_t$test_age),
  "-999 -999 -999") %>% 
  writeLines(.,
             here::here(new_year, 'data', 'ageing_error', 'agerr.dat'))

data <- AgeingError:::CreateData(here::here(new_year, 'data', 'ageing_error', 'agerr.dat'), 
                                 NDataSet = 1, 
                                 verbose = FALSE, 
                                 EchoFile = "")

## format specifications file ----

# cv by age (1 param)
c("# reader BiasOpt SigmaOpt",
  "1 0 1",
  "2 0 -1",
  "Sigma_Pars (low high init, on/off)",
  "0.001 5 0.5 1") %>% 
  writeLines(.,
             here::here(new_year, 'data', 'ageing_error', 'agerr_cv.spc'))
spc_cv <- AgeingError::CreateSpecs(here::here(new_year, 'data', 'ageing_error', 'agerr_cv.spc'), 
                                   DataSpecs = data,
                                   verbose = TRUE)

# sd by age (2 param)
c("# reader BiasOpt SigmaOpt",
  "1 0 7",
  "2 0 -1",
  "Sigma_Pars (low high init, on/off)",
  "0 1 0.1 1",
  "1 3 1 1") %>% 
  writeLines(.,
             here::here(new_year, 'data', 'ageing_error', 'agerr_sd.spc'))
spc_sd <- AgeingError::CreateSpecs(here::here(new_year, 'data', 'ageing_error', 'agerr_sd.spc'), 
                                   DataSpecs = data,
                                   verbose = TRUE)

# spline (5 knots ala steve)
c("# reader BiasOpt SigmaOpt",
  "1 0 5",
  "2 0 -1",
  "# Spline specifications",
  "5",
  "2 4 6 8 10", 
  "Sigma_Pars (low high init, on/off)",
  "-10 40 0 1",
  "-10 40 0 1",
  "-10 40 0 1",
  "-10 40 0 1",
  "-10 40 0 1") %>% 
  writeLines(.,
             here::here(new_year, 'data', 'ageing_error', 'agerr_spl5.spc'))
spc_spl5 <- AgeingError::CreateSpecs(here::here(new_year, 'data', 'ageing_error', 'agerr_spl5.spc'), 
                                     DataSpecs = data,
                                     verbose = TRUE)



## run models ----

# cv by age
agerr_mod_cv <- AgeingError::DoApplyAgeError(Species = "Pcod",
                                             DataSpecs = data,
                                             ModelSpecsInp = spc_cv,
                                             AprobWght = 1e-06,
                                             SlopeWght = 0.01,
                                             SaveDir = here::here(new_year, 'data', 'ageing_error', 'cv_res'),
                                             verbose = TRUE)
cv_out <- AgeingError::ProcessResults(Species = "Pcod", 
                                      SaveDir = here::here(new_year, 'data', 'ageing_error', 'cv_res'), 
                                      CalcEff = TRUE, 
                                      verbose = FALSE)

# sd by age
agerr_mod_sd <- AgeingError::DoApplyAgeError(Species = "Pcod",
                                             DataSpecs = data,
                                             ModelSpecsInp = spc_sd,
                                             AprobWght = 1e-06,
                                             SlopeWght = 0.01,
                                             SaveDir = here::here(new_year, 'data', 'ageing_error', 'sd_res'),
                                             verbose = TRUE)
sd_out <- AgeingError::ProcessResults(Species = "Pcod", 
                                      SaveDir = here::here(new_year, 'data', 'ageing_error', 'sd_res'), 
                                      CalcEff = TRUE, 
                                      verbose = FALSE)

# spline 5 knots
agerr_mod_spl5 <- AgeingError::DoApplyAgeError(Species = "Pcod",
                                               DataSpecs = data,
                                               ModelSpecsInp = spc_spl5,
                                               AprobWght = 1e-06,
                                               SlopeWght = 0.01,
                                               SaveDir = here::here(new_year, 'data', 'ageing_error', 'spl5_res'),
                                               verbose = TRUE)
spl5_out <- AgeingError::ProcessResults(Species = "Pcod", 
                                        SaveDir = here::here(new_year, 'data', 'ageing_error', 'spl5_res'), 
                                        CalcEff = TRUE, 
                                        verbose = FALSE)



cv_out$ModelSelection$AICc
sd_out$ModelSelection$AICc
spl5_out$ModelSelection$AICc

