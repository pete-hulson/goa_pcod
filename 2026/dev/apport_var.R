
# load libraries
library(tidyverse)


# get data
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))
rec_mdl <- "24.0"
niters = 10000

# generate apportionment
load(file = here::here(new_year, 'output', "apport", 'apport.rdata'))

gen_apport_fn <- function(n){
  gen_p <- apport_out$biomass_by_strata %>% 
    tidytable::select(strata, year, log_pred, sd_log_pred) %>% 
    tidytable::filter(year == max(year)) %>% 
    tidytable::mutate(gen_log_pred = rnorm(n, log_pred, sd_log_pred),
                      .by = c(strata, year)) %>% 
    tidytable::mutate(gen_pred = exp(gen_log_pred)) %>% 
    tidytable::mutate(gen_tot_pred = sum(gen_pred), .by = year) %>% 
    tidytable::mutate(gen_prop = gen_pred / gen_tot_pred) %>% 
    tidytable::select(strata, year, gen_prop)
  
  list(gen_p = gen_p)
}

rand_apport <- purrr::map(1:niters, ~gen_apport_fn(1))

gen_apport <- do.call(mapply, c(list, rand_apport, SIMPLIFY = FALSE))$gen_p %>% 
  tidytable::map_df(., ~as.data.frame(.x), .id = "iter")

# generate abc
rec_mdl_res <- r4ss::SS_output(dir = here::here(new_year, "mgmt", rec_mdl),
                               verbose = FALSE,
                               printstats = FALSE)

abc <- rec_mdl_res$derived_quants[which(rec_mdl_res$derived_quants$Label == paste0("ForeCatch_", new_year + 1)), 2]
sd_abc <- rec_mdl_res$derived_quants[which(rec_mdl_res$derived_quants$Label == paste0("ForeCatch_", new_year + 1)), 3]

log_abc <- log(abc)
sd_log_abc <- sd_abc / abc

gen_abc <- data.frame(iter = 1:niters, gen_abc = exp(rnorm(1000, log_abc, sd_log_abc)))

# put em together to get uncert in brd
apport_out$biomass_by_strata %>% 
  tidytable::select(strata, year, pred) %>% 
  tidytable::filter(year == max(year)) %>% 
  tidytable::mutate(p_brd = round(pred / sum(pred), digits = 3),
                    brd = round(abc * p_brd, digits = 0)) %>% 
  tidytable::select(strata, brd) %>% 
  tidytable::left_join(gen_apport %>% 
                         tidytable::left_join(gen_abc) %>% 
                         tidytable::mutate(gen_brd = gen_prop * gen_abc) %>% 
                         tidytable::summarise(upp_qrt = quantile(gen_brd, probs = 0.75),
                                              low_qrt = quantile(gen_brd, probs = 0.25),
                                              uci = quantile(gen_brd, probs = 0.975),
                                              lci = quantile(gen_brd, probs = 0.025),
                                              cv = sd(gen_brd) / mean(gen_brd),
                                              .by = strata)) -> sim_out

vroom::vroom_write(sim_out, here::here(new_year, "dev", "apport_sim.csv"), delim = ",")

                       