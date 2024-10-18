## Script to run 2023 GOA Pacific Cod Assessment plot code

## Load required packages & define parameters ----

libs <- c("r4ss",
          "KernSmooth",
          "stringr",
          "data.table",
          "ggplot2",
          "RODBC",
          "data.table",
          "ggplot2",
          "ggrepel",
          "car",
          "dplyr",
          "tidyr",
          "magrittr",
          "nmfspalette",
          "afscdata")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

if("nmfspalette" %in% installed.packages() == FALSE){
  remotes::install_github("nmfs-fish-tools/nmfspalette")}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name_prev <- "2019.1a-2022"
Model_name_old <- "2019.1a-2023"
Model_name_new <- "2019.1b-2023"

# Current assessment year
new_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# Do you want to call data? If so, set up connections
data_query = FALSE

if(data_query == TRUE){
  db <- read.csv(here::here(new_year, "database_specs.csv"))
  afsc_user = db$username[db$database == "AFSC"]
  afsc_pass = db$password[db$database == "AFSC"]
  akfin_user = db$username[db$database == "AKFIN"]
  akfin_pass = db$password[db$database == "AKFIN"]
  
  AFSC = odbcConnect("AFSC", 
                     afsc_user, 
                     afsc_pass, 
                     believeNRows=FALSE)
  CHINA = odbcConnect("AKFIN", 
                      akfin_user, 
                      akfin_pass, 
                      believeNRows=FALSE)}


# Define plot function
multiplot <- function(..., plotlist = NULL, cols) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    grid::viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}


## Model comparisons (for appendix) ----

# read model outputs
#previous accepted model with new data
model_dir_old <- here::here(new_year, "mgmt", Model_name_old)
model_run_old <- r4ss::SS_output(dir = model_dir_old,
                                 verbose = TRUE,
                                 printstats = TRUE)

# recommended model
model_dir_new <- here::here(new_year, "mgmt", Model_name_new)
model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)

#previous accepted model
model_dir_prev <- here::here(new_year, "mgmt", Model_name_prev)
model_run_prev <- r4ss::SS_output(dir = model_dir_prev,
                                  verbose = TRUE,
                                  printstats = TRUE)

# plot comparisons
#prev accepted with new data vs recommended model
model_comp <- r4ss::SSsummarize(list(model_run_old, model_run_new))
r4ss::SSplotComparisons(model_comp,
                        legendlabels = c(Model_name_old, Model_name_new),
                        print = TRUE,
                        plotdir = here::here(new_year, "plots", "comp_apndx") )

#prev accepted vs recommended model
model_comp_prev <- r4ss::SSsummarize(list(model_run_prev, model_run_new))
r4ss::SSplotComparisons(model_comp_prev,
                        legendlabels = c(Model_name_prev, Model_name_new),
                        print = TRUE,
                        plotdir = here::here(new_year, "plots", "comp_prev") )



## Plot base model ----
# note: before running this you need to delete the folder, otherwise r4ss will error out

load(here::here(new_year, "output", "model_run.RData"))

r4ss::SS_plots(model_run_new,
               printfolder = "",
               dir = here::here(new_year, "plots", "r4ss"))



# plot ssb and stuff ----
# Get ssb and index fit for spreadsheets with figures
model_run_new$timeseries$Yr %>% 
  bind_cols(model_run_new$timeseries$SpawnBio / 2) %>% 
  rename(year = ...1, ssb = ...2) %>% 
  filter(year >= 1977,
         year <= new_year + 1) -> ssb_pred

model_run_new$timeseries$Yr %>% 
  bind_cols(model_run_new$timeseries$Bio_all) %>% 
  rename(year = ...1, biom = ...2) %>% 
  filter(year >= 1984,
         year <= new_year + 1) -> tot_trwl_pred




# plot profiles ----
# m profile
load(here::here(new_year, "output", "profile", "m_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "NatM_uniform_Fem_GP_1",
                    profile.label = "Base M",
                    exact = TRUE)


# r profile
load(here::here(new_year, "output", "profile", "r_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "SR_LN(R0)",
                    profile.label = "Log(Mean Recruitment)",
                    exact = TRUE)


# m 14-16 profile
load(here::here(new_year, "output", "profile", "m14_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "NatM_uniform_Fem_GP_1_BLK4repl_2014",
                    profile.label = "M 2014-2016",
                    exact = TRUE)

# trawl q profile
load(here::here(new_year, "output", "profile", "q_twl_prof.RData"))
r4ss::SSplotProfile(summ_prof,
                    profile.string = "LnQ_base_Srv(4)",
                    profile.label = "Log(q_twl)",
                    exact = TRUE)


# Plot retrospective analysis ----

load(here::here(new_year, "output", "retroSummary.RData"))

# make plots comparing the retrospective models
endyrvec <- retroSummary[["endyrs"]] + 0:-10

# r4ss::SSplotComparisons(retroSummary,
#                         endyrvec = endyrvec,
#                         legendlabels = paste("Data", 0:-10, "years"),
#                         print = TRUE,
#                         plotdir = here::here("plots", "retro"))

rho_output_ss3diags <- ss3diags::SSplotRetro(retroSummary,
                                             subplots = c("SSB"),
                                             endyrvec = endyrvec,
                                             legendlabels = paste("Data", 0:-10, "years"),
                                             print = TRUE,
                                             plotdir = here::here(new_year, "plots", "other"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

## plot year-class retrospective - put into function at some point
yc_retro <- vroom::vroom(here::here(new_year, 'output', 'yc_retro.csv'))

yc_retro %>% 
  group_by(asmnt_yr) %>% 
  mutate(avg_00_05 = mean(c(yc_2000, yc_2001, yc_2002, yc_2003, yc_2004, yc_2005), na.rm = TRUE),
         avg_06_11 = mean(c(yc_2006, yc_2007, yc_2008, yc_2009, yc_2010, yc_2011), na.rm = TRUE)) %>% 
  select(asmnt_yr, avg_00_05, avg_06_11, yc_2012, yc_2013, yc_2014, yc_2015, yc_2016, yc_2017, yc_2018) %>% 
  pivot_longer(c("avg_00_05", "avg_06_11", paste0("yc_", seq(2012, 2018))), names_to = 'yearclass', values_to = 'Recruitment') -> plot_dat

plot_dat %>% 
  filter(asmnt_yr == 2023) %>%
  mutate(yearclass = case_when(yearclass == 'avg_00_05' ~ '00-05',
                               yearclass == 'avg_06_11' ~ '06-11',
                               yearclass == 'yc_2012' ~ '2012',
                               yearclass == 'yc_2013' ~ '2013',
                               yearclass == 'yc_2014' ~ '2014',
                               yearclass == 'yc_2015' ~ '2015',
                               yearclass == 'yc_2016' ~ '2016',
                               yearclass == 'yc_2017' ~ '2017',
                               yearclass == 'yc_2018' ~ '2018')) -> lab_dat
ggplot(data = plot_dat, 
       aes(x = asmnt_yr, y = Recruitment, color = factor(yearclass))) + 
  geom_point(size = 3) + 
  geom_path(aes(group = yearclass)) +
  scale_color_nmfs("waves", name = "") +
  scale_fill_nmfs("waves", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Recruitment (billions)", x = "Assessment year") +
  facet_wrap(~yearclass, 
             scales = "free_y",
             strip.position = "top") +
  scale_x_continuous(limits = c(2012.5, 2023.5), breaks = seq(2013, 2023, by = 1))

dev.print(png, file = here::here(new_year, "plots", "other", "yc_retro_grid.png"), width = 700, height = 700)
dev.off()

ggplot(data = plot_dat, 
       aes(x = asmnt_yr, y = Recruitment, color = factor(yearclass), fill = yearclass)) + 
  geom_point(size = 3) + 
  geom_path(aes(group = yearclass)) +
  scale_color_nmfs("waves", name = "") +
  scale_fill_nmfs("waves", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Recruitment (billions)", x = "Assessment year") +
  geom_label_repel(data = lab_dat,
                   aes(x = asmnt_yr, y = Recruitment, label = yearclass),
                   color = "black",
                   label.padding = unit(0.3, "lines"),
                   nudge_x = 0.5,
                   nudge_y = 0.1) +
  scale_x_continuous(limits = c(2012.5, 2023.5), breaks = seq(2013, 2023, by = 1))

dev.print(png, file = here::here(new_year, "plots", "other", "yc_retro.png"), width = 700, height = 400)
dev.off()


## Plot phase-plane ----

load(here::here(new_year, "output", "mgmnt_scen.RData"))
load(here::here(new_year, "output", "model_run.RData"))
source(here::here(new_year, "R", "plots", "phase_plane_figure.r"))

Fabc = mscen$Tables$F$scenario_1[16]
Fmsy = mscen$Tables$F$scenario_7[16]
SSB0 = mscen$Two_year$SB100[1]
SSBproj1 = mscen$Two_year$SSB[1]
SSBproj2 = mscen$Two_year$SSB[2]
Fproj1 = mscen$Two_year$F40[1]
Fproj2 = mscen$Two_year$F40[2]
BoverBmsy = model_run_new$timeseries$SpawnBio[3:((new_year - 1977) + 5)] / 2 / (SSB0 * 0.35)  ## SSB w/ 2-year projection
FoverFmsy = model_run_new$sprseries$F_report[1:((new_year - 1977) + 3)] / Fmsy  

plot.phase.plane(SSB0 = SSB0,
                 Fabc = Fabc,
                 Fmsy = Fmsy,
                 BoverBmsy = BoverBmsy, 
                 FoverFmsy = FoverFmsy,
                 xlim = c(0, 5),
                 ylim = c(0, 1.2),
                 header = "Pacific cod 2023 Model 19.1b",
                 eyr = new_year + 2)

dev.print(png, file = here::here(new_year, "plots", "other", "phase_plane.png"), width = 700, height = 700)
dev.off()


## Plot index time series ----

source(here::here(new_year, "R", "plots", "index_figures.r"))

# Get data file name
ss_datname <- list.files(here::here(new_year, "output"), pattern = "GOAPcod")


# Plot indices
index_plots <- plot_indices(styr = 1990, 
                            endyr = new_year, 
                            ss_datname = ss_datname)

index_plots[[1]]
dev.print(png, file = here::here(new_year, "plots", "other", "fitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[2]]
dev.print(png, file = here::here(new_year, "plots", "other", "nonfitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[3]]
dev.print(png, file = here::here(new_year, "plots", "other", "age0_index.png"), width = 700, height = 400)
dev.off()


## Plot Leave-One-Out analysis results ----

load(here::here(new_year, "output", "loo", "loo_data.RData"))

# Plot parameters from Leave one out
loo_data[[2]]
dev.print(png, file = here::here(new_year, "plots", "other", "LOO.png"), width = 700, height = 700)
dev.off()


load(here::here(new_year, "output", "LOO_add_data.RData"))

# Plot parameters from Leave one out
LOO_add_data[[1]]
dev.print(png, file = here::here(new_year, "plots", "other", "LOO_add_data.png"), width = 700, height = 700)
dev.off()


## Plot Cumulative catch ----

source(here::here(new_year, "R", "plots", "cumulative_catch_plots.r"))

# Get cumulative catch plots
cumul_plots <- plot_cumulative(data_query = data_query,
                               species = "PCOD",
                               fmp_area = "GOA",
                               cyr = new_year)

cumul_plots[[1]]
dev.print(png, file = here::here(new_year, "plots", "other", "cummC_CG.png"), width = 700, height = 400)
dev.off()

cumul_plots[[2]]
dev.print(png, file = here::here(new_year, "plots", "other", "cummC_WG.png"), width = 700, height = 400)
dev.off()



## Plot number of vessels ----

source(here::here(new_year, "R", "plots", "num_vess.r"))

## number of vessels
num_vess <- num_fish_vess(CYR = new_year,
                          data_query = data_query)

num_vess
dev.print(png, file = here::here(new_year, "plots", "other", "num_vess.png"), width = 700, height = 400)
dev.off()


## Plot PCod bycatch in pollock and swf fisheries ----

source(here::here(new_year, "R", "plots", "cod_bycatch_plots.r"))

# Pollock plots
pol_plots <- pollock_bycatch(CYR = new_year,
                             data_query = data_query)

multiplot(pol_plots[[1]], pol_plots[[2]], cols = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "poll_bycatch.png"), width = 700, height = 700)
dev.off()

# SWF plots (BEWARE: this takes a super-duper long time to run)
swf_plot <- swf_bycatch(CYR = new_year, 
                        data_query = data_query)

swf_plot
dev.print(png, file = here::here(new_year, "plots", "other", "swf_bycatch.png"), width = 700, height = 400)
dev.off()



## Plot environmental indices ----

env_data <- vroom::vroom(here::here(new_year, 'data', 'raw_cfsr.csv'))

source(here::here(new_year, "R", "plots", "env_indices.r"))

env_ind <- plot_env_ind(env_data)

multiplot(env_ind[[1]], env_ind[[2]], cols = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "Env_indx.png"), width = 1024, height = 1000)
dev.off()


## Plot MCMC ----

load(here::here(new_year, "output", "mcmc.RData"))
source(here::here(new_year, "R", "plots", "mcmcplots.r"))

mcmc_dir <- here::here(new_year, "mgmt", Model_name_new, "MCMC")

mcmc_plots <- plot_mcmc(mcmc_dir, new_year)

# Save plot
multiplot(mcmc_plots[[1]], mcmc_plots[[2]], cols = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "SSB_Rec.png"), width = 1024, height = 1000)
dev.off()


# plot mcmc for key parameters ----

parameters <- c('NatM_uniform_Fem_GP_1', 
                'NatM_uniform_Fem_GP_1_BLK4repl_2014', 
                'L_at_Amin_Fem_GP_1', 
                'L_at_Amax_Fem_GP_1', 
                'VonBert_K_Fem_GP_1',
                "SR_LN(R0)",
                "LnQ_base_Srv(4)",
                'LnQ_base_LLSrv(5)',
                'LnQ_base_LLSrv(5)_ENV_mult')

mle_value <- model_run_new$estimated_non_dev_parameters$Value[rownames(model_run_new$estimated_non_dev_parameters) %in% parameters]

data.frame(cbind(rownames(model_run_new$estimated_non_dev_parameters)[rownames(model_run_new$estimated_non_dev_parameters) %in% parameters],
                 model_run_new$estimated_non_dev_parameters$Value[rownames(model_run_new$estimated_non_dev_parameters) %in% parameters])) %>% 
  tidytable::rename(parameter = 'X1',
                    mle_value = 'X2') %>% 
  tidytable::mutate(mle_value = as.numeric(mle_value),
                    parameter = case_when(parameter == 'NatM_uniform_Fem_GP_1' ~ 'M_base',
                                          parameter == 'NatM_uniform_Fem_GP_1_BLK4repl_2014' ~ 'M_14_16',
                                          parameter == 'L_at_Amin_Fem_GP_1' ~ 'Lmin',
                                          parameter == 'L_at_Amax_Fem_GP_1' ~ 'Linf',
                                          parameter == 'VonBert_K_Fem_GP_1' ~ 'k',
                                          parameter == 'SR_LN(R0)' ~ 'R0',
                                          parameter == 'LnQ_base_Srv(4)' ~ 'q_BTS',
                                          parameter == 'LnQ_base_LLSrv(5)' ~ 'q_LL',
                                          parameter == 'LnQ_base_LLSrv(5)_ENV_mult' ~ 'q_LL_env'),
                    mle_value = case_when(parameter %in% c('q_BTS', 'q_LL') ~ exp(mle_value),
                                          !(parameter %in% c('q_BTS', 'q_LL')) ~ mle_value)) -> mle

mcmc %>% 
  tidytable::select(NatM_uniform_Fem_GP_1, 
                    NatM_uniform_Fem_GP_1_BLK4repl_2014, 
                    L_at_Amin_Fem_GP_1, 
                    L_at_Amax_Fem_GP_1, 
                    VonBert_K_Fem_GP_1,
                    `SR_LN(R0)`,
                    `LnQ_base_Srv(4)`,
                    `LnQ_base_LLSrv(5)`,
                    `LnQ_base_LLSrv(5)_ENV_mult`) %>% 
  tidytable::rename(M_base = 'NatM_uniform_Fem_GP_1',
                    M_14_16 = 'NatM_uniform_Fem_GP_1_BLK4repl_2014',
                    Lmin = 'L_at_Amin_Fem_GP_1',
                    Linf = 'L_at_Amax_Fem_GP_1',
                    k = 'VonBert_K_Fem_GP_1',
                    R0 = `SR_LN(R0)`,
                    q_BTS = `LnQ_base_Srv(4)`,
                    q_LL = `LnQ_base_LLSrv(5)`,
                    q_LL_env = `LnQ_base_LLSrv(5)_ENV_mult`) %>% 
  tidytable::mutate(q_BTS = exp(q_BTS),
                    q_LL = exp(q_LL)) %>% 
  tidytable::pivot_longer(names_to = 'parameter') %>% 
  tidytable::left_join(mle) -> mcmc_plot_dat

ggplot(mcmc_plot_dat, aes(x = value, y = after_stat(density))) +
  geom_histogram(bins = 15, position = "identity", alpha = 0.5, fill = "gold", color = "red") +
  geom_vline(aes(xintercept = mle_value), color = "black", linewidth = 2) +
  facet_wrap( ~ parameter,
              scales = 'free') +
  theme_bw() +
  theme_bw(base_size = 24) +
  labs(y = "Density", x = "Parameter value")

# Save plot
dev.print(png, file = here::here(new_year, "plots", "other", "param_mcmc.png"), width = 1024, height = 1000)
dev.off()

# plot mcmc adnuts stuff ----

load(here::here(new_year, "output", "mcmc_nut.RData"))

par = c('MGparm[1]', 'MGparm[2]', 'MGparm[3]', 'MGparm[4]', 'MGparm[22]', 'SR_parm[1]', 'Q_parm[1]', 'Q_parm[2]', 'Q_parm[9]')

adnuts::pairs_admb(mcmc_nut, pars = par)

mcmc_nut2 <- mcmc_nut

dimnames(mcmc_nut2$samples)[[3]][1] <- "M_base"
dimnames(mcmc_nut2$samples)[[3]][2] <- "Lmin"
dimnames(mcmc_nut2$samples)[[3]][3] <- "Linf"
dimnames(mcmc_nut2$samples)[[3]][4] <- "k"
dimnames(mcmc_nut2$samples)[[3]][7] <- "M_14_16"
dimnames(mcmc_nut2$samples)[[3]][10] <- "R0"
dimnames(mcmc_nut2$samples)[[3]][84] <- "ln(q_BTS)"
dimnames(mcmc_nut2$samples)[[3]][85] <- "ln(q_LL)"
dimnames(mcmc_nut2$samples)[[3]][86] <- "q_LL_env"

par = c('M_base', 'Lmin', 'Linf', 'k', 'M_14_16', 'R0', 'ln(q_BTS)', 'ln(q_LL)', 'q_LL_env')

adnuts::pairs_admb(mcmc_nut2, pars = par, label.cex = 1)
dev.print(png, file = here::here(new_year, "plots", "other", "param_mcmc_nuts.png"), width = 1024, height = 1000)
dev.off()


## plot length sampling rates diags ----

load(here::here(new_year, "output", "lencomp.RData"))

lencomp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                    area = trunc(area/10)*10,
                    gear = case_when(gear == 0 ~ "TRAWL",
                                     gear == 2 ~ "POT",
                                     gear == 3 ~ "LONGLINE")) %>% 
  tidytable::summarise(len_ss = sum(freq), .by = c(year, gear)) %>%  
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "TRAWL",
                                                            gear == 'POT' ~ "POT",
                                                            gear == 'HAL' ~ "LONGLINE")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear))) %>% 
  tidytable::filter(!is.na(catch)) %>% 
  tidytable::mutate(len_tot = sum(len_ss),
                    catch_tot = sum(catch), .by = c(year)) %>% 
  tidytable::mutate(p_len = len_ss / len_tot,
                    p_c = catch / catch_tot) %>% 
  tidytable::select(year, gear, p_len, p_c) %>% 
  tidytable::pivot_longer(cols = c(p_len, p_c), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'p_len' ~ "Length frequency",
                                     data == 'p_c' ~ "Catch")) %>% 
  tidytable::filter(year != 2020) -> plot_data


ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_wrap( ~ data) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates.png"), width = 1000, height = 1000)
dev.off()

## plot subregion length sampling rates diags ----

load(here::here(new_year, "output", "lencomp.RData"))

lencomp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                    area = trunc(area/10)*10,
                    gear = case_when(gear == 0 ~ "TRAWL",
                                     gear == 2 ~ "POT",
                                     gear == 3 ~ "LONGLINE")) %>% 
  tidytable::select(year, gear, area, length, freq) %>% 
  tidytable::mutate(subarea = case_when(area %in% c(640, 650) ~ "SE",
                                        area %in% c(620, 630) ~ "CG",
                                        area == 610 ~ "WG")) %>% 
  tidytable::select(year, gear, subarea, length, freq) %>% 
  tidytable::summarise(len_ss = sum(freq), .by = c(year, gear, subarea)) %>% 
  tidytable::filter(year >= 2015) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons, zone) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "TRAWL",
                                                            gear == 'POT' ~ "POT",
                                                            gear == 'HAL' ~ "LONGLINE"),
                                           subarea = case_when(zone %in% c("SE", "WY", "SEI") ~ "SE",
                                                               zone %in% c("CG", "PWSI") ~ "CG",
                                                               zone == "WG" ~ "WG")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear, subarea)) %>% 
                         tidytable::filter(year >= 2015)) %>% 
  tidytable::mutate(len_tot = sum(len_ss),
                    catch_tot = sum(catch), .by = c(year)) %>% 
  tidytable::mutate(p_len = len_ss / len_tot,
                    p_c = catch / catch_tot) %>% 
  tidytable::select(year, gear, subarea, p_len, p_c) %>% 
  tidytable::pivot_longer(cols = c(p_len, p_c), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'p_len' ~ "Length frequency",
                                     data == 'p_c' ~ "Catch")) %>% 
  tidytable::filter(year != 2020,
                    subarea != "SE") -> plot_data

ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_grid(subarea ~ data) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates_subarea.png"), width = 1000, height = 1000)
dev.off()





## plot observer sampling effort ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, `gear description`, `extrapolated weight (kg)`)  %>% 
  tidytable::rename(gear = `gear description`,
                    obs_catch = `extrapolated weight (kg)`) %>% 
  tidytable::filter(gear != "JIG") %>% 
  tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                     gear == "POT OR TRAP" ~ "Pot",
                                     gear == "LONGLINER" ~ "Longline")) %>%
  tidytable::summarise(obs_catch = sum(obs_catch) * 0.001, .by = c(year, gear)) %>% 
  tidytable::filter(!is.na(gear)) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "Trawl",
                                                            gear == 'POT' ~ "Pot",
                                                            gear == 'HAL' ~ "Longline")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear))) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_length.csv')) %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(year, `gear description`, `haul join`, frequency)  %>% 
                         tidytable::rename(gear = `gear description`,
                                           haul_join = `haul join`) %>% 
                         tidytable::filter(gear != "JIG") %>% 
                         tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                            gear == "POT OR TRAP" ~ "Pot",
                                                            gear == "LONGLINER" ~ "Longline")) %>% 
                         tidytable::summarise(len_ss = sum(frequency), .by = c(year, gear, haul_join)) %>% 
                         tidytable::filter(!is.na(haul_join),
                                           !is.na(gear)) %>% 
                         tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
                                                dplyr::rename_all(tolower) %>% 
                                                tidytable::select(year, `gear description`, `haul join`, `extrapolated weight (kg)`)  %>% 
                                                tidytable::rename(gear = `gear description`,
                                                                  obs_catch = `extrapolated weight (kg)`,
                                                                  haul_join = `haul join`) %>% 
                                                tidytable::filter(gear != "JIG") %>% 
                                                tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                                                   gear == "POT OR TRAP" ~ "Pot",
                                                                                   gear == "LONGLINER" ~ "Longline"))) %>% 
                         tidytable::summarise(len_ss = sum(len_ss),
                                              obs_c_len = sum(obs_catch, na.rm = TRUE) * 0.001, .by = c(year, gear))) %>% 
  tidytable::mutate(samp_eff = obs_catch / catch,
                    samp_eff_len = obs_c_len / catch) %>% 
  tidytable::mutate(samp_eff_tot = sum(samp_eff),
                    samp_eff_len_tot = sum(samp_eff_len),
                    catch_tot = sum(catch), .by = c(year)) %>% 
  tidytable::mutate(rel_samp_eff = samp_eff / samp_eff_tot,
                    rel_samp_eff_len = samp_eff_len / samp_eff_len_tot,
                    rel_catch = catch / catch_tot) %>% 
  tidytable::select(year, gear, rel_samp_eff, rel_samp_eff_len, rel_catch) %>% 
  tidytable::pivot_longer(cols = c(rel_samp_eff, rel_samp_eff_len, rel_catch), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'rel_samp_eff' ~ "Observed Catch",
                                     data == 'rel_samp_eff_len' ~ "Observed Catch Sampled for Length Frequency",
                                     data == 'rel_catch' ~ "Catch")) %>% 
  tidytable::filter(year != 2020) -> plot_data

ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_wrap( ~ data,
              labeller = label_wrap_gen(width = 21)) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Relative proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates_all.png"), width = 1000, height = 1000)
dev.off()




## plot regional observer sampling effort ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, `gear description`, `extrapolated weight (kg)`,`fmp subarea`)  %>% 
  tidytable::rename(gear = `gear description`,
                    obs_catch = `extrapolated weight (kg)`,
                    subregion = `fmp subarea`) %>% 
  tidytable::filter(gear != "JIG",
                    subregion %in% c("CG", "WG")) %>% 
  tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                     gear == "POT OR TRAP" ~ "Pot",
                                     gear == "LONGLINER" ~ "Longline")) %>%
  tidytable::summarise(obs_catch = sum(obs_catch) * 0.001, .by = c(year, gear, subregion)) %>% 
  tidytable::filter(!is.na(gear)) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'catch.csv')) %>% 
                         dplyr::rename_all(tolower) %>%
                         tidytable::select(year, gear, tons, zone) %>% 
                         tidytable::rename(subregion = zone) %>% 
                         tidytable::filter(gear %in% c('HAL', 'POT', 'TRW'),
                                           subregion %in% c("CG", "WG")) %>% 
                         tidytable::mutate(gear = case_when(gear == 'TRW' ~ "Trawl",
                                                            gear == 'POT' ~ "Pot",
                                                            gear == 'HAL' ~ "Longline")) %>% 
                         tidytable::summarise(catch = sum(tons), .by = c(year, gear, subregion))) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_length.csv')) %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(year, `gear description`, `haul join`, frequency, `fmp subarea`)  %>% 
                         tidytable::rename(gear = `gear description`,
                                           haul_join = `haul join`,
                                           subregion = `fmp subarea`) %>% 
                         tidytable::filter(gear != "JIG",
                                           subregion %in% c("CG", "WG")) %>% 
                         tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                            gear == "POT OR TRAP" ~ "Pot",
                                                            gear == "LONGLINER" ~ "Longline")) %>% 
                         tidytable::summarise(len_ss = sum(frequency), .by = c(year, gear, haul_join, subregion)) %>% 
                         tidytable::filter(!is.na(haul_join),
                                           !is.na(gear)) %>% 
                         tidytable::left_join(vroom::vroom(here::here(new_year, 'data', 'raw', 'akfin_raw_obscatch.csv')) %>% 
                                                dplyr::rename_all(tolower) %>% 
                                                tidytable::select(year, `gear description`, `haul join`, `extrapolated weight (kg)`,`fmp subarea`)  %>% 
                                                tidytable::rename(gear = `gear description`,
                                                                  obs_catch = `extrapolated weight (kg)`,
                                                                  haul_join = `haul join`,
                                                                  subregion = `fmp subarea`) %>% 
                                                tidytable::filter(gear != "JIG",
                                                                  subregion %in% c("CG", "WG")) %>% 
                                                tidytable::mutate(gear = case_when(gear %in% c("PELAGIC", "NON PELAGIC") ~ "Trawl",
                                                                                   gear == "POT OR TRAP" ~ "Pot",
                                                                                   gear == "LONGLINER" ~ "Longline"))) %>% 
                         tidytable::summarise(len_ss = sum(len_ss),
                                              obs_c_len = sum(obs_catch, na.rm = TRUE) * 0.001, .by = c(year, gear, subregion))) %>% 
  tidytable::mutate(samp_eff = obs_catch / catch,
                    samp_eff_len = obs_c_len / catch) %>% 
  tidytable::mutate(samp_eff_tot = sum(samp_eff),
                    samp_eff_len_tot = sum(samp_eff_len),
                    catch_tot = sum(catch), .by = c(year, subregion)) %>% 
  tidytable::mutate(rel_samp_eff = samp_eff / samp_eff_tot,
                    rel_samp_eff_len = samp_eff_len / samp_eff_len_tot,
                    rel_catch = catch / catch_tot) %>% 
  tidytable::select(year, gear, subregion, rel_samp_eff, rel_samp_eff_len, rel_catch) %>% 
  tidytable::pivot_longer(cols = c(rel_samp_eff, rel_samp_eff_len, rel_catch), names_to = 'data', values_to = 'proportion') %>% 
  tidytable::mutate(data = case_when(data == 'rel_samp_eff' ~ "Observed Catch",
                                     data == 'rel_samp_eff_len' ~ "Observed Catch Sampled for Length Frequency",
                                     data == 'rel_catch' ~ "Catch")) %>% 
  tidytable::filter(year != 2020,
                    year >= 2015) -> plot_data

ggplot(data = plot_data, 
       aes(x = year, y = proportion, fill = gear)) + 
  geom_bar(position="fill", stat="identity", width=0.5) + 
  # scale_x_continuous(breaks = c(2010:CYR), limits = c(2009.5, CYR + 0.5)) +
  scale_fill_nmfs("waves", name = "") +
  facet_grid(subregion ~ data,
              labeller = label_wrap_gen(width = 21)) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(y = "Relative proportion by gear type", x = "Year") +
  coord_flip()

dev.print(png, file = here::here(new_year, "plots", "other", "sampling_rates_subregion.png"), width = 1000, height = 1000)
dev.off()



# plot length-weight relationship ----

vroom::vroom(here::here(new_year, 'data', 'raw', 'afsc_meanage.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, length, weight) %>% 
  tidytable::filter(!is.na(weight)) %>% 
  tidytable::filter(!is.na(length)) %>% 
  tidytable::mutate(length = length / 10,
                    weight = weight / 1000) -> lw_dat


lw_fit <- function(par, data){
  sum((data$weight - par[1] * data$length ^ par[2]) ^ 2)
}


direct_fit <- optim(c(1E-05, 3), lw_fit, data = lw_dat)

lw_fit(c(3.37845E-06, 3.2804959), lw_dat)

lin_fit <- lm(log(weight) ~ log(length), data = lw_dat)

lw_dat %>% 
  tidytable::filter(year <= 2015) %>% 
  tidytable::summarise(weight = mean(weight), .by = c(length)) %>% 
  tidytable::mutate(range = "pre-2015") %>% 
  tidytable::bind_rows(lw_dat %>% 
                         tidytable::filter(year > 2015) %>% 
                         tidytable::summarise(weight = mean(weight), .by = c(length)) %>% 
                         tidytable::mutate(range = "post-2015")) -> plot_dat_mu


ggplot(data = plot_dat_mu, 
       aes(x = length, y = weight, color = range)) + 
  geom_point(size = 2) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

dev.print(png, file = here::here(new_year, "plots", "other", "length_weight.png"), width = 700, height = 500)
dev.off()






ggplot(data = plot_dat, 
       aes(x = length, y = weight, color = range)) + 
  geom_point() +
  geom_line(aes(x = length, y = pred_old), color = "red", linewidth = 1.5) + 
  geom_line(aes(x = length, y = pred_new), color = "navyblue", linewidth = 1.5)
  
ggplot(data = plot_dat, 
       aes(x = log(length), y = log(weight), color = year)) + 
  geom_point() +
  geom_line(aes(x = log(length), y = log(5.63096e-06) + 3.1306 * log(length)), color = "red") + 
  geom_line(aes(x = log(length), y = fit$coefficients[1] + fit$coefficients[2] * log(length)))

exp(fit$coefficients[1])


# plot osa residuals ----

remotes::install_github("fishfollower/compResidual/compResidual", force = TRUE)
library(compResidual)

model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)



# plot ll survey sensitivities ----


dat <- r4ss::SS_readdat_3.30(here::here(new_year, "output", "GOAPcod2023Oct16.dat"), verbose = TRUE)

dat$CPUE %>% 
  tidytable::filter(index == 5) %>% 
  tidytable::mutate(uci_obs = obs + 1.96 * se_log * obs,
                    lci_obs = obs - 1.96 * se_log * obs) %>% 
  tidytable::select(year, obs, uci, lci) -> ll_obs


vroom::vroom(here::here(new_year, 'output', 'llq_cov_pred_ll.csv')) %>% 
  tidytable::filter(model %in% c('base', 'no_cov')) %>% 
  tidytable::pivot_wider(names_from = model, values_from = values) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'output', 'llq_cov_pred_ll.csv')) %>% 
                         tidytable::filter(!(model %in% c('base', 'no_cov'))) %>% 
                         tidytable::summarise(mean_rand = mean(values),
                                              uci_rand = max(values),
                                              lci_rand = min(values), .by = year)) %>% 
  tidytable::left_join(dat$CPUE %>% 
                         tidytable::filter(index == 5) %>% 
                         tidytable::mutate(uci_obs = obs + 1.96 * se_log * obs,
                                           lci_obs = obs - 1.96 * se_log * obs) %>% 
                         tidytable::select(year, obs, uci_obs, lci_obs)) %>% 
  tidytable::left_join(vroom::vroom(here::here(new_year, 'output', 'llq_cov_pred_ll.csv')) %>% 
                         tidytable::filter(model %in% c('rand37', 'rand21')) %>% 
                         tidytable::pivot_wider(names_from = model, values_from = values)) -> plot_dat


colors <- c("No covariate" = "orange", "2019.1b" = "blue", "Random" = "darkgrey")


ggplot(plot_dat, aes(x = year, y = obs, ymin = lci_obs, ymax = uci_obs)) +
  geom_ribbon(aes(ymin = lci_rand, ymax = uci_rand), fill = "grey", alpha = 0.2) +
  geom_line(aes(x = year, y = rand21, color = "Random"), linewidth = 1) +
  geom_line(aes(x = year, y = rand37, color = "Random"), linewidth = 1) + 
  geom_point() +
  geom_linerange() +
  geom_line(aes(x = year, y = no_cov, color = "No covariate"), linewidth = 1) + 
  geom_line(aes(x = year, y = base, color = "2019.1b"), linewidth = 1) +
  theme_bw(base_size = 24) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  labs(x = "Year",
       y = "LL Survey RPN",
       color = "Catchability") +
  scale_color_manual(values = colors)

dev.print(png, file = here::here(new_year, "plots", "other", "ll_srv_fits.png"), width = 1000, height = 500)


# plot apportionment ----

load(file = here::here(new_year, 'output', 'rema_output.rdata'))

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

apport_out$biomass_by_strata %>% 
  tidytable::select(strata, year, pred, pred_lci, pred_uci, obs, obs_cv) %>% 
  tidytable::mutate(Region = case_when(strata == 'CENTRAL GOA' ~ "CGOA",
                                       strata == 'WESTERN GOA' ~ "WGOA",
                                       strata == 'EASTERN GOA' ~ "EGOA"),
                    Region = factor(Region, levels = c("WGOA", "CGOA", "EGOA")),
                    species = "Pacific cod") -> plot_dat1

apport_out$proportion_biomass_by_strata %>% 
  tidytable::pivot_longer(cols = c(`CENTRAL GOA`, `EASTERN GOA`, `WESTERN GOA`)) %>% 
  tidytable::rename(strata = "name",
                    Apportionment = "value") %>% 
  tidytable::select(year, strata, Apportionment) %>% 
  tidytable::mutate(Region = case_when(strata == 'CENTRAL GOA' ~ "CGOA",
                                       strata == 'WESTERN GOA' ~ "WGOA",
                                       strata == 'EASTERN GOA' ~ "EGOA"),
                    Region = factor(Region, levels = c("WGOA", "CGOA", "EGOA")),
                    species = "Pacific cod") -> plot_dat2


ggplot(plot_dat1, aes(x = year, y = pred)) +
  scale_fill_manual(values = cbp1) +
  scale_color_manual(values = cbp1) +
  geom_line(aes(color = Region), linewidth = 1) +
  geom_ribbon(aes(ymin = pred_lci, ymax = pred_uci, fill = Region), alpha = 0.2) +
  geom_point(aes(x = year, y = obs, color = Region)) +
  geom_linerange(aes(ymin = obs - 1.96 * obs_cv * obs, ymax = obs + 1.96 * obs_cv * obs, color = Region)) +
  theme_bw(base_size = 21) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  labs(y = "Biomass (t)") -> p1


ggplot(data = plot_dat2, 
       aes(x = year, y = Apportionment, fill = Region)) + 
  geom_bar(position="fill", stat="identity", width=0.5) +
  scale_fill_manual(values = cbp1) +
  theme_bw(base_size = 21) +
  labs(x = "Year") -> p2


ggarrange(p1, p2,
          ncol = 1, nrow = 2)

dev.print(png, file = here::here(new_year, "plots", "other", "new_apportionment.png"), width = 1000, height = 1000)
