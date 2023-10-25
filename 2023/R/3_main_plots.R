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
          "nmfspalette")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}

if("nmfspalette" %in% installed.packages() == FALSE){
  remotes::install_github("nmfs-fish-tools/nmfspalette")}

lapply(libs, library, character.only = TRUE)

# Current model name
Model_name_old <- "2019.1a-2023"
Model_name_new <- "2019.1b-2023"

# Current assessment year
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# Do you want to call data? If so, set up connections
data_query = FALSE

if(data_query == TRUE){
  db <- read.csv(here::here(new_SS_dat_year, "database_specs.csv"))
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
model_dir_old <- here::here(new_SS_dat_year, "mgmt", Model_name_old)
model_run_old <- r4ss::SS_output(dir = model_dir_old,
                                 verbose = TRUE,
                                 printstats = TRUE)

model_dir_new <- here::here(new_SS_dat_year, "mgmt", Model_name_new)
model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)

model_comp <- r4ss::SSsummarize(list(model_run_old, model_run_new))

r4ss::SSplotComparisons(model_comp,
                        legendlabels = c(Model_name_old, Model_name_new),
                        print = TRUE,
                        plotdir = here::here(new_SS_dat_year, "plots", "comp_apndx") )


## Plot base model ----
# note: before running this you need to delete the folder, otherwise r4ss will error out

load(here::here(new_SS_dat_year, "output", "model_run.RData"))

r4ss::SS_plots(model_run_new,
               printfolder = "",
               dir = here::here(new_SS_dat_year, "plots", "r4ss"))

## Plot retrospective analysis ----

load(here::here(new_SS_dat_year, "output", "retroSummary.RData"))

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
                                             plotdir = here::here(new_SS_dat_year, "plots", "other"),
                                             pwidth = 8.5,
                                             pheight = 4.5)

## plot year-class retrospective - put into function at some point
yc_retro <- vroom::vroom(here::here(new_SS_dat_year, 'output', 'yc_retro.csv'))

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

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "yc_retro_grid.png"), width = 700, height = 700)
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

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "yc_retro.png"), width = 700, height = 400)
dev.off()


## Plot fancy phase-plane ----

load(here::here(new_SS_dat_year, "output", "mgmnt_scen.RData"))
load(here::here(new_SS_dat_year, "output", "model_run.RData"))
source(here::here(new_SS_dat_year, "R", "plots", "phase_plane_figure.r"))

Fabc = mscen$Tables$F$scenario_1[16]
Fmsy = mscen$Tables$F$scenario_7[16]
SSB0 = mscen$Two_year$SB100[1]
SSBproj1 = mscen$Two_year$SSB[1]
SSBproj2 = mscen$Two_year$SSB[2]
Fproj1 = mscen$Two_year$F40[1]
Fproj2 = mscen$Two_year$F40[2]
BoverBmsy = model_run_new$timeseries$SpawnBio[3:((new_SS_dat_year - 1977) + 5)] / 2 / (SSB0 * 0.35)  ## SSB w/ 2-year projection
FoverFmsy = model_run_new$sprseries$F_report[1:((new_SS_dat_year - 1977) + 3)] / Fmsy  

plot.phase.plane(SSB0 = SSB0,
                 Fabc = Fabc,
                 Fmsy = Fmsy,
                 BoverBmsy = BoverBmsy, 
                 FoverFmsy = FoverFmsy,
                 xlim = c(0, 5),
                 ylim = c(0, 1.2),
                 header = "Pacific cod 2023 Model 19.1b",
                 eyr = new_SS_dat_year + 2)

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "phase_plane.png"), width = 700, height = 700)
dev.off()


## Plot index time series ----

source(here::here(new_SS_dat_year, "R", "plots", "index_figures.r"))

# Get data file name
ss_datname <- list.files(here::here(new_SS_dat_year, "output"), pattern = "GOAPcod")


# Plot indices
index_plots <- plot_indices(styr = 1990, 
                            endyr = new_SS_dat_year, 
                            ss_datname = ss_datname)

index_plots[[1]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "fitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[2]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "nonfitted_indices.png"), width = 700, height = 700)
dev.off()

index_plots[[3]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "age0_index.png"), width = 700, height = 400)
dev.off()


## Plot Leave-One-Out analysis results ----

load(here::here(new_SS_dat_year, "output", "LOO.RData"))

# Plot parameters from Leave one out
LOO[[2]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "LOO.png"), width = 700, height = 700)
dev.off()


load(here::here(new_SS_dat_year, "output", "LOO_add_data.RData"))

# Plot parameters from Leave one out
LOO_add_data[[1]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "LOO_add_data.png"), width = 700, height = 700)
dev.off()


## Plot Cumulative catch ----

source(here::here(new_SS_dat_year, "R", "plots", "cumulative_catch_plots.r"))

# Current week
curr_wk <- as.numeric(format(Sys.Date(), format = "%W"))

# Get cumulative catch plots
cumul_plots <- plot_cumulative(data_query = data_query,
                               species = "'PCOD'",
                               FMP_AREA = "'GOA'",
                               syear = new_SS_dat_year - 5,
                               CYR = new_SS_dat_year,
                               curr_wk = curr_wk)

cumul_plots[[1]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "cummC_CG.png"), width = 700, height = 400)
dev.off()

cumul_plots[[2]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "cummC_WG.png"), width = 700, height = 400)
dev.off()


## Plot fishery condition ----

source(here::here(new_SS_dat_year, "R", "plots", "fisheries_condition.r"))

# Fish condition
cond_plot <- plot_fish_cond(CYR = new_SS_dat_year,
                            data_query = data_query)

cond_plot[[1]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "Cond_WGOA.png"), width = 700, height = 700)
dev.off()

cond_plot[[2]]
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "Cond_CGOA.png"), width = 700, height = 700)
dev.off()


## Plot number of vessels ----

source(here::here(new_SS_dat_year, "R", "plots", "num_vess.r"))

## number of vessels
num_vess <- num_fish_vess(CYR = new_SS_dat_year,
                          data_query = data_query)

num_vess
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "num_vess.png"), width = 700, height = 400)
dev.off()


## Plot PCod bycatch in pollock and swf fisheries ----

source(here::here(new_SS_dat_year, "R", "plots", "cod_bycatch_plots.r"))

# Pollock plots
pol_plots <- pollock_bycatch(CYR = new_SS_dat_year,
                             data_query = data_query)

multiplot(pol_plots[[1]], pol_plots[[2]], cols = 1)
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "poll_bycatch.png"), width = 700, height = 700)
dev.off()

# SWF plots (BEWARE: this takes a super-duper long time to run)
swf_plot <- swf_bycatch(CYR = new_SS_dat_year, 
                        data_query = data_query)

swf_plot
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "swf_bycatch.png"), width = 700, height = 400)
dev.off()


## Plot catch weighted depth and mean length ----

source(here::here(new_SS_dat_year, "R", "plots", "mean_depth_len.r"))

mean_dl <- plot_mean_dl(data_query = data_query,
                        CYR = new_SS_dat_year)

multiplot(mean_dl[[1]], mean_dl[[2]], cols = 1)
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "Mean_len.png"), width = 1024, height = 1000)
dev.off()

multiplot(mean_dl[[3]], mean_dl[[4]], cols = 1)
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "Mean_dep.png"), width = 1024, height = 1000)
dev.off()


## Plot environmental indices ----

env_data <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'raw_cfsr.csv'))

source(here::here(new_SS_dat_year, "R", "plots", "env_indices.r"))

env_ind <- plot_env_ind(env_data)

multiplot(env_ind[[1]], env_ind[[2]], cols = 1)
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "Env_indx.png"), width = 1024, height = 1000)
dev.off()


## Plot MCMC ----

load(here::here(new_SS_dat_year, "output", "mcmc.RData"))
source(here::here(new_SS_dat_year, "R", "plots", "mcmcplots.r"))

mcmc_dir <- here::here(new_SS_dat_year, "mgmt", Model_name_new, "MCMC")

mcmc_plots <- plot_mcmc(mcmc_dir, new_SS_dat_year)

# Save plot
multiplot(mcmc_plots[[1]], mcmc_plots[[2]], cols = 1)
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "SSB_Rec.png"), width = 1024, height = 1000)
dev.off()


## plot sampling rates diags ----

load(here::here(new_SS_dat_year, "output", "lencomp.RData"))

vroom::vroom(here::here(new_SS_dat_year, 'data', 'raw', 'catch.csv')) %>% 
  dplyr::rename_all(tolower) %>%
  tidytable::select(year, gear, tons) %>% 
  tidytable::filter(gear %in% c('HAL', 'POT', 'TRW')) %>% 
  tidytable::mutate(gear = case_when(gear == 'TRW' ~ "TRAWL",
                                     gear == 'POT' ~ "POT",
                                     gear == 'HAL' ~ "LONGLINE")) %>% 
  tidytable::summarise(catch = sum(tons), .by = c(year, gear)) -> catch


lencomp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(haul1 = as.character(paste(cruise, permit, haul, sep = "_")),
                    area = trunc(area/10)*10,
                    gear = case_when(gear == 0 ~ "TRAWL",
                                     gear == 2 ~ "POT",
                                     gear == 3 ~ "LONGLINE")) %>% 
  tidytable::summarise(len_ss = sum(freq), .by = c(year, gear)) %>%  
  tidytable::left_join(catch) %>% 
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

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "sampling_rates.png"), width = 1000, height = 1000)
dev.off()

















