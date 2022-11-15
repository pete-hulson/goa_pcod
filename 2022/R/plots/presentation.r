## Function to plot IPHC and ADF&G lengths
library(magrittr)
library(tidyr)
library(tidytable)
library(dplyr)
library(ggplot2)
library(nmfspalette)

# Define schtuff
Model_name_new <- "Model19.1a (22) - wADFG"
Model_name_old <- "Model19.1 (21)"
new_SS_dat_year <- as.numeric(format(Sys.Date(), format = "%Y"))

# Read IPHC.ADF&G data
IPHC <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'IPHCsurv_length.csv'))
ADFG <- vroom::vroom(here::here(new_SS_dat_year, 'data', 'ADFGsurv_length.csv'))


IPHC %>% 
  group_by(year, length) %>% 
  count(length) %>% 
  mutate.(tot_n = sum(n), .by = year) %>% 
  mutate(lcomp = n / tot_n) -> iphc_dat

ADFG %>% 
  mutate.(tot_yr = sum(total), .by = year) %>% 
  mutate.(lfreq = total / tot_yr, .by = c(year, length)) -> adfg_dat

adfg_dat %>% 
  group_by(year) %>% 
  summarise(mean_len = sum(length * lfreq),
            sd_mean_len = sd(length * lfreq)) -> adfg_mean_len
  
# Plot IPHC length freqs
ggplot(data = iphc_dat, 
       aes(x = length, y = lcomp, fill = factor(year))) + 
  geom_bar(stat = "identity", width = 0.777) + 
  scale_fill_nmfs("seagrass", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = 'none') +
  facet_wrap(~year, 
             nrow = 2, 
             scales = "free_y",
             strip.position = "left") +
  labs(y = "IPHC length composition", x = "Length (cm)")

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "iphc_lfreq.png"), width = 700, height = 400)
dev.off()



# Plot ADF&G mean length
ggplot(data = adfg_mean_len, 
       aes(x = year, y = mean_len, color = factor(mean_len))) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean_len - 1.96 * sd_mean_len, ymax = mean_len + 1.96 * sd_mean_len), width = 0, lwd = 1.25) +
  scale_color_nmfs("waves", name = "") +
  theme_bw(base_size = 21) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  labs(y = "ADF&G mean length (cm)", x = 'Year')

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "adfg_meanlen.png"), width = 400, height = 400)
dev.off()



# Plot ADF&G length freqs
ggplot(data = adfg_dat %>% filter(year >= 2021), 
       aes(x = length, y = lfreq, fill = factor(year))) + 
  geom_bar(stat = "identity", width = 0.777) + 
  scale_fill_nmfs("seagrass", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = 'none') +
  facet_wrap(~year, 
             nrow = 2, 
             scales = "free_y",
             strip.position = "left") +
  labs(y = "ADF&G length composition", x = "Length (cm)")

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "adfg_lfreq.png"), width = 700, height = 400)
dev.off()



# Plot survey length comp comparisons

model_dir_new <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new)
model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)

new_SS_dat_filename <- "GOAPcod2022Oct25_wADFG.dat"

model_dat <- r4ss::SS_readdat_3.30(here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new, new_SS_dat_filename), verbose = TRUE)


model_dat$lencomp %>% 
  filter(FltSvy == 4,
         Yr == 2021) %>% 
  pivot_longer(cols = paste0("l", seq(0.5, 116.5, by = 1))) %>% 
  mutate(length = seq(1, 117, 1),
         svy = "AFSC") %>% 
  select(Yr, svy, length, value) %>% 
  rename("lfreq" = value,
         "year" = Yr) -> afsc_twl

adfg_dat %>% 
  select(year, length, lfreq) %>% 
  mutate(svy = "ADF&G") %>% 
  filter(year == 2021) %>% 
  bind_rows(afsc_twl) -> twl_dat
  
model_dat$lencomp %>% 
  filter(FltSvy == 5,
         Yr == 2022) %>% 
  pivot_longer(cols = paste0("l", seq(0.5, 116.5, by = 1))) %>% 
  mutate(length = seq(1, 117, 1),
         svy = "AFSC") %>% 
  select(Yr, svy, length, value) %>% 
  rename("lcomp" = value,
         "year" = Yr) -> afsc_ll

iphc_dat %>% 
  filter(year == 2022) %>% 
  mutate(svy = "IPHC") %>% 
  select(year, svy, length, lcomp) %>% 
  bind_rows(afsc_ll) -> ll_dat

# Trawl surveys length freqs
ggplot(data = twl_dat, 
       aes(x = length, y = lfreq, fill = factor(svy))) + 
  geom_bar(stat = "identity", width = 0.777) + 
  scale_fill_nmfs("seagrass", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = 'none') +
  facet_wrap(~svy, 
             nrow = 2, 
             scales = "free_y",
             strip.position = "left") +
  labs(y = "2021 Trawl survey length composition", x = "Length (cm)")

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "twlsvy_lfreq.png"), width = 700, height = 400)
dev.off()

# LL surveys length freqs
ggplot(data = ll_dat, 
       aes(x = length, y = lcomp, fill = factor(svy))) + 
  geom_bar(stat = "identity", width = 0.777) + 
  scale_fill_nmfs("seagrass", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = 'none') +
  facet_wrap(~svy, 
             nrow = 2, 
             scales = "free_y",
             strip.position = "left") +
  labs(y = "2021 Longline survey length composition", x = "Length (cm)")

dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "llsvy_lfreq.png"), width = 700, height = 400)
dev.off()


# plot M comparisons

model_dir_old <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_old)

model_run_old <- r4ss::SS_output(dir = model_dir_old,
                                 verbose = TRUE,
                                 printstats = TRUE)

model_dir_new <- here::here(new_SS_dat_year, "Stock_Synthesis_files", Model_name_new)

model_run_new <- r4ss::SS_output(dir = model_dir_new,
                                 verbose = TRUE,
                                 printstats = TRUE)


model_run_old$M_at_age %>% 
  select(Yr, '0') %>% 
  rename('year' = Yr,
         'M_2021' = '0') %>% 
  filter(year <= 2021) -> m_21


model_run_new$M_at_age %>% 
  select(Yr, '0') %>% 
  rename('year' = Yr,
         'M_2022' = '0') %>% 
  filter(year <= 2022) %>% 
  left_join(m_21) %>% 
  pivot_longer(c(M_2021, M_2022)) -> m_dat

ggplot(data = m_dat, 
       aes(x = year, y = value, color = factor(name))) + 
  geom_step(lwd = 1.5) + 
  scale_color_nmfs("oceans", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = c(0.5, 0.8)) +
  labs(y = "Natural mortality", x = "Year")
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "natmort.png"), width = 700, height = 400)
dev.off()


# Plot recruitment comparison
model_run_old$recruit %>% 
  select(Yr, pred_recr) %>% 
  rename('year' = Yr,
         'R_2021' = pred_recr) %>% 
  filter(year <= 2020,
         year >= 1978) %>% 
  bind_rows(model_run_old$recruit %>% 
              select(Yr, pred_recr) %>% 
              rename('year' = Yr,
                     'R_2021' = pred_recr) %>% 
              filter(year == 2022)) -> r_21

model_run_new$recruit %>% 
  select(Yr, pred_recr) %>% 
  rename('year' = Yr,
         'R_2022' = pred_recr) %>% 
  filter(year <= 2020,
         year >= 1978) %>% 
  bind_rows(model_run_new$recruit %>% 
              select(Yr, pred_recr) %>% 
              rename('year' = Yr,
                     'R_2022' = pred_recr) %>% 
              filter(year == 2022)) %>% 
  left_join(r_21) %>% 
  pivot_longer(c(R_2021, R_2022)) %>% 
  mutate(value = value / 1000000) -> r_dat

ggplot(data = r_dat, 
       aes(x = year, y = value, fill = factor(name))) + 
  geom_bar(stat = "identity", width = 0.777, position = position_dodge()) + 
  scale_fill_nmfs("waves", name = "") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, angle = 90),
        legend.position = c(0.5, 0.8)) +
  labs(y = "Age-0 recruitment (billions)", x = "Year")
dev.print(png, file = here::here(new_SS_dat_year, "plots", "other", "recruit.png"), width = 700, height = 400)
dev.off()


