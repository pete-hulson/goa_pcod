new_year <- 2024

# read in data ----
catch <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_catch.csv'), delim = ',') %>% 
  tidytable::select(-lat, -lon, -survey, -species_code)
haul <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_haul.csv'), delim = ',') %>% 
  tidytable::select(-lat, -lon, -survey, -stemp)
lfreq <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_lenfreq.csv'), delim = ',') %>% 
  tidytable::select(-lat, -lon, - sex, -survey, -species_code, -stemp) %>% 
  tidytable::summarise(freq = sum(frequency), .by = c(year, stratum, hauljoin, temp, depth, length))
strata <- vroom::vroom(here::here(new_year, 'data', 'raw', 'twl_srvy_strata.csv'), delim = ',') %>% 
  tidytable::mutate(Subarea = case_when(subarea_name == 'WESTERN GOA - INPFC' ~ 'Western',
                                        subarea_name == 'CENTRAL GOA - INPFC' ~ 'Central',
                                        subarea_name == 'EASTERN GOA - INPFC' ~ 'Eastern',
                                        subarea_name == 'WESTERN GOA - NMFS' ~ 'Western',
                                        subarea_name == 'CENTRAL GOA - NMFS' ~ 'Central',
                                        subarea_name == 'EASTERN GOA - NMFS' ~ 'Eastern')) %>% 
  tidytable::select(-area_id, -design_year, -subarea_name)

# set up plot data function ----
plot_dat <- function(subregion, catch, haul, lfreq, strata){
  # weight temperature by area to get mean
  haul %>% 
    tidytable::filter(!is.na(temp)) %>% 
    tidytable::left_join(strata %>% 
                           tidytable::filter(Subarea %in% subregion)) %>% 
    tidytable::filter(!is.na(area)) %>% 
    tidytable::summarise(mtemp = mean(temp),
                         n = .N,
                         .by = c(year, stratum, area, Subarea)) %>% 
    tidytable::mutate(tot_area = sum(area), .by = c(year)) %>% 
    tidytable::mutate(prop_area = area / tot_area) %>% 
    tidytable::summarise(mtemp = weighted.mean(mtemp, prop_area),
                         sd_temp = sqrt(modi::weighted.var(mtemp, prop_area)),
                         .by = c(year, Subarea)) -> temp
  
  # weight depth by cpue to get mean
  catch %>% 
    tidytable::left_join(haul) %>% 
    tidytable::filter(!is.na(cpue),
                      !is.na(depth)) %>% 
    tidytable::left_join(lfreq) %>% 
    tidytable::filter(!is.na(length)) %>% 
    tidytable::mutate(bin = case_when(length <= 60 ~ 1,
                                      length %in% seq(61, 80) ~ 2,
                                      length >= 80 ~ 3)) %>% 
    tidytable::summarise(freq = sum(freq), .by = c(year, stratum, hauljoin, cpue, temp, depth, bin)) %>% 
    tidytable::left_join(strata %>% 
                           tidytable::filter(Subarea %in% subregion)) %>% 
    tidytable::filter(!is.na(area)) %>% 
    tidytable::summarise(mdepth = weighted.mean(depth, cpue),
                         sd_depth = sqrt(modi::weighted.var(depth, cpue)),
                         n = length(unique(hauljoin)),
                         .by = c(year, Subarea, bin)) %>% 
    tidytable::filter(n >= 10) -> depth

  # join mean depth and mean temp
  depth %>% 
    tidytable::left_join(temp) %>% 
    mutate(xtemp = mean(mtemp),
           sdxtemp =sd(mtemp), 
           .by = c(bin)) %>% 
    tidytable::mutate(label = case_when(bin == 1 ~ '0-60 cm',
                                        bin == 2 ~ '61-80 cm',
                                        bin == 3 ~ '81-plus cm'),
                      regi = case_when(mtemp <= (xtemp - 0.66 * sdxtemp) ~ 'Cold',
                                       mtemp >= (xtemp + 0.66 * sdxtemp) ~ 'Warm',
                                       .default = 'Med')) -> dat
  dat
}

# set up plot function ----
plot_temdep <- function(dat){
  
  dat_text <- data.frame(
    text = c("0.15%", "", "",
             "2.67%", "", "",
             "7.77%", "", "",
             "22.3%", "", ""),
    label = c('0-60 cm', '61-80 cm', '81-plus cm',
              '0-60 cm', '61-80 cm', '81-plus cm',
              '0-60 cm', '61-80 cm', '81-plus cm',
              '0-60 cm', '61-80 cm', '81-plus cm'),
    x = 2.5,
    y = c(rep(-50, 3),
          rep(-125, 3),
          rep(-175, 3),
          rep(-225, 3)),
    Subarea = 'Central',
    regi = 'Med')
  
  ggplot(data = dat, aes(x = mtemp, y = -1 * mdepth, group = label, shape = Subarea, color = regi, fill = regi)) +
    geom_point() +
    geom_hline(yintercept = c(-100, -150, -200)) +
    facet_wrap(~ label) +
    scale_colour_manual(name = "Shelf temp.", values = c("blue","darkgrey","red")) +
    scale_fill_manual(name = "Shelf temp.", values = c("blue","darkgrey","red")) +
    geom_pointrange(data = dat, aes(y = -1 * mdepth, x = mtemp,
                                    ymax = -1 * (mdepth + sd_depth), 
                                    ymin = -1 * (mdepth - sd_depth),
                                    color = regi),
                    linewidth = 0.25) +
    geom_errorbarh(data = dat, aes(xmax = mtemp - sd_temp, 
                                   xmin = mtemp + sd_temp,
                                   color = regi),
                   linewidth = 0.25) +
    theme_test(base_size = 15) +
    theme(legend.position = 'top') +
    xlab(expression("Temperature ("* degree * C *")")) +
    ylab("Depth (m)") +
    geom_point(data = dat %>% 
                 tidytable::summarise(mtemp = mean(mtemp),
                                      mdepth = mean(mdepth),
                                      .by = c(bin, label, regi)),
               aes(x = mtemp, y = -1 * mdepth), 
               size = 3, shape = 23, color = "black", stroke = 1.5) +
    xlim(2, 8) + 
    geom_text(data = dat_text, mapping = aes(x = x, y = y, label = text), fontface = 'bold', color = "black")
}

# all ----
dat <- plot_dat(subregion = c('Western', 'Central', 'Eastern'), catch, haul, lfreq, strata)
plot <- plot_temdep(dat)

suppressWarnings(ggplot2::ggsave(plot,
                                 file = here::here(new_year, "plots", 'other','temp_depth.png'),
                                 width = 9, height = 4, unit = 'in', dpi = 520))


# getting proportion of hatchi at depth
# vroom::vroom(here::here(new_year, 'data', 'raw', 'lls_depth_summary.csv'), delim = ',') %>% 
#   tidytable::select(year = "Year",
#                     hachi = "Hachi",
#                     depth_stratum = "Depth Stratum2",
#                     st_desc = "Depth Stratum2 Description") %>% 
#   tidytable::summarise(num_hachi = .N,
#                        .by = c(depth_stratum, st_desc)) %>% 
#   tidytable::mutate(prop = num_hachi / sum(num_hachi)) %>% 
#   tidytable::filter(depth_stratum %in% c('1', '2a', '2b', '3')) %>% 
#   tidytable::select(-num_hachi) -> ll_dat
# 
# ggplot(data = ll_dat, aes(x = st_desc, y = prop, factor = st_desc)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_x_discrete(limits = rev) +
#   theme_bw()


