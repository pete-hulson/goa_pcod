# script to start new assessment year folder
# developed in 2025 by fart in an elevator
# NOTE: make sure you don't run this until the new year, dumb-ass

# get the new assessment year - yehaw
new_year <- as.numeric(format(Sys.Date(), format = '%Y'))

# copy needed data ----
## ageing error info ----
file.copy(from = here::here(new_year - 1, 'data', 'ageing_error'),
          to = here::here(new_year, 'data', 'ageing_error'))
## historical & 'fixed' info ----
file.copy(from = here::here(new_year - 1, 'data', 'historical'),
          to = here::here(new_year, 'data', 'historical'))
#' notes: 
#' - will need to add accepted model to model retro
#' - will need to add accepted apportionment
#' - will need to add accepted reference points