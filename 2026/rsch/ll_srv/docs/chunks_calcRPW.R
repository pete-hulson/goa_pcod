##############################################################################################
# Total Catch
# Calculate total catch by stratum, adjusting for ineffective hooks to 45 hooks
# per skate
# Output: total_catch
##############################################################################################
#
#summarize total catch frequency by cruise, station, hachi, and species, adjusted to 45 hooks
adj_freq = tapply((catches$freq / (45 - catches$ineffective)) * 45, catches$cs.h.spc, sum)
temp.match = match(names(adj_freq), catches$cs.h.spc)
hachi_catch = catches[temp.match, 1:14]
hachi_catch$adj_catch_freq = rep(NA, dim(hachi_catch)[1])
hachi_catch$adj_catch_freq = as.numeric(adj_freq)
rm(adj_freq)

#summarize adjusted catch to stratum2
hachi_catch$cs.str.spc = paste(hachi_catch$csid, hachi_catch$stratum2, hachi_catch$species)

# save an uncorrected version fovariancer  calculations later, i.e. the version
# of hachi_catch used to get variances doesn't include the whale-corrected
# CPUEs, though ultimately the variance is inflated for stations with
# depredation present. The latter is a separate correction. hachi_catch_nodep replace whalehachi
hachi_catch_nodep<-hachi_catch
hachinames<-names(hachi_catch)
hachi_catch<-merge(hachi_catch,station_sperm_pres,all.x=TRUE)
hachi_catch<-merge(hachi_catch,station_sperm_dep,all.x=TRUE) # only in sablefish script, not in rockfish/PT scripts
hachi_catch$pres<-as.numeric(hachi_catch$pres)
hachi_catch[hachi_catch$dep==1&hachi_catch$species==20510,]$adj_catch_freq=wcfd*hachi_catch[hachi_catch$dep==1&hachi_catch$species==20510,]$adj_catch_freq
hachi_catch[hachi_catch$dep==0&hachi_catch$species==20510&hachi_catch$pres==1,]$adj_catch_freq=wcfp*hachi_catch[hachi_catch$dep==0&hachi_catch$species==20510&hachi_catch$pres==1,]$adj_catch_freq # only in sablefish script, not in rockfish/PT scripts
hachi_catch<-hachi_catch[,hachinames]

catch_sum = tapply(hachi_catch$adj_catch_freq, hachi_catch$cs.str.spc, sum)
temp.match = match(names(catch_sum), hachi_catch$cs.str.spc)

#keep both stratum and stratum2 because we need stratum to link to mean lengths/weights below
total_catch = hachi_catch[temp.match,c(1:8,13,11,12,14)]
total_catch$tot_catch_freq = rep(NA, dim(total_catch)[1])
total_catch$tot_catch_freq = as.numeric(catch_sum)
#rm(hachi_catch)
#rm(catch_sum)
#rm(temp.match)
print("Calculated Total Catch")

##############################################################################################
# Total Effort
# Calculate total skates by stratum
# Output: total_skates
##############################################################################################
#
#filter down to the unique hachis (which correspond 1-1 to depth records)
catches$cs.h = paste(catches$csid, catches$hachi)
temp = tapply(catches$freq, catches$cs.h, sum)
temp.match = match(names(temp), catches$cs.h)
skates = catches[temp.match,]

#count the unique hachis at each stratum
skates$cs.str = paste(skates$csid, skates$stratum2)
num_skates = tapply(skates$hachi, skates$cs.str, length)
temp.match = match(names(num_skates), skates$cs.str)
#keep both stratum and stratum2 because we need stratum to link to mean lengths/weights below
total_skates = skates[temp.match, c(1:8,13,11,12,14)]
total_skates$num_hachis = rep(NA, dim(total_skates)[1])
total_skates$num_hachis = as.numeric(num_skates)
rm(skates)
rm(temp)
print("Calculated Total Effort")
##############################################################################################

##############################################################################################
# CPUE
# Now given the summaries of Catch, Effort (skates) and Length/Weight information from above:
# Calculate the Catch Per Unit of Effort by station, species, and stratum 
# Also calculates the Relative Population Numbers (RPN) and Relative Population Weights (RPWs)
# Returns a table of catch summary information by station-stratum-species
# Output: total_catch
##############################################################################################
#tie skates to catch on station and stratum
total_catch$cs.str2 = paste(total_catch$csid, total_catch$stratum2)
total_skates$cs.str2 = paste(total_skates$csid, total_skates$stratum2)
temp.match = match(total_catch$cs.str2, total_skates$cs.str2)
total_catch$total_skates = rep(NA, dim(total_catch)[1])
total_catch$total_skates = total_skates[temp.match, 13]
total_catch$cpue = rep(NA, dim(total_catch)[1])
total_catch$cpue = total_catch$tot_catch_freq / total_catch$total_skates
rm(total_skates)

#tack on mean lengths and weights
#note we use stratum (not stratum2) to link to weights because there are no stratum2 values in weights/lengths
total_catch$cs.str.spc = paste(total_catch$csid, total_catch$stratum, total_catch$species)
mean_weights$cs.str.spc = paste(mean_weights$csid, mean_weights$stratum, mean_weights$species)
temp.match = match(total_catch$cs.str.spc, mean_weights$cs.str.spc)
total_catch$total_len_freq = rep(NA, dim(total_catch)[1])
total_catch$total_len_freq = mean_weights[temp.match, 10]
total_catch$mean_len = rep(NA, dim(total_catch)[1])
total_catch$mean_len = mean_weights[temp.match, 12]
total_catch$mean_wt = rep(NA, dim(total_catch)[1])
total_catch$mean_wt = mean_weights[temp.match, 14]

#calculate RPN & RPW
total_catch$RPN = rep(NA, dim(total_catch)[1])
total_catch$RPN = total_catch$cpue * total_catch$area_size2
total_catch$RPW = rep(NA, dim(total_catch)[1])
total_catch$RPW = total_catch$cpue * total_catch$area_size2 * total_catch$mean_wt
print("Calculated CPUE, RPN, RPW")
##############################################################################################


##############################################################################################
# Variance-covariance calculations with whale correction if specified. Variances
# reported by geographic area, accounting for covariance among strata within a
# geographic area. Variances reported for US and Japanese data separately. User
# beware: that the Japanese survey design has not been reviewed thoroughly to
# ensure the same variance calculations are valid for this data set.
# Output: allvar_2b_7 and allvar_3_7
##############################################################################################

# Variance species defined by Cara
hachi_catch_nodep<-hachi_catch_nodep[hachi_catch_nodep$species%in%unique(var_species$species),]

countries <- unique(hachi_catch_nodep$country)

for(c in countries) {
  
  # Calculate variances for US an Japanese data separately. The survey design for
  # Japanese data were not reviewed to ensure these variance methods are
  # appropriate - user beware.
  hachi_catch_country<-hachi_catch_nodep[hachi_catch_nodep$country==c,]
  
  # Dimension for output array
  n.species<-length(unique(var_species$species)) # length(species[species$web_flag==1&!is.na(species$web_flag),1])
  n.area<-length(levels(factor(hachi_catch_country$area)))
  n.stratum<-length(levels(factor(hachi_catch_country$stratum2)))
  # area.vcov.cf <- array(NA, c(n.area,n.area,n.species))
  
  # weights = area sizes
  weights<-tapply(hachi_catch_country$area_size2,list(hachi_catch_country$stratum2,hachi_catch_country$area),min)
  weights[is.na(weights)]<-0
  
  # station_obs (formerly station.obs) = mean catch per hachi (cpue) by station x
  # stratum2 x geographic area and species. Each of these means equals one
  # observation; observations of mean cpue are not weighted by the number of
  # hachis in the station x stratum2 x area combination.
  station_obs<-tapply(hachi_catch_country$adj_catch_freq,list(hachi_catch_country$station,hachi_catch_country$stratum2,hachi_catch_country$species,hachi_catch_country$area), mean)
  
  dim(station_obs)==c(length(unique(hachi_catch_country$station)),
                      n.stratum,
                      n.species,
                      n.area)
  
  # sperm_pres (formerly 'G') is an array with same dimensions as station_obs
  # populated with 1 or 0 for presence/absence of sperm whales at that station
  # observation
  sperm_pres<-station_obs
  sperm_pres[,,,]<-NA
  # index pointing to sablefish slice in the array (only sablefish are corrected
  # for whale depredation)
  species_index <- sort(unique(hachi_catch_country$species))
  sable_index<-which(species_index %in% 20510)
  not_sable_index<-which(!c(species_index %in% 20510))
  # populate array
  sperm_pres[,,sable_index,]<-as.numeric(station_sperm_pres[station_sperm_pres$station %in% unique(hachi_catch_country$station),"pres"])
  sperm_pres[,,not_sable_index,]<-0
  
  # sperm_pres_wcf (formerly 'D') is an array with same dimensions as station_obs
  # and sperm_pres. replace 1s with the whale correction factors and 0s with 1s.
  # Stations with values not equal to 1 will be corrected.
  sperm_pres_wcf<-sperm_pres
  sperm_pres_wcf[sperm_pres==1]=wcfd
  sperm_pres_wcf[sperm_pres==0]=1
  station=station_obs*sperm_pres_wcf
  
  all.equal(which(is.na(station)), which(is.na(station_obs))) # should be TRUE
  
  # Area covariances
  
  ##(B) Variances + Covariances due to stations observations only (without wcfd.se)
  
  stratum <- apply(station, c(2,3,4), mean, na.rm=TRUE)	# stratum means (by area, each station equal weight)
  for (i in 1:n.area) {stratum[,,i] <- stratum[,,i]*weights[,i]}	# multiply stratum means by area-stratum weights
  # area <- apply(stratum, c(2,3), sum, na.rm=T) # area sum (across weighted strata)
  # index <- rowSums(area) # total (across areas)
  
  stratum.var <- apply(station, c(2,3,4), var, na.rm=TRUE)/apply(!is.na(station), c(2,3,4), sum)	# variances of strata means
  for (i in 1:n.area) {stratum.var[,,i] <- stratum.var[,,i]*weights[,i]^2} # multiply stratum var by weights^2
  stratum.cv<-sqrt(stratum.var)/stratum # stratum CV
  
  # Identify cases where the number of stations in a species/geographic
  # area/stratum is 1, then fix the area/strata combinations with only one station
  # to the average CV e.g. variance = (mean(cv) * mean)^2 for that species across
  # all areas.
  onestation<-apply(!is.na(station), c(2,3,4), sum)
  onestation[onestation>1]<-0
  # stratum.var2 <- stratum.var
  for(s in 1:n.species) {
    stratum.var[,s,][onestation[,s,]==1]=(mean(stratum.cv[,s,],na.rm=TRUE)*stratum[,s,][onestation[,s,]==1])^2
  }
  # length(which(stratum.var == stratum.var2)) + length(which(is.na(stratum.var2))) == prod(dim(stratum.var))
  
  # area.var <- apply(stratum.var, c(2,3), sum, na.rm=T) # geographic area variances (ignoring covariances for now)
  # index.var <- rowSums(area.var) # index variances by year (ignoring covariances)
  # index.var.cv <- sqrt(index.var)/rowSums(area)	# CVs without covariances
  
  # objects for covariance results
  # area.cov <- area.var; area.cov[,] <- NA
  stratum.vcov <- array(NA, c(n.stratum, n.stratum, n.species, n.area))
  dimnames(stratum.vcov) <- list(levels(factor(hachi_catch_country$stratum2)), 
                                 levels(factor(hachi_catch_country$stratum2)),
                                 levels(factor(hachi_catch_country$species)),
                                 levels(factor(hachi_catch_country$area)))
  
  # covariance without whale depredation, using same notation as in the
  # documentation
  for (i in 1:n.area) {		 # loop over areas				
    for (s in 1:n.species) {	# loop over species
      for (j in 1:n.stratum) {	# loop over strata j
        for (m in 1:n.stratum) { # loop over strata m
          w.j <- weights[j,i]; w.m <- weights[m,i]				# stratum weights (j and m)
          k <- station[,j,s,i]; z <- station[,m,s,i]			# station means {k} for j and {z} for m
          q <- cov(k, z, use="pairwise.complete.obs")			# station variance (when j = m) or covariance (j != m)
          n.k <- sum(!is.na(k)); n.z <- sum(!is.na(z))		# numbers of stations with data (j or m)
          n.kz <- sum(!is.na(k*z))						            # numbers of station pairs (j and m) both with data 
          stratum.vcov[j,m,s,i] <- w.j*w.m*n.kz/(n.k*n.z)*q	
          diag(stratum.vcov[,,s,i])=stratum.var[,s,i]
        }
      }
      # area.cov[s,i] <- sum(stratum.vcov[,,s,i], na.rm=T)	# sum of weighted var-covariance by area
    }
  }
  
  # Final geographic areas should be separated for strata 2b-7 and 3-7 so
  # covariances sum properly
  strat_2b_7 <- c("2b","3","4","5","6","7")
  strat_3_7 <- c("3","4","5","6","7")
  
  # Sum stratum variance-covariances to the geographic area level. On the rare
  # occurence when the stratum var-cov matrix sums to a negative number, replace
  # it with the sum of the stratum-specific variances (i.e. ignore the
  # covariances). Repeat for strata 2b-7 and 3-7 combinations.
  var_2b_7 <- apply(stratum.vcov[strat_2b_7, strat_2b_7, , ], c(3,4), sum, na.rm=TRUE)
  var_2b_7[var_2b_7 < 0] <- (apply(stratum.var[strat_2b_7, , ], c(2,3), sum, na.rm=TRUE))[var_2b_7 < 0]
  
  var_3_7 <- apply(stratum.vcov[strat_3_7, strat_3_7, , ], c(3,4), sum, na.rm=TRUE)
  var_3_7[var_3_7 < 0] <- (apply(stratum.var[strat_3_7, , ], c(2,3), sum, na.rm=TRUE))[var_3_7 < 0]
  
  # Sablefish variances and covariances due to sperm whale PRESENCE. Note
  # that only the variances have been used for sablefish RPNs.
  sperm_vcov_2b_7 <- sperm_vcov_3_7 <- array(NA, dim = c(n.area, n.area), 
                                             dimnames = list(levels(factor(hachi_catch_country$area)), 
                                                             levels(factor(hachi_catch_country$area))))
  
  # index pointing to strata slices in the array (only sablefish are corrected
  # for whale depredation)
  strata_index <- sort(unique(hachi_catch_country$stratum2))
  index_2b_7 <- which(strata_index %in% strat_2b_7)
  index_3_7 <- which(strata_index %in% strat_3_7)
  
  # Variance is inflated at stations where sperm whales are present, not just on
  # stations where there is evidence of depredation. As such, the observed station
  # CPUE is used rather than the whale-corrected station CPUE. Note that only the
  # variances by geographic area have been used for the whale-corrected sablefish
  # RPNs, and we have preserved that convention. However, the covariance among
  # areas IS calculated, and could theoretically be used if someone wanted that.
  # station_obs = array indexed as c(station, stratum2, species, geographic area)
  
  # Depth strata 2b-7
  for (i in 1:n.area) {
    for (r in 1:n.area) {
      temp.cov <- 0
      for (j in index_2b_7) {
        for (m in index_2b_7) {
          w.j <- weights[j,i]; w.m <- weights[m,r]				                        # stratum area sizes / weights (j,i) and (r,m)
          k <- station_obs[,j,sable_index,i]; z <- station_obs[,m,sable_index,r]	# observed station means {k} for j and {z} for m
          n.k <- sum(!is.na(k)); n.z <- sum(!is.na(z))			                      # numbers of stations with data (j or m)
          k.sum <- sum(sperm_pres[,j,sable_index,i]*k, na.rm=TRUE)				        # sum of sperm_present (0 or 1) * observed station mean CPUEs {k} in stratum j
          z.sum <- sum(sperm_pres[,m,sable_index,r]*z, na.rm=TRUE)			        	# sum of sperm_present (0 or 1) * observed station mean CPUEs {z} in stratum m
          q <- (w.j*w.m)/(n.k*n.z)*(wcfd.se^2*k.sum*z.sum)			                  # variance/covariance due to correction factor
          if (is.na(q)) q <- 0
          temp.cov <- temp.cov + q	                                              # Sum of stratum combos
        }
      }
      sperm_vcov_2b_7[i,r] <- temp.cov
    }
  }
  
  # Repeat for depth strata 3-7
  for (i in 1:n.area) {
    for (r in 1:n.area) {
      temp.cov <- 0
      for (j in index_3_7) {
        for (m in index_3_7) {
          w.j <- weights[j,i]; w.m <- weights[m,r]				                        # stratum area sizes / weights (j,i) and (r,m)
          k <- station_obs[,j,sable_index,i]; z <- station_obs[,m,sable_index,r]	# observed station means {k} for j and {z} for m
          n.k <- sum(!is.na(k)); n.z <- sum(!is.na(z))			                      # numbers of stations with data (j or m)
          k.sum <- sum(sperm_pres[,j,sable_index,i]*k, na.rm=TRUE)				        # sum of sperm_present (0 or 1) * observed station mean CPUEs {k} in stratum j
          z.sum <- sum(sperm_pres[,m,sable_index,r]*z, na.rm=TRUE)			        	# sum of sperm_present (0 or 1) * observed station mean CPUEs {z} in stratum m
          q <- (w.j*w.m)/(n.k*n.z)*(wcfd.se^2*k.sum*z.sum)			                  # variance/covariance due to correction factor
          if (is.na(q)) q <- 0
          temp.cov <- temp.cov + q	                                              # Sum of stratum combos
        }
      }
      sperm_vcov_3_7[i,r] <- temp.cov
    }
  }
  
  # Following methods in the  sablefish and rockfish/PT code, only add variances
  # (not covariances) for the sperm whale presence correction factor to get final
  # variances for sablefish.
  var_2b_7[sable_index,] <- var_2b_7[sable_index,] + diag(sperm_vcov_2b_7) 
  var_3_7[sable_index,] <- var_3_7[sable_index,] + diag(sperm_vcov_3_7)
  
  # reformat 2b-7
  var_2b_7 <- data.frame(var_2b_7) %>% 
    rownames_to_column(var = "species") 
  
  colnames(var_2b_7) <- c("species", sort(unique(hachi_catch_country$area)))
  
  var_2b_7 <- var_2b_7 %>% 
    mutate(country = c) %>% 
    pivot_longer(-c(species, country), names_to = "area", values_to = "RPN_var") %>%
    mutate(species = as.numeric(species),
           area = as.numeric(area))
  
  # reformat var 3_7
  var_3_7 <- data.frame(var_3_7) %>% 
    rownames_to_column(var = "species") 
  
  colnames(var_3_7) <- c("species", sort(unique(hachi_catch_country$area)))
  
  var_3_7 <- var_3_7 %>% 
    mutate(country = c) %>% 
    pivot_longer(-c(species, country), names_to = "area", values_to = "RPN_var") %>%
    mutate(species = as.numeric(species),
           area = as.numeric(area))
  
  # bind variance output
  if(c == countries[1]) {
    allvar_2b_7 <- var_2b_7
    allvar_3_7 <- var_3_7
  } else {
    allvar_2b_7 <- bind_rows(allvar_2b_7, var_2b_7)
    allvar_3_7 <- bind_rows(allvar_3_7, var_3_7)
  }
  
  # Remove variances for Japanese data, which uses a smaller set of species
  jpn_var_spp <- var_species %>% 
    filter(country == "Japan") %>% 
    pull(species)
  
  # anti_join(allvar_2b_7,  
  #           allvar_2b_7 %>% 
  #             filter(!c(country == "Japan" & 
  #                         !c(species %in% jpn_var_spp)))) %>%
  #   left_join(var_species, by = "species")
  
  allvar_2b_7 <- allvar_2b_7 %>% filter(!c(country == "Japan" & !c(species %in% jpn_var_spp)))
  allvar_3_7 <- allvar_3_7 %>% filter(!c(country == "Japan" & !c(species %in% jpn_var_spp)))
  
  rm(jpn_var_spp)
  rm(hachi_catch_country)
  rm(var_2b_7,var_3_7)
  rm(sperm_vcov_2b_7,sperm_vcov_3_7)
  rm(n.area, n.species, n.stratum)
  rm(weights, station_obs, sperm_pres, 
     species_index, not_sable_index, 
     sable_index, sperm_pres_wcf, station, 
     onestation, stratum, stratum.cv, 
     stratum.vcov, index_2b_7, index_3_7,
     strata_index)
  rm(i, s, j, m, w.j, w.m, r, z, k, q, n.k, n.z, n.kz, 
     k.sum, z.sum, temp.cov)
}

##############################################################################################

##############################################################################################
# Calculate Ave CPUE, RPN, and RPW
# Aggregate total_catch to area, stratum, and species
# and calculate average CPU, RPN, and RPW
# Output: area_stratum_effort
##############################################################################################
#
#summarize cpue by area-stratum-species
cpue = tapply(total_catch$cpue, total_catch$yr.ves.area.str2.spc, sum)
temp.match = match(names(cpue), total_catch$yr.ves.area.str2.spc)
#area_stratum_effort = total_catch[temp.match,c(2:5,7:12,23:25)]
area_stratum_effort = total_catch[temp.match,c("year","country","vessel","cruise","area","area_size","area_size2","stratum","stratum2","species","yr.ves.area.str2.spc","num_stations","rpw_stations")]

#calculate avg cpue
area_stratum_effort$cpue = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$cpue = cpue / area_stratum_effort$num_stations 
rm(cpue)

#calculate avg rpn
totrpn = tapply(total_catch$RPN, total_catch$yr.ves.area.str2.spc, sum, na.rm=TRUE)
area_stratum_effort$RPN = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$RPN = totrpn / area_stratum_effort$num_stations 
# blank out RPN for rows with no area_size2 - NOTE: the following line of code is not in the sablefish or rockfish/PT script
area_stratum_effort$RPN = ifelse(is.na(area_stratum_effort$area_size2),NA,area_stratum_effort$RPN)
rm(totrpn)

#calculate avg rpw
#first update the rpw_station number count from up above - NOTE: the following
#two lines of code are not commented out in the sablefish and rockfish/PT
#scripts
#temp.match = match(area_stratum_effort$yr.ves.area.str2.spc, names(num_rpw_stations))
#area_stratum_effort$rpw_stations = num_rpw_stations[temp.match]
area_stratum_effort$rpw_stations = ifelse(is.na(area_stratum_effort$rpw_stations),
                                          area_stratum_effort$num_stations,
                                          area_stratum_effort$rpw_stations)
#rm(num_rpw_stations)
#then calculate the ave RPW
totrpw = tapply(total_catch$RPW, total_catch$yr.ves.area.str2.spc, sum, na.rm=TRUE)
area_stratum_effort$RPW = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$RPW = totrpw / area_stratum_effort$rpw_stations 
# NOTE: the following line of code is not in the sablefish or rockfish/PT script
area_stratum_effort$RPW = ifelse(is.na(area_stratum_effort$area_size2),NA,area_stratum_effort$RPW)
rm(totrpw)

#left outer join mean lengths and mean weights onto area_stratum_effort
temp.match = match(area_stratum_effort$yr.ves.area.str2.spc, mean_lengths$yr.ves.area.str2.spc)
area_stratum_effort$mean_len = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$mean_len = mean_lengths[temp.match,15]

area_stratum_effort$mean_wt = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$mean_wt = mean_lengths[temp.match,18]
rm(temp.match)
print("Summarized to Area Stratum")

##############################################################################################

##############################################################################################
# Length Frequencies
# UPDATED 2021
# Summarize Length frequencies by area, species, sex, and length (across stations and strata)
# Also extrapolates for gully stations from strata 5 to 6 at stations 142, 143
#  and strata 4 to 5 at stations 144, 145
# Output: len_freq
##############################################################################################
#
# sum up frequencies by csid, stratum, species, sex, length to match LengthFrequenciesGulliesView
lengths$csid.stratum.species.sex.length = paste(lengths$csid,lengths$stratum,lengths$species,lengths$sex,lengths$length)
temp = tapply(lengths$freq, lengths$csid.stratum.species.sex.length, sum)
temp.match = match(names(temp), lengths$csid.stratum.species.sex.length)
lengths_gullies = lengths[temp.match,c(1:11,13:14)]
lengths_gullies$freq = rep(NA, dim(lengths_gullies)[1])
lengths_gullies$freq = temp

#lookup and tack on fmp_area
temp.match = match(lengths_gullies$area, areas$area)
lengths_gullies$fmp_area = areas[temp.match,"fmp_area"]

#extraploate for gullies
lengths142_3 = lengths_gullies[(lengths_gullies$station==142|lengths_gullies$station==143)&lengths_gullies$stratum==5,]
lengths142_3$stratum = rep(6, dim(lengths142_3)[1])
lengths144_5 = lengths_gullies[(lengths_gullies$station==144|lengths_gullies$station==145)&lengths_gullies$stratum==4,]
lengths144_5$stratum = rep(5, dim(lengths144_5)[1])

lengths_gullies = rbind(lengths_gullies, lengths142_3)
lengths_gullies = rbind(lengths_gullies, lengths144_5)

#calculate fmp area wide averages
#get cross join of distinct species from lengths by areas by year (US only) by depths (2-7)
lengthed_species = unique(lengths[lengths$species!=10120,c("vessel","species")], incomparables=FALSE) 
species_by_area = merge(unique(lengthed_species[,"species"]), areas[,c("area","fmp_area")])
names(species_by_area) = c("species","area","fmp_area")
by_year_country = merge(unique(lengths[lengths$country=="United States",c("year","country")]),species_by_area)
templ = merge(by_year_country,strata[strata$stratum==2|strata$stratum==3|strata$stratum==4|strata$stratum==5|strata$stratum==6|strata$stratum==7,])
templ = unique(templ[,c("year","country","fmp_area","species","stratum")])

rm(species_by_area)
rm(by_year_country)

#unique species-area-strata where catch occurred this year
se = area_stratum_effort[!is.na(area_stratum_effort$RPN)&area_stratum_effort$RPN!=0,c("year","country","area","stratum","species")]
temp.match = match(se$area, areas$area)
se$fmp_area = areas[temp.match,"fmp_area"]
se = unique(se[,c("year","country","fmp_area","species","stratum")])

#join unique species-area_strata with catch on template to get new template
se$year.country.fmp_area.stratum.species = paste(se$year, se$country, se$fmp_area, se$stratum, se$species)
templ$year.country.fmp_area.stratum.species = paste(templ$year, templ$country, templ$fmp_area, templ$stratum, templ$species)
temp.match = match(se$year.country.fmp_area.stratum.species, templ$year.country.fmp_area.stratum.species)
templ1 = templ[temp.match,c(1:6)]
templ1 = templ1[!is.na(templ1$year),]

#calculate the fmp-wide averages of length frequencies at each station
lengths_gullies$year.country.fmp_area.stratum.species.sex.length = paste(lengths_gullies$year, lengths_gullies$country, lengths_gullies$fmp_area, lengths_gullies$stratum, lengths_gullies$species, lengths_gullies$sex, lengths_gullies$length)
temp = tapply(lengths_gullies$freq, lengths_gullies$year.country.fmp_area.stratum.species.sex.length, mean)
temp.match = match(names(temp), lengths_gullies$year.country.fmp_area.stratum.species.sex.length)
avg_length_freqs = lengths_gullies[temp.match,c("year","country","fmp_area","stratum","species","sex","length")]
avg_length_freqs$avg_freq = temp;
#join avgs to templ1 to filter to only length freqs in the strata we care about and where we have catch
avg_length_freqs$year.country.fmp_area.stratum.species = paste(avg_length_freqs$year, avg_length_freqs$country, avg_length_freqs$fmp_area, avg_length_freqs$stratum, avg_length_freqs$species)
temp.match = match(avg_length_freqs$year.country.fmp_area.stratum.species,templ1$year.country.fmp_area.stratum.species)
avg_length_freqs = avg_length_freqs[!is.na(temp.match),]

#get the area-stratum RPNs for this year to distribute across length frequencies
ase = area_stratum_effort[!is.na(area_stratum_effort$RPN)&area_stratum_effort$RPN!=0,c("year","country","vessel","cruise","area","stratum","stratum2","species","RPN","RPW")]
#filter test cruises from 95-97 and any species that are never lengthed
ase = ase[ase$cruise!=199502&ase$cruise!=199602&ase$cruise!=199603&ase$cruise!=199702&ase$cruise!=199703,]
ase$vessel.species = paste(ase$vessel, ase$species)
temp.match = match(ase$vessel.species,paste(lengthed_species[,"vessel"],lengthed_species[,"species"]))
ase$species_match = lengthed_species[temp.match,"species"]
ase = ase[!is.na(ase$species_match),c("year","country","vessel","cruise","area","stratum","stratum2","species","RPN","RPW")]
ase$year.country.cruise.area.stratum.species = paste(ase$year,ase$country,ase$cruise,ase$area,ase$stratum,ase$species)

#sum frequencies up to the area level
lengths_gullies$year.country.cruise.area.stratum.species.sex.length = paste(lengths_gullies$year, lengths_gullies$country, lengths_gullies$cruise, lengths_gullies$area, lengths_gullies$stratum, lengths_gullies$species, lengths_gullies$sex, lengths_gullies$length)
temp = tapply(lengths_gullies$freq, lengths_gullies$year.country.cruise.area.stratum.species.sex.length, sum)
temp.match = match(names(temp), lengths_gullies$year.country.cruise.area.stratum.species.sex.length)
area_stratum_lengths = lengths_gullies[temp.match,c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area")]
area_stratum_lengths$freq = temp;
#join area_stratum_lengths to ase (area_stratum_efforts)
area_stratum_lengths$year.country.cruise.area.stratum.species = paste(area_stratum_lengths$year, area_stratum_lengths$country, area_stratum_lengths$cruise, area_stratum_lengths$area, area_stratum_lengths$stratum, area_stratum_lengths$species)
#find area-stratum-efforts where no lengths exists
temp.match = match(ase$year.country.cruise.area.stratum.species, area_stratum_lengths$year.country.cruise.area.stratum.species)
unmatched_efforts = ase[is.na(temp.match),c("year","country","vessel","cruise","area","stratum","species","RPN","RPW")]
unmatched_efforts$sex = rep(3, dim(unmatched_efforts)[1])
unmatched_efforts$length = rep(999, dim(unmatched_efforts)[1])
unmatched_efforts$freq = rep(NA, dim(unmatched_efforts)[1])
unmatched_efforts$total_lengths = rep(NA, dim(unmatched_efforts)[1])
unmatched_efforts$lf_RPN = unmatched_efforts$RPN
unmatched_efforts$lf_RPW = unmatched_efforts$RPW
temp.match = match(unmatched_efforts$area, areas$area)
unmatched_efforts$fmp_area = areas[temp.match,"fmp_area"]
#now join again the other way to get RPNs and RPWs from ase
temp.match = match(area_stratum_lengths$year.country.cruise.area.stratum.species, ase$year.country.cruise.area.stratum.species)
area_stratum_lengths$RPN = rep(NA, dim(area_stratum_lengths)[1])
area_stratum_lengths$RPN = ase[temp.match,"RPN"]
area_stratum_lengths$RPW = rep(NA, dim(area_stratum_lengths)[1])
area_stratum_lengths$RPW = ase[temp.match,"RPW"]

#sum lengths by area-stratum-species
temp = tapply(area_stratum_lengths$freq, area_stratum_lengths$year.country.cruise.area.stratum.species, sum)
temp.match = match(area_stratum_lengths$year.country.cruise.area.stratum.species, names(temp))
area_stratum_lengths$total_lengths = temp[temp.match]

#sum freq x RPN / total_lengths by area-stratum-species-sex-length
area_stratum_lengths$lf_RPN = (area_stratum_lengths$freq * area_stratum_lengths$RPN) / area_stratum_lengths$total_lengths
area_stratum_lengths$lf_RPW = (area_stratum_lengths$freq * area_stratum_lengths$RPW) / area_stratum_lengths$total_lengths

#remove records where RPN is null
area_stratum_lengths = area_stratum_lengths[!is.na(area_stratum_lengths$RPN),]

#find the averages that match any of the unmatched efforts from above and join them together
unmatched_efforts$year.country.fmp_area.stratum.species = paste(unmatched_efforts$year, unmatched_efforts$country, unmatched_efforts$fmp_area, unmatched_efforts$stratum, unmatched_efforts$species)
temp.match = match(avg_length_freqs$year.country.fmp_area.stratum.species, unmatched_efforts$year.country.fmp_area.stratum.species)
avg_length_freqs$matched = temp.match
avg_area_stratum_lengths = avg_length_freqs[!is.na(avg_length_freqs$matched),]
avg_area_stratum_lengths = merge(x = avg_length_freqs[!is.na(avg_length_freqs$matched),], y = unmatched_efforts[,c("year.country.fmp_area.stratum.species","area","cruise","vessel","RPN","RPW")], by = "year.country.fmp_area.stratum.species", all.y = TRUE)
avg_area_stratum_lengths = avg_area_stratum_lengths[!is.na(avg_area_stratum_lengths$year),]

#remove the records from unmatched_efforts that have matches from the averages
temp.match = match(unmatched_efforts$year.country.fmp_area.stratum.species, avg_area_stratum_lengths$year.country.fmp_area.stratum.species)
unmatched_efforts$matched = temp.match
unmatched_efforts = unmatched_efforts[is.na(unmatched_efforts$match),]

#sum avg length freqs by area-stratum-species
temp = tapply(avg_length_freqs$avg_freq, avg_length_freqs$year.country.fmp_area.stratum.species, sum)
temp.match = match(avg_area_stratum_lengths$year.country.fmp_area.stratum.species, names(temp))
avg_area_stratum_lengths$total_avg_lengths = temp[temp.match]

#sum freq x RPN / total_avg_lengths by area-stratum-species-sex-length
avg_area_stratum_lengths$lf_RPN = (avg_area_stratum_lengths$avg_freq * avg_area_stratum_lengths$RPN) / avg_area_stratum_lengths$total_avg_lengths
avg_area_stratum_lengths$lf_RPW = (avg_area_stratum_lengths$avg_freq * avg_area_stratum_lengths$RPW) / avg_area_stratum_lengths$total_avg_lengths

#remove records where RPN is null (shouldn't be any?)
avg_area_stratum_lengths = avg_area_stratum_lengths[!is.na(avg_area_stratum_lengths$RPN),]

all_area_stratum_lengths = area_stratum_lengths[,c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area","RPN","RPW","total_lengths","lf_RPN","lf_RPW")]
avg_area_stratum_lengths = avg_area_stratum_lengths[,c("year","country","vessel","cruise","area","stratum","species","sex","length","avg_freq","fmp_area","RPN","RPW","total_avg_lengths","lf_RPN","lf_RPW")]
names(avg_area_stratum_lengths) = c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area","RPN","RPW","total_lengths","lf_RPN","lf_RPW")
unmatched_efforts = unmatched_efforts[,c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area","RPN","RPW","total_lengths","lf_RPN","lf_RPW")]

all_area_stratum_lengths = rbind(all_area_stratum_lengths, avg_area_stratum_lengths)
all_area_stratum_lengths = rbind(all_area_stratum_lengths, unmatched_efforts)

#create copy with strata 3-7 only
all_area_stratum_lengths3to7 = all_area_stratum_lengths[all_area_stratum_lengths$stratum>2,]

#now sum both strata 2b-7 and 3-7 sets to the Area level
all_area_stratum_lengths$year.country.cruise.area.species.sex.length = paste(all_area_stratum_lengths$year, all_area_stratum_lengths$country, all_area_stratum_lengths$cruise, all_area_stratum_lengths$area, all_area_stratum_lengths$species, all_area_stratum_lengths$sex, all_area_stratum_lengths$length)
area_lfs_RPN = tapply(all_area_stratum_lengths$lf_RPN, all_area_stratum_lengths$year.country.cruise.area.species.sex.length, sum)
area_lfs_RPW = tapply(all_area_stratum_lengths$lf_RPW, all_area_stratum_lengths$year.country.cruise.area.species.sex.length, sum)
temp.match = match(names(area_lfs_RPN), all_area_stratum_lengths$year.country.cruise.area.species.sex.length)
len_freq = all_area_stratum_lengths[temp.match,c("year","country","vessel","cruise","area","species","sex","length")]
len_freq$RPN = area_lfs_RPN
len_freq$RPW = area_lfs_RPW

all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length = paste(all_area_stratum_lengths3to7$year, all_area_stratum_lengths3to7$country, all_area_stratum_lengths3to7$cruise, all_area_stratum_lengths3to7$area, all_area_stratum_lengths3to7$species, all_area_stratum_lengths3to7$sex, all_area_stratum_lengths3to7$length)
area_lfs_RPN = tapply(all_area_stratum_lengths3to7$lf_RPN, all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length, sum)
area_lfs_RPW = tapply(all_area_stratum_lengths3to7$lf_RPW, all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length, sum)
temp.match = match(names(area_lfs_RPN), all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length)
len_freq3to7 = all_area_stratum_lengths3to7[temp.match,c("year","country","vessel","cruise","area","species","sex","length")]
len_freq3to7$RPN = area_lfs_RPN
len_freq3to7$RPW = area_lfs_RPW

rm(lengthed_species)
rm(templ)
rm(templ1)
rm(all_area_stratum_lengths)
rm(all_area_stratum_lengths3to7)
rm(area_lfs_RPN)
rm(area_lfs_RPW)
rm(unmatched_efforts)
rm(temp)
rm(temp.match)
#rm(avg_area_stratum_lengths) -- maybe these should be saved to a table and sent to AKFIN too?

print("Calculated Length Frequencies")
##############################################################################################


##############################################################################################
#Summarize CPUE, RPN, and RPW to Area Aggregate area_stratum_effort over stratum
#to area and species, only including strata 3-7 RPN and RPW are simply summed
#across strata mean_len and cpue are scaled by area_size mean_wt is recalculated
#using vonbert growth factors on newly calculated mean_len Output: area_effort
##############################################################################################
#
#get rid of anything not in strata 3-7. NOTE: in sablefish and rockfish/PT
#scripts good strata is defined using stratum2 not stratum. Tested this and it
#appears these are equivalent.
# good_strata2 = area_stratum_effort[area_stratum_effort$stratum2 %in% c(3:7),]
# length(which(good_strata != good_strata2))
good_strata = area_stratum_effort[area_stratum_effort$stratum %in% c(3:7),]
#summarize good_strata by area-species
#calc sum of area size
good_strata$cntry.area.spc = paste(good_strata$country, good_strata$area, good_strata$species)
area_size = tapply(good_strata$area_size, good_strata$cntry.area.spc, sum, na.rm=TRUE)
temp.match = match(names(area_size), good_strata$cntry.area.spc)
area_effort = good_strata[temp.match,c(1:6,10)]
area_effort$area_size = area_size

#calc sum of cpue, scaled by area size
#replace na with 0
good_strata$area_size = ifelse(is.na(good_strata$area_size),
                               0,
                               good_strata$area_size)
good_strata$cpue = ifelse(is.na(good_strata$cpue),
                          0,
                          good_strata$cpue)
cpue = tapply(good_strata$cpue * good_strata$area_size, good_strata$cntry.area.spc, sum)
area_effort$cpue = ifelse(area_effort$area_size==0,
                          NA,
                          cpue / area_effort$area_size)
rm(cpue)
#calc sum of RPN - NOTE: in sablefish and rockfish/PT scripts, the tapply sums
#for arearpn and arearpw do not include na.rm = TRUE
arearpn = tapply(good_strata$RPN, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort$RPN = arearpn
rm(arearpn)
#calc sum of RPW
arearpw = tapply(good_strata$RPW, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort$RPW = arearpw
rm(arearpw)

# NOTE: This is where area variances are added in the sablefish and rockfish/PT
# code. To avoid conflicts with indexing/etc., I've waited to add variances
# until the very end.

#calc sum of mean_len, scaled by area size
#first remove all cases where mean_len is NA
good_mean_len = good_strata[!is.na(good_strata$mean_len),]
#then calculate mean length scaling by area size
sizexlen = tapply(good_mean_len$mean_len * good_mean_len$area_size, good_mean_len$cntry.area.spc, sum)
areasize = tapply(good_mean_len$area_size, good_mean_len$cntry.area.spc, sum)
meanlen = sizexlen / areasize 
#then join to area_effort
area_effort$cntry.area.spc = paste(area_effort$country, area_effort$area, area_effort$species)
temp.match = match(area_effort$cntry.area.spc, names(meanlen))
area_effort$mean_len = rep(0, dim(area_effort)[1])
area_effort$mean_len = meanlen[temp.match]
#replace NaNs caused by divide by 0 (no area sizes)
area_effort$mean_len = ifelse(is.nan(area_effort$mean_len), 
                              NA,
                              area_effort$mean_len)
#get rid of cntry.area.spc key field no longer needed
area_effort = area_effort[c(1:10,12)]
rm(sizexlen)
rm(areasize)
rm(meanlen)
rm(temp.match)

#now join species so we can recalculate mean weight
ls.match = match(area_effort$species, species$code)
area_effort$vonbert_a = rep(NA, dim(area_effort)[1])
area_effort$vonbert_a = species[ls.match, 3]
area_effort$vonbert_b = rep(NA, dim(area_effort)[1])
area_effort$vonbert_b = species[ls.match, 4]
area_effort$mean_wt = rep(NA, dim(area_effort)[1])
area_effort$mean_wt = (area_effort$vonbert_a * (area_effort$mean_len ^ area_effort$vonbert_b))
rm(ls.match)
print("Summarized to Area")
##############################################################################################

##############################################################################################
# Summarize CPUE, RPN, and RPW to Area INCLUDING new stratum 2b
# This repeats the above three steps (summarization over strata, then extrapolation for areas 16 and 17)
# but including efforts in area_stratum_effort for all strata (i.e. adding in 2b)
# Output: area_effort2b
##############################################################################################
#
good_strata = area_stratum_effort
#summarize good_strata by area-species
#calc sum of area size 2 (includes sizes for stratum 2b)
good_strata$cntry.area.spc = paste(good_strata$country, good_strata$area, good_strata$species)
area_size2 = tapply(good_strata$area_size2, good_strata$cntry.area.spc, sum, na.rm=TRUE)
temp.match = match(names(area_size2), good_strata$cntry.area.spc)
area_effort2b = good_strata[temp.match,c(1:6,10)]
area_effort2b$area_size = area_size2

#calc sum of cpue, scaled by area size
#replace na with 0
good_strata$area_size2 = ifelse(is.na(good_strata$area_size2),
                                0,
                                good_strata$area_size2)
good_strata$cpue = ifelse(is.na(good_strata$cpue),
                          0,
                          good_strata$cpue)
cpue = tapply(good_strata$cpue * good_strata$area_size2, good_strata$cntry.area.spc, sum)
area_effort2b$cpue = ifelse(area_effort2b$area_size==0,
                            NA,
                            cpue / area_effort2b$area_size)
rm(cpue)
#calc sum of RPN
arearpn = tapply(good_strata$RPN, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort2b$RPN = arearpn
rm(arearpn)
#calc sum of RPW
arearpw = tapply(good_strata$RPW, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort2b$RPW = arearpw
rm(arearpw)
#calc sum of mean_len, scaled by area size
#first remove all cases where mean_len is NA
good_mean_len = good_strata[!is.na(good_strata$mean_len),]
#then calculate mean length scaling by area size
sizexlen = tapply(good_mean_len$mean_len * good_mean_len$area_size2, good_mean_len$cntry.area.spc, sum)
areasize = tapply(good_mean_len$area_size2, good_mean_len$cntry.area.spc, sum)
meanlen = sizexlen / areasize 
#then join to area_effort
area_effort2b$cntry.area.spc = paste(area_effort2b$country, area_effort2b$area, area_effort2b$species)
temp.match = match(area_effort2b$cntry.area.spc, names(meanlen))
area_effort2b$mean_len = rep(0, dim(area_effort2b)[1])
area_effort2b$mean_len = meanlen[temp.match]
#replace NaNs caused by divide by 0 (no area sizes)
area_effort2b$mean_len = ifelse(is.nan(area_effort2b$mean_len), 
                                NA,
                                area_effort2b$mean_len)
#get rid of cntry.area.spc key field no longer needed
area_effort2b = area_effort2b[c(1:10,12)]
rm(sizexlen)
rm(areasize)
rm(meanlen)
rm(temp.match)

#now join species so we can recalculate mean weight
ls.match = match(area_effort2b$species, species$code)
area_effort2b$vonbert_a = rep(NA, dim(area_effort2b)[1])
area_effort2b$vonbert_a = species[ls.match, 3]
area_effort2b$vonbert_b = rep(NA, dim(area_effort2b)[1])
area_effort2b$vonbert_b = species[ls.match, 4]
area_effort2b$mean_wt = rep(NA, dim(area_effort2b)[1])
area_effort2b$mean_wt = (area_effort2b$vonbert_a * (area_effort2b$mean_len ^ area_effort2b$vonbert_b))
rm(ls.match)
print("Summarized to Area including Stratum 2b")
##############################################################################################

##############################################################################################
# Extrapolate Area Effort in areas 16 and 17 - same as above, but not excluding stratum 2b
# Output: area_effort2b
##############################################################################################
#

species_ratios = species[!is.na(species$nw_ratio),]
area_16 = area_effort2b[area_effort2b$area==15&area_effort2b$country=='United States',]
temp.match = match(area_16$species, species_ratios$code)
area_16$nw_ratio = species_ratios[temp.match,6]
area_16 = area_16[!is.na(area_16$nw_ratio),]
area_16$cpue = area_16$cpue * area_16$nw_ratio
area_16$RPN = area_16$RPN * area_16$nw_ratio
area_16$RPW = area_16$RPW * area_16$nw_ratio
area_size = sum(area_strata[area_strata$area==16,3])
area_16$area_size = rep(area_size, dim(area_16)[1])
if (length(area_16[,1]) > 0) area_16$area = 16
area_effort2b = rbind(area_effort2b, area_16[c(1:14)])
rm(area_16)

species_ratios = species[!is.na(species$sw_ratio),]
area_17 = area_effort2b[area_effort2b$area==18&area_effort2b$country=='United States',]
temp.match = match(area_17$species, species_ratios$code)
area_17$sw_ratio = species_ratios[temp.match,7]
area_17 = area_17[!is.na(area_17$sw_ratio),]
area_17$cpue = area_17$cpue * area_17$sw_ratio
area_17$RPN = area_17$RPN * area_17$sw_ratio
area_17$RPW = area_17$RPW * area_17$sw_ratio
area_size = sum(area_strata[area_strata$area==17,3])
area_17$area_size = rep(area_size, dim(area_17)[1])
if (length(area_17[,1]) > 0) area_17$area = 17
area_effort2b = rbind(area_effort2b, area_17[c(1:14)])
rm(area_17)
rm(species_ratios)
rm(temp.match)
print("Extrapolated areas 16 and 17 including Stratum 2b")
##############################################################################################



##############################################################################################
# Output Length Frequencies by RPN (all strata)
# Output: len_freq.out
##############################################################################################

#lookup area information
temp.match = match(len_freq$area, areas$area)
len_freq$geo_area = areas[temp.match,5]
len_freq$council_area = areas[temp.match,3]

#lookup species info
temp.match = match(len_freq$species, species$code)
len_freq$Common_Name = species[temp.match,2]

#lookup vessel name
temp.match = match(len_freq$vessel, vessels$Vessel_Number)
len_freq$Vessel_name = vessels[temp.match,2]
rm(temp.match)

len_freq.out = len_freq[,c("year","country","vessel","Vessel_name","cruise","area","geo_area","council_area","species","Common_Name","sex","length","RPN","RPW")]

#Stamp last modified date
len_freq.out$last_mod = rep(Sys.time(), dim(len_freq.out)[1])

#Name columns to match database table column names exactly
names(len_freq.out)=c("Year","Country","Vessel_Number","Vessel_name","Cruise_Number","area_code",
                      "Geographic_area_name","Council_sablefish_management_area","species_code","Common_Name",
                      "sex","length","RPN","RPW","LastModifiedDate")





