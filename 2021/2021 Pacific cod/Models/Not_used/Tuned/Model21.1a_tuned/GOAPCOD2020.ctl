#V3.30.17.beta: not an official version of SS;_safe;_compile_date:_Sep 21 2021;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3
#_Stock_Synthesis_(SS)_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_data_and_control_files: GOAPcod2021SEP28_10P.dat // GOAPCOD2020.ctl
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
6 #_Nblock_Patterns
 2 4 1 1 1 1 #_blocks_per_pattern 
# begin and end years of blocks
 1996 2005 2006 2021
 1990 2004 2005 2006 2007 2016 2017 2021
 2017 2021
 2014 2016
 1976 1976
 1976 2006
#
# controls for all timevary parameters 
1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds
#
# AUTOGEN
 1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: P(y)=f(TVP,env_Zscore) w/ logit to stay in min-max;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  5: like 4 with logit transform to stay in base min-max
#_DevLinks(more):  21-25 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, wt-len, maturity, fecundity, (hermaphro), recr_distr, cohort_grow, (movement), (age error), (catch_mult), sex ratio 
#_NATMORT
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity
  #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0.5 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
2 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)
#_** in option 1, any male parameter with value = 0.0 and phase <0 is set equal to female parameter
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
 0.1 1.5 0.479358 -0.81 0.41 3 5 0 0 0 0 0 4 2 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 0 50 11.7666 6.1252 99 0 1 0 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 70 130 99.461 99.46 0.015 6 1 0 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0 1 0.170522 0.1966 0.03 6 1 0 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0 10 2.45709 0 0 0 10 0 0 0 0 0 0 0 # SD_young_Fem_GP_1
 0 20 8.65501 0 0 0 10 0 0 0 0 0 0 0 # SD_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 -99 99 5.63096e-06 0 0 0 -3 0 0 0 0 0 0 0 # Wtlen_1_Fem_GP_1
 -99 99 3.1306 0 0 0 -3 0 0 0 0 0 0 0 # Wtlen_2_Fem_GP_1
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 -99 99 53.7 0 0 0 -1 0 0 0 0 0 0 0 # Mat50%_Fem_GP_1
 -99 99 -0.273657 0 0 0 -1 0 0 0 0 0 0 0 # Mat_slope_Fem_GP_1
 -99 99 1 0 0 0 -1 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem_GP_1
 -99 99 0 0 0 0 -1 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem_GP_1
# Hermaphroditism
#  Recruitment Distribution  
#  Cohort growth dev base
 0.1 10 1 1 1 0 -1 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
#  Age Error from parameters
 -10 10 3 0 0 0 -5 0 0 0 0 0 0 0 # AgeKeyParm1
 -10 10 0 0 0 0 -10 0 0 0 0 0 6 2 # AgeKeyParm2
 -10 10 0 0 0 0 -10 0 0 0 0 0 6 2 # AgeKeyParm3
 -10 10 0 0 0 0 -1 0 0 0 0 0 0 0 # AgeKeyParm4
 -10 10 0.57 0 0 0 -1 0 0 0 0 0 0 0 # AgeKeyParm5
 -10 10 1.16 0 0 0 -1 0 0 0 0 0 0 0 # AgeKeyParm6
 -10 10 0 0 0 0 -1 0 0 0 0 0 0 0 # AgeKeyParm7
#  catch multiplier
#  fraction female, by GP
 1e-06 0.999999 0.5 0.5 0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#  M2 parameter for each predator fleet
#
# timevary MG parameters 
#_ LO HI INIT PRIOR PR_SD PR_type  PHASE
 0.1 2 0.651969 -0.81 0.41 3 6 # NatM_uniform_Fem_GP_1_BLK4repl_2014
 -9 9 0.382254 0 0 0 9 # AgeKeyParm2_BLK6repl_1976
 -9 9 -0.322229 0 0 0 9 # AgeKeyParm3_BLK6repl_1976
# info on dev vectors created for MGparms are reported with other devs after tag parameter section 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
            10            20       13.0258             0             0             0          1          0          0          0          0          0          0          0 # SR_LN(R0)
             0             1             1             1             0             0         -1          0          0          0          0          0          0          0 # SR_BH_steep
             0            10          0.44          0.44             0             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             0             0         -3          0          0          0          0          0          5          1 # SR_regime
           -99            99             0             0             0             0         -1          0          0          0          0          0          0          0 # SR_autocorr
# timevary SR parameters
 -10 10 -0.299796 0 0 0 1 # SR_regime_BLK5add_1976
2 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1978 # first year of main recr_devs; early devs can preceed this era
2018 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 1967 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 2 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1963.9 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1989.4 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2017.5 #_last_yr_fullbias_adj_in_MPD
 2021.5 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
 0.8374 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -5 #min rec_dev
 5 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1967E 1968E 1969E 1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016R 2017R 2018R 2019F 2020F 2021F 2022F 2023F 2024F 2025F 2026F
#  -0.322184 -0.214715 -0.227439 -0.177582 -0.0444915 0.173063 0.413982 -0.0336856 -0.222742 -0.0984314 1.04628 0.054986 -0.0476734 0.10872 0.224712 0.614682 0.175537 0.398966 0.615681 0.0841224 0.345653 0.213817 0.398108 0.404883 0.0394864 -0.0734679 -0.243597 -0.183689 0.0395355 -0.184683 -0.312898 -0.346508 -0.320653 -0.0251787 -0.603794 -0.789558 -0.661036 -0.261249 -0.150411 0.475767 -0.0907496 0.214633 -0.124526 -0.149836 0.0854847 0.559678 -0.0664349 -0.374636 -0.439353 -0.43132 0.254969 0.347554 -0.504217 0.479638 -0.0432016 0 0 0 0 0
#
#Fishing Mortality info 
0.3489 # F ballpark value in units of annual_F
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid; 4=fleet-specific parm/hybrid (#4 is recommended)
5 # max F or harvest rate, depends on F_Method
# for Fmethod 1; no additional F input needed
# for Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# for Fmethod=3; read N iterations for tuning for Fmethod 3
# for Fmethod=4; read list of fleets needing parameters; syntax is:  fleet, F_starting_value (if start_PH=1), first PH for parms (99 to stay in hybrid)
# for Fmethod=4; then read N tuning loops for hybrid fleets 2-3 normally enough
5  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 0
#_for unconstrained init_F, use an arbitrary initial catch and set lambda=0 for its logL
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#
# F rates by fleet x season
# Yr:  1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# FshTrawl 0.00187155 0.00986332 0.0117117 0.0301883 0.0229936 0.0144591 0.0228522 0.020448 0.012623 0.0131776 0.0450016 0.0552806 0.0740295 0.237907 0.261038 0.275064 0.202185 0.166931 0.229559 0.284206 0.352616 0.358505 0.370385 0.28038 0.279097 0.231835 0.233287 0.234628 0.398719 0.409593 0.350196 0.525051 0.314328 0.404944 0.276622 0.331873 0.356885 0.505748 0.538385 0.426543 0.224699 0.0625475 0.0633526 0.0368924 0.0597349 0.199146 0.199146 0.199146 0.199146 0.199146
# FshLL 0.00602761 0.0303675 0.0422575 0.0940613 0.0620859 0.0541397 0.0681496 0.0420441 0.0424746 0.0767271 0.0248138 0.0127178 0.0127875 0.0267148 0.0301841 0.0691012 0.0414042 0.0311932 0.0516172 0.0541867 0.0691612 0.0750105 0.106322 0.109268 0.0974242 0.147847 0.137124 0.153266 0.0859652 0.129805 0.242771 0.305624 0.328552 0.325502 0.307027 0.269985 0.196651 0.293155 0.337125 0.246993 0.3056 0.107317 0.0875901 0.0286345 0.0913748 0.189874 0.189874 0.189874 0.189874 0.189874
# FshPot 0 0 0 0 0 0 0 0 0 0 0.00225221 0.00488806 0.00127232 0.0202463 0.042008 0.0456804 0.0460535 0.0423512 0.076035 0.0647232 0.0583078 0.0808387 0.166963 0.164588 0.0703212 0.0779393 0.22374 0.294071 0.3018 0.34311 0.449593 0.496358 0.380587 0.463444 0.544562 0.435141 0.363349 0.532322 0.747343 0.762662 1.47099 0.359708 0.281129 0.0529573 0.219895 0.716174 0.716174 0.716174 0.716174 0.716174
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         4         1         0         1         0         0  #  Srv
         5         1         0         1         0         0  #  LLSrv
         6         1         0         0         0         0  #  IPHCLL
         7         1         0         0         0         0  #  ADFG
         8         1         0         0         0         0  #  SPAWN
         9         1         0         1         0         0  #  Seine
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -10            10             0             0            99             0         -1          0          0          0          0          0          0          0  #  LnQ_base_Srv(4)
            0             5              0             0            99             0         10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
           -10            10     0.0380976             0            99             0          1        101          0          0          0          0          0          0  #  LnQ_base_LLSrv(5)
            0             5              0             0            99             0         10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
           -25            25             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_IPHCLL(6)
           -25            25             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_ADFG(7)
           -25            25             0             0            99             0         -1          0          0          0          0          0          0          0  #  LnQ_base_SPAWN(8)
           -10            25             0             0            99             0          1          0          0          0          0          0          0          0  #  LnQ_base_Seine(9)
             0             5             0             0            99             0         10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
# timevary Q parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type     PHASE  #  parm_name
           -10            10        1.9785             0            99             0      5  # LnQ_base_LLSrv(5)_ENV_mult
# info on dev vectors created for Q parms are reported with other devs after tag parameter section 
#
#_size_selex_patterns
#Pattern:_0;  parm=0; selex=1.0 for all sizes
#Pattern:_1;  parm=2; logistic; with 95% width specification
#Pattern:_2;  parm=6; modification of pattern 24 with improved sex-specific offset
#Pattern:_5;  parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_11; parm=2; selex=1.0  for specified min-max population length bin range
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6;  parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8;  parm=8; double_logistic with smooth transitions and constant above Linf option
#Pattern:_9;  parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in length
#Pattern:_27; parm=special+3; cubic spline in length; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=special+3+2; cubic spline; like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 24 0 0 0 # 1 FshTrawl
 24 0 0 0 # 2 FshLL
 24 0 0 0 # 3 FshPot
 24 0 0 0 # 4 Srv
 24 0 0 0 # 5 LLSrv
 15 0 0 4 # 6 IPHCLL
 15 0 0 4 # 7 ADFG
 0 0 0 0 # 8 SPAWN
 0 0 0 0 # 9 Seine
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age; parm1==1 resets knots; parm1==2 resets all 
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 10 0 0 0 # 1 FshTrawl
 10 0 0 0 # 2 FshLL
 10 0 0 0 # 3 FshPot
 10 0 0 0 # 4 Srv
 10 0 0 0 # 5 LLSrv
 10 0 0 0 # 6 IPHCLL
 10 0 0 0 # 7 ADFG
 0 0 0 0 # 8 SPAWN
 0 0 0 0 # 9 Seine
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   FshTrawl LenSelex
            10           110       57.6799            50             0             0          1          0          1       1977       1989          3          2          2  #  Size_DblN_peak_FshTrawl(1)
           -20            10      -4.77759             0             0             0          2          0          0       1977       1989          0          2          2  #  Size_DblN_top_logit_FshTrawl(1)
           -10            10       5.08835             0             0             0          2          0          1       1977       1989          3          2          2  #  Size_DblN_ascend_se_FshTrawl(1)
           -10            10     0.0126947            10             0             0          2          0          1       1977       1989          3          2          2  #  Size_DblN_descend_se_FshTrawl(1)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshTrawl(1)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshTrawl(1)
# 2   FshLL LenSelex
            10            90       65.9373            50             0             0          1          0          1       1978       1989          3          2          2  #  Size_DblN_peak_FshLL(2)
           -20            10      -5.04485             0             0             0          2          0          0       1978       1989          0          2          2  #  Size_DblN_top_logit_FshLL(2)
           -10            10        5.1336             0             0             0          2          0          1       1978       1989          3          2          2  #  Size_DblN_ascend_se_FshLL(2)
             0            10            10            10             0             0         -2          0          0          0          0          0          2          2  #  Size_DblN_descend_se_FshLL(2)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshLL(2)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshLL(2)
# 3   FshPot LenSelex
            10            90       72.6781            50             0             0          1          0          0          0          0          0          3          2  #  Size_DblN_peak_FshPot(3)
           -20            10      -9.19065             0             0             0          2          0          0          0          0          0          3          2  #  Size_DblN_top_logit_FshPot(3)
           -10            10       5.09924             0             0             0          2          0          0          0          0          0          3          2  #  Size_DblN_ascend_se_FshPot(3)
             0            10       3.17485            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_FshPot(3)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshPot(3)
           -10            10       1.93145            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshPot(3)
# 4   Srv LenSelex
            10            90        59.736            50             0             0          1          0          0          0          0          0          1          2  #  Size_DblN_peak_Srv(4)
           -20            10      -10.6215             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_top_logit_Srv(4)
           -10            10       5.61414             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_ascend_se_Srv(4)
             0            10       4.41883            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_descend_se_Srv(4)
           -10       2.71828      -3.70893           -10             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_start_logit_Srv(4)
           -10            10     -0.455324            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_end_logit_Srv(4)
# 5   LLSrv LenSelex
            10            90       66.5717            50             0             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_LLSrv(5)
           -20            10      -10.8883             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_LLSrv(5)
           -10            10       4.71862             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_LLSrv(5)
             0            10        5.0815            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_LLSrv(5)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_LLSrv(5)
           -10            10    -0.0374352            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_LLSrv(5)
# 6   IPHCLL LenSelex
# 7   ADFG LenSelex
# 8   SPAWN LenSelex
# 9   Seine LenSelex
# 1   FshTrawl AgeSelex
# 2   FshLL AgeSelex
# 3   FshPot AgeSelex
# 4   Srv AgeSelex
# 5   LLSrv AgeSelex
# 6   IPHCLL AgeSelex
# 7   ADFG AgeSelex
# 8   SPAWN AgeSelex
# 9   Seine AgeSelex
#_No_Dirichlet parameters
# timevary selex parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type    PHASE  #  parm_name
            10            90       84.1424            50             0             0      1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_1990
            10            91            90            50             0             0      -1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_2005
            10            91            90            50             0             0      -1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_2007
            10            90       77.2556            50             0             0      -1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_peak_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_FshTrawl(1)_dev_autocorr
           -20            10      -4.76705             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_1990
           -20            10      -4.97008             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_2005
           -20            10      -4.94493             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_2007
           -20            10      -4.90071             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_2017
           -10            10        6.1795             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_1990
           -10            10       5.95329             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_2005
           -10            10       6.45676             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_2007
           -10            10       6.03318             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_ascend_se_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_ascend_se_FshTrawl(1)_dev_autocorr
             0            10       5.42092            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_1990
             0            10       5.06172            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_2005
             0            10       5.10082            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_2007
             0            10       5.14455            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_descend_se_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_descend_se_FshTrawl(1)_dev_autocorr
            10            90        75.833            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_1990
            10            90       70.4798            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_2005
            10            90         78.18            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_2007
            10            90       82.9558            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_peak_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_FshLL(2)_dev_autocorr
           -20            10     -0.597305             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_1990
           -20            10      -5.00876             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_2005
           -20            10      -4.88327             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_2007
           -20            10      -4.93486             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_2017
           -10            10       5.55294             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_1990
           -10            10       5.29905             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_2005
           -10            10        5.6559             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_2007
           -10            10       5.77332             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_ascend_se_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_ascend_se_FshLL(2)_dev_autocorr
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_1990
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_2005
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_2007
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_2017
            10            100       89.5686            50             0             0      1  # Size_DblN_peak_FshPot(3)_BLK3repl_2017
           -20            10       1.74483             0             0             0      2  # Size_DblN_top_logit_FshPot(3)_BLK3repl_2017
           -10            10       5.93862             0             0             0      2  # Size_DblN_ascend_se_FshPot(3)_BLK3repl_2017
            10            90       63.3121            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_1996
            10            90       58.5244            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_2006
           -20            10      -5.09554             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_1996
           -20            10      -3.18109             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_2006
           -10            10       5.89701             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_1996
           -10            10        5.3724             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_2006
             0            10        4.5838            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_1996
             0            10       5.34268            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_2006
           -10       2.71828      -2.65629           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_1996
           -10       2.71828       -2.7067           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_2006
             0            10            10            10             0             0      -5  # Size_DblN_end_logit_Srv(4)_BLK1repl_1996
             0            10            10            10             0             0      -5  # Size_DblN_end_logit_Srv(4)_BLK1repl_2006
# info on dev vectors created for selex parms are reported with other devs after tag parameter section 
#
0   #  use 2D_AR1 selectivity(0/1)
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#      1     1     1     4     2     0     0     0     0     0     0     0
#      1    15     2     6     2     0     0     0     0     0     0     0
#      1    16     3     6     2     0     0     0     0     0     0     0
#      2     4     4     5     1     0     0     0     0     0     0     0
#      3     2     5     0     0     1     4     0     0     0     0     0
#      5     1     6     2     2     0     0     1     1  1977  1989     3 -0.240168 -0.0699463 0.0140064 0.353821 -0.1523 -0.186001 -0.00083365 -0.316856 0.550066 -0.163301 -0.00652382 0.109219 0.108797
#      5     2    12     2     2     0     0     0     0     0     0     0
#      5     3    16     2     2     0     0     2     1  1977  1989     3 -0.0363688 0.0300253 -0.013327 0.0146114 -0.0427102 0.140149 -0.172753 -0.633798 0.235234 0.29282 -0.00509542 -0.0798781 0.27123
#      5     4    22     2     2     0     0     3     1  1977  1989     3 -1.26371e-07 -9.91766e-08 -3.7031e-08 2.92141e-07 1.2418e-07 -3.59772e-07 1.55925e-07 3.62898e-08 8.61441e-08 -3.14286e-07 -6.15603e-09 -1.66102e-07 -1.23505e-07
#      5     7    28     2     2     0     0     4     1  1978  1989     3 -0.311243 0.0273605 0.385876 -0.683225 -0.631073 -0.650062 -0.288726 1.17865 1.07842 0.00219564 -0.0959934 -0.029628
#      5     8    34     2     2     0     0     0     0     0     0     0
#      5     9    38     2     2     0     0     5     1  1978  1989     3 -0.19869 0.1052 0.607569 -0.626677 -0.366815 -0.661696 -0.30333 0.839726 0.705294 0.00397321 -0.10965 0.0120018
#      5    10    44     2     2     0     0     0     0     0     0     0
#      5    13    48     3     2     0     0     0     0     0     0     0
#      5    14    49     3     2     0     0     0     0     0     0     0
#      5    15    50     3     2     0     0     0     0     0     0     0
#      5    19    51     1     2     0     0     0     0     0     0     0
#      5    20    53     1     2     0     0     0     0     0     0     0
#      5    21    55     1     2     0     0     0     0     0     0     0
#      5    22    57     1     2     0     0     0     0     0     0     0
#      5    23    59     1     2     0     0     0     0     0     0     0
#      5    24    61     1     2     0     0     0     0     0     0     0
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
      1      8         0
      1      9         0
      1      6         0
      1      7         0
      1      4         0
      1      5         0
      4      1  0.131196
      4      2  0.601489
      4      3  0.150806
      4      4  0.505804
      4      5  0.297418
      5      1  0.008887
      5      2   0.00814
      5      3  0.012292
      5      4  0.006014
 -9999   1    0  # terminator
#
1 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 3 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
 1 8 1 0 1
 1 6 1 0 1
 1 7 1 0 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  0 #_CPUE/survey:_3
#  1 #_CPUE/survey:_4
#  1 #_CPUE/survey:_5
#  0 #_CPUE/survey:_6
#  0 #_CPUE/survey:_7
#  0 #_CPUE/survey:_8
#  1 #_CPUE/survey:_9
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  1 #_lencomp:_4
#  1 #_lencomp:_5
#  0 #_lencomp:_6
#  0 #_lencomp:_7
#  0 #_lencomp:_8
#  0 #_lencomp:_9
#  1 #_agecomp:_1
#  1 #_agecomp:_2
#  1 #_agecomp:_3
#  1 #_agecomp:_4
#  0 #_agecomp:_5
#  0 #_agecomp:_6
#  0 #_agecomp:_7
#  0 #_agecomp:_8
#  0 #_agecomp:_9
#  1 #_init_equ_catch1
#  1 #_init_equ_catch2
#  1 #_init_equ_catch3
#  1 #_init_equ_catch4
#  1 #_init_equ_catch5
#  1 #_init_equ_catch6
#  1 #_init_equ_catch7
#  1 #_init_equ_catch8
#  1 #_init_equ_catch9
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio
 # 0 2 0 0 # Selectivity: (1) fleet, (2) 1=len/2=age/3=both, (3) year, (4) N selex bins
 # 0 0 # Growth: (1) growth pattern, (2) growth ages
 # 0 0 0 # Numbers-at-age: (1) area(-1 for all), (2) year, (3) N ages
 # -1 # list of bin #'s for selex std (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 # -1 # list of ages for NatAge std (-1 in first bin to self-generate)
999

