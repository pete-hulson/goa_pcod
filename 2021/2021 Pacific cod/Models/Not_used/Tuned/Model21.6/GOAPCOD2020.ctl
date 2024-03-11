#V3.30.18.00;_safe;_compile_date:_Sep 30 2021;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
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
 1996 2005 2006 2022
 1990 2004 2005 2006 2007 2016 2017 2022
 2017 2022
 2015 2020
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
 0.1 1.5 0.466387 -0.81 0.41 0 10 106 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 0.1 20 6.38095 6.1252 99 0 1 104 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 70 130 99.4617 99.46 0.015 6 1 101 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0 1 0.166481 0.1966 0.03 0 1 101 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0 10 2.45238 0 0 0 2 0 0 0 0 0 0 0 # SD_young_Fem_GP_1
 0 20 9.60357 0 0 0 2 0 0 0 0 0 0 0 # SD_old_Fem_GP_1
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
 -9 9 0.350617 0 0 0 10 # NatM_uniform_Fem_GP_1_ENV_mult
 -9 9 0.682591 0 0 0 9 # L_at_Amin_Fem_GP_1_ENV_mult
 -9 9 0.100216 0 0 0 9 # L_at_Amax_Fem_GP_1_ENV_mult
 -9 9 -0.122379 0 0 0 9 # VonBert_K_Fem_GP_1_ENV_mult
 -9 9 0.390789 0 0 0 9 # AgeKeyParm2_BLK6repl_1976
 -9 9 -0.258432 0 0 0 9 # AgeKeyParm3_BLK6repl_1976
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
            10            20       12.9098             0             0             0          1        103          0          0          0          0          0          0 # SR_LN(R0)
             0             1             1             1             0             0         -1          0          0          0          0          0          0          0 # SR_BH_steep
             0            10          0.44          0.44             0             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             0             0         -3          0          0          0          0          0          5          1 # SR_regime
           -99            99             0             0             0             0         -1          0          0          0          0          0          0          0 # SR_autocorr
# timevary SR parameters
 -10 10 -0.0065709 0 0 0 1 # SR_LN(R0)_ENV_mult
 -10 10 -0.905899 0 0 0 1 # SR_regime_BLK5add_1976
2 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1978 # first year of main recr_devs; early devs can preceed this era
2018 # last year of main recr_devs; forecast devs start in following year
1 #_recdev phase 
1 # (0/1) to read 13 advanced options
 1967 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 2 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1964.4 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1979.7 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2012.9 #_last_yr_fullbias_adj_in_MPD
 2017 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
 0.9089 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
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
#  1967E 1968E 1969E 1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016R 2017R 2018R 2019F 2020F 2021F 2022F 2023F 2024F 2025F 2026F 2027F 2028F 2029F 2030F 2031F 2032F 2033F 2034F 2035F 2036F
#  -0.565906 -0.370709 -0.279905 -0.232145 -0.147634 -0.0239795 0.417681 0.402095 -0.0528887 -0.0707515 0.741948 -0.119449 -0.257891 0.0114408 0.401676 0.410599 0.108878 0.447111 1.02751 -0.0719031 0.277753 0.451676 0.461157 0.642218 0.126323 -0.0808033 -0.280163 -0.086427 0.163705 -0.290088 -0.412058 -0.196428 0.0274426 -0.0218721 -0.176218 -0.720838 -0.163267 -0.229225 -0.0148998 0.524951 0.0429033 0.400469 -0.299755 -0.188727 0.069699 0.617062 0.0707948 -0.92044 -0.302355 0.335274 -0.26223 -0.62386 -0.255785 -0.447713 -0.0141409 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
#Fishing Mortality info 
0.3489 # F ballpark value in units of annual_F
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope midseason rate; 2=F as parameter; 3=F as hybrid; 4=fleet-specific parm/hybrid (#4 is superset of #2 and #3 and is recommended)
6 # max F (methods 2-4) or harvest fraction (method 1)
5  # N iterations for tuning in hybrid mode; recommend 3 (faster) to 5 (more precise if many fleets)
#
#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 0
#_for unconstrained init_F, use an arbitrary initial catch and set lambda=0 for its logL
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#
# F rates by fleet x season
# Yr:  1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034 2035 2036
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# FshTrawl 0.00471029 0.0249951 0.0356646 0.0761311 0.0486923 0.0280671 0.0405627 0.0275197 0.0175337 0.0231019 0.0697311 0.0739032 0.105953 0.229931 0.408691 0.684052 0.225424 0.199587 0.227728 0.596804 0.301491 0.30521 0.337036 0.276847 0.205566 0.194188 0.143756 0.14716 0.331719 0.421 0.270682 0.243621 0.216948 0.250522 0.172997 0.178026 0.304213 0.245083 0.417184 0.454915 0.431463 0.071004 0.111268 0.0450823 0.217573 0.131004 0.152441 0.178434 0.235797 0.311577 0.324527 0.324527 0.324527 0.324527 0.324527 0.324527 0.324527 0.324527 0.324527 0.324527
# FshLL 0.0162795 0.0653501 0.089543 0.19331 0.106949 0.0906251 0.111487 0.0724604 0.0708138 0.107856 0.0380039 0.0167303 0.0183013 0.0506302 0.033971 0.0821631 0.0693371 0.0354996 0.0663355 0.0509979 0.0594511 0.0566097 0.0938812 0.080014 0.0720461 0.131707 0.141412 0.172161 0.0747069 0.116412 0.165443 0.284844 0.397455 0.283928 0.205485 0.196368 0.184949 0.236916 0.20732 0.138204 0.103084 0.064104 0.0811956 0.0406634 0.186372 0.0502558 0.0584795 0.068451 0.0904564 0.119527 0.124495 0.124495 0.124495 0.124495 0.124495 0.124495 0.124495 0.124495 0.124495 0.124495
# FshPot 0 0 0 0 0 0 0 0 0 0 0.00273004 0.00565857 0.00143721 0.0228062 0.0472672 0.0497461 0.0473483 0.0418464 0.0738125 0.0620864 0.0554294 0.0740761 0.150057 0.153674 0.0675678 0.0785153 0.221844 0.27304 0.268663 0.292657 0.374758 0.4242 0.354947 0.445379 0.508994 0.408958 0.362046 0.49912 0.589451 0.54996 0.419401 0.149032 0.169219 0.0436189 0.276243 0.156587 0.182211 0.21328 0.281845 0.372424 0.387904 0.387904 0.387904 0.387904 0.387904 0.387904 0.387904 0.387904 0.387904 0.387904
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         4         1         0         0         0         0  #  Srv
         5         1         0         0         0         0  #  LLSrv
         6         1         0         0         0         0  #  IPHCLL
         7         1         0         0         0         0  #  ADFG
         8         1         0         0         0         0  #  SPAWN
         9         1         0         1         0         0  #  Seine
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -10            10      0.348672             0            99             0         10          0          0          0          0          0          0          0  #  LnQ_base_Srv(4)
           -10            10      0.252931             0            99             0         10        101          0          0          0          0          0          0  #  LnQ_base_LLSrv(5)
           -10            10             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_IPHCLL(6)
           -10            10             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_ADFG(7)
           -10            10             0             0            99             0         -1          0          0          0          0          0          0          0  #  LnQ_base_SPAWN(8)
           -10            10      -9.57741             0            99             0         10          0          0          0          0          0          0          0  #  LnQ_base_Seine(9)
             0             5       1.03391             0            99             0         10          0          0          0          0          0          0          0  #  Q_extraSD_Seine(9)
# timevary Q parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type     PHASE  #  parm_name
           -10            10      0.563724             0            99             0      5  # LnQ_base_LLSrv(5)_ENV_mult
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
            10           110        79.642            50             0             0          1          0          1       1977       2021          3          0          0  #  Size_DblN_peak_FshTrawl(1)
           -20            10      -6.79874             0             0             0          2          0          1       1977       2021          0          0          0  #  Size_DblN_top_logit_FshTrawl(1)
           -10            10       6.10045             0             0             0          2          0          1       1977       2021          3          0          0  #  Size_DblN_ascend_se_FshTrawl(1)
           -10            10      -1.83224            10             0             0          2          0          1       1977       2021          3          0          0  #  Size_DblN_descend_se_FshTrawl(1)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshTrawl(1)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshTrawl(1)
# 2   FshLL LenSelex
            10           100       73.5728            50             0             0          1          0          1       1978       2021          3          0          0  #  Size_DblN_peak_FshLL(2)
           -20            10     -0.366243             0             0             0          2          0          1       1978       2021          3          0          0  #  Size_DblN_top_logit_FshLL(2)
           -10            10       5.43079             0             0             0          2          0          1       1978       2021          3          0          0  #  Size_DblN_ascend_se_FshLL(2)
             0            10            10            10             0             0         -2          0          0       1978       2021          0          0          0  #  Size_DblN_descend_se_FshLL(2)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshLL(2)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshLL(2)
# 3   FshPot LenSelex
            10            90       72.1002            50             0             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_FshPot(3)
           -20            10      -3.03232             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_FshPot(3)
           -10            10       5.07486             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_FshPot(3)
             0            10       0.13361            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_FshPot(3)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshPot(3)
           -10            10       2.01545            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshPot(3)
# 4   Srv LenSelex
            10            90       62.5867            50             0             0          1          0          0          0          0          0          1          2  #  Size_DblN_peak_Srv(4)
           -20            10      -11.8164             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_top_logit_Srv(4)
           -10            10       5.72353             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_ascend_se_Srv(4)
             0            10       2.95774            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_descend_se_Srv(4)
           -10       2.71828      -3.73292           -10             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_start_logit_Srv(4)
           -10            10     -0.134361            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_end_logit_Srv(4)
# 5   LLSrv LenSelex
            10            90       65.6167            50             0             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_LLSrv(5)
           -20            10      -1.04443             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_LLSrv(5)
           -10            10       4.65966             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_LLSrv(5)
             0            10    0.00531046            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_LLSrv(5)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_LLSrv(5)
           -10            10      0.406516            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_LLSrv(5)
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
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_peak_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_FshTrawl(1)_dev_autocorr
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_top_logit_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_top_logit_FshTrawl(1)_dev_autocorr
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_ascend_se_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_ascend_se_FshTrawl(1)_dev_autocorr
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_descend_se_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_descend_se_FshTrawl(1)_dev_autocorr
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_peak_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_FshLL(2)_dev_autocorr
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_top_logit_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_top_logit_FshLL(2)_dev_autocorr
        0.0001             2           0.1           0.1           0.5             6      -5  # Size_DblN_ascend_se_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_ascend_se_FshLL(2)_dev_autocorr
            10            90       63.8357            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_1996
            10            90       56.0189            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_2006
           -20            10      -5.53642             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_1996
           -20            10      -3.56503             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_2006
           -10            10       5.78659             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_1996
           -10            10        5.0349             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_2006
             0            10       3.80175            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_1996
             0            10       4.63689            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_2006
           -10       2.71828      -2.56169           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_1996
           -10       2.71828       -2.3685           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_2006
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
#      1     1     1     0     0     1     6     0     0     0     0     0
#      1     2     2     0     0     1     4     0     0     0     0     0
#      1     3     3     0     0     1     1     0     0     0     0     0
#      1     4     4     0     0     1     1     0     0     0     0     0
#      1    15     5     6     2     0     0     0     0     0     0     0
#      1    16     6     6     2     0     0     0     0     0     0     0
#      2     1     7     0     0     1     3     0     0     0     0     0
#      2     4     8     5     1     0     0     0     0     0     0     0
#      3     2     9     0     0     1     1     0     0     0     0     0
#      5     1    10     0     0     0     0     1     1  1977  2021     3 -0.998338 -0.670386 0.00926395 -0.357144 -1.05717 -1.19462 -1.5939 -3.19892 -1.36965 -0.258735 -0.680781 -1.38191 -0.554136 -0.245778 1.33541 2.47081 0.83238 0.737499 0.576556 2.09991 -0.110001 0.168099 0.357379 0.550601 -0.425538 0.0623945 -1.0119 -0.960957 1.11095 1.82721 0.983293 -0.172553 0.0647068 -0.207311 -0.468299 -0.102393 0.931009 -0.252626 1.30248 1.73307 1.08288 -0.10466 0.484635 -1.06607 -0.276607
#      5     2    12     0     0     0     0     2     1  1977  2021     0 -2.13486e-09 1.62331e-07 -1.05673e-09 -2.45519e-07 4.35847e-07 -3.30823e-07 5.88059e-07 3.0063e-06 1.24741e-06 1.52579e-07 1.5401e-07 3.49628e-07 1.47747e-07 7.43617e-07 2.72332e-08 -1.64384e-08 -1.14686e-07 -4.75452e-07 -1.03502e-07 -7.46153e-08 2.08113e-06 -7.16969e-08 3.18522e-10 -3.9336e-08 7.9909e-07 8.84925e-08 -1.01669e-07 1.06308e-07 3.04662e-08 -6.8387e-07 -7.28461e-08 1.28927e-07 9.43732e-08 3.66415e-08 -5.05768e-08 9.37211e-07 -6.36055e-08 1.72276e-06 -3.55847e-07 -2.04363e-07 -1.91898e-07 4.09452e-07 -2.15139e-07 9.24979e-07 6.8966e-08
#      5     3    14     0     0     0     0     3     1  1977  2021     3 0.330829 0.123867 -0.00767881 -0.0498349 0.210052 0.102322 -0.782203 -1.98293 -0.606272 0.741649 0.246052 -0.444196 0.40688 -0.340876 0.36776 0.782257 0.344233 -0.0374603 0.197373 0.459566 -0.373775 -0.116884 -0.123977 0.0261087 -0.329386 0.0321884 -0.456087 -0.768135 -0.271136 0.206935 0.502295 0.290374 -0.125336 -0.23004 -0.709431 0.265794 0.570687 0.360799 0.705785 0.600891 -0.105837 -0.0707811 0.162578 0.582597 -0.687499
#      5     4    16     0     0     0     0     4     1  1977  2021     3 -4.73937e-08 2.30213e-07 -8.64352e-10 -7.559e-08 3.5811e-07 -2.45385e-07 2.93191e-09 6.05928e-07 1.64713e-06 1.71732e-07 2.52138e-07 5.39504e-07 2.69015e-07 5.86661e-07 -8.2473e-08 -1.54512e-08 -2.00156e-07 -4.38493e-07 -2.51065e-07 -3.70685e-07 1.25191e-06 5.09229e-08 -2.15462e-09 -1.2223e-07 1.56124e-06 2.62788e-07 -1.41456e-08 1.9416e-07 4.94606e-08 -1.57174e-06 -4.81582e-08 2.81477e-07 2.32612e-07 -5.5717e-08 7.73943e-08 3.79687e-07 -1.57815e-07 1.66439e-06 -4.35511e-07 -3.15774e-07 -1.88373e-07 1.85248e-07 -3.04112e-07 9.33059e-07 1.02918e-07
#      5     7    18     0     0     0     0     5     1  1978  2021     3 -0.952739 -0.352003 0.148592 -1.98605 -1.7852 -1.75726 -0.160675 1.81563 1.33412 -0.039225 -0.39717 -0.0543106 2.01052 -0.0109415 0.451432 1.5624 0.537021 1.14167 0.271629 -0.456044 -0.359926 0.158852 -0.812589 -0.915816 -0.468238 0.381512 0.644796 -0.615993 -0.313084 0.0529691 0.749361 1.13313 0.11964 -0.367118 -0.357744 -0.0396547 -0.0728428 -0.00192903 0.519397 -0.78137 -0.303266 -0.168749 0.257275 0.202061
#      5     8    20     0     0     0     0     6     1  1978  2021     3 5.21302e-07 3.8409e-07 1.83794e-07 6.81693e-08 1.65303e-07 1.44132e-07 6.45426e-08 5.63835e-08 -2.9431e-08 7.65617e-10 1.43791e-08 1.97052e-09 -1.54883e-08 -2.91206e-07 -5.33331e-07 -1.89473e-07 -1.93987e-07 -2.80362e-07 -2.34006e-08 2.58701e-08 -1.05179e-08 -1.44087e-07 3.11766e-08 4.87367e-08 2.61396e-07 -1.31751e-07 3.45304e-08 -2.90224e-07 -2.6071e-07 -3.30796e-07 -1.27338e-07 -1.34311e-08 -2.90384e-08 -2.15684e-08 8.44688e-09 -3.96516e-09 -4.18127e-08 -6.37831e-08 -3.64557e-08 -2.40036e-07 -1.58492e-07 -8.92124e-08 5.31662e-09 -1.36092e-07
#      5     9    22     0     0     0     0     7     1  1978  2021     3 -0.248612 0.14261 0.820041 -1.1379 -0.758238 -1.26963 0.283351 1.3304 0.982979 0.0248493 0.0562971 0.0272682 1.27036 -0.289912 0.217642 0.809844 0.179973 0.728408 0.398485 -0.623853 0.167796 0.123868 -0.631578 -0.673933 -0.646721 0.269524 0.259128 -0.327178 -0.140619 0.265151 0.466321 0.715984 -0.0988569 -0.256162 -0.485033 -0.580713 -0.254764 0.157905 0.947614 -0.249358 -0.461106 -0.846193 -0.440426 -0.202835
#      5    19    24     1     2     0     0     0     0     0     0     0
#      5    20    26     1     2     0     0     0     0     0     0     0
#      5    21    28     1     2     0     0     0     0     0     0     0
#      5    22    30     1     2     0     0     0     0     0     0     0
#      5    23    32     1     2     0     0     0     0     0     0     0
#      5    24    34     1     2     0     0     0     0     0     0     0
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
      1      4         0
      1      5         0
      4      1         1
      4      2         1
      4      3         1
      4      4         1
      4      5         1
      5      1         1
      5      2         1
      5      3         1
      5      4         1
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

