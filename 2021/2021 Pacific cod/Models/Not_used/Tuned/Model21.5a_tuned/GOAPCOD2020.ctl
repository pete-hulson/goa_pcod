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
 0.1 1.5 0.445448 -0.81 0.41 0 10 106 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 0.1 20 5.31685 6.1252 99 0 1 104 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 70 130 99.461 99.46 0.015 6 1 101 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0 1 0.171415 0.1966 0.03 0 1 101 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0 10 2.56459 0 0 0 2 0 0 0 0 0 0 0 # SD_young_Fem_GP_1
 0 20 8.49233 0 0 0 2 0 0 0 0 0 0 0 # SD_old_Fem_GP_1
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
 -9 9 0.383728 0 0 0 10 # NatM_uniform_Fem_GP_1_ENV_mult
 -9 9 0.779516 0 0 0 9 # L_at_Amin_Fem_GP_1_ENV_mult
 -9 9 0.0473275 0 0 0 9 # L_at_Amax_Fem_GP_1_ENV_mult
 -9 9 -0.057323 0 0 0 9 # VonBert_K_Fem_GP_1_ENV_mult
 -9 9 0.389265 0 0 0 9 # AgeKeyParm2_BLK6repl_1976
 -9 9 -0.330086 0 0 0 9 # AgeKeyParm3_BLK6repl_1976
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
            10            20       12.8745             0             0             0          1        103          0          0          0          0          0          0 # SR_LN(R0)
             0             1             1             1             0             0         -1          0          0          0          0          0          0          0 # SR_BH_steep
             0            10          0.44          0.44             0             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             0             0         -3          0          0          0          0          0          5          1 # SR_regime
           -99            99             0             0             0             0         -1          0          0          0          0          0          0          0 # SR_autocorr
# timevary SR parameters
 -10 10 -0.00897708 0 0 0 1 # SR_LN(R0)_ENV_mult
 -10 10 -0.558501 0 0 0 1 # SR_regime_BLK5repl_1976
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
#  1967E 1968E 1969E 1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016R 2017R 2018R 2019F 2020F 2021F 2022F 2023F 2024F 2025F 2026F
#  -0.378104 -0.236915 -0.182637 -0.152694 -0.0573376 0.151645 0.458541 0.056228 -0.168129 -0.0825474 0.888124 0.0491988 -0.106435 -0.0965849 0.210007 0.544967 0.0935882 0.399058 0.734803 -0.0221675 0.221299 0.169657 0.400098 0.295056 -0.105322 -0.196573 -0.332986 -0.197491 -0.0593797 -0.334658 -0.476611 -0.104336 -0.242889 -0.165305 -0.373823 -0.830108 -0.260288 -0.29714 -0.187815 0.432927 -0.126826 0.147758 -0.223804 -0.28422 0.0473985 0.532065 -0.111153 -0.407311 -0.109032 0.188999 0.192265 0.238763 -0.0946584 0.395797 0.156932 0 0 0 0 0
#
#Fishing Mortality info 
0.3489 # F ballpark value in units of annual_F
-1999 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid; 4=fleet-specific parm/hybrid (#4 is recommended)
6 # max F or harvest rate, depends on F_Method
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
# FshTrawl 0.00226337 0.0117451 0.0138794 0.0354463 0.0263966 0.016152 0.0250421 0.0213205 0.0129664 0.01415 0.0482385 0.0581858 0.0796974 0.245509 0.273155 0.287909 0.209354 0.172243 0.239095 0.29737 0.369854 0.368252 0.378949 0.293653 0.292527 0.244925 0.246197 0.24453 0.433136 0.438356 0.371643 0.564242 0.343384 0.440711 0.299922 0.372308 0.412494 0.536994 0.481727 0.357132 0.202265 0.0690895 0.088614 0.0672511 0.0967878 0.123832 0.147355 0.167944 0.180745 0.19262
# FshLL 0.00712659 0.0358783 0.048579 0.105927 0.071768 0.0608685 0.0747669 0.044611 0.0418249 0.0730448 0.025902 0.013155 0.0135457 0.0279896 0.0319588 0.0731255 0.0434329 0.0325878 0.0543672 0.0573331 0.0733911 0.0780244 0.11019 0.116084 0.10357 0.158315 0.147046 0.161992 0.0925421 0.137918 0.265836 0.341985 0.374832 0.369276 0.346105 0.3155 0.23734 0.323137 0.312379 0.214947 0.208568 0.0919153 0.0961685 0.041674 0.128348 0.103137 0.122729 0.139878 0.150539 0.16043
# FshPot 0 0 0 0 0 0 0 0 0 0 0.00234351 0.00502621 0.00133087 0.0217341 0.0454526 0.0493743 0.0494748 0.0453016 0.0818911 0.0699499 0.063228 0.0860208 0.177232 0.179397 0.0767274 0.0854722 0.24657 0.319515 0.32361 0.363966 0.478517 0.538632 0.41779 0.50555 0.592816 0.491971 0.425397 0.565914 0.66807 0.642561 0.915216 0.280175 0.280084 0.0705401 0.273866 0.345528 0.411164 0.468615 0.504332 0.537468
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
         6         0         0         0         0         0  #  IPHCLL
         7         1         0         0         0         0  #  ADFG
         8         1         0         0         0         0  #  SPAWN
         9         1         0         1         0         0  #  Seine
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -10            10      0             0            99             0          10          0          0          0          0          0          0          0  #  LnQ_base_Srv(4)
            0             5       0             0            99             0          10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
 	   
           -10            10      0             0            99             0          10        101          0          0          0          0          0          0  #  LnQ_base_LLSrv(5)
            0             5       0             0            99             0          10          0          0          0          0          0          0          0  #  Q_power_Seine(9)

           -25            25      0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_IPHCLL(6)
           -25            25      0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_ADFG(7)
           -25            25      0             0            99             0         -1          0          0          0          0          0          0          0  #  LnQ_base_SPAWN(8)
            -10           25      0             0            99             0          10          0          0          0          0          0          0          0  #  LnQ_base_Seine(9)
             0             5      0             0            99             0          10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
# timevary Q parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type     PHASE  #  parm_name
           -10            10      0.277074             0            99             0      5  # LnQ_base_LLSrv(5)_ENV_mult
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
            10           110       57.1306            50             0             0          1          0          1       1977       1989          3          2          2  #  Size_DblN_peak_FshTrawl(1)
           -20            10      -4.44207             0             0             0          2          0          0       1977       1989          0          2          2  #  Size_DblN_top_logit_FshTrawl(1)
           -10            10       5.06888             0             0             0          2          0          1       1977       1989          3          2          2  #  Size_DblN_ascend_se_FshTrawl(1)
           -10            10     0.0589835            10             0             0          2          0          1       1977       1989          3          2          2  #  Size_DblN_descend_se_FshTrawl(1)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshTrawl(1)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshTrawl(1)
# 2   FshLL LenSelex
            10            90       65.4812            50             0             0          1          0          1       1978       1989          3          2          2  #  Size_DblN_peak_FshLL(2)
           -20            10      -5.04628             0             0             0          2          0          0       1978       1989          0          2          2  #  Size_DblN_top_logit_FshLL(2)
           -10            10       5.13625             0             0             0          2          0          1       1978       1989          3          2          2  #  Size_DblN_ascend_se_FshLL(2)
             0            10            10            10             0             0         -2          0          0          0          0          0          2          2  #  Size_DblN_descend_se_FshLL(2)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshLL(2)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshLL(2)
# 3   FshPot LenSelex
            10            90       72.6063            50             0             0          1          0          0          0          0          0          3          2  #  Size_DblN_peak_FshPot(3)
           -20            10       -9.1256             0             0             0          2          0          0          0          0          0          3          2  #  Size_DblN_top_logit_FshPot(3)
           -10            10       5.10644             0             0             0          2          0          0          0          0          0          3          2  #  Size_DblN_ascend_se_FshPot(3)
             0            10       2.72697            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_FshPot(3)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshPot(3)
           -10            10       1.89454            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshPot(3)
# 4   Srv LenSelex
            10            90       60.0176            50             0             0          1          0          0          0          0          0          1          2  #  Size_DblN_peak_Srv(4)
           -20            10      -10.4953             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_top_logit_Srv(4)
           -10            10        5.7048             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_ascend_se_Srv(4)
             0            10       4.31861            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_descend_se_Srv(4)
           -10       2.71828       -3.5983           -10             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_start_logit_Srv(4)
           -10            10     -0.573686            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_end_logit_Srv(4)
# 5   LLSrv LenSelex
            10            90       66.4915            50             0             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_LLSrv(5)
           -20            10      -10.6438             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_LLSrv(5)
           -10            10       4.72557             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_LLSrv(5)
             0            10       4.50071            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_LLSrv(5)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_LLSrv(5)
           -10            10      0.267725            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_LLSrv(5)
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
            10            90       83.3482            50             0             0      1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_1990
            10            90            90            50             0             0      -1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_2005
            10            90            90            50             0             0      -1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_2007
            10            90       77.2556            50             0             0      -1  # Size_DblN_peak_FshTrawl(1)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_peak_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_FshTrawl(1)_dev_autocorr
           -20            10      -1.20024             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_1990
           -20            10      -4.95807             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_2005
           -20            10      -4.95331             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_2007
           -20            10      -4.90916             0             0             0      2  # Size_DblN_top_logit_FshTrawl(1)_BLK2repl_2017
           -10            10       6.17453             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_1990
           -10            10       5.95346             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_2005
           -10            10       6.47321             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_2007
           -10            10       6.04633             0             0             0      2  # Size_DblN_ascend_se_FshTrawl(1)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_ascend_se_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_ascend_se_FshTrawl(1)_dev_autocorr
             0            10       5.25949            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_1990
             0            10        5.0839            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_2005
             0            10       5.07905            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_2007
             0            10       5.13955            10             0             0      2  # Size_DblN_descend_se_FshTrawl(1)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_descend_se_FshTrawl(1)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_descend_se_FshTrawl(1)_dev_autocorr
            10            90       75.1939            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_1990
            10            90       70.5416            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_2005
            10            90       78.7878            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_2007
            10            90       78.7095            50             0             0      1  # Size_DblN_peak_FshLL(2)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_peak_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_peak_FshLL(2)_dev_autocorr
           -20            10     -0.513182             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_1990
           -20            10      -5.00517             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_2005
           -20            10      -4.90241             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_2007
           -20            10      -4.91458             0             0             0      2  # Size_DblN_top_logit_FshLL(2)_BLK2repl_2017
           -10            10       5.53436             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_1990
           -10            10       5.31497             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_2005
           -10            10       5.69373             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_2007
           -10            10       5.60655             0             0             0      2  # Size_DblN_ascend_se_FshLL(2)_BLK2repl_2017
        0.0001             2           0.2           0.2           0.5             6      -5  # Size_DblN_ascend_se_FshLL(2)_dev_se
         -0.99          0.99             0             0           0.5             6      -6  # Size_DblN_ascend_se_FshLL(2)_dev_autocorr
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_1990
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_2005
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_2007
             0            10            10            10             0             0      -2  # Size_DblN_descend_se_FshLL(2)_BLK2repl_2017
            10            90       84.4553            50             0             0      1  # Size_DblN_peak_FshPot(3)_BLK3repl_2017
           -20            10       0.93342             0             0             0      2  # Size_DblN_top_logit_FshPot(3)_BLK3repl_2017
           -10            10       5.75602             0             0             0      2  # Size_DblN_ascend_se_FshPot(3)_BLK3repl_2017
            10            90       63.1005            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_1996
            10            90       58.2506            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_2006
           -20            10      -5.17886             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_1996
           -20            10      -4.41468             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_2006
           -10            10       5.86811             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_1996
           -10            10       5.37556             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_2006
             0            10       4.48706            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_1996
             0            10       5.30094            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_2006
           -10       2.71828      -2.50761           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_1996
           -10       2.71828      -2.65835           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_2006
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
#      1     1     1     0     0     1     9     0     0     0     0     0
#      1     2     2     0     0     1     4     0     0     0     0     0
#      1     3     3     0     0     1     1     0     0     0     0     0
#      1     4     4     0     0     1     1     0     0     0     0     0
#      1    15     5     6     2     0     0     0     0     0     0     0
#      1    16     6     6     2     0     0     0     0     0     0     0
#      2     1     7     0     0     1     3     0     0     0     0     0
#      2     4     8     5     2     0     0     0     0     0     0     0
#      3     2     9     0     0     1     1     0     0     0     0     0
#      5     1    10     2     2     0     0     1     1  1977  1989     3 -0.237237 -0.0783391 0.0150107 0.371993 -0.135657 -0.179473 0.0422822 -0.364533 0.510737 -0.181025 0.00776798 0.0889108 0.139537
#      5     2    16     2     2     0     0     0     0     0     0     0
#      5     3    20     2     2     0     0     2     1  1977  1989     3 -0.0389849 0.0330815 -0.0135384 0.0135024 -0.0597264 0.125231 -0.166305 -0.63554 0.238087 0.304281 -0.00631553 -0.0896382 0.296007
#      5     4    26     2     2     0     0     3     1  1977  1989     3 -1.33862e-06 2.65005e-07 1.61011e-07 -1.12271e-06 -6.80403e-08 -1.03909e-06 -3.4391e-07 -1.28407e-06 -1.45629e-06 -1.93327e-06 -1.40082e-07 -3.11648e-07 -7.9454e-07
#      5     7    32     2     2     0     0     4     1  1978  1989     3 -0.300779 -0.049372 0.269851 -0.646366 -0.563021 -0.631837 -0.160638 1.16558 0.998979 -0.000844356 -0.0884063 -0.0146256
#      5     8    38     2     2     0     0     0     0     0     0     0
#      5     9    42     2     2     0     0     5     1  1978  1989     3 -0.188776 0.0238942 0.498278 -0.64034 -0.306864 -0.715548 -0.114612 0.856162 0.674824 0.000277384 -0.0848673 0.00627079
#      5    10    48     2     2     0     0     0     0     0     0     0
#      5    13    52     3     2     0     0     0     0     0     0     0
#      5    14    53     3     2     0     0     0     0     0     0     0
#      5    15    54     3     2     0     0     0     0     0     0     0
#      5    19    55     1     2     0     0     0     0     0     0     0
#      5    20    57     1     2     0     0     0     0     0     0     0
#      5    21    59     1     2     0     0     0     0     0     0     0
#      5    22    61     1     2     0     0     0     0     0     0     0
#      5    23    63     1     2     0     0     0     0     0     0     0
#      5    24    65     1     2     0     0     0     0     0     0     0
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
      4      1  0.144222
      4      2   0.49591
      4      3  0.139511
      4      4  0.440184
      4      5  0.317089
      5      1  0.023444
      5      2  0.021129
      5      3  0.019554
      5      4  0.006236
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

