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
 0.1 1.5 0.524988 -0.81 0.41 0 10 106 0 0 0 0 0 0 # NatM_uniform_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
 0.1 20 5.84666 6.1252 99 0 1 104 0 0 0 0 0 0 # L_at_Amin_Fem_GP_1
 70 130 99.4611 99.46 0.015 6 1 101 0 0 0 0 0 0 # L_at_Amax_Fem_GP_1
 0 1 0.169944 0.1966 0.03 0 1 101 0 0 0 0 0 0 # VonBert_K_Fem_GP_1
 0 10 2.44955 0 0 0 2 0 0 0 0 0 0 0 # SD_young_Fem_GP_1
 0 20 8.7987 0 0 0 2 0 0 0 0 0 0 0 # SD_old_Fem_GP_1
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
 -9 9 0.356325 0 0 0 10 # NatM_uniform_Fem_GP_1_ENV_mult
 -9 9 0.697058 0 0 0 9 # L_at_Amin_Fem_GP_1_ENV_mult
 -9 9 0.105098 0 0 0 9 # L_at_Amax_Fem_GP_1_ENV_mult
 -9 9 -0.137432 0 0 0 9 # VonBert_K_Fem_GP_1_ENV_mult
 -9 9 0.381876 0 0 0 9 # AgeKeyParm2_BLK6repl_1976
 -9 9 -0.222959 0 0 0 9 # AgeKeyParm3_BLK6repl_1976
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
            10            20       13.4152             0             0             0          1        103          0          0          0          0          0          0 # SR_LN(R0)
             0             1             1             1             0             0         -1          0          0          0          0          0          0          0 # SR_BH_steep
             0            10          0.44          0.44             0             0         -4          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             0             0         -3          0          0          0          0          0          5          1 # SR_regime
           -99            99             0             0             0             0         -1          0          0          0          0          0          0          0 # SR_autocorr
# timevary SR parameters
 -10 10 -0.00892868 0 0 0 1 # SR_LN(R0)_ENV_mult
 -10 10 -0.716659 0 0 0 1 # SR_regime_BLK5add_1976
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
#  -0.260109 -0.186096 -0.153009 -0.142231 -0.0870641 0.0408209 0.293955 0.181064 -0.0794173 -0.106187 0.657323 -0.00936329 -0.138119 0.076882 0.306525 0.548172 0.133261 0.465265 0.80185 -0.0378234 0.18766 0.167432 0.438466 0.405108 -0.00243378 -0.153523 -0.311104 -0.14786 0.0120579 -0.334017 -0.564368 -0.181292 -0.268654 -0.174404 -0.398514 -0.897672 -0.36222 -0.394502 -0.199394 0.378345 -0.154275 0.0876808 -0.172637 -0.230173 0.0639632 0.482276 -0.141331 -0.403125 -0.083977 0.221975 0.163057 0.198758 -0.0276109 0.330168 0.160326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
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
# Yr:  1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 2030 2031 2032 2033 2034 2035 2036
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# FshTrawl 0.00429608 0.0237236 0.032368 0.0719854 0.0561569 0.0307168 0.0457422 0.0302785 0.0180441 0.0243871 0.0770798 0.0813705 0.117795 0.244857 0.358249 0.472052 0.232688 0.225053 0.265765 0.550218 0.405073 0.368416 0.387542 0.308887 0.242374 0.211282 0.175775 0.192213 0.413893 0.463018 0.372343 0.436883 0.393994 0.415278 0.261704 0.230593 0.335462 0.299699 0.335877 0.316063 0.392791 0.084868 0.122465 0.0562931 0.314375 0.27676 0.316775 0.376791 0.460256 0.461936 0.461936 0.461936 0.461936 0.461936 0.461936 0.461936 0.461936 0.461936 0.461936 0.461936
# FshLL 0.0119963 0.0493087 0.069703 0.148241 0.0902319 0.0755969 0.0914059 0.0597379 0.0573466 0.0915213 0.0336785 0.0150052 0.0173697 0.0433009 0.0340278 0.0830546 0.0649967 0.037858 0.0725115 0.0562545 0.066234 0.059011 0.0980422 0.0805433 0.0699007 0.130296 0.145976 0.1973 0.0840072 0.135955 0.218196 0.458708 0.66514 0.37616 0.217503 0.211579 0.218703 0.260458 0.189685 0.10824 0.104455 0.0659948 0.0808487 0.0424028 0.220681 0.114389 0.130928 0.155734 0.190231 0.190925 0.190925 0.190925 0.190925 0.190925 0.190925 0.190925 0.190925 0.190925 0.190925 0.190925
# FshPot 0 0 0 0 0 0 0 0 0 0 0.00225949 0.00476258 0.00127434 0.0212068 0.0450406 0.048683 0.0475063 0.0425667 0.0762695 0.0645529 0.0569616 0.073818 0.146826 0.148906 0.065049 0.0764995 0.22619 0.292334 0.295756 0.335442 0.459445 0.535963 0.420888 0.486592 0.537373 0.441507 0.383725 0.478353 0.513366 0.4905 0.394454 0.139963 0.15822 0.0429428 0.250956 0.348932 0.399382 0.475048 0.58028 0.582397 0.582397 0.582397 0.582397 0.582397 0.582397 0.582397 0.582397 0.582397 0.582397 0.582397
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
           -10            10          0             0            99             0          10          0          0          0          0          0          0          0  #  LnQ_base_Srv(4)
            0             5           0             0            99             0          10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
           -10            10          0             0            99             0           5        101          0          0          0          0          0          0  #  LnQ_base_LLSrv(5)
            0             5           0             0            99             0          10          0          0          0          0          0          0          0  #  Q_power_Seine(9)
           -25            25          0             0             1             0          -1          0          0          0          0          0          0          0  #  LnQ_base_IPHCLL(6)
           -25            25          0             0             1             0          -1          0          0          0          0          0          0          0  #  LnQ_base_ADFG(7)
           -25            25          0             0            99             0          -1          0          0          0          0          0          0          0  #  LnQ_base_SPAWN(8)
          -100            25          0             0            99             0           1          0          0          0          0          0          0          0  #  LnQ_base_Seine(9)
            0             5           0             0            99             0           1          0          0          0          0          0          0          0  #  Q_power_Seine(9)
# timevary Q parameters 
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type     PHASE  #  parm_name
           -10            10      0.287191             0            99             0      5  # LnQ_base_LLSrv(5)_ENV_mult
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
            10           110       85.3791            50             0             0          1          0          1       1977       2021          3          0          0  #  Size_DblN_peak_FshTrawl(1)
           -20            10      -5.03792             0             0             0          2          0          1       1977       2021          0          0          0  #  Size_DblN_top_logit_FshTrawl(1)
           -10            10       6.27488             0             0             0          2          0          1       1977       2021          3          0          0  #  Size_DblN_ascend_se_FshTrawl(1)
           -10            10      0.269791            10             0             0          2          0          1       1977       2021          3          0          0  #  Size_DblN_descend_se_FshTrawl(1)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshTrawl(1)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshTrawl(1)
# 2   FshLL LenSelex
            10            90       75.1883            50             0             0          1          0          1       1978       2021          3          0          0  #  Size_DblN_peak_FshLL(2)
           -20            10     -0.627731             0             0             0          2          0          1       1978       2021          3          0          0  #  Size_DblN_top_logit_FshLL(2)
           -10            10       5.49855             0             0             0          2          0          1       1978       2021          3          0          0  #  Size_DblN_ascend_se_FshLL(2)
             0            10            10            10             0             0         -2          0          0       1978       2021          0          0          0  #  Size_DblN_descend_se_FshLL(2)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshLL(2)
           -10            10            10            10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshLL(2)
# 3   FshPot LenSelex
            10            90       73.0797            50             0             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_FshPot(3)
           -20            10       2.10832             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_FshPot(3)
           -10            10       5.10278             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_FshPot(3)
             0            10       4.97694            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_FshPot(3)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_FshPot(3)
           -10            10      0.123464            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_FshPot(3)
# 4   Srv LenSelex
            10            90       62.3377            50             0             0          1          0          0          0          0          0          1          2  #  Size_DblN_peak_Srv(4)
           -20            10      -10.2421             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_top_logit_Srv(4)
           -10            10       5.77163             0             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_ascend_se_Srv(4)
             0            10       3.42375            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_descend_se_Srv(4)
           -10       2.71828      -3.97873           -10             0             0          2          0          0          0          0          0          1          2  #  Size_DblN_start_logit_Srv(4)
           -10            10      0.110056            10             0             0          5          0          0          0          0          0          1          2  #  Size_DblN_end_logit_Srv(4)
# 5   LLSrv LenSelex
            10            90        66.827            50             0             0          1          0          0          0          0          0          0          0  #  Size_DblN_peak_LLSrv(5)
           -20            10      -10.0033             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_top_logit_LLSrv(5)
           -10            10       4.72177             0             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_ascend_se_LLSrv(5)
             0            10        3.9665            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_descend_se_LLSrv(5)
         -1000       2.71828          -999           -10             0             0         -2          0          0          0          0          0          0          0  #  Size_DblN_start_logit_LLSrv(5)
           -10            10      0.840096            10             0             0          2          0          0          0          0          0          0          0  #  Size_DblN_end_logit_LLSrv(5)
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
            10            90       64.6416            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_1996
            10            90       59.4555            50             0             0      1  # Size_DblN_peak_Srv(4)_BLK1repl_2006
           -20            10       -5.2523             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_1996
           -20            10      -4.76158             0             0             0      2  # Size_DblN_top_logit_Srv(4)_BLK1repl_2006
           -10            10       5.89654             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_1996
           -10            10       5.41086             0             0             0      2  # Size_DblN_ascend_se_Srv(4)_BLK1repl_2006
             0            10       4.48101            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_1996
             0            10       5.11909            10             0             0      5  # Size_DblN_descend_se_Srv(4)_BLK1repl_2006
           -10       2.71828      -2.84099           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_1996
           -10       2.71828      -3.00476           -10             0             0      2  # Size_DblN_start_logit_Srv(4)_BLK1repl_2006
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
#      2     4     8     5     1     0     0     0     0     0     0     0
#      3     2     9     0     0     1     1     0     0     0     0     0
#      5     1    10     0     0     0     0     1     1  1977  2021     3 -0.349076 -0.199935 0.00612055 -0.162177 -0.27619 -0.336274 -0.340482 -1.14026 -0.689258 -0.162562 -0.193822 -0.426283 -0.283324 -0.296698 0.335052 0.827221 0.17079 0.273322 0.174543 0.858424 0.0765249 0.0560255 0.114181 0.220378 -0.247142 -0.179372 -0.675001 -0.632957 0.583972 0.842445 0.42539 0.155671 0.269065 0.157394 -0.0953289 -0.177612 0.279716 -0.00527835 0.375055 0.507015 0.389742 0.0070309 0.101436 -0.409528 0.0722857
#      5     2    12     0     0     0     0     2     1  1977  2021     0 6.16771e-08 3.21099e-08 -4.21522e-09 8.04384e-08 1.11758e-08 2.99661e-08 -1.77426e-07 8.24063e-07 4.2027e-07 1.90693e-08 2.40387e-08 1.38714e-10 9.56407e-08 2.16934e-09 -3.72332e-07 -3.75781e-07 -1.10286e-07 -1.76381e-07 -1.72613e-07 -2.41042e-07 -3.27709e-07 1.72843e-07 -1.73803e-08 -2.4166e-07 -3.48515e-08 2.0202e-07 3.97535e-07 6.96501e-07 -1.78651e-07 -1.92419e-07 -3.74137e-07 -3.76019e-08 -5.03971e-08 -5.92422e-08 -2.54685e-08 1.5528e-08 1.02622e-08 -2.53236e-08 -3.19749e-08 -1.36151e-08 -7.881e-08 -2.62084e-08 -2.52697e-08 3.44918e-07 1.00242e-07
#      5     3    14     0     0     0     0     3     1  1977  2021     3 0.47358 0.244904 -0.00422762 0.105719 0.35207 0.407404 0.110309 -0.0246744 -0.0546218 0.414733 0.269337 0.270328 0.373863 -0.210579 -0.0937034 0.0468141 0.00523339 -0.250132 -0.0443071 -0.221974 -0.221511 -0.152264 -0.206488 -0.123119 -0.11388 -0.0183467 -0.0872312 -0.325497 -0.516436 -0.326114 0.108332 0.215978 -0.149035 -0.0929362 -0.361506 0.11113 0.146933 0.357417 0.265511 0.048355 -0.414085 -0.037427 -0.128313 0.44968 -0.599092
#      5     4    16     0     0     0     0     4     1  1977  2021     3 -1.11055e-08 -4.16227e-09 5.62306e-10 -6.4909e-09 -2.65183e-09 -8.22528e-09 2.97091e-08 -7.32984e-08 -4.96894e-08 -2.90302e-09 -3.50453e-09 -2.7225e-10 -1.28476e-08 2.29946e-09 4.36733e-08 7.59215e-08 2.10813e-08 2.03738e-08 2.97677e-08 4.92786e-08 5.61457e-08 -2.55502e-08 7.06122e-09 3.73026e-08 9.89199e-09 -2.96484e-08 -5.6132e-08 -8.33172e-08 3.01757e-08 4.17563e-08 5.63361e-08 5.64471e-09 1.15777e-08 9.68017e-09 3.44154e-09 -2.07267e-09 -2.10766e-09 4.86991e-09 7.68783e-09 3.68347e-09 1.27152e-08 9.3426e-09 1.44759e-10 -4.61825e-08 -1.43225e-08
#      5     7    18     0     0     0     0     5     1  1978  2021     3 -0.985672 -0.384556 -0.139877 -1.69669  -1.47 -1.47715 -0.200219 1.44695 1.09732 -0.00223546 -0.304211 -0.0235443 1.46926 0.0178693 0.330511 1.04136 0.44533 0.967361 0.224567 -0.245288 -0.277052 0.18005 -0.645196 -0.902511 -0.500603 0.236266 0.624297 -0.553352 -0.360631 0.100814 0.930991 1.36531 0.403046 -0.366727 -0.469371 0.0171166 0.148566 0.102038 -0.0339249 -0.526086 -0.0757654 -0.0578173 0.147028 0.386996
#      5     8    20     0     0     0     0     6     1  1978  2021     3 4.66316e-07 3.25252e-07 1.72146e-07 7.73966e-08 1.33908e-07 6.06012e-08 -2.09965e-08 1.51732e-08 -8.5119e-08 -1.91791e-10 9.43027e-09 5.79192e-10 -6.98875e-08 -2.2179e-07 -5.45495e-07 -2.13602e-07 -2.14079e-07 -2.81106e-07 -4.77227e-08 -3.39671e-09 -9.82687e-09 -1.96338e-07 -7.13421e-08 -4.19761e-08 1.20763e-07 -2.72169e-07 -5.02255e-08 -3.25436e-07 -2.58958e-07 -2.78913e-07 -7.90443e-08 -8.51934e-09 -1.74418e-08 -1.99498e-08 -1.65775e-09 -5.10906e-09 -3.41028e-08 -5.23781e-08 -6.02238e-08 -2.13487e-07 -1.25088e-07 -9.98448e-08 -8.91095e-09 -1.12215e-07
#      5     9    22     0     0     0     0     7     1  1978  2021     3 -0.218743 0.151557 0.610748 -0.683806 -0.384842 -0.761951 0.254736 1.04133 0.763098 0.00234973 0.138399 0.0141614 0.90954 -0.189352 0.107669 0.396331 0.0817489 0.50724 0.240532 -0.396186 0.18734 0.113956 -0.365644 -0.542308 -0.543268 0.157153 0.210174 -0.21647 -0.184523 0.179296 0.387313 0.635481 0.0588468 -0.205922 -0.614919 -0.611176 -0.132785 0.267137 0.554372 -0.0969446 -0.24443 -0.580653 -0.60593 -0.380718
#      5    10    24     0     0     0     0     8     1  1978  2021     3 -1.03132e-07 -5.90456e-08 -2.76741e-08 -2.76879e-08 -3.23629e-08 -2.60534e-08 -7.60247e-09 -2.38924e-09 5.84256e-09 4.82707e-11 -1.45741e-09 -6.06661e-11 4.10153e-09 4.81365e-08 7.38045e-08 2.50022e-08 2.21845e-08 2.79618e-08 4.75683e-09 1.49203e-10 1.23305e-09 3.07472e-08 6.40583e-09 1.47347e-08 -1.93034e-08 1.56737e-08 -1.50063e-09 4.84566e-08 4.55258e-08 3.3831e-08 6.98869e-09 5.24183e-10 1.46561e-09 2.09239e-09 3.84242e-10 8.7239e-10 2.71729e-09 4.63538e-09 6.14549e-09 2.39314e-08 1.60507e-08 8.39913e-09 -2.11585e-10 1.1937e-08
#      5    19    26     1     2     0     0     0     0     0     0     0
#      5    20    28     1     2     0     0     0     0     0     0     0
#      5    21    30     1     2     0     0     0     0     0     0     0
#      5    22    32     1     2     0     0     0     0     0     0     0
#      5    23    34     1     2     0     0     0     0     0     0     0
#      5    24    36     1     2     0     0     0     0     0     0     0
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

