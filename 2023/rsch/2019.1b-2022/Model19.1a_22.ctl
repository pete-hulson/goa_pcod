#V3.30
#C file created using the SS_writectl function in the R package r4ss
#C file write time: 2023-08-25 10:18:43.466927
#
0 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
4 # recr_dist_method for parameters
1 # not yet implemented; Future usage:Spawner-Recruitment; 1=global; 2=by area
1 # number of recruitment settlement assignments 
0 # unused option
# for each settlement assignment:
#_GPattern	month	area	age
1	1	1	0	#_recr_dist_pattern1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
6 #_Nblock_Patterns
2 4 1 1 1 1 #_blocks_per_pattern
#_begin and end years of blocks
1996 2005 2006 2022
1990 2004 2005 2006 2007 2016 2017 2022
2017 2022
2014 2016
1976 1976
1976 2006
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
#_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0.5 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
2 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
0.1	1.5	   0.499189	 -0.81	 0.41	3	 5	  0	0	0	0	0	4	2	#_NatM_p_1_Fem_GP_1        
  0	 50	     6.3923	6.1252	   99	0	 1	101	0	0	0	0	0	0	#_L_at_Amin_Fem_GP_1       
 70	130	    99.4617	 99.46	0.015	6	 1	101	0	0	0	0	0	0	#_L_at_Amax_Fem_GP_1       
  0	  1	   0.188842	0.1966	 0.03	6	 1	101	0	0	0	0	0	0	#_VonBert_K_Fem_GP_1       
  0	 10	    3.82037	     0	    0	0	10	  0	0	0	0	0	0	0	#_CV_young_Fem_GP_1        
  0	 20	    7.42895	     0	    0	0	10	  0	0	0	0	0	0	0	#_CV_old_Fem_GP_1          
-99	 99	5.63096e-06	     0	    0	0	-3	  0	0	0	0	0	0	0	#_Wtlen_1_Fem_GP_1         
-99	 99	     3.1306	     0	    0	0	-3	  0	0	0	0	0	0	0	#_Wtlen_2_Fem_GP_1         
-99	 99	       53.7	     0	    0	0	-1	  0	0	0	0	0	0	0	#_Mat50%_Fem_GP_1          
-99	 99	  -0.273657	     0	    0	0	-1	  0	0	0	0	0	0	0	#_Mat_slope_Fem_GP_1       
-99	 99	          1	     0	    0	0	-1	  0	0	0	0	0	0	0	#_Eggs/kg_inter_Fem_GP_1   
-99	 99	          0	     0	    0	0	-1	  0	0	0	0	0	0	0	#_Eggs/kg_slope_wt_Fem_GP_1
0.1	 10	          1	     1	    1	0	-1	  0	0	0	0	0	0	0	#_CohortGrowDev            
-10	 10	          3	     0	    0	0	-5	  0	0	0	0	0	0	0	#_FracFemale_GP_1          
#_timevary MG parameters
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE
-10	10	0	0	0	0	-10	#_NatM_p_1_Fem_GP_1_BLK4repl_2014
  0	 0	0	0	0	6	  2	#_L_at_Amin_Fem_GP_1_ENV_add     
-10	10	0	0	0	0	-10	#_L_at_Amax_Fem_GP_1_ENV_add     
  0	 0	0	0	0	6	  2	#_VonBert_K_Fem_GP_1_ENV_add     
# info on dev vectors created for MGparms are reported with other devs after tag parameter section
#
#_seasonal_effects_on_biology_parms
-10 10 0 0 0 0 -1 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
0 #_Spawner-Recruitment; 2=Ricker; 3=std_B-H; 4=SCAA;5=Hockey; 6=B-H_flattop; 7=survival_3Parm;8=Shepard_3Parm
0 # 0/1 to use steepness in initial equ recruitment calculation
0 # future feature: 0/1 to make realized sigmaR a function of SR curvature
