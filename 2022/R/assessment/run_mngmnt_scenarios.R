
## ALASKA SCENARIOS FOR SINGLE SEX WITH NO B20% CUTOFF...
## Version June 17, 2020
## Created by Steve Barbeaux E-mail: steve.barbeaux@noaa.gov  Phone: (206) 729-0871 
## Altered in 2022 by Pete Hulson
##
## Note that for any biological parameter that varies over time you need to rerun the model with the values fixed to the mean as a block out 15 years and rerun your model.
## (This should be fixed in future models...)
##
## Also in the starter.ss file you should change it to read from the converged .par file 
##   1 # 0=use init values in control file; 1=use ss.par
## 
## Assumes you already have the forecast parameters already specified appropriately in the forecast.ss for scenario 1, 
## Make sure there is no catch or F already specified in the forecast file.
##
## DIR is the model directory
## CYR is the model current year, SYR is the start year for the model, FCASTY is the number of forcast years and must match what is in the forevase.ss file, also make sure the max yr for sdreport outputs in the starter.ss file is set to -2,
## SEXES is the number of sexes in model, fleets= the fleet number in SS for your fisheries,
## Scenario2 indicates whether you wish to have a different catch for scenario 2 (1= FmaxABC,2= F as S2_F, 3 = specified catch from a 
## formatted csv saved in the root directory named 'Scenario2_catch.csv', must have an entry for each year, season, and fleet for the years 
## that differ from Fmaxabc
## with columns "Year,Seas,Fleet,Catch_or_F"
## do_fig whether to plot figures
##
##

Do_AK_Scenarios<-function(Model_name = NULL,
                          Model_dir = NULL,
                          CYR = NULL,
                          SYR = 1977,
                          FCASTY = 15,
                          FLEETS = c(1:3),
                          do_fig = TRUE,
                          SEXES = 1){

  # Set up management scenario folder
  if (!file.exists(paste0(Model_dir, "/mgmnt_scenarios"))) 
    dir.create(paste0(Model_dir, "/mgmnt_scenarios"))

  # Read in base model forecast.ss file
  base_fore <- r4ss::SS_readforecast(file = paste0(Model_dir, "/forecast.ss"))
  
###############################
## Scenario 1: F is set equal to max FABC
  
  # Set up forecast.ss file
  scenario_1 <- base_fore
  scenario_1$Btarget <- 0.4
  scenario_1$SPRtarget <- 0.4
  scenario_1$Flimitfraction <- 1.0
  
  # Write SS files
  r4ss::copy_SS_inputs(dir.old = Model_dir, 
                       dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1"),
                       overwrite = TRUE)
  base::file.copy(from = paste0(Model_dir, "/ss.exe"),
            to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1", "/ss.exe"),
            overwrite = TRUE)
  base::file.copy(from = paste0(Model_dir, "/ss.par"),
            to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1", "/ss.par"),
            overwrite = TRUE)
  r4ss::SS_writeforecast(scenario_1, 
                         dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1"), 
                         file = "forecast.ss", 
                         writeAll = TRUE, 
                         overwrite = TRUE)
    
  # Run model
  r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1"),
            skipfinished = FALSE,
            show_in_console = TRUE)

  
###############################
## Scenario 2: For GOA Pcod, Scenario 2 = Scenario 1
  
  # Copy scenario 1 files
	R.utils::copyDirectory(from = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1"),
	                       to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_2"),
	                       recursive = FALSE)
  

###############################	  
## Scenario 3: Average f for previous 5 years
	
	# Set up forecast.ss file
	scenario_3 <- base_fore
	scenario_3$Forecast <- 4
	scenario_3$Fcast_years [c(3,4)] <- c(CYR - 5, CYR - 1)
		
	# Write SS files
	r4ss::copy_SS_inputs(dir.old = Model_dir, 
	                     dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_3"),
	                     overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.exe"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_3", "/ss.exe"),
	                overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.par"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_3", "/ss.par"),
	                overwrite = TRUE)
	r4ss::SS_writeforecast(scenario_3, 
	                       dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_3"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE)
	
	# Run model
	r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_3"),
	          skipfinished = FALSE,
	          show_in_console = TRUE)
	
	
###############################	
## Scenario 4: F is set equal to the F75%. 

	# Set up forecast.ss file
	scenario_4 <- base_fore
	scenario_4$Btarget <- 0.75
	scenario_4$SPRtarget <- 0.75
	
	# Write SS files
	r4ss::copy_SS_inputs(dir.old = Model_dir, 
	                     dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_4"),
	                     overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.exe"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_4", "/ss.exe"),
	                overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.par"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_4", "/ss.par"),
	                overwrite = TRUE)
	r4ss::SS_writeforecast(scenario_4, 
	                       dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_4"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE)
	
	# Run model
	r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_4"),
	          skipfinished = FALSE,
	          show_in_console = TRUE)
	
###############################	
## Scenario 5: F is 0
	
	# Set up forecast.ss file
	scenario_5 <- base_fore
	catch <- expand.grid(Year = c((CYR + 1):(CYR + FCASTY)),
	                     Seas = 1,
	                     Fleet = FLEETS,
	                     Catch_or_F = 0)
	names(catch) <- names(scenario_5$ForeCatch)
	scenario_5$ForeCatch <- rbind(scenario_5$ForeCatch,catch)	
	
	# Write SS files
	r4ss::copy_SS_inputs(dir.old = Model_dir, 
	                     dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_5"),
	                     overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.exe"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_5", "/ss.exe"),
	                overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.par"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_5", "/ss.par"),
	                overwrite = TRUE)
	r4ss::SS_writeforecast(scenario_5, 
	                       dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_5"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE)
	
	# Run model
	r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_5"),
	          skipfinished = FALSE,
	          show_in_console = TRUE)
	

###############################	
## Scenario 6: F is set equal to FOFL
	
	# Set up forecast.ss file
	scenario_6 <- base_fore
	scenario_6$Btarget <- 0.35
	scenario_6$SPRtarget <- 0.35
	scenario_6$Flimitfraction <- 1.0
	
	# Write SS files
	r4ss::copy_SS_inputs(dir.old = Model_dir, 
	                     dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_6"),
	                     overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.exe"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_6", "/ss.exe"),
	                overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.par"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_6", "/ss.par"),
	                overwrite = TRUE)
	r4ss::SS_writeforecast(scenario_6, 
	                       dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_6"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE)
	
	# Run model
	r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_6"),
	          skipfinished = FALSE,
	          show_in_console = TRUE)
	
	
###############################	
## Scenario 7: F = FABC for endyr + 1 & 2 and FOFL for all remaining years
	
	# Set up forecast.ss file
	scenario_7 <- base_fore
	scenario_7$Btarget <- 0.35
	scenario_7$SPRtarget <- 0.35
	scenario_7$Flimitfraction <- 1.0
	x <- r4ss::SS_output(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1"))
	scenario_7$ForeCatch <- r4ss::SS_ForeCatch(x, yrs = CYR:(CYR + 2))
	
	# Write SS files
	r4ss::copy_SS_inputs(dir.old = Model_dir, 
	                     dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_7"),
	                     overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.exe"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_7", "/ss.exe"),
	                overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.par"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_7", "/ss.par"),
	                overwrite = TRUE)
	r4ss::SS_writeforecast(scenario_7, 
	                       dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_7"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE)
	
	# Run model
	r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_7"),
	          skipfinished = FALSE,
	          show_in_console = TRUE)

	
###############################	
## Scenario 8: for calculating OFL value for endyr + 2

	# Set up forecast.ss file
	scenario_8 <- base_fore
	scenario_8$Btarget <- 0.35
	scenario_8$SPRtarget <- 0.35
	scenario_8$Flimitfraction <- 1.0
	x <- r4ss::SS_output(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_1"))
	scenario_8$ForeCatch <- r4ss::SS_ForeCatch(x, yrs = CYR:(CYR + 1))
	
	# Write SS files
	r4ss::copy_SS_inputs(dir.old = Model_dir, 
	                     dir.new = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_8"),
	                     overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.exe"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_8", "/ss.exe"),
	                overwrite = TRUE)
	base::file.copy(from = paste0(Model_dir, "/ss.par"),
	                to = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_8", "/ss.par"),
	                overwrite = TRUE)
	r4ss::SS_writeforecast(scenario_8, 
	                       dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_8"), 
	                       file = "forecast.ss", 
	                       writeAll = TRUE, 
	                       overwrite = TRUE)
	
	# Run model
	r4ss::run(dir = paste0(Model_dir, "/mgmnt_scenarios", "/scenario_8"),
	          skipfinished = FALSE,
	          show_in_console = TRUE)
	

###############################	
## Compile scenario output

	scen <- c("scenario_1", "scenario_2", "scenario_3", "scenario_4", "scenario_5", "scenario_6", "scenario_7", "scenario_8")
	mods1 <- r4ss::SSgetoutput(dirvec =  paste0(Model_dir, "/mgmnt_scenarios/", scen[1:8]))
	
	if(SEXES == 1) sex = 2
	if(SEXES > 1) sex = 1

	summ <- vector("list", length = 7)
	Pcatch <- vector("list", length = 7)
	EYR <- CYR + FCASTY
	yr1 <- EYR - SYR + 3
	
	# For standard scenarios 1-7
	for(i in 1:7){
	  summ[[i]] <- data.table(Yr = SYR:EYR,
	                          TOT = data.table(mods1[[i]]$timeseries)[Yr %in% c(SYR:EYR)]$Bio_all,
	                          SUMM = data.table(mods1[[i]]$timeseries)[Yr %in% c(SYR:EYR)]$Bio_smry,
	                          SSB = data.table(mods1[[i]]$timeseries)[Yr %in% c(SYR:EYR)]$SpawnBio / sex,
	                          std = data.table(mods1[[i]]$stdtable)[name %like% "SSB"][3:yr1,]$std / sex,
	                          F = data.table(mods1[[i]]$sprseries)[Yr %in% c(SYR:EYR)]$F_report,
	                          Catch = data.table(mods1[[i]]$sprseries)[Yr %in% c(SYR:EYR)]$Enc_Catch,
	                          SSB_unfished = data.table(mods1[[i]]$derived_quants)[Label == "SSB_unfished"]$Value / sex,
	                          model = scen[i])
	  
	  Pcatch[[i]] <- data.table(Yr = (CYR + 1):EYR,
	                            Catch = data.table(mods1[[i]]$sprseries)[Yr %in% c((CYR + 1):EYR)]$Enc_Catch,
	                            Catch_std = data.table(mods1[[i]]$stdtable)[name %like% "ForeCatch_"]$std, 
	                            model = scen[i])
	}
	Pcatch[[7]]$Catch_std[1:2] <- Pcatch[[1]]$Catch_std[1:2]

	# For 2-year out OFL scenario 8
	summ8 <- data.table(Yr = SYR:EYR,
	                    TOT = data.table(mods1[[8]]$timeseries)[Yr %in% c(SYR:EYR)]$Bio_all,
	                    SUMM = data.table(mods1[[8]]$timeseries)[Yr %in% c(SYR:EYR)]$Bio_smry,
	                    SSB = data.table(mods1[[8]]$timeseries)[Yr %in% c(SYR:EYR)]$SpawnBio / sex,
	                    std = data.table(mods1[[8]]$stdtable)[name %like% "SSB"][3:yr1, ]$std / sex,
	                    F = data.table(mods1[[8]]$sprseries)[Yr %in% c(SYR:EYR)]$F_report,
	                    Catch = data.table(mods1[[8]]$sprseries)[Yr %in% c(SYR:EYR)]$Enc_Catch,
	                    SSB_unfished = data.table(mods1[[8]]$derived_quants)[Label == "SSB_unfished"]$Value / sex,
	                    model = scen[8])
	Pcatch8 <- data.table(Yr = (CYR + 1):EYR,
	                      Catch = data.table(mods1[[8]]$sprseries)[Yr %in% c((CYR + 1):EYR)]$Enc_Catch,
	                      Catch_std = data.table(mods1[[8]]$stdtable)[name %like% "ForeCatch_"]$std, 
	                      model = scen[8])
	
	## Calculate 2 year projections for catch and F
	SB100 = summ[[1]][Yr == CYR + 1]$SSB_unfished
	F40_1 = summ[[1]][Yr == CYR + 1]$F
	F35_1 = summ[[6]][Yr == CYR + 1]$F
	catchABC_1 = Pcatch[[1]][Yr == CYR + 1]$Catch
	catchOFL_1 = Pcatch[[6]][Yr == CYR + 1]$Catch
	
	F40_2 = summ[[1]][Yr == CYR + 2]$F
	F35_2 = summ8[Yr == CYR + 2]$F
	catchABC_2 = Pcatch[[1]][Yr == CYR + 2]$Catch
	catchOFL_2 = Pcatch8[Yr == CYR + 2]$Catch
	SSB_1 <- summ[[1]][Yr == CYR + 1]$SSB
	SSB_2 <- summ[[1]][Yr == CYR + 2]$SSB

	Two_Year = data.table(Yr = c((CYR + 1):(CYR + 2)),
	                      SSB = c(SSB_1, SSB_2),
	                      SSB_PER = c(SSB_1 / SB100, SSB_2 / SB100),
	                      SB100 = c(SB100, SB100),
	                      SB40 = c(SB100 * 0.4, SB100 * 0.4),
	                      SB35 = c(SB100 * 0.35, SB100 * 0.35),
	                      F40 = c(F40_1, F40_2),
	                      F35 = c(F35_1, F35_2),
	                      C_ABC = c(catchABC_1, catchABC_2),
	                      C_OFL = c(catchOFL_1, catchOFL_2))
	
	## rbind vectors into tables
	summ = do.call(rbind, summ)
	Pcatch = do.call(rbind, Pcatch)
	output = list(SSB = summ,
	              CATCH = Pcatch,
	              Two_year = Two_Year)
	
	## create scenario tables for document
	BC = vector("list")
	BC$Catch <- dcast(output$SSB[Yr >= CYR],
	                  Yr ~ model,
	                  value.var = "Catch") 
	BC$F <- dcast(output$SSB[Yr >= CYR],
	              Yr ~ model,
	              value.var = "F")
	BC$SSB <- dcast(output$SSB[Yr >= CYR],
	                Yr ~ model,
	                value.var = "SSB")
	output$Tables <- BC
	
	
###############################	
## Figure code

	if(do_fig){
	  
	  # Get figure data together
	  x <- r4ss::SS_output(Model_dir)
	  SSB_unfished <- data.table(x$derived_quants)[Label == "SSB_unfished"]$Value / sex
	  
	  y <- data.table(Yr = c(SYR:EYR),
	                  TOT = 0,
	                  SUMM = 0,
	                  SSB = SSB_unfished * 0.4,
	                  std = 0,
	                  F = 0,
	                  Catch = 0,
	                  SSB_unfished = SSB_unfished,
	                  model = "SSB40%")
	  y1 <- data.table(Yr = c(SYR:EYR),
	                   TOT = 0,
	                   SUMM = 0,
	                   SSB = SSB_unfished * 0.35,
	                   std = 0,
	                   F = 0,
	                   Catch = 0,
	                   SSB_unfished = SSB_unfished,
	                   model = "SSB35%")
	  y2 <- data.table(Yr = c(SYR:EYR),
	                   TOT = 0,
	                   SUMM = 0,
	                   SSB = SSB_unfished * 0.2,
	                   std = 0,
	                   F = 0,
	                   Catch = 0,
	                   SSB_unfished = SSB_unfished,
	                   model = "SSB20%")
	  summ2 <- rbind(y, y1, y2, summ)
	  summ2 <- summ2[Yr >= year(Sys.time()) - 1]
	  summ2$model <- factor(summ2$model, levels = unique(summ2$model))
	  summ2$UCI <- summ2$SSB + 1.96 * summ2$std
	  summ2$LCI <- summ2$SSB - 1.96 * summ2$std
	  summ2[LCI < 0]$LCI = 0
	  
	  y <- data.table(Yr = c(CYR + 1:EYR),
	                  Catch = Pcatch[model == "scenario_1" & Yr == EYR]$Catch,
	                  Catch_std = 0,
	                  model = "Catch Fmaxabc")
	  y1 <- data.table(Yr = c(CYR + 1:EYR),
	                   Catch = Pcatch[model == "scenario_6" & Yr == EYR]$Catch,
	                   Catch_std = 0,
	                   model = "Catch Fofl")
	  Pcatch2 <- rbind(y, y1, Pcatch)
	  Pcatch2$model <- factor(Pcatch2$model, levels = unique(Pcatch2$model))
	  Pcatch2$UCI <- Pcatch2$Catch + 1.96 * Pcatch2$Catch_std
	  Pcatch2$LCI <- Pcatch2$Catch - 1.96 * Pcatch2$Catch_std
	  Pcatch2[LCI < 0]$LCI = 0

##SSB_Figures
	  SS_ALL <- ggplot(summ2[model %in% unique(summ2$model)[1:10]],
	                   aes(x = Yr, y = SSB, size = model, color = model, linetype = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR - 1,EYR)) +
	    scale_linetype_manual(values = c(rep(1, 3), 2:8), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", "red", 2:6, 8, 9), name = "Scenarios")+
	    scale_size_manual(values = c(rep(1.5, 3), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Spawning biomass (t)", x = "Year", title = "Projections")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_ALL.png"),
	         plot = SS_ALL)
	  
	  SS_1 <- ggplot(summ2[model %in% unique(summ2$model)[1:4]],
	                 aes(x = Yr, y = SSB, size = model, color = model, linetype = model, fill = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR - 1, EYR)) +
	    geom_ribbon(aes(ymin = LCI, ymax = UCI, linetype = model), alpha = 0.2,color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1,3),2),name = "Scenarios")+
	    scale_fill_manual(values = c("dark green","orange","red",2),name = "Scenarios")+
	    scale_color_manual(values = c("dark green","orange","red",2),name = "Scenarios")+
	    scale_size_manual(values = c(rep(1.5,3),rep(1,7)),name = "Scenarios")+labs(y = "Spawning biomass (t)",x = "Year",title = "Projections MaxFABC")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_1.png"),
	         plot = SS_1)
	  
	  SS_2 <- ggplot(summ2[model %in% unique(summ2$model)[c(1:3, 5)]],
	                 aes(x = Yr, y = SSB,size = model, color = model, linetype = model, fill = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) + 
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR-1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 3), 3), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", "red", 3), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", "red", 3), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 3), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Spawning biomass (t)", x = "Year", title = "Projections Scenario_2")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_2.png"),
	         plot = SS_2)
	  
	  SS_3 <- ggplot(summ2[model %in% unique(summ2$model)[c(1:3, 6)]], 
	               aes(x = Yr, y = SSB, size = model, color = model, linetype = model, fill = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR-1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 3), 4), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", "red", 4), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", "red", 4), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 3), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Spawning biomass (t)", x = "Year", title = "Projections Scenario 3 - Average F")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_3.png"),
	         plot = SS_3)
	  
	  SS_4 <- ggplot(summ2[model %in% unique(summ2$model)[c(1:3, 7)]], 
	               aes(x = Yr, y = SSB, size = model, color = model, linetype = model, fill = model)) +
	    geom_line() + 
	    theme_bw(base_size = 16) + 
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR-1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 3), 5), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", "red", 5), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", "red", 5), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 3), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Spawning biomass (t)", x = "Year", title = "Projections Scenario 4 - F75%")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_4.png"),
	         plot = SS_4)
	  
	  SS_5 <- ggplot(summ2[model %in% unique(summ2$model)[c(1:3, 8)]], 
	               aes(x = Yr, y = SSB, size = model, color = model, linetype = model, fill = model))+
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR-1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 3), 6), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", "red", 6), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", "red", 6), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 3), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Spawning biomass (t)", x = "Year", title = "Projections Scenario 5 - No catch")   
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_2.png"),
	         plot = SS_5)
	  
	  SS_6 <- ggplot(summ2[model %in% unique(summ2$model)[c(1:3, 9, 10)]], 
	               aes(x = Yr, y = SSB, size = model, color = model, linetype = model, fill = model))+
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(summ2$UCI)), x = c(CYR-1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 3), 2:8), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", "red", 8, 9), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", "red", 8, 9), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 3), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Spawning biomass (t)", x = "Year", title = "Projections Scenarios 6 and 7")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "SS_6.png"),
	         plot = SS_6)
	  
	  Figs_SSB <- list(SS_ALL, SS_1, SS_2, SS_3, SS_4, SS_5, SS_6)
	  
	  
	  ## Catch Figures
	  C_ALL <- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[1:9]], 
	                  aes(x = Yr, y = Catch, size = model, color = model, linetype = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(Pcatch2$UCI)), x = c(CYR+1, EYR)) +
	    scale_linetype_manual(values = c(rep(1, 2), 2:8), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", 2:6, 8, 9), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Catch (t)", x = "Year", title = "Projections")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "C_ALL.png"),
	         plot = C_ALL)
	  
	  C_1 <- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[1:3]], 
	              aes(x = Yr, y = Catch, size = model, color = model, linetype = model, fill = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(Pcatch2$UCI)), x = c(CYR+1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 2), 2), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", 2), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", 2), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Catch (t)", x = "Year", title = "Projections MaxFABC")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "C_1.png"),
	         plot = C_1)
	  
	  C_2 <- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[c(1:2, 4)]], 
	                aes(x = Yr, y = Catch, size = model, color = model, linetype = model, fill = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(Pcatch2$UCI)), x = c(CYR+1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 2), 3), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", 3), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", 3), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Catch (t)", x = "Year", title = "Projections Scenario 2")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "C_2.png"),
	         plot = C_2)
	  
	  C_3 <- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[c(1:2, 5)]], 
	                aes(x = Yr, y = Catch, size = model, color = model, linetype = model, fill = model)) +
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(Pcatch2$UCI)*1.25), x = c(CYR+1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 2), 5), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", 5), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", 5), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Catch (t)", x = "Year", title = "Projections Scenario 3 - Average F")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "C_3.png"),
	         plot = C_3)
	  
	  C_4 <- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[c(1:2, 6)]], 
	                aes(x = Yr, y = Catch, size = model, color = model, linetype = model, fill = model))+
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(Pcatch2$UCI)), x = c(CYR+1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 2), 4), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", 4), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", 4), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Catch (t)", x = "Year", title = "Projections Scenario 4 - F75%")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "C_4.png"),
	         plot = C_4)
	  
	  C_6 <- ggplot(Pcatch2[model %in% unique(Pcatch2$model)[c(1:2, 8, 9)]], 
	                aes(x = Yr, y = Catch, size = model, color = model, linetype = model, fill = model))+
	    geom_line() +
	    theme_bw(base_size = 16) +
	    lims(y = c(0, max(Pcatch2$UCI)), x = c(CYR+1, EYR)) +
	    geom_ribbon(aes(ymin = LCI,  ymax = UCI, linetype = model),  alpha = 0.2, color = "black", size = 0.2) +
	    scale_linetype_manual(values = c(rep(1, 2), 8, 9), name = "Scenarios") +
	    scale_fill_manual(values = c("dark green", "orange", 8, 9), name = "Scenarios") +
	    scale_color_manual(values = c("dark green", "orange", 8, 9), name = "Scenarios") +
	    scale_size_manual(values = c(rep(1.5, 2), rep(1, 7)), name = "Scenarios") +
	    labs(y = "Catch (t)", x = "Year", title = "Projections Scenarios 6 and 7")
	  ggsave(here::here(CYR, "plots", "proj", Model_name, "C_6.png"),
	         plot = C_6)
	  
	  Figs_Catch <- list(C_ALL, C_1, C_2, C_3, C_4, C_6)
	  output$FIGS = list(Figs_SSB, Figs_Catch)
	}
 
	return(output)
}

