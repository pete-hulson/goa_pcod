

getAIC <- function(model="Model19.1"){
	mods1<- SS_output(model,verbose = FALSE,printstats = FALSE)
	marlik <- log(2*pi)+(-1/2)*mods1$log_det_hessian+ -1*mods1$likelihoods_used[1,1]   #log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL
	nparms <- max(mods1$parameters$Active_Cnt[!is.na(mods1$parameters$Active_Cnt)])
	AIC<- -2*marlik+2*nparms
	AIC}


getMARLIK<- function(model="Model19.1"){
	mods1<- SS_output(model,verbose = FALSE,printstats = FALSE)
	marlik <- log(2*pi)+(-1/2)*mods1$log_det_hessian+ -1*mods1$likelihoods_used[1,1]   #log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL
	marlik}






filen <- file("admodel.hes", "rb")
nopar <- readBin(filen, what = "integer", n = 1)
hes <- readBin(filen, what = "double", n = nopar * nopar)
hes <- matrix(hes, byrow = TRUE, ncol = nopar)
colnames(hes) <- rownames(hes) <- sdparnames[seq(ncol(vcov))]
close(filen)




library(r4ss);
library(corpcor) #first get these libraries


# install.packages("corpcor",repos="https://cloud.r-project.org",dependencies=TRUE)
#source code - use this function.

model="Model21.1b"

ingrid_AIC<-function(model="Model21.1b"){
mods1= SS_output(model,verbose = FALSE,printstats = FALSE)

get.admb.hes <- function(model.path=getwd()){
	wd.old <- getwd(); 
	on.exit(setwd(wd.old))
	setwd(model.path)
	filename <- file("admodel.hes", "rb")
	on.exit(close(filename), add=TRUE)
	num.pars <- readBin(filename, "integer", 1)
	hes.vec <- readBin(filename, "numeric", num.pars^2)
	hes <- matrix(hes.vec, ncol=num.pars, nrow=num.pars)
	hybrid_bounded_flag <- readBin(filename, "integer", 1)
	scale <- readBin(filename, "numeric", num.pars)
	result <- list(num.pars=num.pars, hes=hes, hybrid_bounded_flag=hybrid_bounded_flag, scale=scale)
        return(result)
}   

#this pulls out the hessian and then transforms it into parameter space
HESS = get.admb.hes(model)
# Calculate Hessian
cov <- pseudoinverse(HESS$hes)
scale <- HESS$scale
cov.bounded <- cov*(scale %o% scale)
Hess = pseudoinverse(cov.bounded) 

#TRy for GOA ATF Model 17.1a

NLL= mods1$likelihoods_used[1,1]
num.pars=HESS$num.pars
LnDet = determinant(Hess, logarithm=TRUE)$modulus[[1]]
Ln_Integral = log(2*pi) + (-1/2)*sum(LnDet) + -1*NLL #this is the MARGINAL likelihood
AIC = -2*Ln_Integral + 2*num.pars;
AIC
}




stats[["log_det_hessian"]] <- read.table(corfile, 
            nrows = 1)[1, 10]