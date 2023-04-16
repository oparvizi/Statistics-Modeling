# Source: https://link.springer.com/book/10.1007/978-3-031-17785-9
#         https://www.routledge.com/Using-R-for-Modelling-and-Quantitative-Methods-in-Fisheries/Haddon/p/book/9780367469887

# Types of Uncertainty: Process uncertainty, Observation uncertainty, 
#                       Model uncertainty (Different structural models, Residual error structure, Estimation uncertainty)
#                       Implementation uncertainty (Institutional uncertainty, Time-lags between making decisions and implementing)

# Available strategies for characterizing uncertainty: Bootstrapping, Asymptotic errors, Likelihood profiles, Bayesian marginal posteriors

library(MQMF)
#Fit a surplus production model to abdat fisheries data
data(abdat); logce <- log(abdat$cpue)
param <- log(c(0.42,9400,3400,0.05))
label=c("r","K","Binit","sigma") # simpspm returns
bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,logobs=logce)
outfit(bestmod,title="SP-Model",parnames=label) #backtransforms
# nlm solution: SP-Model------------------------------
# minimum : -41.37511
# iterations : 20
# code : 2 >1 iterates in tolerance, probably solution
# par gradient transpar
# r -0.9429555 6.707523e-06 0.38948
# K 9.1191569 -9.225209e-05 9128.50173
# Binit 8.1271026 1.059296e-04 3384.97779
# sigma -3.1429030 -8.161433e-07 0.04316


#plot the abdat data and the optimum sp-model fit Fig. 6.1
predce <- exp(simpspm(bestmod$estimate,abdat))
optresid <- abdat[,"cpue"]/predce #multiply by predce for obsce
ymax <- getmax(c(predce,abdat$cpue))
plot1(abdat$year,(predce*optresid),type="l",maxy=ymax,cex=0.9,
      ylab="CPUE",xlab="Year",lwd=3,col="grey",lty=1)
points(abdat$year,abdat$cpue,pch=1,col=1,cex=1.1)
lines(abdat$year,predce,lwd=2,col=1) # best fit line
# FIGURE: --------------------------------------------
# The optimum fit of the Schaefer surplus production model to
# the abdat data-set plotted in linear-space (solid red-line). The grey line passes
# through the data points to clarify the difference with the predicted line.

# ------------------------------------------------------------------------------
# Bootstrapping
# ------------------------------------------------------------------------------
#regression between catches of NPF prawn species Fig.
data(npf)
model <- lm(endeavour ~ tiger,data=npf)
plot1(npf$tiger,npf$endeavour,type="p",xlab="Tiger Prawn (t)",
      ylab="Endeavour Prawn (t)",cex=0.9)
abline(model,col=1,lwd=2)
correl <- sqrt(summary(model)$r.squared)
pval <- summary(model)$coefficients[2,4]
label <- paste0("Correlation ",round(correl,5)," P = ",round(pval,8))
text(2700,180,label,cex=1.0,font=7,pos=4)
# FIGURE: --------------------------------------------
# The positive correlation between the catches of endeavour and
# tiger prawns in the Australian Northern Prawn Fishery between 1970–1992
# (data from Robins and Somers, 1994).

# 5000 bootstrap estimates of correlation coefficient Fig 
set.seed(12321) # better to use a less obvious seed, if at all
N <- 5000 # number of bootstrap samples
result <- numeric(N) #a vector to store 5000 correlations
for (i in 1:N) { #sample index from 1:23 with replacement
  pick <- sample(1:23,23,replace=TRUE) #sample is an R function
  result[i] <- cor(npf$tiger[pick],npf$endeavour[pick])
}
rge <- range(result) # store the range of results
CI <- quants(result) # calculate quantiles; 90%CI = 5% and 95%
restrim <- result[result > 0] #remove possible -ve values for plot
parset(cex=1.0) # set up a plot window and draw a histogram
bins <- seq(trunc(range(restrim)[1]*10)/10,1.0,0.01)
outh <- hist(restrim,breaks=bins,main="",col=0,xlab="Correlation")
abline(v=c(correl,mean(result)),col=c(4,3),lwd=c(3,2),lty=c(1,2))
abline(v=CI[c(2,4)],col=4,lwd=2) # and 90% confidence intervals
text(0.48,400,makelabel("Range ",rge,sep=" ",sigdig=4),font=7,pos=4)
label <- makelabel("90%CI ",CI[c(2,4)],sep=" ",sigdig=4)
text(0.48,300,label,cex=1.0,font=7,pos=4)
# FIGURE: --------------------------------------------
# 5000 bootstrap estimates of the correlation between endeavour
# and tiger prawn catches with the original mean in dashed green and bootstrap
# mean and 90% CI in solid blue. Possible negative correlations have been
# removed for plotting purposes (though none occurred).

# ------------------------------------------------------------------------------
# Bootstrapping Time-Series Data
# ------------------------------------------------------------------------------
cat("The abdat data-set with the associated optimum predicted
cpue (predce), and the optimum residuals (optres).
-------------------------------------------------------------
year catch cpue  predce optres year catch cpue  predce optres
1985 1020  1.000 1.135  0.881  1997 655   2.051 1.998  1.027
1986 743   1.096 1.071  1.023  1998 494   2.124 2.049  1.037
1987 867   1.130 1.093  1.034  1999 644   2.215 2.147  1.032
1988 724   1.147 1.076  1.066  2000 960   2.253 2.180  1.033
1989 586   1.187 1.105  1.075  2001 938   2.105 2.103  1.001
1990 532   1.202 1.183  1.016  2002 911   2.082 2.044  1.018
1991 567   1.265 1.288  0.983  2003 955   2.009 2.003  1.003
1992 609   1.320 1.388  0.951  2004 936   1.923 1.952  0.985
1993 548   1.428 1.479  0.966  2005 941   1.870 1.914  0.977
1994 498   1.477 1.593  0.927  2006 954   1.878 1.878  1.000
1995 480   1.685 1.724  0.978  2007 1027  1.850 1.840  1.005
1996 424   1.920 1.856  1.034  2008 980   1.727 1.782  0.969
")

# fitting Schaefer model with log-normal residuals with 24 years
data(abdat); logce <- log(abdat$cpue) # of abalone fisheries data
param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05)) #log values
bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,logobs=logce)
optpar <- bestmod$estimate # these are still log-transformed
predce <- exp(simpspm(optpar,abdat)) #linear-scale pred cpue
optres <- abdat[,"cpue"]/predce # optimum log-normal residual
optmsy <- exp(optpar[1])*exp(optpar[2])/4
sampn <- length(optres) # number of residuals and of years


# 1000 bootstrap Schaefer model fits; takes a few seconds
start <- Sys.time() # use of as.matrix faster than using data.frame
bootfish <- as.matrix(abdat) # and avoid altering original data
N <- 1000; years <- abdat[,"year"] # need N x years matrices
columns <- c("r","K","Binit","sigma")
results <- matrix(0,nrow=N,ncol=sampn,dimnames=list(1:N,years))
bootcpue <- matrix(0,nrow=N,ncol=sampn,dimnames=list(1:N,years))
parboot <- matrix(0,nrow=N,ncol=4,dimnames=list(1:N,columns))
for (i in 1:N) { # fit the models and save solutions
  bootcpue[i,] <- predce * sample(optres, sampn, replace=TRUE)
  bootfish[,"cpue"] <- bootcpue[i,] #calc and save bootcpue
  bootmod <- nlm(f=negLL,p=optpar,funk=simpspm,indat=bootfish,
                 logobs=log(bootfish[,"cpue"]))
  parboot[i,] <- exp(bootmod$estimate) #now save parameters
  results[i,] <- exp(simpspm(bootmod$estimate,abdat)) #and predce
}
cat("total time = ",Sys.time()-start, "seconds \n")
# total time = 3.831626 seconds

# bootstrap replicates in grey behind main plot Fig 
plot1(abdat[,"year"],abdat[,"cpue"],type="n",xlab="Year",
      ylab="CPUE") # type="n" just lays out an empty plot
for (i in 1:N) # ready to add the separate components
  lines(abdat[,"year"],results[i,],lwd=1,col="grey")
points(abdat[,"year"],abdat[,"cpue"],pch=16,cex=1.0,col=1)
lines(abdat[,"year"],predce,lwd=2,col=1)
# FIGURE: --------------------------------------------
# 1000 bootstrap estimates of the optimum predicted cpue from
# the abdat data-set for an abalone fishery. Black points are the original data,
# the black line is the optimum predicted cpue from the original model fit, and
# the grey trajectories are the 1000 bootstrap estimates of the predicted cpue.

#histograms of bootstrap parameters and model outputs Fig.
dohist <- function(invect,nmvar,bins=30,bootres,avpar) { #adhoc
  hist(invect[,nmvar],breaks=bins,main="",xlab=nmvar,col=0)
  abline(v=c(exp(avpar),bootres[pick,nmvar]),lwd=c(3,2,3,2),
         col=c(3,4,4,4))
}
msy <- parboot[,"r"]*parboot[,"K"]/4 #calculate bootstrap MSY
msyB <- quants(msy) #from optimum bootstrap parameters
parset(plots=c(2,2),cex=0.9)
bootres <- apply(parboot,2,quants); pick <- c(2,3,4) #quantiles
dohist(parboot,nmvar="r",bootres=bootres,avpar=optpar[1])
dohist(parboot,nmvar="K",bootres=bootres,avpar=optpar[2])
dohist(parboot,nmvar="Binit",bootres=bootres,avpar=optpar[3])
hist(msy,breaks=30,main="",xlab="MSY",col=0)
abline(v=c(optmsy,msyB[pick]),lwd=c(3,2,3,2),col=c(3,4,4,4))
# FIGURE: --------------------------------------------
# The 1000 bootstrap estimates of each of the first three model
# parameters and MSY as histograms. In each plot the two fine outer lines define
# the inner 90% confidence bounds around the median, the central vertical line
# denotes the optimum estimates, but these are generally immediately below
# the medians, except for the Binit.

# Parameter Correlation
#relationships between parameters and MSY Fig. 6.6
parboot1 <- cbind(parboot,msy)
# note rgb use, alpha allows for shading, try 1/15 or 1/10
pairs(parboot1,pch=16,col=rgb(red=1,green=0,blue=0,alpha = 1/20))

# ------------------------------------------------------------------------------
# BAsymptotic Errors
# ------------------------------------------------------------------------------

#Fit Schaefer model and generate the Hessian
data(abdat)
param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
# Note inclusion of the option hessian=TRUE in nlm function
bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,
               logobs=log(abdat[,"cpue"]),hessian=TRUE)
outfit(bestmod,backtran = TRUE) #try typing bestmod in console
# Now generate the confidence intervals
vcov <- solve(bestmod$hessian) # solve inverts matrices
sterr <- sqrt(diag(vcov)) #diag extracts diagonal from a matrix
optpar <- bestmod$estimate #use qt for t-distrib quantiles
U95 <- optpar + qt(0.975,20)*sterr # 4 parameters hence
L95 <- optpar - qt(0.975,20)*sterr # (24 - 4) df
cat("\n r K Binit sigma \n")
cat("Upper 95% ",round(exp(U95),5),"\n") # backtransform
cat("Optimum ",round(exp(optpar),5),"\n")#\n =linefeed in cat
cat("Lower 95% ",round(exp(L95),5),"\n")
# nlm solution:---------------------------------------
# minimum : -41.37511
# iterations : 20
# code : 2 >1 iterates in tolerance, probably solution
# par gradient transpar
# 1 -0.9429555 6.707523e-06 0.38948
# 2 9.1191569 -9.225209e-05 9128.50173
# 3 8.1271026 1.059296e-04 3384.97779
# 4 -3.1429030 -8.161433e-07 0.04316
# hessian :
# [,1] [,2] [,3] [,4]
# [1,] 3542.8630987 2300.305473 447.63247 -0.3509669
# [2,] 2300.3054733 4654.008776 -2786.59928 -4.2155105
# [3,] 447.6324677 -2786.599276 3183.93947 -2.5662898
# [4,] -0.3509669 -4.215511 -2.56629 47.9905538
#
# r K Binit sigma
# Upper 95% 0.45025 10948.12 4063.59 0.05838
# Optimum 0.38948 9128.502 3384.978 0.04316
# Lower 95% 0.33691 7611.311 2819.693 0.0319

# Sampling from a Multivariate Normal Distribution
# Use multivariate normal to generate percentile CI Fig. 6.7
library(mvtnorm) # use RStudio, or install.packages("mvtnorm")
N <- 1000 # number of multi-variate normal parameter vectors
years <- abdat[,"year"]; sampn <- length(years) # 24 years
mvncpue <- matrix(0,nrow=N,ncol=sampn,dimnames=list(1:N,years))
columns <- c("r","K","Binit","sigma")
# Fill parameter vectors with N vectors from rmvnorm
mvnpar <- matrix(exp(rmvnorm(N,mean=optpar,sigma=vcov)),
                 nrow=N,ncol=4,dimnames=list(1:N,columns))
# Calculate N cpue trajectories using simpspm
for (i in 1:N) mvncpue[i,] <- exp(simpspm(log(mvnpar[i,]),abdat))
msy <- mvnpar[,"r"]*mvnpar[,"K"]/4 #N MSY estimates
# plot data and trajectories from the N parameter vectors
plot1(abdat[,"year"],abdat[,"cpue"],type="p",xlab="Year",
      ylab="CPUE",cex=0.9)
for (i in 1:N) lines(abdat[,"year"],mvncpue[i,],col="grey",lwd=1)
points(abdat[,"year"],abdat[,"cpue"],pch=16,cex=1.0)#orig data
lines(abdat[,"year"],exp(simpspm(optpar,abdat)),lwd=2,col=1)
# FIGURE: --------------------------------------------
# The 1000 predicted cpue trajectories derived from random
# parameter vectors sampled from the multivariate Normal distribution defined
# by the optimum parameters and their related variance-covariance matrix.

#correlations between parameters when using mvtnorm Fig. 6.8
pairs(cbind(mvnpar,msy),pch=16,col=rgb(red=1,0,0,alpha = 1/10))
# FIGURE: --------------------------------------------
# The relationships between the 1000 parameter estimates sampled
# from the estimated multivariate Normal distribution assumed to surround
# the optimum parameter estimates and the derived MSY values for the Schaefer
# model fitted to the abdat data-set.

#N parameter vectors from the multivariate normal Fig. 6.9
mvnres <- apply(mvnpar,2,quants) # table of quantiles
pick <- c(2,3,4) # select rows for 5%, 50%, and 95%
meanmsy <- mean(msy) # optimum bootstrap parameters
msymvn <- quants(msy) # msy from multi-variate normal estimates
plothist <- function(x,optp,label,resmvn) {
  hist(x,breaks=30,main="",xlab=label,col=0)
  abline(v=c(exp(optp),resmvn),lwd=c(3,2,3,2),col=c(3,4,4,4))
} # repeated 4 times, so worthwhile writing a short function
par(mfrow=c(2,2),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
plothist(mvnpar[,"r"],optpar[1],"r",mvnres[pick,"r"])
plothist(mvnpar[,"K"],optpar[2],"K",mvnres[pick,"K"])
plothist(mvnpar[,"Binit"],optpar[3],"Binit",mvnres[pick,"Binit"])
plothist(msy,meanmsy,"MSY",msymvn[pick])
# FIGURE: --------------------------------------------
# Histograms of the 1000 parameter estimates for r, K, Binit, and
# the derived MSY, from the multivariate Normal estimated at the optimum
# solution. In each plot, the vertical central green line denotes the arithmetic
# mean, the overlapping vertical thick blue line, the median, and the two fine
# outer blue lines the inner 90% confidence bounds around the median.
cat("A comparison of the bootstrap percentile confidence
bounds on parameters with those derived from the Asymptotic estimate
of the variance-covariance matrix. The top table relates to the
bootstrapping and the bottom to the values from the multivariate
Normal.
----------------------------------------------------------
      r       K         Binit     sigma   msyB
2.5%  0.3353  7740.636  2893.714  0.0247  854.029
5%    0.3449  8010.341  2970.524  0.0263  857.660
50%   0.3903  9116.193  3387.077  0.0385  888.279
95%   0.4353  10507.708 3889.824  0.0501  929.148
97.5% 0.4452  11003.649 4055.071  0.0523  940.596

      r       K         Binit     sigma   msymvn
2.5%  0.3387  7717.875  2882.706  0.0328  844.626
5%    0.3474  7943.439  2945.264  0.0342  851.257
50%   0.3889  9145.146  3389.279  0.0432  889.216
95%   0.4368  10521.651 3900.034  0.0550  928.769
97.5% 0.4435  10879.571 4031.540  0.0581  934.881
")

# ------------------------------------------------------------------------------
# Likelihood Profiles
# ------------------------------------------------------------------------------
#Fit the Schaefer surplus production model to abdat
data(abdat); logce <- log(abdat$cpue) # using negLL
param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
optmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,logobs=logce)
outfit(optmod,parnames=c("r","K","Binit","sigma"))
# nlm solution:---------------------------------------
# minimum : -41.37511
# iterations : 20
# code : 2 >1 iterates in tolerance, probably solution
# par gradient transpar
# r -0.9429555 6.707523e-06 0.38948
# K 9.1191569 -9.225209e-05 9128.50173
# Binit 8.1271026 1.059296e-04 3384.97779
# sigma -3.1429030 -8.161433e-07 0.04316

#the code for MQMF's negLLP function
negLLP <- function(pars, funk, indat, logobs, initpar=pars,
                   notfixed=c(1:length(pars)),...) {
  usepar <- initpar #copy the original parameters into usepar
  usepar[notfixed] <- pars[notfixed] #change 'notfixed' values
  npar <- length(usepar)
  logpred <- funk(usepar,indat,...) #funk uses the usepar values
  pick <- which(is.na(logobs)) # proceed as in negLL
  if (length(pick) > 0) {
    LL <- -sum(dnorm(logobs[-pick],logpred[-pick],exp(pars[npar]),
                     log=T))
  } else {
    LL <- -sum(dnorm(logobs,logpred,exp(pars[npar]),log=T))
  }
  return(LL)
} # end of negLLP

#does negLLP give same answers as negLL when no parameters fixed?
param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
bestmod <- nlm(f=negLLP,p=param,funk=simpspm,indat=abdat,logobs=logce)
outfit(bestmod,parnames=c("r","K","Binit","sigma"))
# nlm solution:---------------------------------------
# minimum : -41.37511
# iterations : 20
# code : 2 >1 iterates in tolerance, probably solution
# par gradient transpar
# r -0.9429555 6.707523e-06 0.38948
# K 9.1191569 -9.225209e-05 9128.50173
# Binit 8.1271026 1.059296e-04 3384.97779
# sigma -3.1429030 -8.161433e-07 0.04316

#Likelihood profile for r values 0.325 to 0.45
rval <- seq(0.325,0.45,0.001) # set up the test sequence
ntrial <- length(rval) # create storage for the results
columns <- c("r","K","Binit","sigma","-veLL")
result <- matrix(0,nrow=ntrial,ncol=length(columns),
                 dimnames=list(rval,columns))# close to optimum
bestest <- c(r= 0.32,K=11000,Binit=4000,sigma=0.05)
for (i in 1:ntrial) { #i <- 1
  param <- log(c(rval[i],bestest[2:4])) #recycle bestest values
  parinit <- param #to improve the stability of nlm as r changes
  bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,
                  indat=abdat,logobs=log(abdat$cpue),notfixed=c(2:4),
                  typsize=magnitude(param),iterlim=1000)
  bestest <- exp(bestmodP$estimate)
  result[i,] <- c(bestest,bestmodP$minimum) # store each result
}
minLL <- min(result[,"-veLL"]) #minimum across r values used.

cat("The first 12 records from the 126 rows of the nlm
solutions that are used to make the likelihood profile on r. The
strong correlation between r, K, and Binit is, once again, apparent.
--------------------------------------------------------
      r         K       Binit sigma     -veLL
0.325 0.325 11449.17 4240.797 0.0484 -38.61835
0.326 0.326 11403.51 4223.866 0.0483 -38.69554
0.327 0.327 11358.24 4207.082 0.0481 -38.77196
0.328 0.328 11313.35 4190.442 0.0480 -38.84759
0.329 0.329 11268.83 4173.945 0.0478 -38.92242
0.33  0.330 11224.69 4157.589 0.0477 -38.99643
0.331 0.331 11180.91 4141.373 0.0475 -39.06961
0.332 0.332 11137.49 4125.293 0.0474 -39.14194
0.333 0.333 11094.43 4109.350 0.0472 -39.21339
0.334 0.334 11051.72 4093.540 0.0471 -39.28397
0.335 0.335 11009.11 4077.752 0.0469 -39.35364
0.336 0.336 10967.34 4062.316 0.0468 -39.42239
")

# Likelihood Ratio-Based Confidence Intervals
#likelihood profile on r from the Schaefer model Fig. 6.10
plotprofile(result,var="r",lwd=2) # review the code
# FIGURE:
# A likelihood profile for the r parameter from the Schaefer
# surplus production model fitted to the abdat data-set. The horizontal solid
# lines are the minimum and the minimum minus 1.92 (95% level for 𝜒2 with 1
# degree of freedom, see text). The outer vertical lines are the approximate 95%
# confidence bounds around the central mean of 0.389.

#Likelihood profile for K values 7200 to 12000
Kval <- seq(7200,12000,10)
ntrial <- length(Kval)
columns <- c("r","K","Binit","sigma","-veLL")
resultK <- matrix(0,nrow=ntrial,ncol=length(columns),
                  dimnames=list(Kval,columns))
bestest <- c(r= 0.45,K=7500,Binit=2800,sigma=0.05)
for (i in 1:ntrial) {
  param <- log(c(bestest[1],Kval[i],bestest[c(3,4)]))
  parinit <- param
  bestmodP <- nlm(f=negLLP,p=param,funk=simpspm,initpar=parinit,
                  indat=abdat,logobs=log(abdat$cpue),
                  notfixed=c(1,3,4),iterlim=1000)
  bestest <- exp(bestmodP$estimate)
  resultK[i,] <- c(bestest,bestmodP$minimum)
}
minLLK <- min(resultK[,"-veLL"])
#kable(head(result,12),digits=c(4,3,3,4,5)) # if wanted.
#likelihood profile on K from the Schaefer model Fig 6.11
plotprofile(resultK,var="K",lwd=2)
# FIGURE: --------------------------------------------
# A likelihood profile for the K parameter from the Schaefer
# surplus production model fitted to the abdat data-set, conducted in the same
# manner as the r parameter. The red lines are the minimum and the minimum
# plus 1.92 (95% level for Chi2 with 1 degree of freedom, see text). The vertical
# thick lines are the approximate 95% confidence bounds around the mean of
# 9128.5.

# -ve Log-Likelihoods or Likelihoods
#translate -velog-likelihoods into likelihoods
likes <- exp(-resultK[,"-veLL"])/sum(exp(-resultK[,"-veLL"]),
                                     na.rm=TRUE)
resK <- cbind(resultK,likes,cumlike=cumsum(likes))
# The records from the nlm solutions---------------------------------
# that are used to make the likelihood profile on K. Included is
# the back-transformed -ve log-likelihoods, scaled to sum to 1.0, and
# their running cumulative sum.

# Percentile Likelihood Profiles for Model Outputs++++++++++++++++++++++++++++++

#examine effect on -veLL of MSY values from 740 - 1050t
#need a different negLLP() function, negLLO(): O for output.
#now optvar=888.831 (rK/4), the optimum MSY, varval ranges 740-1050
#and wght is the weighting to give to the penalty
negLLO <- function(pars,funk,indat,logobs,wght,optvar,varval) {
  logpred <- funk(pars,indat)
  LL <- -sum(dnorm(logobs,logpred,exp(tail(pars,1)),log=T)) +
    wght*((varval - optvar)/optvar)^2 #compare with negLL
  return(LL)
} # end of negLLO
msyP <- seq(740,1020,2.5);
optmsy <- exp(optmod$estimate[1])*exp(optmod$estimate[2])/4
ntrial <- length(msyP)
wait <- 400
columns <- c("r","K","Binit","sigma","-veLL","MSY","pen",
             "TrialMSY")
resultO <- matrix(0,nrow=ntrial,ncol=length(columns),
                  dimnames=list(msyP,columns))
bestest <- c(r= 0.47,K=7300,Binit=2700,sigma=0.05)
for (i in 1:ntrial) { # i <- 1
  param <- log(bestest)
  bestmodO <- nlm(f=negLLO,p=param,funk=simpspm,indat=abdat,
                  logobs=log(abdat$cpue),wght=wait,
                  optvar=optmsy,varval=msyP[i],iterlim=1000)
  bestest <- exp(bestmodO$estimate)
  ans <- c(bestest,bestmodO$minimum,bestest[1]*bestest[2]/4,
           wait *((msyP[i] - optmsy)/optmsy)^2,msyP[i])
  resultO[i,] <- ans
}
minLLO <- min(resultO[,"-veLL"])

cat("The first and last 7 records from the 113 rows of the
nlm solutions that are used to make the likelihood profile on MSY
(one might do more). The row names are the trial MSY values and
pen is the penalty value.
---------------------------------------------------------
      r     K         Binit     sigma -veLL      MSY pen
740   0.389 9130.914  3385.871 0.0432 -30.16 888.883 11.22
742.5 0.389 9130.911  3385.872 0.0432 -30.53 888.883 10.84
745   0.389 9130.911  3385.872 0.0432 -30.90 888.883 10.47
747.5 0.389 9130.911  3385.872 0.0432 -31.26 888.883 10.11

        r     K        Binit    sigma  -veLL  MSY     pen
1012.5  0.389 9130.911 3385.872 0.0432 -33.63 888.883 7.74
1015    0.389 9130.911 3385.872 0.0432 -33.32 888.883 8.06
1017.5  0.389 9130.911 3385.872 0.0432 -32.99 888.883 8.38
1020    0.389 9130.911 3385.872 0.0432 -32.66 888.883 8.71
")

#########code??

# FIGURE: --------------------------------------------
# A likelihood profile for the MSY implied by the Schaefer
# surplus production model fitted to the abdat data-set. The horizontal red
# lines are the minimum and the minimum plus 1.92 (95% level for Chi2 with
# 1 degree of freedom, see text). The vertical lines are the approximate 95%
# confidence bounds around the mean of 887.729t.


# ------------------------------------------------------------------------------
# Bayesian Posterior Distributions
# ------------------------------------------------------------------------------
# Application of MCMC to the Example
#activate and plot the fisheries data in abdat Fig. 6.14
data(abdat) # type abdat in the console to see contents
plotspmdat(abdat) #use helper function to plot fishery stats vs year

# Markov Chain Monte Carlo
# Conduct MCMC analysis to illustrate burn-in. Fig. 6.15
data(abdat); logce <- log(abdat$cpue)
fish <- as.matrix(abdat) # faster to use a matrix than a data.frame!
begin <- Sys.time() # enable time taken to be calculated
chains <- 1 # 1 chain per run; normally do more
burnin <- 0 # no burn-in for first three chains
N <- 100 # Number of MCMC steps to keep
step <- 4 # equals one step per parameter so no thinning
priorcalc <- calcprior # define the prior probability function
scales <- c(0.065,0.055,0.065,0.425) #found by trial and error
set.seed(128900) #gives repeatable results in book; usually omitted
inpar <- log(c(r= 0.4,K=11000,Binit=3600,sigma=0.05))
result1 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,
                   calcdat=fish,obsdat=logce,priorcalc,scales)
inpar <- log(c(r= 0.35,K=8500,Binit=3400,sigma=0.05))
result2 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,
                   calcdat=fish,obsdat=logce,priorcalc,scales)
inpar <- log(c(r= 0.45,K=9500,Binit=3200,sigma=0.05))
result3 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,
                   calcdat=fish,obsdat=logce,priorcalc,scales)
burnin <- 50 # strictly a low thinning rate of 4; not enough
step <- 16 # 16 thinstep rate = 4 parameters x 4 = 16
N <- 10000 # 16 x 10000 = 160,000 steps + 50 burnin
inpar <- log(c(r= 0.4,K=9400,Binit=3400,sigma=0.05))
result4 <- do_MCMC(chains,burnin,N,step,inpar,negLL,calcpred=simpspm,
                   calcdat=fish,obsdat=logce,priorcalc,scales)
post1 <- result1[[1]][[1]]
post2 <- result2[[1]][[1]]
post3 <- result3[[1]][[1]]
postY <- result4[[1]][[1]]
cat("time = ",Sys.time() - begin,"\n")
cat("Accept = ",result4[[2]],"\n")

# time = 10.83922
# Accept = 0.3471241 0.3437158 0.354289 0.3826251

#first example and start of 3 initial chains for MCMC Fig. 6.15
parset(cex=0.85)
P <- 75 # the first 75 steps only start to explore parameter space
plot(postY[,"K"],postY[,"r"],type="p",cex=0.2,xlim=c(7000,13000),
     ylim=c(0.28,0.47),col=8,xlab="K",ylab="r",panel.first=grid())
lines(post2[1:P,"K"],post2[1:P,"r"],lwd=1,col=1)
points(post2[1:P,"K"],post2[1:P,"r"],pch=15,cex=1.0)
lines(post1[1:P,"K"],post1[1:P,"r"],lwd=1,col=1)
points(post1[1:P,"K"],post1[1:P,"r"],pch=1,cex=1.2,col=1)
lines(post3[1:P,"K"],post3[1:P,"r"],lwd=1,col=1)
points(post3[1:P,"K"],post3[1:P,"r"],pch=2,cex=1.2,col=1)
# FIGURE: --------------------------------------------
# The first 75 points in three separate MCMC chains starting
# from different origins (triangles, squares, circles). No burn-in was set for these
# short chains so records begin from the starting points. The grey dots are 10000
# points from a single fourth chain, with a 50-point burn-in and a thinning rate
# of four, giving an approximate idea of the stationary distribution towards
# which all chains should converge.


#pairs plot of parameters from the first MCMC Fig. 6.16
posterior <- result4[[1]][[1]]
msy <-posterior[,1]*posterior[,2]/4
pairs(cbind(posterior[,1:4],msy),pch=16,col=rgb(1,0,0,1/50),font=7)
# FIGURE: --------------------------------------------
# The relationship between the 10000 samples from the posterior
# distributions of the Schaefer model parameters and MSY. Normally one
# would use a much longer thinning step than 4 to characterize the posterior.
# Full colour in the plot comes from at least 50 points.

#plot the traces from the first MCMC example Fig. 6.17
posterior <- result4[[1]][[1]]
par(mfrow=c(4,2),mai=c(0.4,0.4,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.8, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
label <- colnames(posterior)
N <- dim(posterior)[1]
for (i in 1:4) {
  ymax <- getmax(posterior[,i]); ymin <- getmin(posterior[,i])
  plot(1:N,posterior[,i],type="l",lwd=1,ylim=c(ymin,ymax),
       panel.first=grid(),ylab=label[i],xlab="Step")
  plot(density(posterior[,i]),lwd=2,col=2,panel.first=grid(),main="")
}
# FIGURE: --------------------------------------------
# The traces for the four Schaefer model parameters along
# with the implied marginal distribution of each parameter. The obvious
# auto-correlation within traces should be improved if the thinning step were
# increased to 128, 256, or, as we shall see, much longer.

#Use acf to examine auto-correlation with thinstep = 16 Fig. 6.18
posterior <- result4[[1]][[1]]
label <- colnames(posterior)[1:4]
parset(plots=c(2,2),cex=0.85)
for (i in 1:4) auto <- acf(posterior[,i],type="correlation",lwd=2,
                           plot=TRUE,ylab=label[i],lag.max=20)
# FIGURE: --------------------------------------------
# The auto-correlation exhibited in the traces for the four Schaefer
# model parameters. This is with a thinning step of 16 = 4 for each of four
# parameters. Clearly a large increase is needed to remove the strong correlations
# that occur.

#setup MCMC with thinstep of 128 per parameter Fig 6.19
begin=gettime()
scales <- c(0.06,0.05,0.06,0.4)
inpar <- log(c(r= 0.4,K=9400,Binit=3400,sigma=0.05))
result <- do_MCMC(chains=1,burnin=100,N=1000,thinstep=512,inpar,
                  negLL,calcpred=simpspm,calcdat=fish,
                  obsdat=logce,calcprior,scales,schaefer=TRUE)
posterior <- result[[1]][[1]]
label <- colnames(posterior)[1:4]
parset(plots=c(2,2),cex=0.85)
for (i in 1:4) auto <- acf(posterior[,i],type="correlation",lwd=2,
                           plot=TRUE,ylab=label[i],lag.max=20)

# FIGURE: --------------------------------------------
# The auto-correlation exhibited in the traces for the four Schaefer
# model parameters when the thinning step is 512 = 4 x 128, which is 128
# times that used in the first auto-correlation diagram.

cat(gettime() - begin)
# 37.39104

# Marginal Distributions
# plot marginal distributions from the MCMC Fig. 6.20
dohist <- function(x,xlab) { # to save a little space
  return(hist(x,main="",breaks=50,col=0,xlab=xlab,ylab="",
              panel.first=grid()))
}
# ensure we have the optimum solution available
param <- log(c(r= 0.42,K=9400,Binit=3400,sigma=0.05))
bestmod <- nlm(f=negLL,p=param,funk=simpspm,indat=abdat,
               logobs=log(abdat$cpue))
optval <- exp(bestmod$estimate)
posterior <- result[[1]][[1]] #example above N=1000, thin=512
par(mfrow=c(5,1),mai=c(0.4,0.3,0.025,0.05),oma=c(0,1,0,0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
np <- length(param)
for (i in 1:np) { #store invisible output from hist for later use
  outH <- dohist(posterior[,i],xlab=colnames(posterior)[i])
  abline(v=optval[i],lwd=3,col=4)
  tmp <- density(posterior[,i])
  scaler <- sum(outH$counts)*(outH$mids[2]-outH$mids[1])
  tmp$y <- tmp$y * scaler
  lines(tmp,lwd=2,col=2)
}
msy <- posterior[,"r"]*posterior[,"K"]/4
mout <- dohist(msy,xlab="MSY")
tmp <- density(msy)
tmp$y <- tmp$y * (sum(mout$counts)*(mout$mids[2]-mout$mids[1]))
lines(tmp,lwd=2,col=2)
abline(v=(optval[1]*optval[2]/4),lwd=3,col=4)
mtext("Frequency",side=2,outer=T,line=0.0,font=7,cex=1.0)

# The Use of Rcpp
#profile the running of do_MCMC using the now well known abdat
data(abdat); logce <- log(abdat$cpue); fish <- as.matrix(abdat)
param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))
Rprof(append=TRUE) # note the use of negLL1()
result <- do_MCMC(chains=1,burnin=100,N=20000,thinstep=16,inpar=param,
                  infunk=negLL1,calcpred=simpspm,calcdat=fish,
                  obsdat=logce,priorcalc=calcprior,
                  scales=c(0.07,0.06,0.07,0.45))
Rprof(NULL)
outprof <- summaryRprof()

cat("The output from applying the Rprof() function to
the do_MCMC() function call, looking only at the by.self part of
the output list (ordered by time spent in each function), check
the structure of outprof. The total sampling time is in outprof$
sampling.time. The whole of self.pct sums to 99.99 so those
values are the ones to focus upon.
-------------------------------------------------------
                self.time self.pct  total.time  total.pct
“funk”          619.60    52.35     959.94      81.11
“[.data.frame”  75.22     6.36      155.92      13.17
“mean”          68.08     5.75      142.68      12.06
“do_MCMC”       55.74     4.71      1182.64     99.93
“max”           51.92     4.39      51.92       4.39
“infunk”        32.20     2.72      1094.86     92.51
“dnorm”         30.48     2.58      71.48       6.04
“which”         28.68     2.42      70.58       5.96
“names”         27.12     2.29      27.12       2.29
“[”             22.30     1.88      178.22      15.06
“tail”          18.20     1.54      41.00       3.46
“priorcalc”     18.12     1.53      21.72       1.84
")

# Addressing Vectors and Matrices
# Replacement for simpspm()
library(Rcpp)
#Send a text string containing the C++ code to cppFunction this will
#take a few seconds to compile, then the function simpspmC will
#continue to be available during the rest of your R session. The
#code in this chunk could be included into its own R file, and then
#the R source() function can be used to include the C++ into a
#session. indat must have catch in col2 (col1 in C++), and cpue in
#col3 (col2 in C++). Note the use of ; at the end of each line.
#Like simpspm(), this returns only the log(predicted cpue).
cppFunction('NumericVector simpspmC(NumericVector pars,
             NumericMatrix indat, LogicalVector schaefer) {
  int nyrs = indat.nrow();
  NumericVector predce(nyrs);
  NumericVector biom(nyrs+1);
  double Bt, qval;
  double sumq = 0.0;
  double p = 0.00000001;
  if (schaefer(0) == TRUE) {
  p = 1.0;
  }
  NumericVector ep = exp(pars);
  biom[0] = ep[2];
  for (int i = 0; i < nyrs; i++) {
  Bt = biom[i];
  biom[(i+1)]=Bt+(ep[0]/p)*Bt*(1-pow((Bt/ep[1]),p))-
  indat(i,1);
  if (biom[(i+1)] < 40.0) biom[(i+1)] = 40.0;
  sumq += log(indat(i,2)/biom[i]);
  }
  qval = exp(sumq/nyrs);
  for (int i = 0; i < nyrs; i++) {
  predce[i] = log(biom[i] * qval);
  }
  return predce;
}')


#Ensure results obtained from simpspm and simpspmC are same
library(microbenchmark)
data(abdat)
fishC <- as.matrix(abdat) # Use a matrix rather than a data.frame
inpar <- log(c(r= 0.389,K=9200,Binit=3300,sigma=0.05))
spmR <- exp(simpspm(inpar,fishC)) # demonstrate equivalence
#need to declare all arguments in simpspmC, no default values
spmC <- exp(simpspmC(inpar,fishC,schaefer=TRUE))
out <- microbenchmark( # verything identical calling function
  simpspm(inpar,fishC,schaefer=TRUE),
  simpspmC(inpar,fishC,schaefer=TRUE),
  times=1000
)
out2 <- summary(out)[,2:8]
out2 <- rbind(out2,out2[2,]/out2[1,])
rownames(out2) <- c("simpspm","simpspmC","TimeRatio")

cat("The predictions from simpspm() and simpspmC()
side-by-side to demonstrate that the code generates identical
answers from the parameters c(r= 0.389, K=9200, Binit=3300,
sigma=0.05).
-------------------------------------------
  spmR   spmC   spmR   spmC   spmR   spmC
1 1.1251 1.1251 1.4538 1.4538 2.1314 2.1314
2 1.0580 1.0580 1.5703 1.5703 2.0773 2.0773
3 1.0774 1.0774 1.7056 1.7056 2.0396 2.0396
4 1.0570 1.0570 1.8446 1.8446 1.9915 1.9915
5 1.0827 1.0827 1.9956 1.9956 1.9552 1.9552
6 1.1587 1.1587 2.0547 2.0547 1.9208 1.9208
7 1.2616 1.2616 2.1619 2.1619 1.8852 1.8852
8 1.3616 1.3616 2.2037 2.2037 1.8276 1.8276
")

#How much does using simpspmC in do_MCMC speed the run time?
#Assumes Rcpp code has run, eg source("Rcpp_functions.R")
set.seed(167423) #Can use getseed() to generate a suitable seed
beginR <- gettime() #to enable estimate of time taken
setscale <- c(0.07,0.06,0.07,0.45)
reps <- 2000 #Not enough but sufficient for demonstration
param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))
resultR <- do_MCMC(chains=1,burnin=100,N=reps,thinstep=128,
                   inpar=param,infunk=negLL1,calcpred=simpspm,
                   calcdat=fishC,obsdat=log(abdat$cpue),schaefer=TRUE,
                   priorcalc=calcprior,scales=setscale)
timeR <- gettime() - beginR
cat("time = ",timeR,"\n")
cat("acceptance rate = ",resultR$arate," \n")
postR <- resultR[[1]][[1]]
set.seed(167423) # Use the same pseudo-random numbers and the
beginC <- gettime() # same starting point to make the comparsion
param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))
resultC <- do_MCMC(chains=1,burnin=100,N=reps,thinstep=128,
                   inpar=param,infunk=negLL1,calcpred=simpspmC,
                   calcdat=fishC,obsdat=log(abdat$cpue),schaefer=TRUE,
                   priorcalc=calcprior,scales=setscale)
timeC <- gettime() - beginC
cat("time = ",timeC,"\n") # note the same acceptance rates
cat("acceptance rate = ",resultC$arate," \n")
postC <- resultC[[1]][[1]]
cat("Time Ratio = ",timeC/timeR)
# time = 18.37542----------------------------------------
# acceptance rate = 0.319021 0.3187083 0.3282664 0.368747
# time = 3.603301
# acceptance rate = 0.319021 0.3187083 0.3282664 0.368747
# Time Ratio = 0.1960935

#compare marginal distributions of the 2 chains Fig. 6.21
par(mfrow=c(1,1),mai=c(0.45,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
maxy <- getmax(c(density(postR[,"K"])$y,density(postC[,"K"])$y))
plot(density(postR[,"K"]),lwd=2,col=1,xlab="K",ylab="Density",
     main="",ylim=c(0,maxy),panel.first=grid())
lines(density(postC[,"K"]),lwd=3,col=5,lty=2)
# FIGURE: --------------------------------------------
# A comparison of the K parameter density distribution for the
# chains produced by the simpspm function (solid black line) and the simpspmC
# function (dashed blue line), with each chain having identical starting positions
# and the same random seed they lie on top of each other. Repeat these examples
# with different seeds, and or different starting positions to see the effect.

# Multiple Independent Chains
#run multiple = 3 chains
setscale <- c(0.07,0.06,0.07,0.45) # I only use a seed for
set.seed(9393074) # reproducibility within this book
reps <- 10000 # reset the timer
beginC <- gettime() # remember a thinstep=256 is insufficient
resultC <- do_MCMC(chains=3,burnin=100,N=reps,thinstep=256,
                   inpar=param,infunk=negLL1,calcpred=simpspmC,
                   calcdat=fishC,obsdat=log(fishC[,"cpue"]),
                   priorcalc=calcprior,scales=setscale,schaefer=TRUE)
cat("time = ",gettime() - beginC," secs \n")
# time = 105.4873 secs--------------------------

#3 chain run using simpspmC, 10000 reps, thinstep=256 Fig. 6.22
par(mfrow=c(2,2),mai=c(0.4,0.45,0.05,0.05),oma=c(0.0,0,0.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
label <- c("r","K","Binit","sigma")
for (i in 1:4) {
  plot(density(resultC$result[[2]][,i]),lwd=2,col=1,
       xlab=label[i],ylab="Density",main="",panel.first=grid())
  lines(density(resultC$result[[1]][,i]),lwd=2,col=2)
}
# FIGURE: --------------------------------------------  
# The variation between three chains in the marginal density
# distributions for the four Schaefer parameters using 10000 replicates at a thinning
# rate of 64 (4 × 64 = 256), and the simpspmC function. Slight differences
# are apparent where the line is wider than average.   
  
#generate summary stats from the 3 MCMC chains
av <- matrix(0,nrow=3,ncol=4,dimnames=list(1:3,label))
sig2 <- av # do the variance
relsig <- av # relative to mean of all chains
for (i in 1:3) {
  tmp <- resultC$result[[i]]
  av[i,] <- apply(tmp[,1:4],2,mean)
  sig2[i,] <- apply(tmp[,1:4],2,var)
}
cat("Average \n")
av
cat("\nVariance per chain \n")
sig2
cat("\n")
for (i in 1:4) relsig[,i] <- sig2[,i]/mean(sig2[,i])
cat("Variance Relative to Mean Variance of Chains \n")
relsig  
# Average---------------------------------------
# r K Binit sigma
# 1 0.3821707 9495.580 3522.163 0.04805695
# 2 0.3809524 9530.307 3537.186 0.04811021
# 3 0.3822318 9487.911 3522.021 0.04810015
#
# Variance per chain
# r K Binit sigma
# 1 0.0009018616 1060498.2 151208.8 6.264484e-05
# 2 0.0008855405 998083.0 142153.1 6.177037e-05
# 3 0.0009080043 978855.6 138585.3 6.288734e-05
#
# Variance Relative to Mean Variance of Chains
# r K Binit sigma
# 1 1.0037762 1.0474275 1.0501896 1.0033741
# 2 0.9856108 0.9857815 0.9872949 0.9893677
# 3 1.0106130 0.9667911 0.9625155 1.0072582 

#compare quantile from the 2 most widely separate MCMC chains
tmp <- resultC$result[[2]] # the 10000 values of each parameter
cat("Chain 2 \n")
msy1 <- tmp[,"r"]*tmp[,"K"]/4
ch1 <- apply(cbind(tmp[,1:4],msy1),2,quants)
round(ch1,4)
tmp <- resultC$result[[3]]
cat("Chain 3 \n")
msy2 <- tmp[,"r"]*tmp[,"K"]/4
ch2 <- apply(cbind(tmp[,1:4],msy2),2,quants)
round(ch2,4)
cat("Percent difference ") 
cat("\n2.5% ",round(100*(ch1[1,] - ch2[1,])/ch1[1,],4),"\n")
cat("50% ",round(100*(ch1[3,] - ch2[3,])/ch1[3,],4),"\n")
cat("97.5% ",round(100*(ch1[5,] - ch2[5,])/ch1[5,],4),"\n")
# Chain 2----------------------------------------
# r K Binit sigma msy1
# 2.5% 0.3206 7926.328 2942.254 0.0356 853.1769
# 5% 0.3317 8140.361 3016.340 0.0371 859.6908
# 50% 0.3812 9401.467 3489.550 0.0472 896.5765
# 95% 0.4287 11338.736 4214.664 0.0624 955.1773
# 97.5% 0.4386 11864.430 4425.248 0.0662 970.7137
# Chain 3
# r K Binit sigma msy2
# 2.5% 0.3225 7855.611 2920.531 0.0355 853.0855
# 5% 0.3324 8090.493 3001.489 0.0371 859.3665
# 50% 0.3826 9370.715 3475.401 0.0471 895.8488
# 95% 0.4316 11248.955 4188.052 0.0626 952.1486
# 97.5% 0.4416 11750.426 4376.639 0.0665 966.2832
# Percent difference
# 2.5% -0.6006 0.8922 0.7383 0.4636 0.0107
# 50% -0.3871 0.3271 0.4055 0.2278 0.0812
# 97.5% -0.6817 0.9609 1.0985 -0.5278 0.4564

# Replicates Required to Avoid Serial Correlation

#compare two higher thinning rates per parameter in MCMC
param <- log(c(r=0.39,K=9200,Binit=3400,sigma=0.05))
setscale <- c(0.07,0.06,0.07,0.45)
result1 <- do_MCMC(chains=1,burnin=100,N=2000,thinstep=1024,
                   inpar=param,infunk=negLL1,calcpred=simpspmC,
                   calcdat=fishC,obsdat=log(abdat$cpue),
                   priorcalc=calcprior,scales=setscale,schaefer=TRUE)
result2 <- do_MCMC(chains=1,burnin=50,N=1000,thinstep=2048,
                   inpar=param,infunk=negLL1,calcpred=simpspmC,
                   calcdat=fishC,obsdat=log(abdat$cpue),
                   priorcalc=calcprior,scales=setscale,schaefer=TRUE)

#autocorrelation of 2 different thinning rate chains Fig. 6.23
posterior1 <- result1$result[[1]]
posterior2 <- result2$result[[1]]
label <- colnames(posterior1)[1:4]
par(mfrow=c(4,2),mai=c(0.25,0.45,0.05,0.05),oma=c(1.0,0,1.0,0.0))
par(cex=0.85, mgp=c(1.35,0.35,0), font.axis=7,font=7,font.lab=7)
for (i in 1:4) {
  auto <- acf(posterior1[,i],type="correlation",plot=TRUE,
              ylab=label[i],lag.max=20,xlab="",ylim=c(0,0.3),lwd=2)
  if (i == 1) mtext(1024,side=3,line=-0.1,outer=FALSE,cex=1.2)
  auto <- acf(posterior2[,i],type="correlation",plot=TRUE,
              ylab=label[i],lag.max=20,xlab="",ylim=c(0,0.3),lwd=2)
  if (i == 1) mtext(2048,side=3,line=-0.1,outer=FALSE,cex=1.2)
}
mtext("Lag",side=1,line=-0.1,outer=TRUE,cex=1.2)

# FIGURE: --------------------------------------------
# The auto-correlation of two chains across the four parameters
# of the Schaefer model with combined thinning rates of 1024 and 2048. Note
# the reduced maximum on the y-axis to make the differences between the two
# more apparent.

#visual comparison of 2 chains marginal densities Fig. 6.24
parset(plots=c(2,2),cex=0.85)
label <- c("r","K","Binit","sigma")
for (i in 1:4) {
  plot(density(result1$result[[1]][,i]),lwd=4,col=1,xlab=label[i],
       ylab="Density",main="",panel.first=grid())
  lines(density(result2$result[[1]][,i]),lwd=2,col=5,lty=2)
}
# FIGURE: --------------------------------------------
# The variation between two chains in the marginal density
# distributions for the K parameter using 1000 and 2000 replicates at thinning
# rates of 2048 (dashed line) and 1024 (solid black line).

#tablulate a summary of the two different thinning rates.
cat("1024 thinning rate \n")
posterior <- result1$result[[1]]
msy <-posterior[,1]*posterior[,2]/4
tmp1 <- apply(cbind(posterior[,1:4],msy),2,quants)
rge <- apply(cbind(posterior[,1:4],msy),2,range)
tmp1 <- rbind(tmp1,rge[2,] - rge[1,])
rownames(tmp1)[6] <- "Range"
print(round(tmp1,4))
posterior2 <- result2$result[[1]]
msy2 <-posterior2[,1]*posterior2[,2]/4
cat("2048 thinning rate \n")
tmp2 <- apply(cbind(posterior2[,1:4],msy2),2,quants)
rge2 <- apply(cbind(posterior2[,1:4],msy2),2,range)
tmp2 <- rbind(tmp2,rge2[2,] - rge2[1,])
rownames(tmp2)[6] <- "Range"
print(round(tmp2,4))
cat("Inner 95% ranges and Differences between total ranges \n")
cat("95% 1 ",round((tmp1[5,] - tmp1[1,]),4),"\n")
cat("95% 2 ",round((tmp2[5,] - tmp2[1,]),4),"\n")
cat("Diff ",round((tmp2[6,] - tmp1[6,]),4),"\n")
# 1024 thinning rate-----------------------------
# r K Binit sigma msy
# 2.5% 0.3221 7918.242 2943.076 0.0352 853.5243
# 5% 0.3329 8139.645 3016.189 0.0367 858.8872
# 50% 0.3801 9429.118 3499.826 0.0470 895.7376
# 95% 0.4289 11235.643 4172.932 0.0627 953.9948
# 97.5% 0.4392 11807.732 4380.758 0.0663 973.2185
# Range 0.2213 7621.901 2859.858 0.0612 238.5436
# 2048 thinning rate
# r K Binit sigma msy2
# 2.5% 0.3216 7852.002 2920.198 0.0351 855.8295
# 5% 0.3329 8063.878 3000.767 0.0368 859.8039
# 50% 0.3820 9400.708 3482.155 0.0468 896.6774
# 95% 0.4313 11235.368 4184.577 0.0628 959.2919
# 97.5% 0.4434 11638.489 4456.164 0.0676 975.4358
# Range 0.2189 8156.444 3161.232 0.0546 257.1803
# Inner 95% ranges and Differences between total ranges
# 95% 1 0.1172 3889.49 1437.682 0.0311 119.6942
# 95% 2 0.1218 3786.487 1535.966 0.0325 119.6064
# Diff -0.0024 534.5429 301.3746 -0.0066 18.6367




