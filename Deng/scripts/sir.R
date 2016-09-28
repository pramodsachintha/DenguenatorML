##################################################################################
# An R script to solve ODE's of a Susceptible Infected Recovered (SIR) model 
# http://www.sherrytowers.com/sir.R
#
# Author: Sherry Towers
#         admin@sherrytowers.com
# Created: Dec 1st, 2012
#
# Copyright Sherry Towers, 2012
#
# This script is not guaranteed to be free of bugs and/or errors.
#
# This script can be freely used and shared as long as the author and
# copyright information in this header remain intact.
#
##################################################################################

##################################################################################
#
# You can edit script files (for example, this file)
# and either cut and paste lines from file into R command line
# (in Windows you can use ctrl-R to do this)
# or in the R command line type:
#
#    source("sir.R")
#
# You may need to use full path name in the filename, or alternatively in the R console
# window change to the directory containing the file sir.R by using the command:
#
#    setwd("<path of your directory>") 
#
# you will also need to have downloaded the file sir_func.R into that directory
# from http://www.sherrytowers.com/sir_func.R
##################################################################################
require("sfsmisc")
source("scripts/sir_func.R")  # this file contains a function SIRfunc that calculates
                      # the derivatives of S I and R wrt to time

##################################################################################
##################################################################################
# Let's set up some initial conditions at time t=0
##################################################################################
npop = 500000
Ih_0 = 2       # put one infected person in the population
Eh_0 = 2
Sh_0 = npop-(Ih_0 + Eh_0)
Rh_0 = 0       # initially no one has recovered yet

nvpop = 3*npop
Iv_0 = 1000
Ev_0 = 1000
Sv_0 = nvpop - (Iv_0 + Ev_0)
##################################################################################
# now the parameters of the model.  Note that in some posts on sherrytowers.com
# I refer to the recovery rate as k (here it is gamma), and the transmission
# rate as b (here is is beta).
##################################################################################
vt = seq(0,365,1)  # let's determine the values of S,I and R at times in vt
betav = 0.25
betah = 0.23
gamma = 1/5.5
delta = 0.25
kappa = 1/5.5
muV = 1/10.5


calculate <- function() {
  vparameters = c(gamma=gamma,betav=betav,betah=betah,delta=delta, kappa=kappa, muV=muV)
  inits = c(Sh=Sh_0, Eh=Eh_0, Ih=Ih_0, Rh=Rh_0, Sv=Sv_0, Ev=Ev_0, Iv=Iv_0, d=0)
  
  sirmodel = as.data.frame(lsoda(inits, vt, SIRfunc, vparameters))
  cat("The item names in the sirmodel object are:",names(sirmodel),"\n")
  
  ##################################################################################
  # now let's plot the results
  ##################################################################################
  par(mfrow=c(2,2))                                              # divides the plot area into two up and two down
  mult.fig(1,main="SIR model of Dengue") # this is from the sfsmisc package that also
                                                                 # divides into two up and two down, *and*
                                                                 # prints a title at the top
  
  plot(sirmodel$time,sirmodel$Ih,type="l",xlab="time",ylab="Total infected",lwd=3,col=4,main="Infected")
  n=length(sirmodel$time)
  lines(sirmodel$time[2:n],diff(sirmodel$d),type="l",lwd=3,col=2)
  lines(sirmodel$time[1:n],sirmodel$Eh,type="l",lwd=3,col=2)
  legend("topright",legend=c("total infected (prevalence)","newly infected/day (incidence)"),bty="n",lwd=3,col=c(4,2))
}

##################################################################################
# output the final size
# first solve for the final fraction of suscetibles at time=infinity
# using the final size relationship in 
# www.fields.utoronto.ca/programs/scientific/10-11/drugresistance/emergence/fred1.pdf 
##################################################################################
dsinf = 0.00001
vsinf = seq(0,1-dsinf,dsinf)
sinf_predicted = vsinf[which.min(abs(R0*(1-vsinf)+log(vsinf)-log(S_0/npop)))]  # numerically solve -log(sinf) = R0*(1-sinf)
cat("The final fraction of susceptibles at the end of the epidemic from the model simulation is ",min(sirmodel$S)/npop,"\n")
cat("The final fraction of susceptibles at the end of the epidemic predicted by the final size relation is ",sinf_predicted,"\n")


calculate()

