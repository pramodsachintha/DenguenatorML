require("sfsmisc")
require("Metrics")

temp_func = approxfun(c(1:52), tempCsv[138,][3:54])

colomboTemperature = data.frame(day = c(1:365), temperature = temp_func(seq(1,53,1/7)))
colomboTemperature$temperature[is.na(colomboTemperature$temperature)] <- colomboTemperature$temperature[358]


###########################################################

require("deSolve")

validate = function(x) ifelse(x<0, 0, x)

shiHV = function(temperature) {
  temperature[temperature > 32.461] = 32.461
  0.001044*temperature*(temperature - 12.286)*sqrt(32.461 - temperature)
}

shiVH = function(temperature) {
  answer = 0.0729*temperature - 0.97
  answer[answer < 0] = 0
  answer[answer > 1] = 1
  return(answer)
}

thetahA = function(temperature) {
  answer = -5.4 + 1.8*temperature - 0.2124*temperature^2 + 0.01015*temperature^3 - 0.0001515*temperature^4 
  ifelse(answer < 0, 0, answer)
}

epsilonAV = function(temperature) {
  answer = 0.1310 - 0.05723*temperature + 0.01164*temperature^2 -0.001341*temperature^3 + 0.00008723*temperature^4 - 3.017*10^(-6)*temperature^5 + 5.153*10^(-8)*temperature^6 - 3.420*10^(-10)*temperature^7
  answer
}

muAV = function(temperature) {
  answer = 2.13 - 0.3787*temperature + 0.02457*temperature^2 - 0.0006778*temperature^3 + 0.000006794*temperature^4
  ifelse(answer < 0, 0, answer)
}

muVV = function(temperature) { 
  answer = 0.8692 - 0.1590*temperature + 0.01116*temperature^2 - 0.0003408*temperature^3 + 0.000003809*temperature^4
  #ifelse(answer < 0, 0, answer)
  answer
}

gammaVV = function(temperature) {
  Tk = temperature + 273.15
  R = 1.9872036 #cal deg-1 mol-1
  answer = ((0.003359*Tk/298) * exp((1500/R)*(1/298-1/Tk)))/(1 + exp((6.203*10^21/R) * (1/(-2.176*10^30) - 1/Tk)))
  #ifelse(answer < 0, 0, answer)
  answer
}

q = function(temperature) 
  epsilonAV(temperature)/(epsilonAV(temperature) + muAV(temperature)) * thetahA(temperature)/muVV(temperature)

a = function(temperature) {
  answer = K*(1 - 1/q(temperature))
  ifelse(answer < 0, 0, answer)
}

v = function(temperature) {
  answer = K*(1-1/q(temperature)) * epsilonAV(temperature)/muVV(temperature)
  ifelse(answer < 0, 0, answer)
}

count = 1
incr = function() count <<- count+1

SEIRfunc=function(t, x, vparameters){
  Sh = x[1]
  Eh = x[2]
  Ih = x[3]
  Rh = x[4]
  
  A = x[5]
  Sv = x[6]
  Ev = x[7]
  Iv = x[8]
  
  A = validate(A)
  Ih = validate(Ih)
  Iv = validate(Iv)
  Eh = validate(Eh)
  Ev = validate(Ev)
  
#  count = t+1
   with(as.list(vparameters),{
    npop = Sh+Eh+Ih+Rh   
    V = Sv+Ev+Iv
    incr()
    dSh = -bittingRate*shiVH(colomboTemperature$temperature[count])*Sh*Iv/npop            
    dEh = +bittingRate*shiVH(colomboTemperature$temperature[count])*Sh*Iv/npop - gammaH*Eh
    dIh = +gammaH*Eh - sigmaH*Ih  
    dRh = +sigmaH*Ih                 
    
    dA = thetahA(1 - A/K)*V - (epsilonAV(colomboTemperature$temperature[count]) + muAV(colomboTemperature$temperature[count]))*A
    dSv = epsilonAV(colomboTemperature$temperature[count])*A - bittingRate*shiHV(colomboTemperature$temperature[count])*Ih*Sv/npop - muVV(colomboTemperature$temperature[count])*Sv
    dEv = +bittingRate*shiHV(colomboTemperature$temperature[count])*Ih*Sv/npop - muVV(colomboTemperature$temperature[count])*Ev - gammaVV(colomboTemperature$temperature[count])*Ev
    dIv = +gammaVV(colomboTemperature$temperature[count])*Ev - muVV(colomboTemperature$temperature[count])*Iv
    
    d = dRh + dIh
    
    out = c(dSh,dEh,dIh,dRh, dA,dSv,dEv,dIv,d)
    list(out)
  })
}


##########################################################



###############################################################
##     Initialize the compartments

c = 0
index = 0
ev = 0
iv = 0

vt = seq(0,6,1)  # let's determine the values of S,I and R at times in vt
K = 10^5
bittingRate = 0.35
gammaH = 0.5
sigmaH = 1/4

for (index in c(1:52)) {
  
  if(index == 1) {
    Eh_0 = 0    
    Rh_0 = 0
    Ih_0 = 0
    Sh_0 = npop
    
    A_0 = a(colomboTemperature$temperature[1])
    V = v(temperature = colomboTemperature$temperature[1])
    Iv_0 = 1000
    Ev_0 = 1000
  }
  
  if(index != 1) {
    Eh_0 = sirmodel$Eh[7]    
    Rh_0 = sirmodel$Rh[7]
    Ih_0 = currentMOH[,2+index-1]
    Sh_0 = npop-(Ih_0 + Eh_0 + Rh_0)
    
    A_0 = a(colomboTemperature$temperature[(index-1)*7])
    V = v(temperature = colomboTemperature$temperature[(index-1)*7])
    Iv_0 = sirmodel$Iv[7]
    Ev_0 = sirmodel$Ev[7]
  }
  
  npop = 560000

  #Sh_0 = npop-(Ih_0 + Eh_0 + Rh_0)
         
  Sv_0 = V - (Iv_0 + Ev_0)
  ev = as.array(ev, Ev_0)
  iv = as.array(iv, Iv_0)
    
  vparameters = c(gammaH=gammaH, sigmaH = sigmaH, bittingRate = bittingRate, K = K)
  inits = c(Sh=Sh_0, Eh=Eh_0, Ih=Ih_0, Rh=Rh_0, A = A_0, Sv=Sv_0, Ev=Ev_0, Iv=Iv_0, d=0)
  
  sirmodel = as.data.frame(euler(inits, vt, SEIRfunc, vparameters))
  #c[index+1] = ifelse(sirmodel$Ih[7] > 200, 70, sirmodel$Ih[7])
  c[index] = sirmodel$Ih[7]
}

plot(x = c(1:52), y = actual, type = "l", xlab="Week",ylab="Total incidences",lwd=3,col=4,main="Actual and predicted dengue incidences")
#lines(c(1:52),c,type="l",lwd=3,col=2)
legend("topleft",legend=c("Actual incidences","Predicted incidences"),bty="n",lwd=3,col=c(4,2))
par(new = T)
plot(c(1:52),c, xaxt="n",yaxt="n", xlab = "", ylab = "", type="l",lwd=3,col=2)
axis(4)

#plot diagrams
par(mfrow=c(2,2))
mult.fig(1,main="SEIR model of Dengue")

plot(sirmodel$time,sirmodel$Ih,type="l",xlab="time",ylab="Total infected",lwd=3,col=4,main="Infected")
n=length(sirmodel$time)
lines(sirmodel$time[2:n],diff(sirmodel$d),type="l",lwd=3,col=2)
lines(sirmodel$time[1:n],sirmodel$Eh,type="l",lwd=3,col=2)
legend("topright",legend=c("total infected (prevalence)","newly infected/day (incidence)"),bty="n",lwd=3,col=c(4,2))



#plot parameters
mfrow = c(1,2)
par(mfrow=mfrow)                                             
#mult.fig(mfrow = mfrow,main="Temperature based Entomological parameters")

#plot(x = seq(10,33,0.01), y = muVV(seq(10,33,0.01)))
#plot(x = seq(10,35,0.01), y = thetahA(seq(10,35,0.01)))
#plot(x = seq(10,40,0.01), y = muAV(seq(10,40,0.01)))
#plot(x = seq(10,40,0.01), y = epsilonAV(seq(10,40,0.01)))
plot(x = seq(28,34,0.01), y = 1/gammaVV(seq(28,34,0.01)))
plot(x = seq(10,35,0.01), y = shiHV(seq(10,35,0.01)))
plot(x = seq(10,35,0.01), y = shiVH(seq(10,35,0.01)))


#plot parameters
mfrow = c(1,1)
par(mfrow=mfrow)                                             
mult.fig(mfrow = mfrow,main="Temperature based Entomological parameters")

plot(c(1:365), shiHV(colomboTemperature$temperature))
plot(c(1:365), shiVH(colomboTemperature$temperature))
plot(c(1:365), thetahA(colomboTemperature$temperature))
plot(c(1:365), epsilonAV(colomboTemperature$temperature))
plot(c(1:365), muAV(colomboTemperature$temperature))
plot(c(1:365), muVV(colomboTemperature$temperature))
plot(c(1:365), gammaVV(colomboTemperature$temperature))



plot(x = c(1:52), y = currentMOH[3:54], lwd=3, xlab="Week",ylab="Total Infected", type = "l", col = "red", main = "Dengue 2013")
par(new = T)
plot(x = c(1:52), y = tempCsv[138,][3:54], xaxt="n",yaxt="n", xlab = "", ylab = "", lwd=3, type = "l", col = "blue")
axis(4)

