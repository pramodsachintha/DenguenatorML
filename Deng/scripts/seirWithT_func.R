require("deSolve")

validate = function(x) ifelse(x<0, 0, x)

shiHV = function(temperature) ifelse(temperature > 32.461, 0, 
                                     0.001044*temperature*(temperature - 12.286)*sqrt(32.461 - temperature))
shiVH = function(temperature) {
  answer = 0.0729*temperature - 0.97
  if(answer < 0) return(0)
  if(answer > 1) return(1)
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
  answer = ((0.0033589*Tk)/298 * exp((1500/R)*(1/298-1/Tk)))/(1 + exp((6.203*10^21)/R * (1/(-2.176*10^30) - 1/Tk)))
  ifelse(answer < 0, 0, answer)
}

q = function(temperature) 
  epsilonAV(temperature)/(epsilonAV(temperature) + muAV(temperature)) * thetahA(temperature)/muVV(temperature)

a = function(temperature) {
  answer = kappa*(1 - 1/q(temperature))
  ifelse(answer < 0, 0, answer)
}

v = function(temperature) {
  answer = kappa*(1-1/q(temperature)) * epsilonAV(temperature)/muVV(temperature)
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
  
  count = t+1
  with(as.list(vparameters),{
    npop = Sh+Eh+Ih+Rh   
    V = Sv+Ev+Iv
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
