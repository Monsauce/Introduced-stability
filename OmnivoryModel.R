library(deSolve)

#model 
Omnivory <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dR = R*r*(1-R/K)-(acr*R/(1+bcr*R))*C-(1-W)*(apr*R/(1+bpr*R+bpc*C))*P
    dC = (acr*R/(1+bcr*R))*C-W*(apc*C/(1+bpr*R+bpc*C))*P-mc*C
    dP = (1-W)*(apr*R/(1+bpr*R+bpc*C))*P+W*(apc*C/(1+bpr*R+bpc*C))*P-mp*P
    return(list(c(dR, dC, dP)))
  })
}

#Parameter Set 1 (equilibrium)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.79188, mp=0.28516, bcr=5, bpr=0.5, bpc=2, r=2.5)
State <- c(R = 0.5, C = 0.5, P=0.5)
Time <- seq(0, 1000, by = 1)

#Parameter Set 2 (limit cycles)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=2.5)
State <- c(R = 0.5, C = 0.5, P=0.5)
Time <- seq(0, 1000, by = 1)

#Parameter Set 3 (chaos)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=2.5)
State <- c(R = 0.5, C = 0.5, P=0.5)
Time <- seq(0, 1000, by = 1)

#plot time series with parameter sets
out <- as.data.frame(ode(func = Omnivory, y = State, parms = Pars, times = Time))

#function to get the peaks for predator over parameter range 
peaks <- function(x, method="max") {
  l <- length(x)
  xm1 <- c(x[-1], x[l])
  xp1 <- c(x[1], x[-l])
  if (method=="both") {
    p <- x[x > xm1 & x > xp1 | x < xm1 & x < xp1]
  } else {
    p <- x[x > xm1 & x > xp1]
  }
  p
}

#make empty data frames for loop 
maxpr<-NULL
pikpr<-NULL


####Limit cycles Predator####
#make bifurcation plot over parameter range of w (from 0 to 0.5) using Parameter Sets
plot(0,0, xlim=c(0, 0.5), ylim=c(0,0.7), type="n", xlab="Strength of omnivory", ylab="Maxima of Predator")
for (w in seq(0,0.5,0.01)) {
  Pars["W"] <- w
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
#Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$P) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$P))
  
  pikpr <- peaks((outpr$P), method="max")
  
  l <- length(out)
  
  points(rep(w, length(pikpr)), pikpr, pch=".", col="black")
}

####Limit cycles Consumer####
#make bifurcation plot over parameter range of w (from 0 to 0.5) using Parameter Sets
plot(0,0, xlim=c(0, 0.5), ylim=c(0,0.7), type="n", xlab="Strength of omnivory", ylab="Maxima of Predator")
for (w in seq(0,0.5,0.01)) {
  Pars["W"] <- w
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$P))
  
  pikpr <- peaks((outpr$C), method="max")
  
  l <- length(out)
  #State <- c(R=out$R[l], C=out$C[l], P=out$P[l])
  
  points(rep(w, length(pikpr)), pikpr, pch=".", col="black", cex=2)
}

