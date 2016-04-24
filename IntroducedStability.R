#Code for "Introduced omnivores can increase stability in recipient food webs"

#required packages
library(deSolve)

####Omnivory model#### 
Omnivory <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dR = R*r*(1-R/K)-(acr*R/(1+bcr*R))*C-(1-W)*(apr*R/(1+bpr*R+bpc*C))*P
    dC = e*(acr*R/(1+bcr*R))*C-W*(apc*C/(1+bpr*R+bpc*C))*P-mc*C
    dP = (1-W)*e*(apr*R/(1+bpr*R+bpc*C))*P+W*e*(apc*C/(1+bpr*R+bpc*C))*P-mp*P
    return(list(c(dR, dC, dP)))
  })
}

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

#Parameters from Fussman & Herber (2002)
Time <- seq(0, 1000, by = 1)

####limit cycles####
#set plot area for 3 rows and 2 columns of plots
par(mfrow=c(4,1))

#make empty data frames for loop 
maxpr<-NULL
pikpr<-NULL

#make bifurcation plot over parameter range of acr () for minima of consumer when predator is absent 
State <- c(R = 0.5, C = 0.5, P=0)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.5, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of Consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of consumer when omnivory is weak 
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.7, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of consumer when omnivory is intermediate 
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.5, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of consumer when omnivory is intermediate 
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.3, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#set plot area for 3 rows and 2 columns of plots
par(mfrow=c(4,1))

#make empty data frames for loop 
maxpr<-NULL
pikpr<-NULL

#make bifurcation plot over parameter range of acr () for minima of resource when predator is absent 
State <- c(R = 0.5, C = 0.5, P=0)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.5, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of resource when omnivory is weak  
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.7, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of resource when omnivory is intermediate 
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.5, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of resource when omnivory is strong
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.84617, mp=0.20586, bcr=5, bpr=0.5, bpc=2, r=4, W=0.3, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

####chaos####

#set plot area for 3 rows and 2 columns of plots
par(mfrow=c(4,1))

#make empty data frames for loop 
maxpr<-NULL
pikpr<-NULL

#make bifurcation plot over parameter range of acr () for minima of consumer when predator is absent 
State <- c(R = 0.5, C = 0.5, P=0)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, e=1, W=0.5)
plot(0,0, xlim=c(6, 9), ylim=c(0,0.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}


#make bifurcation plot over parameter range of acr () for minima of resource when omnivory is weak  
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, e=1, W=0.7)
plot(0,0, xlim=c(6, 9), ylim=c(0,0.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of consumer omnivory is intermediate  
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, e=1, W=0.5)
plot(0,0, xlim=c(6, 9), ylim=c(0,0.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of consumer when omnivory is strong  
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, e=1, W=0.3)
plot(0,0, xlim=c(6, 9), ylim=c(0,0.5), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of consumer")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$C) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$C))
  
  pikpr <- -peaks(-outpr$C, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#set plot area for 3 rows and 2 columns of plots
par(mfrow=c(4,1))

#make empty data frames for loop 
maxpr<-NULL
pikpr<-NULL

#make bifurcation plot over parameter range of acr () for minima of resource when predator is absent 
State <- c(R = 0.5, C = 0.5, P=0)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, W=0.5, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of resource when omnivory is weak  
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, W=0.7, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of resource when predator is intermediate 
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, W=0.5, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

#make bifurcation plot over parameter range of acr () for minima of resource when omnivory is strong
State <- c(R = 0.5, C = 0.5, P=0.5)
Pars <- c(K=1, acr=7.5, apr=0.5, apc=2, mc=0.62898, mp=0.21953, bcr=5, bpr=0.5, bpc=2, r=4, W=0.3, e=1)
plot(0,0, xlim=c(6, 9), ylim=c(0,1), type="n", xlab="acr (attack rate of consumer on resource)", ylab="Minima of resource")
for (acr in seq(6,9,0.1)) {
  Pars["acr"] <- acr
  out <- as.data.frame(lsoda(State, Time, Omnivory, Pars))
  
  #Remove first three fourths of time series and consider last fourth for finding the peaks
  
  lpr <- length(out$R) %/% 4
  
  outpr <- out[(3*lpr):(4*lpr),]
  
  maxpr <- c(maxpr, max(outpr$R))
  
  pikpr <- -peaks(-outpr$R, method="max")
  
  l <- length(out)
  
  points(rep(acr, length(pikpr)), pikpr, pch=".", col="black", cex=4)
}

