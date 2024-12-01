# IDW for data given to you. Change the location of the data according to the location in your drive
sample<- read.table('/Users/suryadurbha/rdata/gnr605/sample.txt',header=T)
# and convert to sp object
spat.samp <- sample
coordinates(spat.samp) <- c('x','y')
# construct a grid of locations to predict at
grid <- expand.grid(x=seq(0,1,0.05), y=seq(0,1,0.05))
plot(grid)
spat.grid <- grid
# convert grid to a SpatialPoints object
coordinates(spat.grid) <- c('x','y')
# and tell sp that this is a grid
gridded(spat.grid) <- T
library(gstat)
sampinterp.idw <- idw(z~1, spat.samp, spat.grid)
# first argument is a formula.  
#   left hand side is the response					 variable
#   right hand side specifies the trend model variables
#   for idw, r.h.s. MUST be ~1, i.e. constant mean
# second is the spatial data set from which to get the obs.
# third is the set of locations at which to predict

# result is an spatial object with coordinates and two data columns:
#  var1.pred: the predictions for variable 1 (z here)
#  var1.var: the prediction variance.  Not computed for idw

sampinterp.idw@data$var1.pred
# or, if predicting on a grid, can use bubble or spplot to plot the gridded values
spplot(sampinterp.idw, 'var1.pred')
# default power is 2, can change by specifying idp in call to idw
sampinterp1.idw1 <- idw(z~1,spat.samp, spat.grid, idp=1)

spplot(sampinterp1.idw1, 'var1.pred')
# compare to previous spplot plor


# Trend surface
sample.lm <- lm(z ~ x + y, data=spat.samp)
sample.lmq <- lm(z ~ x + y + I(x^2) + I(y^2) + I(x*y), data=spat.samp)
sample.ts <- predict(sample.lm, newdata=spat.grid)
sample.tsq <- predict(sample.lmq, newdata=spat.grid)
spat.grid<- SpatialPixelsDataFrame(spat.grid, data.frame(ts=sample.ts, tsq=sample.tsq) )
spplot(spat.grid, 'ts')
spplot(spat.grid, 'tsq')


# variogram for example data available in R (This is given as a parctice exercise on an example data)
data(coalash)
names(coalash)
attach(coalash)
variog1 <- variogram(coalash~1, locations=~x+y, data=coalash)
variog2 <- variogram(coalash~1, locations=~x+y, 
boundaries=c(0,1,2,3,4,5,6,7,8,9), data=coalash)
variog1 <- variogram(coalash~1, locations=~x+y, Cressie=TRUE, 
data=coalash)
plot(variog1)
model.variog <- vgm(psill=1, model="Exp", nugget=1, range=60)
fit.variog <- fit.variogram(variog1, model.variog)
plot(variog1, model=fit.variog)

# kriging, Variogram for data given to you. Change the location of the data according to the location in your drive
rain<- read.table('/Users/suryadurbha/rdata/gnr605/rain.txt',header=T)
variog1<- variogram(z~1, locations=~x+y, data=rain)
variog1 <- variogram(z~1, locations=~x+y, Cressie=TRUE, 
data=rain)
variog2 <- variogram(z~1, locations=~x+y, 
boundaries=c(0,1,2,3,4,5,6,7,8,9), data=rain)
plot(variog1)
model.variog <- vgm(psill=4000, model="Sph", nugget=2000, range=80000)
fit.variog <- fit.variogram(variog1, model.variog)
plot(variog1, model=fit.variog)


