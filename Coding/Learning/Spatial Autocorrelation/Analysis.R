#Autocorrelation


########################################################
#Temporal Autocorrelation
########################################################
set.seed(0)

#  autocorrelation between random values (weak)
d <- sample(100,10)
d
## d =  14  68  39   1  34  87  43 100  82  59
a <- d[-length(d)]
## a =  14  68  39   1  34  87  43 100  82
b <- d[-1]
## b =  68  39   1  34  87  43 100  82  59
plot(a,b)
cor(a,b)
#  0.1227634


#  autocorrelation between sorted random values (strong)
sort_d <- sort(d)
sort_a <- sort_d[-length(sort_d)]
sort_b <- sort_d[-1]
plot(sort_a, sort_b)
cor(sort_a, sort_b)
#  0.98192258


#  autocorrelation for several different lags (shift lengths)
#  above dotted blue line = reject H0 that correlation is 0
par(mfrow=c(1,1))
acf(d)
acf(sort_d)


########################################################
#Spatial Autocorrelation (manual)
########################################################
library(raster)
#  raster comes with datasets
p <- shapefile(system.file("external/lux.shp", package = "raster"))
#  include rows where NAME_1 == "Diekrich"
p_Diekirch <- p[p$NAME_1=="Diekirch",]
#  create new column called value
p_Diekirch$value <- c(10, 8, 4, 11, 6)
p_Diekirch$value <- c(80, 90, 65, 100, 70)
data.frame(p_Diekirch)


#  Let's investigate spatial autocorrelation for value
#  "Do adjacent regions affect each other?
#  We will go through the process manually first
par(mai=c(0,0,0,0))
plot(p_Diekirch, col=2:7)
xy <- coordinates(p_Diekirch)
points(xy, cex=6, pch=20, col='white')
text(p_Diekirch, 'ID_2', cex=1.5)


#  Now we define what polygons are "near"
#  We'll use adjacency as a criterion
library(spdep)
w <- poly2nb(p_Diekirch, row.names = p_Diekirch$Id)
class(w)
summary(w)
# Neighbour list object:
# Number of regions: 5 
# Number of nonzero links: 14 
# Percentage nonzero weights: 56 
# Average number of links: 2.8 
# Link number distribution:
#   
# 2 3 4 
# 2 2 1 
# 2 least connected regions:
# 3 4 with 2 links
# 1 most connected region:
# 2 with 4 links
str(w)
# List of 5
# $ : int [1:3] 2 4 5
# $ : int [1:4] 1 3 4 5
# $ : int [1:2] 2 5
# $ : int [1:2] 1 2
# $ : int [1:3] 1 2 3
# - attr(*, "class")= chr "nb"
# - attr(*, "region.id")= chr [1:5] "1" "2" "3" "4" ...
# - attr(*, "call")= language poly2nb(pl = p_Diekirch, row.names = p_Diekirch$Id)
# - attr(*, "type")= chr "queen"
# - attr(*, "sym")= logi TRUE


#  Try to decipher the meaning of the above data


#  Now let's plot the links between polygons
plot(p_Diekirch, col='gray', border='blue', lwd=2)
plot(w, xy, col='red', lwd=2, add=TRUE)


#  Transform w into spatial weights matrix
#  Since we are only considering adjacency, w will be a simple binary matrix
wm <- nb2mat(w, style='B')
wm
# [,1] [,2] [,3] [,4] [,5]
# 1    0    1    0    1    1
# 2    1    0    1    1    1
# 3    0    1    0    0    1
# 4    1    1    0    0    0
# 5    1    1    1    0    0
# attr(,"call")
# nb2mat(neighbours = w, style = "B")


#  Now we can compute Moran's I (statistic measuring spatial correlation)
n <- length(p_Diekirch)
y <- p_Diekirch$value
ybar <- mean(y)
dy <- y - ybar
g <- expand.grid(dy, dy)
yiyj <- g[,1] * g[,2]
pm <- matrix(yiyj, ncol=n)
#  NOTE THIS IS NOT MATRIX MULTIPLICATION
pmw <- pm * wm
spmw <- sum(pmw)
spmw
smw <- sum(wm)
sw <- spmw /smw
vr <- n / sum(dy^2)
MI <- vr * sw
MI

pm

########################################################
#Spatial Autocorrelation (manual) + Significance Test
########################################################
#  Now we will do the same process, but using the features
#  of the library. We also will do a significance test.

#  Using binary for TRUE/FALSE distance, as we are only considering adjacency
ww <- nb2listw(w, style='B')

#  Use the moran function to compute Moran's I
moran(p_Diekirch$value, ww, n=length(ww$neighbours), S0 = Szero(ww))


#  Now we will test for significance. We will use a Monte Carlo simulation.
#  Here's how it works: Values will be RANDOMLY assigned to polygons, and then
#  the Moran's I is computed. This is repeated several times to create a 
#  random distribution of Moran's I's. Then, we calculate the Moran's I that we
#  actually get from our dataset. We compare this value with the distribution
#  acquired from the Monte Carlo simulation. If our value isn't too much of an outlier
#  in our distribution, than we will fail to reject H0 and conclude that adjacent reigons
#  do not have a significant affect on each others value's. However, if the value 
#  is a big outlier, than we will reject H0 and conclude adjacent regions have a significant affect 
#  on each other's values

#  Here we run our MC simulation. Here we do 99 iterations.
moran.mc(p_Diekirch$value, ww, nsim=99)

#  looks like we reject H0. 


#  We can also make a "Moran scatter plot" to visualize spatial autocorrelation.
#  First, get the neighbouring values for each value
n <- length(p_Diekirch)
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))

#  Remove zeros
ms <- ms[ms[,3] > 0,]

#  Compute average neighbour value
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'spatially lagged y')
head(ams)

#  Now plot
plot(ams)
reg <- lm(ams[,2] ~ ams[,1])
abline(reg, lwd=2)
abline(h=mean(ams[,2]), lt=2)
abline(v=ybar, lt=2)

#  Notice that the slope of the regression line is almost the
#  same as Moran's 1
coefficients(reg)[2]
