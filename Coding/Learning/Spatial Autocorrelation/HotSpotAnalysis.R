# Hotspot Analysis w/o GIS packages
library(geosphere)

data <- read.csv("UnmetSurgicalNeedTestData.csv")


# function getBinaryWeightMatrix
# requires:
# data = csv file with columns: region, USN_Rate, Lat, Lon
# threshold = distance threshold
# returns:
# weighted matrix of binary neighbors (1, 0)
getBinaryWeightMatrix <- function(data, threshold)
{
  
}


# function GiSTAR
# requires:
# n = sample size
# w = weight matrix
# x = vector of values for each cell
# returns:
# Gi* statistic
GiSTAR <- function(n, w, x, i)
{
  xbar <- sum(x)/n
  s <- sqrt((sum(x**2)/n) - xbar**2)
  
  
}


for(i in 1:nrow(data)-1)
{
  for(j in i+1:nrow(data))
  {
    print()
  }
}