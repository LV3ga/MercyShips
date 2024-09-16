library(scales)
library(ggplot2)
# Exploring statistical relationship between procedures per 100k
# and number of surgeons, anesthesiologists and obstetricians per 100k



# read in data
temp_SAO <- read.csv("SAO_Cleaned.csv")
SAO <-  Filter(function(x)!all(is.na(x)), temp_SAO)

temp_Procedures <- read.csv("Procedures_Cleaned.csv")
Procedures <-  Filter(function(x)!all(is.na(x)), temp_Procedures)

# removing rows that are not countries
SAO <- SAO[-c(2, 4, 8, 35, 37, 50, 62, 63, 64, 65, 66, 69, 74, 75, 96, 99, 103, 104, 105, 106, 108, 111, 129, 135, 136, 137, 140, 141, 143, 154, 162, 182, 184, 192, 216, 218, 219, 231, 232, 237, 239, 241, 242, 243, 250, 260),]
Procedures <- Procedures[-c(2, 4, 8, 35, 37, 50, 62, 63, 64, 65, 66, 69, 74, 75, 96, 99, 103, 104, 105, 106, 108, 111, 129, 135, 136, 137, 140, 141, 143, 154, 162, 182, 184, 192, 216, 218, 219, 231, 232, 237, 239, 241, 242, 243, 250, 260),]

# removing years 2002-2007 and 2019-2023 from Procedures since SAO only contains 2008-2018
Procedures <- Procedures[ ,-c(5, 6, 7, 8, 20, 21, 22, 23, 24)]




# putting data in single dataframe
# we will only include the most recent SAO density and # of Procedures for each country
S_and_P <- data.frame(Country.Name = SAO$Country.Name, Country.Code = SAO$Country.Code, SAO.Density = rep(c(0), 220), SAO.Density.Date = rep(c(0), 220), Num.Procedures = rep(c(0), 220), Num.Procedures.Date = rep(c(0), 220))

for (i in 1:220) {
  print(i)
  
  # getting most recent SAO and procedures data
  for(j in 15:5)
  {
    # check if country has SAO data for the year in column j
    if(!is.na(SAO[i,j])){
      
      # check if country also has Procedures data for the year in column j
      if(!is.na(Procedures[i,j])){
        
        # record data
        S_and_P[i,3] <- SAO[i,j]                 # density
        S_and_P[i,4] <- colnames(SAO)[j]         # year data was collected
        S_and_P[i,5] <- Procedures[i,j]          # num procedures
        S_and_P[i,6] <- colnames(Procedures)[j]  # year data was collected
        break
        
      }
    
    }
  }
}

# countries with both SAO and procedures data in the same year
S_and_P_SAMEYEAR <- S_and_P[apply(S_and_P!=0, 1, all),]

# plotting SAO vs Procedures (normalized)
matplot(rescale(S_and_P_SAMEYEAR$SAO.Density), rescale(S_and_P_SAMEYEAR$Num.Procedures))

# plotting SAO vs Procedures (no scaling)
matplot(S_and_P_SAMEYEAR$SAO.Density, S_and_P_SAMEYEAR$Num.Procedures)

# plotting SAO vs Procedures (log scaling Num.Procedures)
matplot(S_and_P_SAMEYEAR$SAO.Density, log(S_and_P_SAMEYEAR$Num.Procedures))

# plotting SAO vs Procedures (square-root scaling Num.Procedures)
matplot(S_and_P_SAMEYEAR$SAO.Density, sqrt(S_and_P_SAMEYEAR$Num.Procedures))





# linear regression
m1 <- lm(Num.Procedures ~ SAO.Density, data=S_and_P_SAMEYEAR)
qqnorm(residuals(m1)) # checking for normality

# plotting line with data
ggplot(data = S_and_P_SAMEYEAR, aes(SAO.Density, Num.Procedures), stat_summary(fun.data = mean_cl_normal), geom_smooth(method='lm', formula = Num.Procedures ~ SAO.Density))

predict(m1, newdata=data.frame(SAO.Density = c(0.02)))
summary(m1)

# plotting residuals
ggplot(aes(m1$residuals))



# logsitic regression
# Assumptions: 
# 1) independence of cases
# 2) there exists a linear relationship between the logit of the outcome variable and
#    any continuous predictor
# 3) no multicollinearity
# 4) no complete separation

# testing assumption 1
#   nothing to test - cases are independent

# testing assumption 2
summary(lm(Num.Procedures ~ SAO.Density + log(SAO.Density) + (SAO.Density*log(SAO.Density)), data=S_and_P_SAMEYEAR))
matplot(log(S_and_P_SAMEYEAR$Num.Procedures), S_and_P_SAMEYEAR$SAO.Density)
