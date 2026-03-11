

#############################################################################################
# Playground
#############################################################################################
missing_MA_indicies <- rep(c(FALSE), nrow(assess))

for(i in 1:nrow(assess)){
  print(i)
  
  # We consider MA missing if numerical value is missing OR measurement type is empty AND numerical value is not 0
  if(assess$cleaned_mech_axis_degrees[i] == "" || (assess$cleaned_mech_axis_varus_valgus[i] == "" && assess$cleaned_mech_axis_degrees[i] != "0")){
    missing_MA_indicies[i] <- TRUE
  }
  
}

assess$First.name[assess$SLF.. == "8216"]
assess$Last.Name[assess$SLF.. == "8216"]


# Legs missing valid MA values
length(assess$Mech.Axis[missing_MA_indicies])


# Writing the records to a csv
write.csv(assess[missing_MA_indicies,],"records_missing_mechAxis.csv", row.names = FALSE)



# Number of records with both blounts and rickets BEFORE changing sadie's diagnosis to blounts
# Answer: 10 (run third line below)
assess <- read.csv("Cleaned_Assess.csv")
assess <- assess[assess$Operated.on. == "Y",]
nrow(assess[grepl("blounts", assess$DIagnosis) & (grepl("rickets", assess$DIagnosis)),])


# Number of records with both blounts and rickets AFTER changing sadie's diagnosis to blounts
# Answer: 0
assess <- read.csv("Cleaned_Assess.csv")
assess <- assess[assess$Operated.on. == "Y",]
assess$DIagnosis[(assess$SLF.. == "8216")] <- "blounts"
nrow(assess[grepl("blounts", assess$DIagnosis) & (grepl("rickets", assess$DIagnosis)),])


# Here we investigate why there are only 3 rickets cases 
# in our cohort.

# First, let's acquire the SLF numbers from rickets patients that
# were excluded from the cohort

SLF_rickets_cohort <- cohort_assess_preop$SLF..[!(grepl("blounts", cohort_assess_preop$DIagnosis)) & (grepl("rickets", cohort_assess_preop$DIagnosis))]
SLF_rickets_all <- assess_preop[!(grepl("blounts", assess_preop$DIagnosis)) & (grepl("rickets", assess_preop$DIagnosis)), c(1,17, 9)]
SLF_rickets_not_cohort <- SLF_rickets_all[!(SLF_rickets_all$SLF.. %in% SLF_rickets_cohort),]


# We will store the information these patients are missing in this table
rickets_missing_data <- data.frame(
  SLF = SLF_rickets_not_cohort$SLF..,
  Leg = SLF_rickets_not_cohort$Right.or.Left.Leg.,
  Diagnosis = SLF_rickets_not_cohort$DIagnosis,
  Preop_Alignment_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Preop_MechAxis_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Preop_MechAxisDeviation = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Preop_MPTA = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Preop_Alignment_MeasureType = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Preop_MechAxis_VarVal = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XAfterWedge_MechAxis_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XAfterWedge_MechAxisDeviation = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XAfterWedge_MPTA = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XAfterWedge_MechAxis_VarVal = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XOutCast_MechAxis_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XOutCast_MechAxisDeviation = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XOutCast_MPTA = rep(c(0), nrow(SLF_rickets_not_cohort)),
  XOutCast_MechAxis_VarVal = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Discharge_Alignment_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Discharge_MechAxis_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Discharge_MechAxisDeviation = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Discharge_MPTA = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Discharge_Alignment_MeasureType = rep(c(0), nrow(SLF_rickets_not_cohort)),
  Discharge_MechAxis_VarVal = rep(c(0), nrow(SLF_rickets_not_cohort)),
  TwelveMonth_Alignment_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  TwelveMonth_MechAxis_Number = rep(c(0), nrow(SLF_rickets_not_cohort)),
  TwelveMonth_MechAxisDeviation = rep(c(0), nrow(SLF_rickets_not_cohort)),
  TwelveMonth_MPTA = rep(c(0), nrow(SLF_rickets_not_cohort)),
  TwelveMonth_Alignment_MeasureType = rep(c(0), nrow(SLF_rickets_not_cohort)),
  TwelveMonth_MechAxis_VarVal = rep(c(0), nrow(SLF_rickets_not_cohort))
)



for(i in 1:nrow(SLF_rickets_not_cohort)){
  
  # counter
  print(k)
  
  # Filtering dataset for patient data
  preop_data <- assess_preop[assess_preop$SLF.. == SLF_rickets_not_cohort$SLF..[i] & assess_preop$Right.or.Left.Leg. == SLF_rickets_not_cohort$Right.or.Left.Leg.[i], ]
  xafterwedge_data <- assess_xafterwedge[assess_xafterwedge$SLF.. == SLF_rickets_not_cohort$SLF..[i] & assess_xafterwedge$Right.or.Left.Leg. == SLF_rickets_not_cohort$Right.or.Left.Leg.[i], ]
  xoutofcast_data <- assess_xoutofcast[assess_xoutofcast$SLF.. == SLF_rickets_not_cohort$SLF..[i] & assess_xoutofcast$Right.or.Left.Leg. == SLF_rickets_not_cohort$Right.or.Left.Leg.[i], ]
  discharge_data <- assess_discharge[assess_discharge$SLF.. == SLF_rickets_not_cohort$SLF..[i] & assess_discharge$Right.or.Left.Leg. == SLF_rickets_not_cohort$Right.or.Left.Leg.[i], ]
  twelvemonth_data <- assess_12month[assess_12month$SLF.. == SLF_rickets_not_cohort$SLF..[i] & assess_12month$Right.or.Left.Leg. == SLF_rickets_not_cohort$Right.or.Left.Leg.[i], ]
  
  k <- 4
  
  ### PREOP: If alignment values, Mech axis values, MAD or MPTA are empty, set column to 1 ###
  for(j in c(24, 39, 41, 45)){
    if(preop_data[1,j] == "" ){
      rickets_missing_data[i,k] <- 1
    }
    
    k <- k + 1
  }
  
  ### PREOP: If missing varus/valgus AND Alignment does not equal 0, set column to 1 ###
  if(preop_data[1,25] == "" && preop_data[1,24] != 0){
    rickets_missing_data[i,k] <- 1
  }
  
  k <- k + 1
  
  ### PREOP: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(preop_data[1,40] == "" && preop_data[1,39] != 0){
    rickets_missing_data[i,k] <- 1
  }
  
  k <- k + 1
  
  
  ### XAFTERWEDGE: (NOT CHECKING ALIGNMENT)
  ### If Mech axis values, MAD or MPTA are empty, remove leg
  for(j in c(39, 41, 45)){
    if(xafterwedge_data[1,j] == "" ){
      rickets_missing_data[i,k] <- 1
    }
    
    k <- k + 1
  }
  
  ### XAFTERWEDGE: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(xafterwedge_data[1,40] == "" && xafterwedge_data[1,39] != 0){
    rickets_missing_data[i,k] <- 1
  }
  
  k <- k + 1
  
  
  ### XOUTOFCAST: (NOT CHECKING ALIGNMENT)
  ### Mech axis values, MAD or MPTA are empty, remove leg###
  for(j in c(39, 41, 45)){
    if(xoutofcast_data[1,j] == "" ){
      rickets_missing_data[i,k] <- 1
    }
    k <- k + 1
  }
  
  ### XOUTOFCAST: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(xoutofcast_data[1,40] == "" && xoutofcast_data[1,39] != 0){
    rickets_missing_data[i,k] <- 1
  }
  
  k <- k + 1
  
  
  
  
  ### DISCHARGE: If alignment values, Mech axis values, MAD or MPTA are empty, remove leg ###
  for(j in c(24, 39, 41, 45)){
    if(discharge_data[1,j] == "" ){
      rickets_missing_data[i,k] <- 1
    }
    k <- k + 1
  }
  ### DISCHARGE: If missing varus/valgus AND Alignment does not equal 0, remove leg ###
  if(discharge_data[1,25] == "" && discharge_data[1,24] != 0){
    rickets_missing_data[i,k] <- 1
  }
  k <- k + 1
  
  ### DISCHARGE: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(discharge_data[1,40] == "" && discharge_data[1,39] != 0){
    rickets_missing_data[i,k] <- 1
  }
  k <- k + 1
  
  
  
  ### 12 MONTH: If alignment values, Mech axis values, MAD or MPTA are empty, remove leg ###
  for(j in c(24, 39, 41, 45)){
    if(twelvemonth_data[1,j] == "" ){
      rickets_missing_data[i,k] <- 1
    }
    k <- k + 1
  }
  ### 12 MONTH: If missing varus/valgus AND Alignment does not equal 0, remove leg ###
  if(twelvemonth_data[1,25] == "" && twelvemonth_data[1,24] != 0){
    rickets_missing_data[i,k] <- 1
  }
  
  k <- k + 1
  
  ### 12 MONTH: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(twelvemonth_data[1,40] == "" && twelvemonth_data[1,39] != 0){
    rickets_missing_data[i,k] <- 1
  }
  
  
}


# Here are the column sums. This gives us a count of patients missing data for each outcome.
colSums(rickets_missing_data[, 4:29])[1:26]

# To make sure the counts are correct, I am auditing the data with  9350 Right and 8190 Left

# Data for 9350 Right Audit
# PASSED!
preop_data <- assess_preop[assess_preop$SLF.. == 9350 & assess_preop$Right.or.Left.Leg. == "right", ]
xafterwedge_data <- assess_xafterwedge[assess_xafterwedge$SLF.. == 9350 & assess_xafterwedge$Right.or.Left.Leg. == "right", ]
xoutofcast_data <- assess_xoutofcast[assess_xoutofcast$SLF.. == 9350 & assess_xoutofcast$Right.or.Left.Leg. == "right", ]
discharge_data <- assess_discharge[assess_discharge$SLF.. == 9350 & assess_discharge$Right.or.Left.Leg. == "right", ]
twelvemonth_data <- assess_12month[assess_12month$SLF.. == 9350 & assess_12month$Right.or.Left.Leg. == "right", ]


# Data for 8190 Left Audit
# PASSED!
preop_data <- assess_preop[assess_preop$SLF.. == 8190 & assess_preop$Right.or.Left.Leg. == "left", ]
xafterwedge_data <- assess_xafterwedge[assess_xafterwedge$SLF.. == 8190 & assess_xafterwedge$Right.or.Left.Leg. == "left", ]
xoutofcast_data <- assess_xoutofcast[assess_xoutofcast$SLF.. == 8190 & assess_xoutofcast$Right.or.Left.Leg. == "left", ]
discharge_data <- assess_discharge[assess_discharge$SLF.. == 8190 & assess_discharge$Right.or.Left.Leg. == "left", ]
twelvemonth_data <- assess_12month[assess_12month$SLF.. == 8190 & assess_12month$Right.or.Left.Leg. == "left", ]


# Now here are the row sums. This tells us outcomes each patient was missing.
# All but 7 of the patients have less than 3 outcomes missing. For these patients, however, they only have 1 outcome missing.
rowSums(rickets_missing_data[, 4:29])


# For patients missing just one outcome, what are they missing?
# Turns out, they're all just missing MPTA in X_After_Wedge
rickets_missing_data_only1 <- rickets_missing_data[rowSums(rickets_missing_data[, 4:29]) == 1, ]
rickets_missing_data_only1


# Thoughts and Questions:
#
# So if we can fill in MPTA in X_After_Wedge, we'll have 10 rickets cases in our cohort.
# If we do not fill in MPTA, and do not change our cohort requirements, we will only
# have 3 rickets cases in our cohort. This is not enough for a blounts v. rickets 
# comparison.
#
# Why do we need xafterwedge and xoutcast? Can't we still get good insight with just
# preop and discharge? Disregarding xafterwedge and xoutcast would increase our cohort
# size from 41 -> 56, and rickets from 3 -> 10. Feel like this has been answered several times.
#
# Okay, we may not be statistically rigorous. But perhaps if there's nothing that contradicts our
# findings, like a visual plot of data, then everything is fine.
#
# For step 4. Is it just tibial plateu elevation for one category and other surgical technique for the other?
# Or do we add more categories?



####################################################
# DIAGNOSIS LINEAR REGRESSION



# So we seem to have some evidence that blounts and rickets and others
# differed in their alignment outcomes.
# However, are there confounding variables that could explain this?


# There's a lot of variables, where to start?
# Well, let's start with the variables that seem to differ 
# between groups.


# These are preop alignment: (numeric value) and preop alignment (measurement value)


# First let's look at preop alignment
# We know that blounts had a lower mean preop alignment 
# than rickets and others. 
# Does preop alignment affect the 12month follow up alignment?
# We look at absolute distance from zero in this analysis.

# First, let's do this analysis in our cohort
# We are plotting alignment at preop vs alignment at 12month follow up
plot(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm)), abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)))


# It looks like there may be a linear relationship
# Now let's do this analysis on the entire dataset
plot(abs(as.numeric(assess_preop$cleaned_Alignment..V2..in.cm)), abs(as.numeric(assess_12month$cleaned_Alignment..V2..in.cm)))



# In both plots, there appears to be some form of a linear relationship.
# We have enough data to do a linear regression test to further validate this.
# First, with just our cohort
m1 <- lm(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)) ~ abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm)))
summary(m1)



# Now, with the entire dataset (empty values are converted to NAs and excluded)
m2 <- lm(abs(as.numeric(assess_12month$cleaned_Alignment..V2..in.cm)) ~ abs(as.numeric(assess_preop$cleaned_Alignment..V2..in.cm)))
summary(m2)



# Before we talk about the test results, we need to first check if they were valid.
# For this test, there are two things we need to check
# 1) The residuals are normally distributed
# 2) Homoskedasticity (variances of residuals should be consistent across values of independent variables)
# Let's check these two

# 1) Looks pretty good (should be a straight-ish line)
qqnorm(m2$residuals)
# 2) If we see any funnling/changing patterns across y = 0 that's bad.
#    Seems to look fine.
plot(fitted(m2), resid(m2))
abline(a = 0, b = 0)


# Looks like there are no glaring issues with using linear
# regression. Now we interpret the test results.
summary(m2)


# It looks like there is a statistically significant positive 
# linear relationship between preop alignment and 12month alignment


# Could we run a linear regression model on the entire population using
# diagnosis as an independent variable? Let's see.
# Trust me these are right:
#   blounts = 43
#   rickets = 13
#   others = 12
# This test essentially estimates group means
m3 <- lm(abs(as.numeric(assess_12month$cleaned_Alignment..V2..in.cm)) ~ factor(assess_preop$DIagnosis))

# The linear model is not statistically 
# significant (p-value = 0.31)
summary(m3)


# However, it appears the conditions necessary for linear regression may
# not be met with our data.
# Residuals don't look to be distributed normally (line is not straight enough)
qqnorm(m3$residuals)
# Homoskedasticity also might slightly be violated (variance of residuals may differ 
# between values of independent variable)
plot(fitted(m3), resid(m3))
abline(a = 0, b = 0)
# Thus, it is wrong to use the linear models' results to argue that diagnosis has no
# impact on alignment. This is likely due to the smaller sample size of rickets and others


# To further investigate, let's throw it into a model with preop alignment
m4 <- lm(abs(as.numeric(assess_12month$cleaned_Alignment..V2..in.cm)) ~ abs(as.numeric(assess_preop$cleaned_Alignment..V2..in.cm)) + factor(assess_preop$DIagnosis))


# checking residuals normality
# not bad
qqnorm(m4$residuals)
# checking homoskedasticity
# looks fine
plot(fitted(m4), resid(m4))


# This output is interesting!
# It's possible that diagnosis does have an effect on alignment at 12 months,
# it's plausible that preop alignment accounted for noise in diagnosis
summary(m4)

####################################################
# DIAGNOSIS LINEAR REGRESSION


#############################################################################################
# Step 1.5: Does the alignment measurement type at preop (IC/IM)
#           differ between the blounts and rickets groups?
#
#############################################################################################

### Preparing data ###
preop_alignment_IC_indicies <- assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
preop_alignment_IM_indicies <- assess_preop$cleaned_Alignment..V2..measurement.type == "IM"
preop_blounts_indicies <- assess_preop$DIagnosis == "blounts"
preop_rickets_indicies <- assess_preop$DIagnosis == "rickets"
preop_others_indicies <- assess_preop$DIagnosis == "others"

cohort_alignment_IC_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
cohort_alignment_IM_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"
cohort_blounts_indicies <- cohort_assess_preop$DIagnosis == "blounts"
cohort_rickets_indicies <- cohort_assess_preop$DIagnosis == "rickets"
cohort_others_indicies <- cohort_assess_preop$DIagnosis == "others"
### Preparing data ###



### Performing Analysis ###

# Number of legs with blounts and "IC" at preop (not just our cohort)
# 51
nrow(assess_preop[preop_alignment_IC_indicies & preop_blounts_indicies,])
# Number of just blounts
# 52
nrow(assess_preop[preop_blounts_indicies,])
# Number of legs with rickets at "IM" at preop (not just our cohort)
# 18
nrow(assess_preop[preop_alignment_IM_indicies & preop_rickets_indicies,])
# Number of just rickets 
# 24
nrow(assess_preop[preop_rickets_indicies,])
# Number of legs without blounts or rickets and "IC" at preop (not just our cohort)
# 7
nrow(assess_preop[preop_alignment_IC_indicies & preop_others_indicies,])
# Number of legs without blounts or rickets at preop (not just our cohort)
# 14
nrow(assess_preop[preop_others_indicies,])



# Number of legs with blounts and "IC" at preop in our cohort
# 30 out of 31
nrow(cohort_assess_preop[cohort_alignment_IC_indicies & cohort_blounts_indicies,])
# Number of legs with rickets and "IM" at preop in our cohort
# 8 out of 10
nrow(cohort_assess_preop[cohort_alignment_IM_indicies & cohort_rickets_indicies,])
# Number of legs without blounts and rickets and "IC" at preop in our cohort
# 5 out of 7
nrow(cohort_assess_preop[cohort_alignment_IC_indicies & cohort_others_indicies,])


### Performing Analysis ###


# Thoughts:
#           In the entire dataset, 49/50 blounts were measured "IC" at preop and
#           18/24 rickets were measured "IM" at preop. I could run a t-test, but
#           clearly, there is a difference between the groups. This implies that
#           rickets and blounts present differently. Perhaps this is already known.


