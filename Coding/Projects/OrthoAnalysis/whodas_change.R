library(dplyr)
library(car)



# Getting data
assess1 <- read.csv("C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\SLFWHODAS.csv")



#############################################################################################
# Step 1: CLEAN the data 
# << variable by variable >>
#############################################################################################

#################
#Clean Diagnosis
################


#Insert a column to store the old diagnosis
assess1$DIagnosis_ORIGINAL <- assess1$DIagnosis

# SLF 8216 has both rickets and blounts listed in the Diagnosis column. Rachel says he has blounts
assess1$DIagnosis[(assess1$SLF.. == "8216")] <- "blounts"

# Cleaning diagnosis column (standardizing to blounts, rickets or other)
assess1$DIagnosis[grepl("blounts", tolower(assess1$DIagnosis))] <- "blounts"
assess1$DIagnosis[grepl("rickets", tolower(assess1$DIagnosis))] <- "rickets"
assess1$DIagnosis[!(grepl("blounts", tolower(assess1$DIagnosis))) & !(grepl("rickets", assess1$DIagnosis))] <- "others"

# Good! All rows have been standardized
nrow(assess1[!(assess1$DIagnosis == "blounts" | assess1$DIagnosis == "rickets" | assess1$DIagnosis == "others"),])
print(table(assess1$DIagnosis))


# Removing SLF 8326 (Sesay)
assess1 <- assess1[!(assess1$SLF.. == 8326),]

# Rachel said this patient had blounts
assess1$DIagnosis[(assess1$SLF.. == "8216")] <- "blounts"

# Getting diagnosis demographics on all legs (90)
assess1Preop <- assess1[assess1$Stage.of.Care == "Assess_Preop" & assess1$Operated.on. == "Y",]
nrow(assess1Preop[assess1Preop$DIagnosis == "blounts",])
nrow(assess1Preop[assess1Preop$DIagnosis == "rickets",])
nrow(assess1Preop[assess1Preop$DIagnosis == "others",])


#################
#Clean IM measurements
################

#Insert a column to store the original measurements
assess1$cleaned_Alignment..V2..in.cm_ORIGINAL<-assess1$cleaned_Alignment..V2..in.cm

# Making IM measurements negative
##---- Therefore, any space between the ankles when the knees are touching is Negative and any space between the knees when the ankles are touching is Positive
assess1$cleaned_Alignment..V2..in.cm <- ifelse(assess1$cleaned_Alignment..V2..measurement.type == "IM", -as.numeric(assess1$cleaned_Alignment..V2..in.cm), as.numeric(assess1$cleaned_Alignment..V2..in.cm))
#nrow(assess[grepl("blounts", assess1$DIagnosis) & grepl("rickets", assess1$DIagnosis), ])


#################
#Clean Sex
################

#Insert a column to store the original  sex
assess1$Sex_ORIGINAL <- assess1$Sex

# Cleaning sex column
assess1$Sex <- ifelse(grepl("m", tolower(assess1$Sex)), "M", "F")


#################
#Age of first surgery as numeric
################
assess1$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns<-as.numeric(assess1$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)


#################
#Clean NAs throughout dataset
################

# Replacing NAs with empty strings (coding is simpler when I don't have both NAs and "" to worry about)
assess1[is.na(assess1)] <- "" 


#################
#Create Weeks In Cast
################

# Creating Weeks.In.Cast value. 
assess1$Weeks.in.Cast <- ifelse(as.numeric(assess1$Number.of.days.cast.was.on.from.first.day.of.surgery) %% 7 >= 4, 
                                ceiling(as.numeric(assess1$Number.of.days.cast.was.on.from.first.day.of.surgery) / 7), 
                                floor(as.numeric(assess1$Number.of.days.cast.was.on.from.first.day.of.surgery) / 7))




############################################################################################
##
# MAKE THE DATA SET TO INCLUDE THOSE WHO HAVE RADIOLOGY INCLUDED IN 12 MO POST OP!!!!!!!!!!!!!!!!!!!!!!!
#
#############################################################################################

#Filtering if they had radiology data in 12 MO FU
#dataframe of those who had attended follow up
follow_up_ids_2 <- assess1 %>%
  filter(`X12.MO.radiology.DATA` == "y") %>%       # Keep only rows where follow-up was attended
  distinct(`SLF..`) %>%                         # Keep only one row per person
  pull(`SLF..`)                                 # Extract just the ID vector
assess_RADIO12mo <- assess1 %>%
  filter(`SLF..` %in% follow_up_ids_2)

# Filtering for legs operated on
assess_RADIO12mo <- assess_RADIO12mo[assess_RADIO12mo$Operated.on. == "Y",]


#############################################################################################
# Renaming data so it works seemlessly with the other code
# 
#############################################################################################

assess<-assess_RADIO12mo


# Partitioning data into Preop, Discharge and 12 Month Postop
cohort_assess_preop <- assess[assess$Stage.of.Care == "Assess_Preop",]
cohort_assess_xafterwedge <- assess[assess$Stage.of.Care == "XAssess_After_Wedge",]
cohort_assess_xoutofcast <- assess[assess$Stage.of.Care == "XAssess_Immediately_Out_of_Cast",]
cohort_assess_discharge <- assess[assess$Stage.of.Care == "Assess_Initial_Discharge",]
cohort_assess_12month <- assess[assess$Stage.of.Care == "Assess_12_month_postop", ]




#############################################################################################
# PREPARING Data
#
#############################################################################################

# Getting indicies for dissagregating data in our cohort

#demographics
cohort_BMI <- as.numeric(cohort_assess_12month$BMI..V11)
cohort_BMI <- ifelse(cohort_BMI == 0, NA, cohort_BMI)
cohort_SEX <- cohort_assess_preop$Sex
cohort_AGE <- as.numeric(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)

#diagnosis
blounts_indicies <- cohort_assess_preop$DIagnosis == "blounts"
rickets_indicies <- cohort_assess_preop$DIagnosis == "rickets"
others_indicies <- cohort_assess_preop$DIagnosis == "others"

#surgery type
tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "Y"
not_tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "N"

#MPTA
cohort_preop_MPTA <- as.numeric(cohort_assess_preop$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_discharge_MPTA <-as.numeric(cohort_assess_discharge$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_12month_MPTA <- as.numeric(cohort_assess_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
#MAD
cohort_preop_MAD <- as.numeric(cohort_assess_preop$Mech.Axis.Deviation..mm.)
cohort_xafterwedge_MAD <- as.numeric(cohort_assess_xafterwedge$Mech.Axis.Deviation..mm.)
cohort_xoutofcast_MAD <- as.numeric(cohort_assess_xoutofcast$Mech.Axis.Deviation..mm.)
cohort_discharge_MAD <-as.numeric(cohort_assess_discharge$Mech.Axis.Deviation..mm.)
cohort_12month_MAD <- as.numeric(cohort_assess_12month$Mech.Axis.Deviation..mm.)
#MechAxis
cohort_preop_mechAxis <- as.numeric(cohort_assess_preop$cleaned_mech_axis_degrees)
cohort_xafterwedge_mechAxis <- as.numeric(cohort_assess_xafterwedge$cleaned_mech_axis_degrees)
cohort_xoutofcast_mechAxis <- as.numeric(cohort_assess_xoutofcast$cleaned_mech_axis_degrees)
cohort_discharge_mechAxis <-as.numeric(cohort_assess_discharge$cleaned_mech_axis_degrees)
cohort_12month_mechAxis <- as.numeric(cohort_assess_12month$cleaned_mech_axis_degrees)
#Alignment
cohort_preop_alignment <- as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm)
cohort_discharge_alignment <-as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm)
cohort_12month_alignment <- as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)
cohort_alignment_IC_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC" ###duplicate?
cohort_alignment_IM_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM" ###duplicate?
IC_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC" ###duplicate?
IM_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM" ###duplicate?

# Getting distance from "normal" range for each outcome at each stage of care
# Absolute values

# Alignment normal = 0
alignment_preop_dist <- abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm))
alignment_discharge_dist <- abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm))
alignment_12month_dist <- abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm))

# Mech Axis normal = 0
mechAxis_preop_dist <- abs(as.numeric(cohort_assess_preop$cleaned_mech_axis_degrees))
mechAxis_xafterwedge_dist <- abs(as.numeric(cohort_assess_xafterwedge$cleaned_mech_axis_degrees))
mechAxis_xoutofcast_dist <- abs(as.numeric(cohort_assess_xoutofcast$cleaned_mech_axis_degrees))
mechAxis_discharge_dist <- abs(as.numeric(cohort_assess_discharge$cleaned_mech_axis_degrees))
mechAxis_12month_dist <- abs(as.numeric(cohort_assess_12month$cleaned_mech_axis_degrees))

# MAD normal = 3-17 varus (positive)
MAD_preop <- as.numeric(cohort_assess_preop$Mech.Axis.Deviation..mm.)
MAD_xafterwedge <- as.numeric(cohort_assess_xafterwedge$Mech.Axis.Deviation..mm.)
MAD_xoutofcast <- as.numeric(cohort_assess_xoutofcast$Mech.Axis.Deviation..mm.)
MAD_12month <- as.numeric(cohort_assess_12month$Mech.Axis.Deviation..mm.)

MAD_temp <- ifelse(abs(3-MAD_12month) < abs(17-MAD_12month), abs(3-MAD_12month), abs(17-MAD_12month)) ##12 months
MAD_temp_discharge <- ifelse(abs(3-cohort_discharge_MAD) < abs(17-cohort_discharge_MAD), abs(3-cohort_discharge_MAD), abs(17-cohort_discharge_MAD))
MAD_temp_xafterwedge <- ifelse(abs(3-MAD_xafterwedge) < abs(17-MAD_xafterwedge), abs(3-MAD_xafterwedge), abs(17-MAD_xafterwedge))
MAD_temp_xoutofcast <- ifelse(abs(3-MAD_xoutofcast) < abs(17-MAD_xoutofcast), abs(3-MAD_xoutofcast), abs(17-MAD_xoutofcast))

MAD_preop_dist <- ifelse(MAD_preop <= 17 & MAD_preop >= 3, 0, abs(17 - MAD_preop)) # all values are greater than 17
MAD_xafterwedge_dist <- ifelse(MAD_xafterwedge <= 17 & MAD_xafterwedge >= 3, 0, MAD_temp_xafterwedge)
MAD_xoutofcast_dist <- ifelse(MAD_xoutofcast <= 17 & MAD_xoutofcast >= 3, 0, MAD_temp_xoutofcast)
MAD_discharge_dist <- ifelse(cohort_discharge_MAD <= 17 & cohort_discharge_MAD >= 3, 0, MAD_temp_discharge)
MAD_12month_dist <- ifelse(MAD_12month <= 17 & MAD_12month >= 3, 0, MAD_temp)

# MPTA normal = 85-90 degrees 
MPTA_preop <- as.numeric(cohort_assess_preop$cleaned_MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
MPTA_12month <- as.numeric(cohort_assess_12month$cleaned_MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
MPTA_temp_preop <- ifelse(abs(85-MPTA_preop) < abs(90 - MPTA_preop), abs(85-MPTA_preop), abs(90-MPTA_preop))
MPTA_temp_discharge <- ifelse(abs(85-cohort_discharge_MPTA) < abs(90 - cohort_discharge_MPTA), abs(85-cohort_discharge_MPTA), abs(90-cohort_discharge_MPTA))
MPTA_temp_12month <- ifelse(abs(85-MPTA_12month) < abs(90 - MPTA_12month), abs(85-MPTA_12month), abs(90-MPTA_12month))
MPTA_preop_dist <- ifelse(MPTA_preop <= 90 & MPTA_preop >= 85, 0, MPTA_temp_preop)
MPTA_discharge_dist <- ifelse(cohort_discharge_MPTA <= 90 & cohort_discharge_MPTA >= 85, 0, MPTA_temp_discharge)
MPTA_12month_dist <- ifelse(MPTA_12month <= 90 & MPTA_12month >= 85, 0, MPTA_temp_12month)


# Change in distance from normal range between 12month and discharge outcomes
alignment_change_after_discharge <- alignment_12month_dist - alignment_discharge_dist
mechAxis_change_after_discharge <- mechAxis_12month_dist - mechAxis_discharge_dist
MAD_change_after_discharge <- MAD_12month_dist - MAD_discharge_dist
MPTA_change_after_discharge <- MPTA_12month_dist - MPTA_discharge_dist


# WHODAS Stuff

# PreOp
whodas_adult_preop <- cbind(cohort_assess_preop$SLF.., cohort_assess_preop[,107:122])
whodas_pediatric_preop <- cbind(cohort_assess_preop$SLF.., cohort_assess_preop[,55:67])

whodas_adult_preop[,-1] <- lapply(whodas_adult_preop[,-1], as.numeric)
whodas_pediatric_preop[,-1] <- lapply(whodas_pediatric_preop[,-1], as.numeric)

whodas_adult_preop <- as.data.frame(whodas_adult_preop)
whodas_pediatric_preop <- as.data.frame(whodas_pediatric_preop)



# 12month
whodas_adult_12month <- cbind(cohort_assess_12month$SLF.., cohort_assess_12month[,107:122])
whodas_pediatric_12month <- cbind(cohort_assess_12month$SLF.., cohort_assess_12month[,55:67])

whodas_adult_12month[,-1] <- lapply(whodas_adult_12month[,-1], as.numeric)
whodas_pediatric_12month[,-1] <- lapply(whodas_pediatric_12month[,-1], as.numeric)

whodas_adult_12month <- as.data.frame(whodas_adult_12month)
whodas_pediatric_12month <- as.data.frame(whodas_pediatric_12month)


# Indicies
# they all have the same missingness
whodas_indicies <- !(is.na(whodas_adult_12month$WA.1.Standing.up.for.long.periods.of.time.such.as.30.minutes.))


# Subsetting based on duplicate SLF numbers
# COUNTED TWICE
# Age
# Sex
# BMI
# QOL Score
# NOT COUNTED TWICE
# Leg Measurements
duplicate_indicies <- duplicated(whodas_adult_12month$`cohort_assess_12month$SLF..`)
whodas_unique_indicies <- whodas_indicies & !duplicate_indicies


# Change in scores
whodas_adult_change <- cbind(cohort_assess_preop$SLF.., whodas_adult_preop[,-1] - whodas_adult_12month[,-1])
whodas_pediatric_change <- cbind(cohort_assess_preop$SLF.., whodas_pediatric_preop[,-1] - whodas_pediatric_12month[,-1])


#######################################################################
# WHICH PATIENTS DID NOT HAVE IMPROVEMENTS IN WHODAS (ADULT) QUESTIONS?
#######################################################################
less_than_0 <- function(x){
  return(x < 0)
}


greater_than_1 <- function(x){
  return(x > 1)
}


# What are the questions?
colnames(whodas_adult_12month)


# Which question had the most regressions (worse scores) at 12 months?
# It appears to be taking care of household responsibilities.
sapply(whodas_adult_change[whodas_unique_indicies,-1], less_than_0) %>% apply(2,sum) %>% sort()


# Which patients had the most regressions (worse scores) at 12 months?
sapply(whodas_adult_change[whodas_unique_indicies,-1], less_than_0) %>% apply(1,sum) %>% sort()


# Getting indicies with >1 regression
regression_indicies <- sapply(whodas_adult_change[whodas_indicies,-1], less_than_0) %>% apply(1,sum) %>% greater_than_1
regression_indicies_unique <- sapply(whodas_adult_change[whodas_unique_indicies,-1], less_than_0) %>% apply(1,sum) %>% greater_than_1


# We are defining two cohorts. The regression/non-regression cohorts.

(whodas_adult_change$`cohort_assess_preop$SLF..`[whodas_unique_indicies])[regression_indicies_unique]

# Do the two cohorts differ in preop Alignment?
# The regression cohort appears to have closer distance to normal range
# This difference is not statistically significant
as.numeric((alignment_preop_dist[whodas_unique_indicies])[regression_indicies_unique])
mean(as.numeric((alignment_preop_dist[whodas_unique_indicies])[regression_indicies_unique]))
as.numeric((alignment_preop_dist[whodas_unique_indicies])[!regression_indicies_unique])
mean(as.numeric((alignment_preop_dist[whodas_unique_indicies])[!regression_indicies_unique]))
t.test(as.numeric((alignment_preop_dist[whodas_unique_indicies])[regression_indicies_unique]),
       as.numeric((alignment_preop_dist[whodas_unique_indicies])[!regression_indicies_unique]))



# Do the two cohorts differ in PreOp Mech Axis?
# The regression cohort appears to have closer distance to normal range
# This difference is not statistically significant
as.numeric((mechAxis_preop_dist[whodas_unique_indicies])[regression_indicies_unique])
mean(as.numeric((mechAxis_preop_dist[whodas_unique_indicies])[regression_indicies_unique]))
as.numeric((mechAxis_preop_dist[whodas_unique_indicies])[!regression_indicies_unique])
mean(as.numeric((mechAxis_preop_dist[whodas_unique_indicies])[!regression_indicies_unique]))
t.test(as.numeric((mechAxis_preop_dist[whodas_unique_indicies])[regression_indicies_unique]),
       as.numeric((mechAxis_preop_dist[whodas_unique_indicies])[!regression_indicies_unique]))



# Do the two cohorts differ in PreOp MAD?
# The regression cohort appears to have closer distance to normal range
# This difference is pretty statistically significant
as.numeric((MAD_preop_dist[whodas_unique_indicies])[regression_indicies_unique])
mean(as.numeric((MAD_preop_dist[whodas_unique_indicies])[regression_indicies_unique]))
as.numeric((MAD_preop_dist[whodas_unique_indicies])[!regression_indicies_unique])
mean(as.numeric((MAD_preop_dist[whodas_unique_indicies])[!regression_indicies_unique]))
t.test(as.numeric((MAD_preop_dist[whodas_unique_indicies])[regression_indicies_unique]),
       as.numeric((MAD_preop_dist[whodas_unique_indicies])[!regression_indicies_unique]))



# Do the two cohorts differ in age?
# The regression cohort appears to be younger than the non-regression cohort
# The difference in the means is very statistically significant
(cohort_assess_preop$Age.at.First.Surgery[whodas_unique_indicies])[regression_indicies_unique]
mean(as.numeric((cohort_assess_preop$Age.at.First.Surgery[whodas_unique_indicies])[regression_indicies_unique]), na.rm= TRUE)

(cohort_assess_preop$Age.at.First.Surgery[whodas_unique_indicies])[!regression_indicies_unique]
mean(as.numeric((cohort_assess_preop$Age.at.First.Surgery[whodas_unique_indicies])[!regression_indicies_unique]), na.rm= TRUE)

t.test(as.numeric((cohort_assess_preop$Age.at.First.Surgery[whodas_unique_indicies])[regression_indicies_unique]),
       as.numeric((cohort_assess_preop$Age.at.First.Surgery[whodas_unique_indicies])[!regression_indicies_unique]))


# Do the two cohorts differ in BMI?
# The regression cohort appears to have lower BMI than the non-regression cohort
# The difference in the means is not statistically significant
(cohort_assess_preop$BMI..V11.[whodas_unique_indicies])[regression_indicies_unique]
mean(as.numeric((cohort_assess_preop$BMI..V11.[whodas_unique_indicies])[regression_indicies_unique]), na.rm= TRUE)

(cohort_assess_preop$BMI..V11.[whodas_unique_indicies])[!regression_indicies_unique]
mean(as.numeric((cohort_assess_preop$BMI..V11.[whodas_unique_indicies])[!regression_indicies_unique]), na.rm= TRUE)


t.test(as.numeric((cohort_assess_preop$BMI..V11.[whodas_unique_indicies])[regression_indicies_unique]),
       as.numeric((cohort_assess_preop$BMI..V11.[whodas_unique_indicies])[!regression_indicies_unique]))




# Do the two cohorts differ in Sex distribution?
# The regression cohort appears to have more females than the non-regression cohort
# The difference is not statistically significant
(cohort_assess_preop$Sex[whodas_unique_indicies])[regression_indicies_unique]

(cohort_assess_preop$Sex[whodas_unique_indicies])[!regression_indicies_unique]

data <- matrix(c(5, 3, 3, 8), nrow = 2)
fisher.test(data)




# Which patients had the most regressions (worse scores) at 12 months?
sapply(whodas_adult_change[whodas_unique_indicies,], less_than_0) %>% apply(1,sum) 



# For each SLF, getting the questions that they regressed on.
whodas_adult_unique <- whodas_adult_change[whodas_unique_indicies,]
questions <- colnames(whodas_adult_change[,-1])
for(i in 1:nrow(whodas_adult_unique)){
  message <- paste("SLF:", whodas_adult_unique[i,1])
  for(j in 1:ncol(whodas_adult_unique[,-1])){
    if(whodas_adult_unique[i,j] < 0){
      message <- paste(message, questions[j])
      message <- paste(message, " - ")
    }
  }
  print(message)
}


whodas_change_data <- data.frame(SLF = rep(c(0), 19*16),
                                 Question = rep(c(0), 19*16),
                                 PreopValue = rep(c(0), 19*16),
                                 TwelveMonthValue = rep(c(0), 19*16),
                                 Change = rep(c(0), 19*16))



whodas_adult_unique <- whodas_adult_change[whodas_unique_indicies,]
whodas_adult_preop_uni <- whodas_adult_preop[whodas_unique_indicies,]
whodas_adult_12month_uni <- whodas_adult_12month[whodas_unique_indicies,]

shift <- 0
for(i in 1:nrow(whodas_adult_unique)){
  print(i)
  
  SLF <- whodas_adult_unique[i,1]
  
  for(j in 1:ncol(whodas_adult_unique[,-1])){
    whodas_change_data[j + shift,1] <- SLF
    whodas_change_data[j + shift,2] <- j
    whodas_change_data[j + shift,3] <- whodas_adult_preop_uni[i,j+1]
    whodas_change_data[j + shift,4] <- whodas_adult_12month_uni[i,j+1]
    whodas_change_data[j + shift,5] <- whodas_change_data[j + shift,3] - whodas_change_data[j + shift,4] 
  }
  shift <- shift + 16

}

whodas_change_data_regress <- filter(whodas_change_data, Change < 0)
whodas_change_data_regress

write.csv(whodas_change_data, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\whodas_change.csv")
write.csv(whodas_change_data_regress, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\whodas_change_regress.csv")


which(whodas_adult_12month$`cohort_assess_12month$SLF..` == "9485")


