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
##*******STEP 2:
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
# Step 3: REVIEWING the dataset by 
# A. Checking missingness at all stages of care and for all variables
# B. Checking the number of blounts from the full cohort to this reduced cohort
#
#                 <this can all be minimized - it's more for reference. not necessary for determining cohort>
#############################################################################################


##### A. Checking missingness at all stages of care and for all variables
#####
# Columns we are checking:
# [24] "cleaned_Alignment..V2..in.cm"                                                                                              
# [25] "cleaned_Alignment..V2..measurement.type"

# [39] "cleaned_mech_axis_degrees"                                                                                                 
# [40] "cleaned_mech_axis_varus_valgus" 

# [41] "Mech.Axis.Deviation..mm."
####################### MISSING ALIGNMENT ##################################################

###########
#PREOP: Count, ID numbers of those missing data for Alignment MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`cleaned_Alignment..V2..in.cm` == "",
         `Stage.of.Care` == "Assess_Preop") %>%   count()
#how many are missing measurement type ----------------------------------- 0
assess_RADIO12mo %>%
  filter(`cleaned_Alignment..V2..in.cm` != 0,
         `cleaned_Alignment..V2..measurement.type` == "",
         `Stage.of.Care` == "Assess_Preop") %>%  count()

##########
#DISCHARGE: Count, ID numbers of those missing data for Alignment MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ------------------------------------------------3 legs ("9350" "8180" "9350") [2pts]
assess_RADIO12mo %>% 
  filter(`cleaned_Alignment..V2..in.cm` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%  count()
assess_RADIO12mo %>% 
  filter(`cleaned_Alignment..V2..in.cm` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%  pull(`SLF..`)

#how many are missing measurement type ------------------------------------------ 3 legs ("9350" "8180" "9350") [2pts]
assess_RADIO12mo %>%
  filter(`cleaned_Alignment..V2..in.cm` != 0,
         `cleaned_Alignment..V2..measurement.type` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%  count()
assess_RADIO12mo %>%
  filter(`cleaned_Alignment..V2..in.cm` != 0,
         `cleaned_Alignment..V2..measurement.type` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%  pull(`SLF..`)

###########
#12 MO FOLLOW UP: Count, ID numbers of those missing data for Alignment MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 8 legs ("8277" "8357" "9021" "9292" "9505" "8180" "9292" "9505") [6 pts]
assess_RADIO12mo %>% 
  filter(`cleaned_Alignment..V2..in.cm` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%  count()
assess_RADIO12mo %>% 
  filter(`cleaned_Alignment..V2..in.cm` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%  pull(`SLF..`)
#how many are missing measurement type ----------------------------------- 8 legs ("8277" "8357" "9021" "9292" "9505" "8180" "9292" "9505") [6 pts]
assess_RADIO12mo %>%
  filter(`cleaned_Alignment..V2..in.cm` != 0,
         `cleaned_Alignment..V2..measurement.type` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%  count()
assess_RADIO12mo %>%
  filter(`cleaned_Alignment..V2..in.cm` != 0,
         `cleaned_Alignment..V2..measurement.type` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%  pull(`SLF..`)


####################### MISSING MECH AXIS ##################################################

###########
#PREOP: Count, ID numbers of those missing data for Mech Axis MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "Assess_Preop") %>%   count()
#how many are missing measurement type ----------------------------------- 0
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "Assess_Preop") %>%  count()

###########
#AFTER WEDGE: Count, ID numbers of those missing data for Mech Axis MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 2 legs ("8077" "8077") [1 pt]
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "XAssess_After_Wedge") %>%   count()
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "XAssess_After_Wedge") %>%   pull(`SLF..`)
#how many are missing measurement type ----------------------------------- 2 legs ("8077" "8077")[1 pt]
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "XAssess_After_Wedge") %>%  count()
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "XAssess_After_Wedge") %>%  pull(`SLF..`)

###########
#OUT OF CAST: Count, ID numbers of those missing data for Mech Axis MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 3 legs ("8289" "8209" "8289") [2 pts]
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "XAssess_Immediately_Out_of_Cast") %>%   count()
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "XAssess_Immediately_Out_of_Cast") %>%   pull(`SLF..`)
#how many are missing measurement type ----------------------------------- 3 legs ("8289" "8209" "8289") [2 pts]
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "XAssess_Immediately_Out_of_Cast") %>%  count()
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "XAssess_Immediately_Out_of_Cast") %>%  pull(`SLF..`)

###########
#DISCHARGE: Count, ID numbers of those missing data for Mech Axis MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%   count()
#how many are missing measurement type ----------------------------------- 0
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%  count()

###########
#12 MO FOLLOW UP: Count, ID numbers of those missing data for Mech Axis MEASUREMENT and MEASUREMENT TYPE
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`cleaned_mech_axis_degrees` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%   count()
#how many are missing measurement type ----------------------------------- 0
assess_RADIO12mo %>%
  filter(`cleaned_mech_axis_degrees` != 0,
         `cleaned_mech_axis_varus_valgus` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%  count()


####################### MISSING MECH AXIS DEVIATION ##################################################

###########
#PREOP: Count, ID numbers of those missing data for Mech Axis Deviation MEASUREMENT
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "Assess_Preop") %>%   count()

###########
#AFTER WEDGE: Count, ID numbers of those missing data for Mech Axis Deviation MEASUREMENT
#
#how many are missing measurement ----------------------------------- 2 legs ("8077" "8077") [1 pt]
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "XAssess_After_Wedge") %>%   count()
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "XAssess_After_Wedge") %>%   pull(`SLF..`)

###########
#OUT OF CAST: Count, ID numbers of those missing data for Mech Axis Deviation MEASUREMENT
#
#how many are missing measurement ----------------------------------- 3 legs ("8289" "8209" "8289") [2 pts]
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "XAssess_Immediately_Out_of_Cast") %>%   count()
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "XAssess_Immediately_Out_of_Cast") %>%   pull(`SLF..`)

###########
#DISCHARGE: Count, ID numbers of those missing data for Mech Axis Deviation
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "Assess_Initial_Discharge") %>%   count()

###########
#12 MO FOLLOW UP: Count, ID numbers of those missing data for Mech Axis Deviation
#
#how many are missing measurement ----------------------------------- 0
assess_RADIO12mo %>% 
  filter(`Mech.Axis.Deviation..mm.` == "",
         `Stage.of.Care` == "Assess_12_month_postop") %>%   count()



##### B. Checking the number of blounts from the full cohort to this reduced cohort
#####

assess1 %>%
  distinct(`SLF..`, .keep_all = TRUE) %>%
  count(DIagnosis)
assess_RADIO12mo %>%
  distinct(`SLF..`, .keep_all = TRUE) %>%
  count(DIagnosis)
assess_RADIO12mo %>%
  #  distinct(`SLF..`, .keep_all = TRUE) %>%
  count(DIagnosis,`Right.or.Left.Leg.`, `SLF..`)






#############################################################################################
# Step 4: Renaming data so it works seemlessly with the other code
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
# Step 5: DEMOGRAPHICS
#
#############################################################################################


# Getting cohort demographics
# Using cohort_assess_preop, because these variables are consistent across all stages


# Number of patients
length(unique(cohort_assess_preop$SLF..))
# Number of knees
nrow(cohort_assess_preop)

#Diagnosis distribution [legs]
print(table(cohort_assess_preop$DIagnosis))
#Diagnosis distribution [pts]
assess %>%
  distinct(`SLF..`, .keep_all = TRUE) %>%
  count(DIagnosis)

# Female to Male (legs) Ratio
print(table(cohort_assess_preop$Sex,cohort_assess_preop$DIagnosis ))

# Average Age
mean(as.numeric(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns))

# Range
range(as.numeric(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns))



# Tibial Plateau Elevation 
nrow(cohort_assess_preop[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y", ])

# Number of starting valgus (this one we actually need to be on preop) 
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "valgus", ])
# Number of starting varus (this one we actually need to be on preop) 
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "varus", ])






#############################################################################################
# Step 6: PREPARING Data
#
#############################################################################################



#complications
SLF_complication_count <- read.csv("C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\SLF_Complications_Count.csv")
cohort_assess_preop$Complication.Count <- rep(c(0), nrow(cohort_assess_preop))
cohort_assess_preop$Complication.Count <- ifelse(cohort_assess_preop$SLF.. %in% SLF_complication_count$PatientID, 1,0)
cohort_assess_preop$Complication.Count[cohort_assess_preop$SLF.. == "9324"] <- 5 #SLF 9324 was the only patient to have more than 1 complication count (5)
complication_indicies <- cohort_assess_preop$SLF.. %in% SLF_complication_count$PatientID


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



# Getting indices of legs without 70% reduction in distance to normal range in at least one outcome
poor_alignment_reduction <- alignment_12month_dist >= (alignment_preop_dist - (alignment_preop_dist * 0.7))
poor_mechAxis_reduction <- mechAxis_12month_dist >= (mechAxis_preop_dist - (mechAxis_preop_dist* 0.7))
poor_MAD_reduction <- MAD_12month_dist >= (MAD_preop_dist - (MAD_preop_dist * 0.7))
poor_reduction_indicies <- poor_alignment_reduction | poor_mechAxis_reduction | poor_MAD_reduction 


# Getting indicies of patients younger than 9
less_9_indicies <- cohort_AGE < 9



#############################################################################################
# Step 7: How did diagnosis impact outcomes (alignment)?
#
#############################################################################################
library(ggplot2)



# We will make a table to contain mean and sd values for each outcome of each type of surgery

# Alignment (raw values)
diagnosis_alignment <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                                  Blounts_means <- rep(c(0), 3),
                                  Blounts_upper2sd <- rep(c(0), 3),
                                  Blounts_lower2sd <- rep(c(0), 3),
                                  Rickets_means <- rep(c(0), 3),
                                  Rickets_upper2sd <- rep(c(0), 3),
                                  Rickets_lower2sd <- rep(c(0), 3),
                                  Others_means <- rep(c(0), 3),
                                  Other_upper2sd <- rep(c(0), 3),
                                  Other_lower2sd <- rep(c(0), 3)
)


# Mech Axis (raw values)
diagnosis_mechAxis <- data.frame(StagesOfCare = c("Preop", "XAfterWedge", "XOutOfCast", "Discharge", "12Month"),
                                 Blounts_means <- rep(c(0), 5),
                                 Blounts_upper2sd <- rep(c(0), 5),
                                 Blounts_lower2sd <- rep(c(0), 5),
                                 Rickets_means <- rep(c(0), 5),
                                 Rickets_upper2sd <- rep(c(0), 5),
                                 Rickets_lower2sd <- rep(c(0), 5),
                                 Others_means <- rep(c(0), 5),
                                 Other_upper2sd <- rep(c(0), 5),
                                 Other_lower2sd <- rep(c(0), 5)
)


# MAD (raw values)
diagnosis_MAD <- data.frame(StagesOfCare = c("Preop", "XAfterWedge", "XOutOfCast", "Discharge", "12Month"),
                            Blounts_means <- rep(c(0), 5),
                            Blounts_upper2sd <- rep(c(0), 5),
                            Blounts_lower2sd <- rep(c(0), 5),
                            Rickets_means <- rep(c(0), 5),
                            Rickets_upper2sd <- rep(c(0), 5),
                            Rickets_lower2sd <- rep(c(0), 5),
                            Others_means <- rep(c(0), 5),
                            Other_upper2sd <- rep(c(0), 5),
                            Other_lower2sd <- rep(c(0), 5)
)


# alignment (absolute distance from normal range)
diagnosis_alignment_abs <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                                      Blounts_means <- rep(c(0), 3),
                                      Blounts_upper2sd <- rep(c(0), 3),
                                      Blounts_lower2sd <- rep(c(0), 3),
                                      Rickets_means <- rep(c(0), 3),
                                      Rickets_upper2sd <- rep(c(0), 3),
                                      Rickets_lower2sd <- rep(c(0), 3),
                                      Others_means <- rep(c(0), 3),
                                      Other_upper2sd <- rep(c(0), 3),
                                      Other_lower2sd <- rep(c(0), 3)
)


# Mech Axis (absolute distance from normal range)
diagnosis_mechAxis_abs <- data.frame(StagesOfCare = c("Preop", "XAfterWedge", "XOutOfCast", "Discharge", "12Month"),
                                     Blounts_means <- rep(c(0), 5),
                                     Blounts_upper2sd <- rep(c(0), 5),
                                     Blounts_lower2sd <- rep(c(0), 5),
                                     Rickets_means <- rep(c(0), 5),
                                     Rickets_upper2sd <- rep(c(0), 5),
                                     Rickets_lower2sd <- rep(c(0), 5),
                                     Others_means <- rep(c(0), 5),
                                     Other_upper2sd <- rep(c(0), 5),
                                     Other_lower2sd <- rep(c(0), 5)
)


# MAD (absolute distance from normal range)
diagnosis_MAD_abs <- data.frame(StagesOfCare = c("Preop", "XAfterWedge", "XOutOfCast", "Discharge", "12Month"),
                                Blounts_means <- rep(c(0), 5),
                                Blounts_upper2sd <- rep(c(0), 5),
                                Blounts_lower2sd <- rep(c(0), 5),
                                Rickets_means <- rep(c(0), 5),
                                Rickets_upper2sd <- rep(c(0), 5),
                                Rickets_lower2sd <- rep(c(0), 5),
                                Others_means <- rep(c(0), 5),
                                Other_upper2sd <- rep(c(0), 5),
                                Other_lower2sd <- rep(c(0), 5)
)



# The column names came out weird. I fix it below

colnames(diagnosis_alignment) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                   "Others_upper2sd", "Others_lower2sd")

colnames(diagnosis_alignment_abs) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                       "Others_upper2sd", "Others_lower2sd")


colnames(diagnosis_mechAxis) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                  "Others_upper2sd", "Others_lower2sd")

colnames(diagnosis_mechAxis_abs) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                      "Others_upper2sd", "Others_lower2sd")


colnames(diagnosis_MAD) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                             "Others_upper2sd", "Others_lower2sd")

colnames(diagnosis_MAD_abs) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                 "Others_upper2sd", "Others_lower2sd")




# Alignment data
diagnosis_alignment$Blounts_means[1] <- mean(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_means[2] <-  mean(cohort_discharge_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_means[3] <-  mean(cohort_12month_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_upper2sd[1] <- diagnosis_alignment$Blounts_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_upper2sd[2] <- diagnosis_alignment$Blounts_means[2] + 2*sd(cohort_discharge_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_upper2sd[3] <- diagnosis_alignment$Blounts_means[3] + 2*sd(cohort_12month_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_lower2sd[1] <- diagnosis_alignment$Blounts_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_lower2sd[2] <- diagnosis_alignment$Blounts_means[2] - 2*sd(cohort_discharge_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_lower2sd[3] <- diagnosis_alignment$Blounts_means[3] - 2*sd(cohort_12month_alignment[blounts_indicies], na.rm = TRUE)



diagnosis_alignment$Rickets_means[1] <- mean(cohort_preop_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_means[2] <-  mean(cohort_discharge_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_means[3] <-  mean(cohort_12month_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_upper2sd[1] <- diagnosis_alignment$Rickets_means[1] + 2*sd(cohort_preop_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_upper2sd[2] <- diagnosis_alignment$Rickets_means[2] + 2*sd(cohort_discharge_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_upper2sd[3] <- diagnosis_alignment$Rickets_means[3] + 2*sd(cohort_12month_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_lower2sd[1] <- diagnosis_alignment$Rickets_means[1] - 2*sd(cohort_preop_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_lower2sd[2] <- diagnosis_alignment$Rickets_means[2] - 2*sd(cohort_discharge_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_lower2sd[3] <- diagnosis_alignment$Rickets_means[3] - 2*sd(cohort_12month_alignment[rickets_indicies], na.rm = TRUE)


diagnosis_alignment$Others_means[1] <- mean(cohort_preop_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_means[2] <-  mean(cohort_discharge_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_means[3] <-  mean(cohort_12month_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_upper2sd[1] <- diagnosis_alignment$Others_means[1] + 2*sd(cohort_preop_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_upper2sd[2] <- diagnosis_alignment$Others_means[2] + 2*sd(cohort_discharge_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_upper2sd[3] <- diagnosis_alignment$Others_means[3] + 2*sd(cohort_12month_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_lower2sd[1] <- diagnosis_alignment$Others_means[1] - 2*sd(cohort_preop_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_lower2sd[2] <- diagnosis_alignment$Others_means[2] - 2*sd(cohort_discharge_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_lower2sd[3] <- diagnosis_alignment$Others_means[3] - 2*sd(cohort_12month_alignment[others_indicies], na.rm = TRUE)
Diagnosis_alignment <- c(rep("blounts", length(cohort_preop_alignment[blounts_indicies])), 
               rep("rickets", length(cohort_preop_alignment[rickets_indicies])),
               rep("others", length(cohort_preop_alignment[others_indicies])))
Preop_alignment <- c(cohort_preop_alignment[blounts_indicies], 
                     cohort_preop_alignment[rickets_indicies],
                     cohort_preop_alignment[others_indicies])
Discharge_alignment <- c(cohort_discharge_alignment[blounts_indicies],
                         cohort_discharge_alignment[rickets_indicies],
                         cohort_discharge_alignment[others_indicies])
Twelvemonth_alignment <- c(cohort_12month_alignment[blounts_indicies],
                         cohort_12month_alignment[rickets_indicies],
                         cohort_12month_alignment[others_indicies])


df_diagnosis_alignment <- data.frame(Diagnosis = Diagnosis_alignment,
                                     Preop = Preop_alignment,
                                     Discharge = Discharge_alignment,
                                     TwelveMonth = Twelvemonth_alignment)

write.csv(df_diagnosis_alignment, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\df_diagnosis_alignment.csv")


# Mech Axis data
diagnosis_mechAxis$Blounts_means[1] <- mean(cohort_preop_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_means[2] <- mean(cohort_xafterwedge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_means[3] <- mean(cohort_xafterwedge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_means[4] <-  mean(cohort_discharge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_means[5] <-  mean(cohort_12month_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[1] <- diagnosis_mechAxis$Blounts_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[2] <- diagnosis_mechAxis$Blounts_means[2] + 2*sd(cohort_xafterwedge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[3] <- diagnosis_mechAxis$Blounts_means[3] + 2*sd(cohort_xoutofcast_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[4] <- diagnosis_mechAxis$Blounts_means[4] + 2*sd(cohort_discharge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[5] <- diagnosis_mechAxis$Blounts_means[5] + 2*sd(cohort_12month_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[1] <- diagnosis_mechAxis$Blounts_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[2] <- diagnosis_mechAxis$Blounts_means[2] - 2*sd(cohort_xafterwedge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[3] <- diagnosis_mechAxis$Blounts_means[3] - 2*sd(cohort_xoutofcast_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[4] <- diagnosis_mechAxis$Blounts_means[4] - 2*sd(cohort_discharge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[5] <- diagnosis_mechAxis$Blounts_means[5] - 2*sd(cohort_12month_mechAxis[blounts_indicies], na.rm = TRUE)


diagnosis_mechAxis$Rickets_means[1] <- mean(cohort_preop_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_means[2] <- mean(cohort_xafterwedge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_means[3] <- mean(cohort_xafterwedge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_means[4] <-  mean(cohort_discharge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_means[5] <-  mean(cohort_12month_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[1] <- diagnosis_mechAxis$Rickets_means[1] + 2*sd(cohort_preop_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[2] <- diagnosis_mechAxis$Rickets_means[2] + 2*sd(cohort_xafterwedge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[3] <- diagnosis_mechAxis$Rickets_means[3] + 2*sd(cohort_xoutofcast_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[4] <- diagnosis_mechAxis$Rickets_means[4] + 2*sd(cohort_discharge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[5] <- diagnosis_mechAxis$Rickets_means[5] + 2*sd(cohort_12month_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[1] <- diagnosis_mechAxis$Rickets_means[1] - 2*sd(cohort_preop_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[2] <- diagnosis_mechAxis$Rickets_means[2] - 2*sd(cohort_xafterwedge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[3] <- diagnosis_mechAxis$Rickets_means[3] - 2*sd(cohort_xoutofcast_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[4] <- diagnosis_mechAxis$Rickets_means[4] - 2*sd(cohort_discharge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[5] <- diagnosis_mechAxis$Rickets_means[5] - 2*sd(cohort_12month_mechAxis[rickets_indicies], na.rm = TRUE)


diagnosis_mechAxis$Others_means[1] <- mean(cohort_preop_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_means[2] <- mean(cohort_xafterwedge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_means[3] <- mean(cohort_xafterwedge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_means[4] <-  mean(cohort_discharge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_means[5] <-  mean(cohort_12month_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[1] <- diagnosis_mechAxis$Others_means[1] + 2*sd(cohort_preop_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[2] <- diagnosis_mechAxis$Others_means[2] + 2*sd(cohort_xafterwedge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[3] <- diagnosis_mechAxis$Others_means[3] + 2*sd(cohort_xoutofcast_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[4] <- diagnosis_mechAxis$Others_means[4] + 2*sd(cohort_discharge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[5] <- diagnosis_mechAxis$Others_means[5] + 2*sd(cohort_12month_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[1] <- diagnosis_mechAxis$Others_means[1] - 2*sd(cohort_preop_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[2] <- diagnosis_mechAxis$Others_means[2] - 2*sd(cohort_xafterwedge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[3] <- diagnosis_mechAxis$Others_means[3] - 2*sd(cohort_xoutofcast_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[4] <- diagnosis_mechAxis$Others_means[4] - 2*sd(cohort_discharge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[5] <- diagnosis_mechAxis$Others_means[5] - 2*sd(cohort_12month_mechAxis[others_indicies], na.rm = TRUE)



# MAD data
diagnosis_MAD$Blounts_means[1] <- mean(cohort_preop_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_means[2] <- mean(cohort_xafterwedge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_means[3] <- mean(cohort_xafterwedge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_means[4] <-  mean(cohort_discharge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_means[5] <-  mean(cohort_12month_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[1] <- diagnosis_MAD$Blounts_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[2] <- diagnosis_MAD$Blounts_means[2] + 2*sd(cohort_xafterwedge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[3] <- diagnosis_MAD$Blounts_means[3] + 2*sd(cohort_xoutofcast_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[4] <- diagnosis_MAD$Blounts_means[4] + 2*sd(cohort_discharge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[5] <- diagnosis_MAD$Blounts_means[5] + 2*sd(cohort_12month_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[1] <- diagnosis_MAD$Blounts_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[2] <- diagnosis_MAD$Blounts_means[2] - 2*sd(cohort_xafterwedge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[3] <- diagnosis_MAD$Blounts_means[3] - 2*sd(cohort_xoutofcast_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[4] <- diagnosis_MAD$Blounts_means[4] - 2*sd(cohort_discharge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[5] <- diagnosis_MAD$Blounts_means[5] - 2*sd(cohort_12month_MAD[blounts_indicies], na.rm = TRUE)


diagnosis_MAD$Rickets_means[1] <- mean(cohort_preop_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_means[2] <- mean(cohort_xafterwedge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_means[3] <- mean(cohort_xafterwedge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_means[4] <-  mean(cohort_discharge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_means[5] <-  mean(cohort_12month_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[1] <- diagnosis_MAD$Rickets_means[1] + 2*sd(cohort_preop_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[2] <- diagnosis_MAD$Rickets_means[2] + 2*sd(cohort_xafterwedge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[3] <- diagnosis_MAD$Rickets_means[3] + 2*sd(cohort_xoutofcast_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[4] <- diagnosis_MAD$Rickets_means[4] + 2*sd(cohort_discharge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[5] <- diagnosis_MAD$Rickets_means[5] + 2*sd(cohort_12month_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[1] <- diagnosis_MAD$Rickets_means[1] - 2*sd(cohort_preop_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[2] <- diagnosis_MAD$Rickets_means[2] - 2*sd(cohort_xafterwedge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[3] <- diagnosis_MAD$Rickets_means[3] - 2*sd(cohort_xoutofcast_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[4] <- diagnosis_MAD$Rickets_means[4] - 2*sd(cohort_discharge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[5] <- diagnosis_MAD$Rickets_means[5] - 2*sd(cohort_12month_MAD[rickets_indicies], na.rm = TRUE)


diagnosis_MAD$Others_means[1] <- mean(cohort_preop_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_means[2] <- mean(cohort_xafterwedge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_means[3] <- mean(cohort_xafterwedge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_means[4] <-  mean(cohort_discharge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_means[5] <-  mean(cohort_12month_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[1] <- diagnosis_MAD$Others_means[1] + 2*sd(cohort_preop_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[2] <- diagnosis_MAD$Others_means[2] + 2*sd(cohort_xafterwedge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[3] <- diagnosis_MAD$Others_means[3] + 2*sd(cohort_xoutofcast_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[4] <- diagnosis_MAD$Others_means[4] + 2*sd(cohort_discharge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[5] <- diagnosis_MAD$Others_means[5] + 2*sd(cohort_12month_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[1] <- diagnosis_MAD$Others_means[1] - 2*sd(cohort_preop_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[2] <- diagnosis_MAD$Others_means[2] - 2*sd(cohort_xafterwedge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[3] <- diagnosis_MAD$Others_means[3] - 2*sd(cohort_xoutofcast_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[4] <- diagnosis_MAD$Others_means[4] - 2*sd(cohort_discharge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[5] <- diagnosis_MAD$Others_means[5] - 2*sd(cohort_12month_MAD[others_indicies], na.rm = TRUE)


#printing these tables
library(gt)
diagnosis_alignment%>%gt()
diagnosis_mechAxis%>%gt()
diagnosis_MAD%>%gt()



# Plotting ALIGNMENT data from table
# blue = blounts 
# orange = rickets
# red = others
ggplot() +
  
  geom_line(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Alignment (cm)")+
  scale_x_discrete(limits = rev)





# Plotting MECH AXIS data from table
# blue = blounts 
# orange = rickets 
# red = others

# reordering stages of care for plot
diagnosis_mechAxis$StagesOfCare <- factor(diagnosis_mechAxis$StagesOfCare, levels = c("12Month", "Discharge",  
                                                                                      "XOutOfCast", "XAfterWedge", "Preop"))
ggplot() +
  
  geom_line(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Mech Axis (cm)")+
  scale_x_discrete(limits = rev)




# Plotting MAD data from table
# blue = blounts 
# orange = rickets 
# red = others

# reordering stages of care for plot
diagnosis_MAD$StagesOfCare <- factor(diagnosis_MAD$StagesOfCare, levels = c("12Month", "Discharge",  
                                                                            "XOutOfCast", "XAfterWedge", "Preop"))
ggplot() +
  
  geom_line(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "MAD (mm)")+
  scale_x_discrete(limits = rev)





# The above plot is potentially skewed because the majority of rickets are IM and preop and
# vice versa for blounts. Another issue is that, since our sample size is small for rickets 
# (n = 10) and others (n = 8), the 2sds from the mean will create a huge range that, while very likely to capture
# 95% of the population data, may not be representative of the actual spread. We address this 
# problem by replacing 2sds with simply max and mins.

diagnosis_alignment$Blounts_upper2sd[1] <- max(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_upper2sd[2] <- max(cohort_discharge_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_upper2sd[3] <- max(cohort_12month_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_lower2sd[1] <- min(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_lower2sd[2] <- min(cohort_discharge_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_alignment$Blounts_lower2sd[3] <- min(cohort_12month_alignment[blounts_indicies], na.rm = TRUE)


diagnosis_alignment$Rickets_upper2sd[1] <- max(cohort_preop_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_upper2sd[2] <- max(cohort_discharge_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_upper2sd[3] <- max(cohort_12month_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_lower2sd[1] <- min(cohort_preop_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_lower2sd[2] <- min(cohort_discharge_alignment[rickets_indicies], na.rm = TRUE)
diagnosis_alignment$Rickets_lower2sd[3] <- min(cohort_12month_alignment[rickets_indicies], na.rm = TRUE)


diagnosis_alignment$Others_upper2sd[1] <- max(cohort_preop_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_upper2sd[2] <- max(cohort_discharge_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_upper2sd[3] <- max(cohort_12month_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_lower2sd[1] <- min(cohort_preop_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_lower2sd[2] <- min(cohort_discharge_alignment[others_indicies], na.rm = TRUE)
diagnosis_alignment$Others_lower2sd[3] <- min(cohort_12month_alignment[others_indicies], na.rm = TRUE)



# Mech Axis data

diagnosis_mechAxis$Blounts_upper2sd[1] <- max(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[2] <- max(cohort_xafterwedge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[3] <- max(cohort_xoutofcast_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[4] <- max(cohort_discharge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_upper2sd[5] <- max(cohort_12month_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[1] <- min(cohort_preop_alignment[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[2] <- min(cohort_xafterwedge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[3] <- min(cohort_xoutofcast_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[4] <- min(cohort_discharge_mechAxis[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis$Blounts_lower2sd[5] <- min(cohort_12month_mechAxis[blounts_indicies], na.rm = TRUE)


diagnosis_mechAxis$Rickets_upper2sd[1] <- max(cohort_preop_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[2] <- max(cohort_xafterwedge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[3] <- max(cohort_xoutofcast_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[4] <- max(cohort_discharge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_upper2sd[5] <- max(cohort_12month_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[1] <- min(cohort_preop_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[2] <- min(cohort_xafterwedge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[3] <- min(cohort_xoutofcast_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[4] <- min(cohort_discharge_mechAxis[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis$Rickets_lower2sd[5] <- min(cohort_12month_mechAxis[rickets_indicies], na.rm = TRUE)


diagnosis_mechAxis$Others_upper2sd[1] <- max(cohort_preop_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[2] <- max(cohort_xafterwedge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[3] <- max(cohort_xoutofcast_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[4] <- max(cohort_discharge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_upper2sd[5] <- max(cohort_12month_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[1] <- min(cohort_preop_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[2] <- min(cohort_xafterwedge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[3] <- min(cohort_xoutofcast_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[4] <- min(cohort_discharge_mechAxis[others_indicies], na.rm = TRUE)
diagnosis_mechAxis$Others_lower2sd[5] <- min(cohort_12month_mechAxis[others_indicies], na.rm = TRUE)



# MAD data

diagnosis_MAD$Blounts_upper2sd[1] <- max(cohort_preop_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[2] <- max(cohort_xafterwedge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[3] <- max(cohort_xoutofcast_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[4] <- max(cohort_discharge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_upper2sd[5] <- max(cohort_12month_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[1] <- min(cohort_preop_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[2] <- min(cohort_xafterwedge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[3] <- min(cohort_xoutofcast_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[4] <- min(cohort_discharge_MAD[blounts_indicies], na.rm = TRUE)
diagnosis_MAD$Blounts_lower2sd[5] <- min(cohort_12month_MAD[blounts_indicies], na.rm = TRUE)


diagnosis_MAD$Rickets_upper2sd[1] <- max(cohort_preop_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[2] <- max(cohort_xafterwedge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[3] <- max(cohort_xoutofcast_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[4] <- max(cohort_discharge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_upper2sd[5] <- max(cohort_12month_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[1] <- min(cohort_preop_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[2] <- min(cohort_xafterwedge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[3] <- min(cohort_xoutofcast_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[4] <- min(cohort_discharge_MAD[rickets_indicies], na.rm = TRUE)
diagnosis_MAD$Rickets_lower2sd[5] <- min(cohort_12month_MAD[rickets_indicies], na.rm = TRUE)


diagnosis_MAD$Others_upper2sd[1] <- max(cohort_preop_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[2] <- max(cohort_xafterwedge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[3] <- max(cohort_xoutofcast_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[4] <- max(cohort_discharge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_upper2sd[5] <- max(cohort_12month_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[1] <- min(cohort_preop_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[2] <- min(cohort_xafterwedge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[3] <- min(cohort_xoutofcast_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[4] <- min(cohort_discharge_MAD[others_indicies], na.rm = TRUE)
diagnosis_MAD$Others_lower2sd[5] <- min(cohort_12month_MAD[others_indicies], na.rm = TRUE)




# Plotting ALIGNMENT data from table
# Compared to the previous plot, it is similar but the scale has shrunk.
# This is still pretty confusing to look at.
# blue = blounts
# orange = rickets
# red = others
ggplot() +
  
  geom_line(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_alignment, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Alignment (cm)")+
  scale_x_discrete(limits = rev)



# Plotting MECH AXIS data from table
# blue = blounts 
# orange = rickets 
# red = others

# reordering stages of care for plot
diagnosis_mechAxis$StagesOfCare <- factor(diagnosis_mechAxis$StagesOfCare, levels = c("12Month", "Discharge",  
                                                                                      "XOutOfCast", "XAfterWedge", "Preop"))
ggplot() +
  
  geom_line(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_mechAxis, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Mech Axis (degrees)")+
  scale_x_discrete(limits = rev)




# Plotting MAD data from table
# blue = blounts 
# orange = rickets 
# red = others

# reordering stages of care for plot
diagnosis_MAD$StagesOfCare <- factor(diagnosis_MAD$StagesOfCare, levels = c("12Month", "Discharge",  
                                                                            "XOutOfCast", "XAfterWedge", "Preop"))
ggplot() +
  
  geom_line(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_MAD, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "MAD (mm)")+
  scale_x_discrete(limits = rev)



# Now, lets try taking the absolute value distance from 0 (normal)
# and plotting rickets vs blounts vs others. (still using min and max instead of 2sd)

# fills table with absolute values. I know this is a lot of code, but hopefully
# what I'm doing is clear.
diagnosis_alignment_abs$Blounts_means[1] <- mean(abs(cohort_preop_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_means[2] <-  mean(abs(cohort_discharge_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_means[3] <-  mean(abs(cohort_12month_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_upper2sd[1] <- max(abs(cohort_preop_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_upper2sd[2] <- max(abs(cohort_discharge_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_upper2sd[3] <- max(abs(cohort_12month_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_lower2sd[1] <- min(abs(cohort_preop_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_lower2sd[2] <- min(abs(cohort_discharge_alignment[blounts_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Blounts_lower2sd[3] <- min(abs(cohort_12month_alignment[blounts_indicies]), na.rm = TRUE)


diagnosis_alignment_abs$Rickets_means[1] <- mean(abs(cohort_preop_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_means[2] <-  mean(abs(cohort_discharge_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_means[3] <-  mean(abs(cohort_12month_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_upper2sd[1] <- max(abs(cohort_preop_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_upper2sd[2] <- max(abs(cohort_discharge_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_upper2sd[3] <- max(abs(cohort_12month_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_lower2sd[1] <- min(abs(cohort_preop_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_lower2sd[2] <- min(abs(cohort_discharge_alignment[rickets_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Rickets_lower2sd[3] <- min(abs(cohort_12month_alignment[rickets_indicies]), na.rm = TRUE)


diagnosis_alignment_abs$Others_means[1] <- mean(abs(cohort_preop_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_means[2] <-  mean(abs(cohort_discharge_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_means[3] <-  mean(abs(cohort_12month_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_upper2sd[1] <- max(abs(cohort_preop_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_upper2sd[2] <- max(abs(cohort_discharge_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_upper2sd[3] <- max(abs(cohort_12month_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_lower2sd[1] <- min(abs(cohort_preop_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_lower2sd[2] <- min(abs(cohort_discharge_alignment[others_indicies]), na.rm = TRUE)
diagnosis_alignment_abs$Others_lower2sd[3] <- min(abs(cohort_12month_alignment[others_indicies]), na.rm = TRUE)
mean(abs(cohort_preop_alignment), na.rm = TRUE)


# Mech Axis data
diagnosis_mechAxis_abs$Blounts_means[1] <- mean(mechAxis_preop_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_means[2] <- mean(mechAxis_xafterwedge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_means[3] <- mean(mechAxis_xoutofcast_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_means[4] <-  mean(mechAxis_discharge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_means[5] <-  mean(mechAxis_12month_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_upper2sd[1] <- max(mechAxis_preop_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_upper2sd[2] <- max(mechAxis_xafterwedge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_upper2sd[3] <- max(mechAxis_xoutofcast_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_upper2sd[4] <- max(mechAxis_discharge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_upper2sd[5] <- max(mechAxis_12month_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_lower2sd[1] <- min(mechAxis_preop_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_lower2sd[2] <- min(mechAxis_xafterwedge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_lower2sd[3] <- min(mechAxis_xoutofcast_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_lower2sd[4] <- min(mechAxis_discharge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Blounts_lower2sd[5] <- min(mechAxis_12month_dist[blounts_indicies], na.rm = TRUE)


diagnosis_mechAxis_abs$Rickets_means[1] <- mean(mechAxis_preop_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_means[2] <- mean(mechAxis_xafterwedge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_means[3] <- mean(mechAxis_xoutofcast_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_means[4] <-  mean(mechAxis_discharge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_means[5] <-  mean(mechAxis_12month_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_upper2sd[1] <- max(mechAxis_preop_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_upper2sd[2] <- max(mechAxis_xafterwedge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_upper2sd[3] <- max(mechAxis_xoutofcast_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_upper2sd[4] <- max(mechAxis_discharge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_upper2sd[5] <- max(mechAxis_12month_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_lower2sd[1] <- min(mechAxis_preop_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_lower2sd[2] <- min(mechAxis_xafterwedge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_lower2sd[3] <- min(mechAxis_xoutofcast_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_lower2sd[4] <- min(mechAxis_discharge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Rickets_lower2sd[5] <- min(mechAxis_12month_dist[rickets_indicies], na.rm = TRUE)


diagnosis_mechAxis_abs$Others_means[1] <- mean(mechAxis_preop_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_means[2] <- mean(mechAxis_xafterwedge_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_means[3] <- mean(mechAxis_xoutofcast_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_means[4] <-  mean(mechAxis_discharge_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_means[5] <-  mean(mechAxis_12month_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_upper2sd[1] <- max(mechAxis_preop_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_upper2sd[2] <- max(mechAxis_xafterwedge_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_upper2sd[3] <- max(mechAxis_xoutofcast_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_upper2sd[4] <- max(mechAxis_discharge_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_upper2sd[5] <- max(mechAxis_12month_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_lower2sd[1] <- min(mechAxis_preop_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_lower2sd[2] <- min(mechAxis_xafterwedge_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_lower2sd[3] <- min(mechAxis_xoutofcast_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_lower2sd[4] <- min(mechAxis_discharge_dist[others_indicies], na.rm = TRUE)
diagnosis_mechAxis_abs$Others_lower2sd[5] <- min(mechAxis_12month_dist[others_indicies], na.rm = TRUE)
mean(abs(mechAxis_preop_dist))


# MAD data
diagnosis_MAD_abs$Blounts_means[1] <- mean(MAD_preop_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_means[2] <- mean(MAD_xafterwedge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_means[3] <- mean(MAD_xoutofcast_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_means[4] <-  mean(MAD_discharge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_means[5] <-  mean(MAD_12month_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_upper2sd[1] <- max(MAD_preop_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_upper2sd[2] <- max(MAD_xafterwedge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_upper2sd[3] <- max(MAD_xoutofcast_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_upper2sd[4] <- max(MAD_discharge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_upper2sd[5] <- max(MAD_12month_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_lower2sd[1] <- min(MAD_preop_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_lower2sd[2] <- min(MAD_xafterwedge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_lower2sd[3] <- min(MAD_xoutofcast_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_lower2sd[4] <- min(MAD_discharge_dist[blounts_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Blounts_lower2sd[5] <- min(MAD_12month_dist[blounts_indicies], na.rm = TRUE)


diagnosis_MAD_abs$Rickets_means[1] <- mean(MAD_preop_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_means[2] <- mean(MAD_xafterwedge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_means[3] <- mean(MAD_xoutofcast_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_means[4] <-  mean(MAD_discharge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_means[5] <-  mean(MAD_12month_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_upper2sd[1] <- max(MAD_preop_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_upper2sd[2] <- max(MAD_xafterwedge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_upper2sd[3] <- max(MAD_xoutofcast_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_upper2sd[4] <- max(MAD_discharge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_upper2sd[5] <- max(MAD_12month_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_lower2sd[1] <- min(MAD_preop_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_lower2sd[2] <- min(MAD_xafterwedge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_lower2sd[3] <- min(MAD_xoutofcast_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_lower2sd[4] <- min(MAD_discharge_dist[rickets_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Rickets_lower2sd[5] <- min(MAD_12month_dist[rickets_indicies], na.rm = TRUE)


diagnosis_MAD_abs$Others_means[1] <- mean(MAD_preop_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_means[2] <- mean(MAD_xafterwedge_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_means[3] <- mean(MAD_xoutofcast_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_means[4] <-  mean(MAD_discharge_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_means[5] <-  mean(MAD_12month_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_upper2sd[1] <- max(MAD_preop_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_upper2sd[2] <- max(MAD_xafterwedge_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_upper2sd[3] <- max(MAD_xoutofcast_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_upper2sd[4] <- max(MAD_discharge_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_upper2sd[5] <- max(MAD_12month_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_lower2sd[1] <- min(MAD_preop_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_lower2sd[2] <- min(MAD_xafterwedge_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_lower2sd[3] <- min(MAD_xoutofcast_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_lower2sd[4] <- min(MAD_discharge_dist[others_indicies], na.rm = TRUE)
diagnosis_MAD_abs$Others_lower2sd[5] <- min(MAD_12month_dist[others_indicies], na.rm = TRUE)
mean(abs(MAD_preop_dist), na.rm=TRUE)



# Plotting data from table
# This is a bit easier to digest. 
############################GRAPH 1
# blue = blounts
# orange = rickets
# red = others

ggplot() +
  
  geom_line(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_text(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, label = round(Blounts_means,1)), vjust = -1, colour = 'blue', size = 3, position = position_jitter(width = 0.2)) +
  geom_errorbar(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'darkgreen')+
  geom_point(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'darkgreen')+
  geom_text(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, label = round(Rickets_means,1)), vjust = -1, colour = 'darkgreen', size = 3, position = position_jitter(width = 0.2)) +
  geom_errorbar(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='darkgreen')+
  
  geom_line(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_text(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means, label = round(Others_means,1)), vjust = -1, colour = 'red', size = 3, position = position_jitter(width = 0.2)) +
  geom_errorbar(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Absolute Alignment (cm)")+
  scale_x_discrete(limits = rev)

?geom_text()

#Better version: 
library(ggplot2)
library(ggrepel)

ggplot() +
  
  geom_line(data = diagnosis_alignment_abs, 
            mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), 
            color = 'blue') +
  geom_point(data = diagnosis_alignment_abs, 
             mapping = aes(x = StagesOfCare, y = Blounts_means), 
             color = 'blue') +
  geom_text_repel(data = diagnosis_alignment_abs, 
                  mapping = aes(x = StagesOfCare, y = Blounts_means, label = round(Blounts_means, 1)), 
                  colour = 'blue', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_alignment_abs, 
                mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd), 
                width = 0.1, color = 'blue') +
  
  geom_line(data = diagnosis_alignment_abs, 
            mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), 
            color = 'darkgreen') +
  geom_point(data = diagnosis_alignment_abs, 
             mapping = aes(x = StagesOfCare, y = Rickets_means), 
             color = 'darkgreen') +
  geom_text_repel(data = diagnosis_alignment_abs, 
                  mapping = aes(x = StagesOfCare, y = Rickets_means, label = round(Rickets_means, 1)), 
                  colour = 'darkgreen', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_alignment_abs, 
                mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd), 
                width = 0.1, color = 'darkgreen') +
  
  geom_line(data = diagnosis_alignment_abs, 
            mapping = aes(x = StagesOfCare, y = Others_means, group = 1), 
            color = 'red') +
  geom_point(data = diagnosis_alignment_abs, 
             mapping = aes(x = StagesOfCare, y = Others_means), 
             color = 'red') +
  geom_text_repel(data = diagnosis_alignment_abs, 
                  mapping = aes(x = StagesOfCare, y = Others_means, label = round(Others_means, 1)), 
                  colour = 'red', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_alignment_abs, 
                mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd), 
                width = 0.1, color = 'red') +
  
  labs(x = "Stage of Care", y = "Absolute Alignment (cm)") +
  scale_x_discrete(limits = rev)


# Plotting data from table
# This is a bit easier to digest. 
# blue = blounts
# orange = rickets
# red = others

# reordering stages of care for plot
diagnosis_mechAxis_abs$StagesOfCare <- factor(diagnosis_mechAxis$StagesOfCare, levels = c("12Month", "Discharge",  
                                                                                          "XOutOfCast", "XAfterWedge", "Preop"))


ggplot() +
  
  geom_line(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_text_repel(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, label = round(Blounts_means, 1)), colour = 'blue', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'darkgreen')+
  geom_point(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'darkgreen')+
  geom_text_repel(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, label = round(Rickets_means, 1)), colour = 'darkgreen', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='darkgreen')+
  
  geom_line(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_text_repel(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means, label = round(Others_means, 1)), colour = 'red', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Absolute distance from normal Mech Axis range (degrees)")+
  scale_x_discrete(limits = rev)

diagnosis_mechAxis_abs


# Plotting data from table
# This is a bit easier to digest. 
# blue = blounts
# orange = rickets
# red = others

# reordering stages of care for plot
diagnosis_MAD_abs$StagesOfCare <- factor(diagnosis_mechAxis$StagesOfCare, levels = c("12Month", "Discharge",  
                                                                                     "XOutOfCast", "XAfterWedge", "Preop"))


ggplot() +
  
  geom_line(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_text_repel(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, label = round(Blounts_means, 1)), colour = 'blue', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'darkgreen')+
  geom_point(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'darkgreen')+
  geom_text_repel(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, label = round(Rickets_means, 1)), colour = 'darkgreen', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='darkgreen')+
  
  geom_line(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_text_repel(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Others_means, label = round(Others_means, 1)), colour = 'red', size = 3, max.overlaps = Inf) +
  geom_errorbar(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Absolute distance from normaml MAD (mm)")+
  scale_x_discrete(limits = rev)



# Here's another plot of blounts vs rickets vs others using absolute value
plot(factor(cohort_assess_preop$DIagnosis), abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)))



# Proportion of blounts legs in cohort in normal range (0)
# ANSWER: 13
length(cohort_12month_alignment[blounts_indicies & cohort_12month_alignment == 0])



# Proportion of rickets legs in cohort in normal range (0)
# ANSWER: 0/11
cohort_12month_alignment[rickets_indicies]




# Proportion of other (not blounts or rickets) legs in cohort in normal range (0)
# ANSWER: 2/12
cohort_12month_alignment[others_indicies]



# The following SLF numbers are patients in the other category
# (no blounts or rickets) whose legs (both right and left)
# have the exact same alignment values at all stages of care.
# This seems odd.
# 8010, 8057, 8287, 9257





### Data Summary ###


# FOR BLOUNTS (absolute value)
#       0. sample size = 38
nrow(cohort_assess_preop[blounts_indicies,])
#       1. preop means, max and mins for alignment (absolute value) 15.065789, 26, 3.5
#                                                  (normal values)  14.6973684, 26, -7
diagnosis_alignment_abs
diagnosis_alignment
#       2. discharge means, max and mins for alignment (absolute value) 1.881579, 8, 0
#                                                      (normal values)  0.1184211, 8, -5
diagnosis_alignment_abs
diagnosis_alignment
#       3. 12 month means, max and mins for alignment (absolute value) 2.855263, 11, 0 
#                                                      (normal values) 2.864864, 11, 0
diagnosis_alignment_abs
diagnosis_alignment
#       4. Proportion of IC at preop 37/38
nrow(cohort_assess_preop[blounts_indicies & cohort_alignment_IC_indicies, ])
#       5. Proportion of IM at preop 1/38
nrow(cohort_assess_preop[blounts_indicies & cohort_alignment_IM_indicies, ])
#       6. Number of normal alignments at 12 month post op 10/38
length(cohort_12month_alignment[blounts_indicies & cohort_12month_alignment == 0])
#       7. Female to Male (legs) ratio 12:26
nrow(cohort_assess_12month[blounts_indicies & tolower(cohort_assess_12month$Sex) == "m", ])



# FOR RICKETS (absolute value)
#       0. sample size = 11
nrow(cohort_assess_preop[rickets_indicies,])
#       1. preop means, max and mins for alignment (absolute value) 15.727273, 22, 9
#                                                  (normal values)  -9.5454545, 17, -22
diagnosis_alignment_abs
diagnosis_alignment
#       2. discharge means, max and mins for alignment (absolute value) 3.227273, 7, 1  
#                                                      (normal values)  0.7727273, 5, -7
diagnosis_alignment_abs
diagnosis_alignment
#       3. 12 month means, max and mins for alignment (absolute value) 5.000000, 11, 2   
#                                                     (normal values)  0.4545455, 7, -11
diagnosis_alignment_abs
diagnosis_alignment
#       4. Proportion of IC at preop 2/11
nrow(cohort_assess_preop[rickets_indicies & cohort_alignment_IC_indicies, ])
#       5. Proportion of IM at preop 9/11
nrow(cohort_assess_preop[rickets_indicies & cohort_alignment_IM_indicies, ])
#       6. Number of normal alignments at 12 month post op 0/11
cohort_12month_alignment[rickets_indicies]
#       7. Female to Male (legs) ratio 3:8
nrow(cohort_assess_12month[rickets_indicies & tolower(cohort_assess_12month$Sex) == "m", ])



# FOR OTHERS (absolute value)
#       0. sample size = 12
nrow(cohort_assess_preop[others_indicies,])
#       1. preop means, max and mins for alignment (absolute value) 14.416667, 25.5, 7
#                                                  (normal values)  -1.91666667,  13,  -25.5
diagnosis_alignment_abs
diagnosis_alignment
#       2. discharge means, max and mins for alignment (absolute value) 1.750000, 5.0, 0       
#                                                      (normal values)  -0.08333333, 5.0, -4.0
diagnosis_alignment_abs
diagnosis_alignment
#       3. 12 month means, max and mins for alignment (absolute value) 3.291667, 7.0, 0  
#                                                      (normal values) --1.54166667, 2.5, -7.0
#       4. Proportion of IC at preop 7/12
nrow(cohort_assess_preop[others_indicies & cohort_alignment_IC_indicies, ])
#       5. Proportion of IM at preop 5/12
nrow(cohort_assess_preop[others_indicies & cohort_alignment_IM_indicies, ])
#       6. Number of normal alignments at 12 month post op 2/12
cohort_12month_alignment[others_indicies]
#       7. Female to Male (legs) ratio 7:5
nrow(cohort_assess_12month[others_indicies & tolower(cohort_assess_12month$Sex) == "m", ])


### Data Summary ###


#############################################################################################
# Step 8: Checking for correlation with outcomes and weeks in cast
#
#############################################################################################



# Visually plotting weeks in cast VS outcomes (raw value)
plot(cohort_assess_preop$Weeks.in.Cast, cohort_12month_alignment, xlab = "Weeks in Cast", ylab = "Alignment at 12 months (cm)")
plot(cohort_assess_preop$Weeks.in.Cast, cohort_12month_mechAxis, xlab = "Weeks in Cast", ylab = "Mech Axis at 12 months (degrees)")
plot(cohort_assess_preop$Weeks.in.Cast, cohort_12month_MAD, xlab = "Weeks in Cast", ylab = "Mech Axis Deviation at 12 months (mm)")
plot(cohort_assess_preop$Weeks.in.Cast, cohort_12month_MPTA,  xlab = "Weeks in Cast", ylab = "MPTA at 12 months (degrees)")



# Visually plotting weeks in cast VS outcomes (distance from normal)
plot(cohort_assess_preop$Weeks.in.Cast, alignment_12month_dist, xlab = "Weeks in Cast", ylab = "Distance from normal Alignment at 12 months (cm)")
plot(cohort_assess_preop$Weeks.in.Cast, mechAxis_12month_dist, xlab = "Weeks in Cast", ylab = "Distance from normal Mech Axis at 12 months (degrees)")
plot(cohort_assess_preop$Weeks.in.Cast, MAD_12month_dist, xlab = "Weeks in Cast", ylab = "Distance from normal Mech Axis Deviation at 12 months (mm)")
plot(cohort_assess_preop$Weeks.in.Cast, MPTA_12month_dist,  xlab = "Weeks in Cast", ylab = "Distance from normal MPTA at 12 months (degrees)")



# Visually plotting weeks in cast VS outcomes (raw value)
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_alignment, xlab = "Days in Cast", ylab = "Alignment at 12 months (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_mechAxis, xlab = "Days in Cast", ylab = "Mech Axis at 12 months (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MAD, xlab = "Days in Cast", ylab = "Mech Axis Deviation at 12 months (cm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MPTA,  xlab = "Days in Cast", ylab = "MPTA at 12 months (degrees)")



# Visually plotting days in cast vs outcomes
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, alignment_12month_dist, xlab = "Days in Cast", ylab = "Distance from normal Alignment at 12 months (cm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, mechAxis_12month_dist, xlab = "Days in Cast", ylab = "Distance from normal Mech Axis at 12 months (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, MAD_12month_dist , xlab = "Days in Cast", ylab = "Distance from normal MAD at 12 months (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, MPTA_12month_dist,  xlab = "Days in Cast", ylab = "Distance from normal MPTA at 12 months (degrees)")



# Linear Regression - ALIGNMENT
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop Alignment
# Results: Model 1: Not significant
#          Model 2: Significant, but Preop Alignment was the only significant variable
model_alignment_1 <- lm(alignment_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_alignment_2 <- lm(alignment_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + cohort_AGE + factor(cohort_assess_preop$DIagnosis)) 
summary(model_alignment_1)
summary(model_alignment_2)



# Linear Regression - MECH AXIS
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis
# Results: Model 1: Not significant
#          Model 2: Significant, but only  Preop mech Axis were significant. BMI and Age were almost significant.
model_mechAxis_1 <- lm(mechAxis_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_mechAxis_2 <- lm(mechAxis_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$DIagnosis)) 
summary(model_mechAxis_1)
summary(model_mechAxis_2)


# Linear Regression - MECH AXIS DEVIATION
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis deviation
# Results: Model 1: Significant, days in cast was positively correlated with distance from normal range
#          Model 2: Significant, but Preop mech Axis was the only significant variable. Days in cast no longer significant.
model_MAD_1 <- lm(MAD_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_MAD_2 <- lm(MAD_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$DIagnosis)) 
summary(model_MAD_1)
summary(model_MAD_2)



# Linear Regression - MPTA
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis deviation
# Results: Model 1: Not significant
# Results: Model 2: Not significant
model_MPTA_1 <- lm(MPTA_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_MPTA_2 <- lm(MPTA_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + cohort_AGE + factor(cohort_assess_preop$DIagnosis)) 
summary(model_MPTA_1)
summary(model_MPTA_2)




# Thoughts:
#
# Generally, for weeks 6-12, it doesn't seem like the outcomes change that much. Perhaps something 
# would happen after 13 weeks, but we just don't have that many samples unfortunately. For Alignment 
# and MPTA, the few values we have start to narrow in variance after 13 weeks. However, this trend is 
# not evident for Mech Axis and Mech Axis Deviation.
#
# Perhaps we could group our cohort by "weeks in cast", get the average outcome value for each group,
# and then use T-Tests to see if they differed from each other. An interpretation of this would be that
# "weeks in cast" affected the outcome(s). However, much work would be involved making sure this method
# is robust, and given our data, I doubt new insights would be gained. 
# I could do it later, but for now I will move on.



#############################################################################################
# Step 9: Understand whether the surgical technique impacted long term outcomes 
# - Tibial plateau elevation for blounts vs ‘other surgical technique’ for blounts 
# - We will compare techniques with the 'Alignment' outcome
#############################################################################################
library(ggplot2)

cohort_preop_alignment


# We will make a table to contain mean and sd values for Alignment for each type of surgery

data_alignment <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                             Tib_means <- rep(c(0), 3),
                             Tib_upper2sd <- rep(c(0), 3),
                             Tib_lower2sd <- rep(c(0), 3),
                             Other_means <- rep(c(0), 3),
                             Other_upper2sd <- rep(c(0), 3),
                             Other_lower2sd <- rep(c(0), 3)
)


# The column names came out weird. I fix it below

colnames(data_alignment) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")



# Filling data table with raw outcomes

data_alignment$Tib_means[1] <- mean(cohort_preop_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_means[2] <- mean(cohort_discharge_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_means[3] <- mean(cohort_12month_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_upper2sd[1] <- data_alignment$Tib_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_upper2sd[2] <- data_alignment$Tib_means[2] + 2*sd(cohort_discharge_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_upper2sd[3] <- data_alignment$Tib_means[3] + 2*sd(cohort_12month_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_lower2sd[1] <- data_alignment$Tib_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_lower2sd[2] <- data_alignment$Tib_means[2] - 2*sd(cohort_discharge_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_lower2sd[3] <- data_alignment$Tib_means[3] - 2*sd(cohort_12month_alignment[blounts_indicies & tib_indicies], na.rm = TRUE)

data_alignment$Other_means[1] <- mean(cohort_preop_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_means[2] <- mean(cohort_discharge_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_means[3] <- mean(cohort_12month_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_upper2sd[1] <- data_alignment$Other_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_upper2sd[2] <- data_alignment$Other_means[2] + 2*sd(cohort_discharge_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_upper2sd[3] <- data_alignment$Other_means[3] + 2*sd(cohort_12month_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_lower2sd[1] <- data_alignment$Other_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_lower2sd[2] <- data_alignment$Other_means[2] - 2*sd(cohort_discharge_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_lower2sd[3] <- data_alignment$Other_means[3] - 2*sd(cohort_12month_alignment[blounts_indicies & !(tib_indicies)], na.rm = TRUE)



# Plotting data from table

ggplot() +
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Alignment (mm)")+
  scale_x_discrete(limits = rev)





# We will do the same thing, but:
# 1. We will use absolute values.
# 2. We will use max/min instead of 2sds

data_alignment$Tib_means[1] <- mean(alignment_preop_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_means[2] <- mean(alignment_discharge_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_means[3] <- mean(alignment_12month_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_upper2sd[1] <- max(alignment_preop_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_upper2sd[2] <- max(alignment_discharge_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_upper2sd[3] <- max(alignment_12month_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_lower2sd[1] <- min(alignment_preop_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_lower2sd[2] <- min(alignment_discharge_dist[blounts_indicies & tib_indicies], na.rm = TRUE)
data_alignment$Tib_lower2sd[3] <- min(alignment_12month_dist[blounts_indicies & tib_indicies], na.rm = TRUE)

data_alignment$Other_means[1] <- mean(alignment_preop_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_means[2] <- mean(alignment_discharge_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_means[3] <- mean(alignment_12month_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_upper2sd[1] <- max(alignment_preop_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_upper2sd[2] <- max(alignment_discharge_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_upper2sd[3] <- max(alignment_12month_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_lower2sd[1] <- min(alignment_preop_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_lower2sd[2] <- min(alignment_discharge_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_alignment$Other_lower2sd[3] <- min(alignment_12month_dist[blounts_indicies & !(tib_indicies)], na.rm = TRUE)



# Plotting data from table, but with absolute values.

ggplot() +
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Alignment (mm)")+
  scale_x_discrete(limits = rev)






# In both plots, its clear that there was more variability at 12month for blounts treated with 
# tib elevation than other techniques.



# T-test of changes in final outcomes
# Do not differ
temp1 <- abs(alignment_12month_dist)[blounts_indicies & tib_indicies]
temp2 <- abs(alignment_12month_dist)[blounts_indicies & !tib_indicies]
t.test(temp1, temp2)


# T-test of changes in final outcomes
# Do not differ
temp1 <- abs(alignment_12month_dist - alignment_discharge_dist)[blounts_indicies & tib_indicies]
temp2 <- abs(alignment_12month_dist - alignment_discharge_dist)[blounts_indicies & !tib_indicies]
t.test(temp1, temp2)




# Now let's acquire the following data on the groups


# FOR TIB PLAT ELEVATION (absolute value)
#       0. sample size = 26
length(cohort_preop_alignment[blounts_indicies & tib_indicies])
#       1. preop means, max and mins for alignment (absolute value) 16.2380952, 26, 7
#       2. discharge means, max and mins for alignment (absolute value) 2.096154, 8, 0                             
#       3. 12 month means, max and mins for alignment (absolute value) 2.903846, 11, 0               
#       4. Proportion of IC at preop 26/26
length(cohort_preop_alignment[blounts_indicies & tib_indicies & IC_preop_indicies])
#       5. Proportion of IM at preop 0/21
#       6. Number of normal alignments at 12 month post op 8/26
length(cohort_12month_alignment[cohort_12month_alignment == 0 & tib_indicies & blounts_indicies])
#       7. Male to female ratio 7/26
length(cohort_SEX[cohort_SEX == "M" & blounts_indicies & tib_indicies])


# FOR NOT TIB PLAT ELEVATION (absolute value)
#       0. sample size = 12
length(cohort_preop_alignment[blounts_indicies & !(tib_indicies)])
#       1. preop means, max and mins for alignment (absolute value) 12.708333, 24, 3.5
#       2. discharge means, max and mins for alignment (absolute value) 1.416667, 4, 0                             
#       3. 12 month means, max and mins for alignment (absolute value) 2.750000, 4.5, 0               
#       4. Proportion of IC at preop 11/12
length(cohort_preop_alignment[blounts_indicies & !(tib_indicies) & IC_preop_indicies])
#       5. Proportion of IM at preop 1/12
#       6. Number of normal alignments at 12 month post op 2/12
length(cohort_12month_alignment[cohort_12month_alignment == 0 & !(tib_indicies) & blounts_indicies])
#       7. Male to female ratio 5:7
length(cohort_SEX[cohort_SEX == "M" & blounts_indicies & !(tib_indicies)])


# Linear models

# Did it influence alignment? (12 month)
technique_alignment_m1 <- lm(alignment_12month_dist[blounts_indicies] ~ factor(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies]) + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)[blounts_indicies] + alignment_preop_dist[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + as.numeric(cohort_AGE)[blounts_indicies] + as.numeric(cohort_BMI)[blounts_indicies])
summary(technique_alignment_m1)
print(vif(technique_alignment_m1))


# Did it influence mech axis?
technique_mechAxis_m1 <- lm(mechAxis_12month_dist[blounts_indicies] ~ factor(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies]) + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)[blounts_indicies] + mechAxis_preop_dist[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + as.numeric(cohort_AGE)[blounts_indicies] + as.numeric(cohort_BMI)[blounts_indicies])
summary(technique_mechAxis_m1)
print(vif(technique_mechAxis_m1))


# Did it influence mech axis deviation?
technique_MAD_m1 <- lm(MAD_12month_dist[blounts_indicies] ~ factor(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies]) + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)[blounts_indicies] + MAD_preop_dist[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + as.numeric(cohort_AGE)[blounts_indicies] + as.numeric(cohort_BMI)[blounts_indicies])
summary(technique_MAD_m1)
print(vif(technique_MAD_m1))


# Did it influence MPTA?
technique_MPTA_m1 <- lm(MPTA_12month_dist[blounts_indicies] ~ factor(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies]) + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)[blounts_indicies] + MPTA_preop_dist[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + as.numeric(cohort_AGE)[blounts_indicies] + as.numeric(cohort_BMI)[blounts_indicies])
summary(technique_MPTA_m1)
print(vif(technique_MPTA_m1))





# MECH AXIS


data_mechAxis <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                             Tib_means <- rep(c(0), 3),
                             Tib_upper2sd <- rep(c(0), 3),
                             Tib_lower2sd <- rep(c(0), 3),
                             Other_means <- rep(c(0), 3),
                             Other_upper2sd <- rep(c(0), 3),
                             Other_lower2sd <- rep(c(0), 3)
)

colnames(data_mechAxis) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")


data_mechAxis$Tib_means[1] <- mean(mechAxis_preop_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_means[2] <- mean(mechAxis_discharge_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_means[3] <- mean(mechAxis_12month_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_upper2sd[1] <- max(mechAxis_preop_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_upper2sd[2] <- max(mechAxis_discharge_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_upper2sd[3] <- max(mechAxis_12month_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_lower2sd[1] <- min(mechAxis_preop_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_lower2sd[2] <- min(mechAxis_discharge_dist[blounts_indicies & tib_indicies])
data_mechAxis$Tib_lower2sd[3] <- min(mechAxis_12month_dist[blounts_indicies & tib_indicies])

data_mechAxis$Other_means[1] <- mean(mechAxis_preop_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_means[2] <- mean(mechAxis_discharge_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_means[3] <- mean(mechAxis_12month_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_upper2sd[1] <- max(mechAxis_preop_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_upper2sd[2] <- max(mechAxis_discharge_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_upper2sd[3] <- max(mechAxis_12month_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_lower2sd[1] <- min(mechAxis_preop_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_lower2sd[2] <- min(mechAxis_discharge_dist[blounts_indicies & !(tib_indicies)])
data_mechAxis$Other_lower2sd[3] <- min(mechAxis_12month_dist[blounts_indicies & !(tib_indicies)])



# Plotting data from table, but with absolute values.

ggplot() +
  geom_line(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Mech Axis (mm)")+
  scale_x_discrete(limits = rev)




# MAD

data_MAD <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                            Tib_means <- rep(c(0), 3),
                            Tib_upper2sd <- rep(c(0), 3),
                            Tib_lower2sd <- rep(c(0), 3),
                            Other_means <- rep(c(0), 3),
                            Other_upper2sd <- rep(c(0), 3),
                            Other_lower2sd <- rep(c(0), 3)
)

colnames(data_MAD) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")


data_MAD$Tib_means[1] <- mean(MAD_preop_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_means[2] <- mean(MAD_discharge_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_means[3] <- mean(MAD_12month_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_upper2sd[1] <- max(MAD_preop_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_upper2sd[2] <- max(MAD_discharge_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_upper2sd[3] <- max(MAD_12month_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_lower2sd[1] <- min(MAD_preop_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_lower2sd[2] <- min(MAD_discharge_dist[blounts_indicies & tib_indicies])
data_MAD$Tib_lower2sd[3] <- min(MAD_12month_dist[blounts_indicies & tib_indicies])

data_MAD$Other_means[1] <- mean(MAD_preop_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_means[2] <- mean(MAD_discharge_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_means[3] <- mean(MAD_12month_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_upper2sd[1] <- max(MAD_preop_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_upper2sd[2] <- max(MAD_discharge_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_upper2sd[3] <- max(MAD_12month_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_lower2sd[1] <- min(MAD_preop_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_lower2sd[2] <- min(MAD_discharge_dist[blounts_indicies & !(tib_indicies)])
data_MAD$Other_lower2sd[3] <- min(MAD_12month_dist[blounts_indicies & !(tib_indicies)])



# Plotting data from table, but with absolute values.

ggplot() +
  geom_line(data = data_MAD, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_MAD, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_MAD, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_MAD, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_MAD, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_MAD, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "MAD (degrees)")+
  scale_x_discrete(limits = rev)






# MPTA


data_MPTA <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                       Tib_means <- rep(c(0), 3),
                       Tib_upper2sd <- rep(c(0), 3),
                       Tib_lower2sd <- rep(c(0), 3),
                       Other_means <- rep(c(0), 3),
                       Other_upper2sd <- rep(c(0), 3),
                       Other_lower2sd <- rep(c(0), 3)
)

colnames(data_MPTA) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")


data_MPTA$Tib_means[1] <- mean(MPTA_preop_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_means[2] <- mean(MPTA_discharge_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_means[3] <- mean(MPTA_12month_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_upper2sd[1] <- max(MPTA_preop_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_upper2sd[2] <- max(MPTA_discharge_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_upper2sd[3] <- max(MPTA_12month_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_lower2sd[1] <- min(MPTA_preop_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_lower2sd[2] <- min(MPTA_discharge_dist[blounts_indicies & tib_indicies])
data_MPTA$Tib_lower2sd[3] <- min(MPTA_12month_dist[blounts_indicies & tib_indicies])

data_MPTA$Other_means[1] <- mean(MPTA_preop_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_means[2] <- mean(MPTA_discharge_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_means[3] <- mean(MPTA_12month_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_upper2sd[1] <- max(MPTA_preop_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_upper2sd[2] <- max(MPTA_discharge_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_upper2sd[3] <- max(MPTA_12month_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_lower2sd[1] <- min(MPTA_preop_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_lower2sd[2] <- min(MPTA_discharge_dist[blounts_indicies & !(tib_indicies)])
data_MPTA$Other_lower2sd[3] <- min(MPTA_12month_dist[blounts_indicies & !(tib_indicies)])



# Plotting data from table, but with absolute values.

ggplot() +
  geom_line(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Absolute Distance from Normal MPTA (degrees)")+
  scale_x_discrete(limits = rev)

#############################################################################################
# Step 10: How did BMI, age, and sex affect outcomes?
#
#############################################################################################




### Analysis - Plots ###

# BMI and outcomes (raw and absolute distance from normal)

plot(cohort_BMI, cohort_12month_alignment, xlab = "BMI", ylab = "Alignment at 12 months (mm)")                     
plot(cohort_BMI, alignment_12month_dist, xlab = "BMI", ylab = "Distance from normal Alignment at 12 months (mm)") 
plot(cohort_BMI, cohort_12month_mechAxis, xlab = "BMI", ylab = "Mech Axis at 12 months (degrees)")                      
plot(cohort_BMI, mechAxis_12month_dist, xlab = "BMI", ylab = "Distance from normal Mech Axis at 12 months (degrees)")    
plot(cohort_BMI, cohort_12month_MAD, xlab = "BMI", ylab = "Mech Axis Deviation at 12 months (mm)")       
plot(cohort_BMI, MAD_12month_dist, xlab = "BMI", ylab = "Distance from normal Mech Axis Deviation at 12 months (mm)")         
plot(cohort_BMI, cohort_12month_MPTA, xlab = "BMI", ylab = "MPTA at 12 months (degrees)")    
plot(cohort_BMI, MPTA_12month_dist, xlab = "BMI", ylab = "Distance from normal at 12 months (mm)")



# AGE and outcomes (raw and absolute distance from normal)

plot(cohort_AGE, cohort_12month_alignment, xlab = "AGE", ylab = "Alignment at 12 months (mm)")
plot(cohort_AGE, alignment_12month_dist, xlab = "AGE", ylab = "Distance from normal Alignment at 12 months (mm)")
plot(cohort_AGE, cohort_12month_mechAxis, xlab = "AGE", ylab = "Mech Axis at 12 months (degrees)")
plot(cohort_AGE, mechAxis_12month_dist, xlab = "AGE", ylab = "Distance from normal Mech Axis at 12 months (degrees)")
plot(cohort_AGE, cohort_12month_MAD, xlab = "AGE", ylab = "Mech Axis Deviation at 12 months (mm)") 
plot(cohort_AGE, MAD_12month_dist, xlab = "AGE", ylab = "Distance from normal Mech Axis Deviation at 12 months (mm)")   
plot(cohort_AGE, cohort_12month_MPTA, xlab = "AGE", ylab = "MPTA at 12 months (degrees)")  
plot(cohort_AGE, MPTA_12month_dist, xlab = "AGE", ylab = "Distance from normal at 12 months (mm)")   




# SEX and outcomes (raw and absolute distance from normal)

plot(factor(cohort_SEX), cohort_12month_alignment, xlab = "SEX", ylab = "Alignment at 12 months (mm)") 
plot(factor(cohort_SEX), alignment_12month_dist, xlab = "SEX", ylab = "Distance from normal Alignment at 12 months (mm)")
plot(factor(cohort_SEX), cohort_12month_mechAxis, xlab = "SEX", ylab = "Mech Axis at 12 months (degrees)")  
plot(factor(cohort_SEX), mechAxis_12month_dist, xlab = "SEX", ylab = "Distance from normal Mech Axis at 12 months (degrees)")  
plot(factor(cohort_SEX), cohort_12month_MAD, xlab = "SEX", ylab = "Mech Axis Deviation at 12 months (mm)")
plot(factor(cohort_SEX), MAD_12month_dist, xlab = "SEX", ylab = "Distance from normal Mech Axis Deviation at 12 months (mm)") 
plot(factor(cohort_SEX), cohort_12month_MPTA, xlab = "SEX", ylab = "MPTA at 12 months (degrees)")  
plot(factor(cohort_SEX), MPTA_12month_dist, xlab = "SEX", ylab = "Distance from normal at 12 months (mm)") 


### Analysis - Plots ###



### Analysis - Linear Models - Raw Outcomes ###


# Model Alignment
#     Model is not significant

model_alignment <- lm(cohort_12month_alignment ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_alignment$residuals)
plot(fitted(model_alignment), resid(model_alignment))
abline(a = 0, b = 0)
summary(model_alignment)

# Model Alignment w/ preop alignment
#     Model is significant (p-value = 1.739e-05)
#     Only preop_alignment was a significant variable. BMI did not matter at all.

model_alignment_2 <- lm(cohort_12month_alignment ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + cohort_preop_alignment)
qqnorm(model_alignment_2$residuals)
plot(fitted(model_alignment_2), resid(model_alignment_2))
abline(a = 0, b = 0)
summary(model_alignment_2)




# Mech Axis
#     Model is significant (p-value = 0.003357)
#     BMI is significant (positive) and age is significant (negative)

model_mechAxis <- lm(cohort_12month_mechAxis ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_mechAxis$residuals)
plot(fitted(model_mechAxis), resid(model_mechAxis))
abline(a = 0, b = 0)
summary(model_mechAxis)

# Mech Axis w/ preop Mech Axis value
#     Model is significant (p-value = 0.0002455)
#     preop_mechAxis value is significant (positive) and BMI/AGE are almost significant (just abve 0.05)
model_mechAxis_2 <- lm(cohort_12month_mechAxis ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + cohort_preop_mechAxis)
qqnorm(model_mechAxis_2$residuals)
plot(fitted(model_mechAxis_2), resid(model_mechAxis_2))
abline(a = 0, b = 0)
summary(model_mechAxis_2)




# Mech Axis Deviation (MAD)
#     Model is significant (p-value = 0.01046)
#     BMI is significant (positive) and age is significant (negative)
model_MAD <- lm(cohort_12month_MAD ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MAD$residuals)
plot(fitted(model_MAD), resid(model_MAD))
abline(a = 0, b = 0)
summary(model_MAD)

# Mech Axis Deviation (MAD) w/ preop MAD value
#     Model is significant (p-value = p-value: 0.01049)
#     BMI is significant (positive) and AGE is significant (negative)
model_MAD_2 <- lm(cohort_12month_MAD ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + cohort_preop_MAD)
qqnorm(model_MAD_2$residuals)
plot(fitted(model_MAD_2), resid(model_MAD_2))
abline(a = 0, b = 0)
summary(model_MAD_2)




# MPTA
#     Model is significant (p-value = 0.001129)
#     BMI is significant (negative) and AGE is significant (negative)
model_MPTA <- lm(cohort_12month_MPTA ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MPTA$residuals)
plot(fitted(model_MPTA), resid(model_MPTA))
abline(a = 0, b = 0)
summary(model_MPTA)

# MPTA w/ preop MTPA
#     Model is significant (p-value = 0.0004387)
#     AGE is significant (negative) and Preop_MPTA is almost significant (positive)
model_MPTA_2 <- lm(cohort_12month_MPTA ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + cohort_preop_MPTA)
qqnorm(model_MPTA_2$residuals)
plot(fitted(model_MPTA_2), resid(model_MPTA_2))
abline(a = 0, b = 0)
summary(model_MPTA_2)


### Analysis - Linear Models - Raw Outcomes ###




### Analysis - Linear Models - Absolute value from normal ###

# Alignment
#     Model is not significant (p-value = 0.09385)
#     
model_alignment_dist <- lm(alignment_12month_dist ~ cohort_BMI + factor(cohort_SEX) + as.numeric(cohort_AGE))
qqnorm(model_alignment_dist$residuals)
plot(fitted(model_alignment_dist), resid(model_alignment_dist))
abline(a = 0, b = 0)
summary(model_alignment_dist)

# Alignment w/ preop alignment
#     Model is not significant (p-value = 0.003351)
#     preop alignment distance is significant (positive) and AGE is almost significant (just above 0.05)
#     
model_alignment_dist_2 <- lm(alignment_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + alignment_preop_dist)
qqnorm(model_alignment_dist_2$residuals)
plot(fitted(model_alignment_dist_2), resid(model_alignment_dist_2))
abline(a = 0, b = 0)
summary(model_alignment_dist_2)



# Mech Axis
#     Model is significant (p-value = 0.003357)
#     BMI is significant (positive) and age is significant (negative)
model_mechAxis_dist <- lm(mechAxis_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_mechAxis_dist$residuals)
plot(fitted(model_mechAxis_dist), resid(model_mechAxis_dist))
abline(a = 0, b = 0)
summary(model_mechAxis_dist)

# Mech Axis w/ preop mech Axis
#     Model is significant (p-value = 0.0002455)
#     MechAxis preop dist is significant (positive) and BMI/AGE are almost significant (Just above 0.05)
model_mechAxis_dist_2 <- lm(mechAxis_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + mechAxis_preop_dist)
qqnorm(model_mechAxis_dist_2$residuals)
plot(fitted(model_mechAxis_dist_2), resid(model_mechAxis_dist_2))
abline(a = 0, b = 0)
summary(model_mechAxis_dist_2)



# Mech Axis Deviation (MAD)
#     Model is significant (p-value = 0.001148)
#     BMI is significant (positive) and AGE is significant (negative)
model_MAD_dist <- lm(MAD_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MAD_dist$residuals)
plot(fitted(model_MAD_dist), resid(model_MAD_dist))
abline(a = 0, b = 0)
summary(model_MAD_dist)

# Mech Axis Deviation (MAD) w/ preop MAD values
#     Model is significant (p-value = 0.0009156)
#     BMI is significant (positive) and AGE is significant (negative)
model_MAD_dist_2 <- lm(MAD_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + MAD_preop_dist)
qqnorm(model_MAD_dist_2$residuals)
plot(fitted(model_MAD_dist_2), resid(model_MAD_dist_2))
abline(a = 0, b = 0)
summary(model_MAD_dist_2)



# MPTA
#     Model is not significant

model_MPTA_dist <- lm(MPTA_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MPTA_dist$residuals)
plot(fitted(model_MPTA_dist), resid(model_MPTA_dist))
abline(a = 0, b = 0)
summary(model_MPTA_dist)

# MPTA w/ preop MPTA
#     Model is not significant

model_MPTA_dist_2 <- lm(MPTA_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MPTA_dist_2$residuals)
plot(fitted(model_MPTA_dist_2), resid(model_MPTA_dist_2))
abline(a = 0, b = 0)
summary(model_MPTA_dist_2)

### Actual Analysis - Linear Models - Absolute value from normal ###




### Thoughts ###
#

####################################################################################################
# Step 11: How did QOL correlate with (absolute distance from normal outcome) OR (absolute change in outcome)
####################################################################################################



### Analysis - How did QOL scores correlate with change in absolute distance from normal range? ###

# Getting QOL scores + change in QOL scores
QOL_sum_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL)
QOL_sum_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL)
delta_QOL <- QOL_sum_12month - QOL_sum_preop




# Getting change in distance from normal range for outcomes
delta_alignment <- alignment_preop_dist - alignment_12month_dist
delta_mechAxis <- mechAxis_preop_dist - mechAxis_12month_dist
delta_MAD <- MAD_preop_dist - MAD_12month_dist
delta_MPTA <- MPTA_preop_dist - MPTA_12month_dist



# Plots 1: QOL scores vs 12 month distance
# QOL scores don't seem to correlate with 12 month distance
plot(QOL_sum_12month, alignment_12month_dist)
plot(QOL_sum_12month, mechAxis_12month_dist)
plot(QOL_sum_12month, MAD_12month_dist)
plot(QOL_sum_12month, MPTA_12month_dist)



# Plots 2: QOL scores vs change in distance from preop to 12month
# QOL scores don't seem to correlate with change in distance from preop to 12 month
plot(QOL_sum_12month, delta_alignment)
plot(QOL_sum_12month, delta_mechAxis)
plot(QOL_sum_12month, delta_MAD)
plot(QOL_sum_12month, delta_MPTA)



# Plots 3: change in QOL scores vs 12 month distance
# The change in QOL doesn't seem to correlate with 12 month distance
plot(delta_QOL, alignment_12month_dist)
plot(delta_QOL, mechAxis_12month_dist)
plot(delta_QOL, MAD_12month_dist)
plot(delta_QOL, MPTA_12month_dist)



# Plots 4: change in QOL scores vs change in distance from preop to 12 month
# The change in QOL doesn't seem to correlate with the change in distance to normal range
plot(delta_QOL, delta_alignment)
plot(delta_QOL, delta_mechAxis)
plot(delta_QOL, delta_MAD)
plot(delta_QOL, delta_MPTA)


# Plots 5: QOL scores vs Preop Distance from Normal Range
plot(QOL_sum_preop, alignment_preop_dist)
plot(QOL_sum_preop, )


# Plots 6: QOL scores vs Raw Preop Values
plot(QOL_sum_preop, cohort_preop_alignment)


### Analysis - How did QOL scores correlate with change in absolute distance from normal range? ###



### Thoughts ###

# It doesn't seem like QOL scores/change in QOL scores correlate well
# with outcomes/change in outcomes. Weird. 


####################################################################################################
# Step 12: Were changes in ALIGNMENT sustained from discharge to 12month?
####################################################################################################


# PLOTTING CHANGE IN ALIGNMENT FOR EACH PATIENT
################################################
discharge_12month_alignment <- data.frame(alignment = c(cohort_discharge_alignment, cohort_12month_alignment), 
                                          StagesOfCare = c( c(rep("Discharge", length(cohort_discharge_alignment))), c(rep("12Month", length(cohort_12month_alignment)))),
                                          paired_points = c(1:length(cohort_discharge_alignment)), 1:length(cohort_12month_alignment))
discharge_12month_alignment$StagesOfCare <- factor(discharge_12month_alignment$StagesOfCare, levels = c("Discharge", "12Month"))
ggplot(discharge_12month_alignment, aes(x = StagesOfCare, y = alignment)) +
  geom_point(aes(color = StagesOfCare)) +
  geom_line(aes(group = paired_points)) +
  labs(x = "Stage of Care", y = "Alignment (cm)")




# MEAN CHANGE IN ALIGNMENT
#############################
# raw values
mean(cohort_12month_alignment - cohort_discharge_alignment, na.rm = TRUE)
# absolute value
mean(abs(cohort_12month_alignment - cohort_discharge_alignment), na.rm = TRUE)





# DID THE DIAGNOSIS IMPACT CHANGE IN ALIGNMENT?
################################################
#12 Month
boxplot((alignment_12month_dist)[blounts_indicies],
        (alignment_12month_dist)[rickets_indicies],
        (alignment_12month_dist)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Absolute Distance From Normal Alignment (cm)")
# raw values
boxplot((cohort_12month_alignment - cohort_discharge_alignment)[blounts_indicies],
        (cohort_12month_alignment - cohort_discharge_alignment)[rickets_indicies],
        (cohort_12month_alignment - cohort_discharge_alignment)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Change in Alignment from Discharge to 12month (mm)")
# absolute values
boxplot(abs(cohort_12month_alignment - cohort_discharge_alignment)[blounts_indicies],
        abs(cohort_12month_alignment - cohort_discharge_alignment)[rickets_indicies],
        abs(cohort_12month_alignment - cohort_discharge_alignment)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        ylab = "Absolute Change in Alignment - Discharge to 12month (cm)")
# BLOUNTS HAD MORE POSITIVE CHANGE THAN RICKETS, BUT IN TERMS OF ABSOLUTE CHANGE, THEY DIDN'T
# DIFFER SIGNIFICANTLY




# DID LENGTH OF TIME IN CAST AFFECT CHANGE IN ALIGNMENT?
#########################################################
# raw values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_alignment - cohort_discharge_alignment,
     xlab = "Days in Cast Since First Day of Surgery",
     ylab = "Change in Alignment from Discharge to 12month (mm)")

# absolute values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, abs(cohort_12month_alignment - cohort_discharge_alignment),
     xlab = "Days in Cast Since First Day of Surgery",
     ylab = "Absolute Change in Alignment from Discharge to 12month (cm)")


# Linear model absolute change in alignment and days in cast
m_dayscast_alignment <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
summary(m_dayscast_alignment)
# THE PLOT SUGGESTS AS THE DAYS INCREASE, THE CHANGE DECREASES (LOOK AT CLUSTERING AROUND 0-2).
# HOWEVER, THE MODEL IS FAR FROM SIGNIFICANT.




# DID SURGICAL TECHNIQUE AFFECT CHANGE IN ALIGNMENT? (TIB ELEVATION VS OTHER - BLOUNTS ONLY)
############################################################################################
# This plot is from Step 9, be sure to run it before running this code.
ggplot() +
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Alignment (cm)")+
  scale_x_discrete(limits = rev)

change_alignment_tib <- cohort_12month_alignment[blounts_indicies & tib_indicies] - cohort_discharge_alignment[blounts_indicies & tib_indicies]
change_alignment_not_tib <- cohort_12month_alignment[blounts_indicies & !(tib_indicies)] - cohort_discharge_alignment[blounts_indicies & !(tib_indicies)]
boxplot(change_alignment_tib, change_alignment_not_tib,
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Change in Alignment from Discharge to 12Month (cm)")
t.test(change_alignment_tib, change_alignment_not_tib)
# GIVEN THE PLOT, BOXPLOT, AND T-TEST, IT IS POSSIBLE THAT TIB ELEVATION COULD HAVE LED TO 
# BETTER MAINTAINED CHANGES, BUT IT IS NOT STATISTICALLY SIGNIFICANT.
change_alignment_tib1 <- alignment_12month_dist[blounts_indicies & tib_indicies] - alignment_discharge_dist[blounts_indicies & tib_indicies]
change_alignment_not_tib1 <- alignment_12month_dist[blounts_indicies & !(tib_indicies)] - alignment_discharge_dist[blounts_indicies & !(tib_indicies)]
boxplot(abs(change_alignment_tib), abs(change_alignment_not_tib),
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Change in Alignment from Discharge to 12Month (cm)")




# DID BMI AFFECT CHANGE IN ALIGNMENT?
#######################################
# raw values
plot(cohort_BMI, cohort_12month_alignment - cohort_discharge_alignment,
     xlab = "BMI",
     ylab = "Change in Alignment from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_BMI, abs(cohort_12month_alignment - cohort_discharge_alignment),
     xlab = "BMI",
     ylab = "Absolute Change in Alignment from Discharge to 12 Month (degrees)")
# linear model for absolute value
m_BMI_alignment <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ cohort_BMI)
summary(m_BMI_alignment)
# GIVEN THE PLOT AND MODEL, I WOULD SAY NO.





# DID AGE AFFECT CHANGE IN ALIGNMENT?
#######################################
# raw values
plot(cohort_AGE, cohort_12month_alignment - cohort_discharge_alignment,
     xlab = "AGE",
     ylab = "Change in Alignment from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_AGE, abs(cohort_12month_alignment - cohort_discharge_alignment),
     xlab = "AGE",
     ylab = "Absolute Change in Alignment from Discharge to 12 Month (degrees)")
# linear model for absolute value
m_AGE_alignment <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ as.numeric(cohort_AGE))
summary(m_AGE_alignment)
# GIVEN THE PLOT AND MODEL, I WOULD SAY YES. THE OLDER THE PATIENT, THE GREATER LIKELIHOOD
# OF SUSTAINED CHANGES.





# DID SEX AFFECT CHANGE IN ALIGNMENT?
######################################
boxplot((cohort_12month_alignment - cohort_discharge_alignment)[cohort_assess_preop$Sex == "M"],
        (cohort_12month_alignment - cohort_discharge_alignment)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in Alignment from Discharge to 12Month (mm)")
# GIVEN THE BOXPLOT, I WOULD DEFINITELY SAY NO


boxplot((alignment_12month_dist)[cohort_assess_preop$Sex == "M"],
        (alignment_12month_dist)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in Alignment from Discharge to 12Month (mm)")



boxplot((alignment_preop_dist)[cohort_assess_preop$Sex == "M"],
        (alignment_preop_dist)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in Alignment from Discharge to 12Month (mm)")



# HOW DO BMI, AGE, AND SEX INTERACT IN A LINAER MODEL WITH CHANGE IN ALIGNMENT?
################################################################################
# absolute values
m_DEMO_alignment <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$Sex) + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_assess_preop$DIagnosis))
summary(m_DEMO_alignment)
# GIVEN THE MODEL, IT SEEMS LIKE AGE MATTERS. 




####################################################################################################
# Step 13: Were changes in MECH AXIS sustained from discharge to 12month?
####################################################################################################


# PLOTTING CHANGE IN MECH AXIS FOR EACH PATIENT
################################################
discharge_12month_mechAxis <- data.frame(mechAxis = c(cohort_discharge_mechAxis, cohort_12month_mechAxis), 
                                         StagesOfCare = c( c(rep("Discharge", length(cohort_discharge_mechAxis))), c(rep("12Month", length(cohort_12month_mechAxis)))),
                                         paired_points = c(1:length(cohort_discharge_mechAxis)), 1:length(cohort_12month_mechAxis))
discharge_12month_mechAxis$StagesOfCare <- factor(discharge_12month_mechAxis$StagesOfCare, levels = c("Discharge", "12Month"))
ggplot(discharge_12month_mechAxis, aes(x = StagesOfCare, y = mechAxis)) +
  geom_point(aes(color = StagesOfCare)) +
  geom_line(aes(group = paired_points)) +
  labs(x = "Stage of Care", y = "Mech Axis (degrees)")




# MEAN CHANGE IN MECH AXIS
#############################
# raw values
mean(cohort_12month_mechAxis - cohort_discharge_mechAxis, na.rm = TRUE)
# absolute change
mean(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis), na.rm = TRUE)





# DID THE DIAGNOSIS IMPACT CHANGE IN MECH AXIS?
################################################
#12 Month
boxplot((mechAxis_12month_dist)[blounts_indicies],
        (mechAxis_12month_dist)[rickets_indicies],
        (mechAxis_12month_dist)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Absolute Distance From Normal Mech Axis (cm)")
# raw values
boxplot((cohort_12month_mechAxis - cohort_discharge_mechAxis)[blounts_indicies],
        (cohort_12month_mechAxis - cohort_discharge_mechAxis)[rickets_indicies],
        (cohort_12month_mechAxis - cohort_discharge_mechAxis)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Change in Mech Axis from Discharge to 12month (degrees)")
# absolute values
boxplot(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis)[blounts_indicies],
        abs(cohort_12month_mechAxis - cohort_discharge_mechAxis)[rickets_indicies],
        abs(cohort_12month_mechAxis - cohort_discharge_mechAxis)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        ylab = "Absolute Change in Mech Axis - Discharge to 12month (mm)")
# BLOUNTS HAD MORE POSITIVE CHANGE THAN RICKETS, HOWEVER IN TERMS OF ABSOLUTE CHANGE,
# THE TWO GROUPS WERE ABOUT THE SAME




# DID LENGTH OF TIME IN CAST AFFECT CHANGE IN MECH AXIS?
#########################################################
# raw values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_mechAxis - cohort_discharge_mechAxis,
     xlab = "Days in Cast Since First Day of Surgery",
     ylab = "Change in Mech Axis from Discharge to 12 Month (degrees)")
# absolute value
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, abs(cohort_12month_mechAxis - cohort_discharge_mechAxis),
     xlab = "Days in Cast Since First Day of Surgery",
     ylab = "Absolute Change in Mech Axis from Discharge to 12 Month (mm)")
m_dayscast_mechAxis <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
summary(m_dayscast_mechAxis)
# GIVEN THE PLOT AND MODEL, IT DOESN'T LOOK LIKE TIME IN CAST AFFECTS MECH AXIS CHANGE




# DID SURGICAL TECHNIQUE AFFECT CHANGE IN MECH AXIS? (TIB ELEVATION VS OTHER - BLOUNTS ONLY)
############################################################################################
# This plot is from Step 9, be sure to run it before running this code.
# We will make a table to contain mean and sd values for mechAxis for each type of surgery

data_mechAxis <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                            Tib_means <- rep(c(0), 3),
                            Tib_upper2sd <- rep(c(0), 3),
                            Tib_lower2sd <- rep(c(0), 3),
                            Other_means <- rep(c(0), 3),
                            Other_upper2sd <- rep(c(0), 3),
                            Other_lower2sd <- rep(c(0), 3)
)


# The column names came out weird. I fix it below

colnames(data_mechAxis) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")



# Filling data table with raw outcomes

data_mechAxis$Tib_means[1] <- mean(cohort_preop_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_means[2] <- mean(cohort_discharge_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_means[3] <- mean(cohort_12month_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_upper2sd[1] <- data_mechAxis$Tib_means[1] + 2*sd(cohort_preop_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_upper2sd[2] <- data_mechAxis$Tib_means[2] + 2*sd(cohort_discharge_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_upper2sd[3] <- data_mechAxis$Tib_means[3] + 2*sd(cohort_12month_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_lower2sd[1] <- data_mechAxis$Tib_means[1] - 2*sd(cohort_preop_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_lower2sd[2] <- data_mechAxis$Tib_means[2] - 2*sd(cohort_discharge_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)
data_mechAxis$Tib_lower2sd[3] <- data_mechAxis$Tib_means[3] - 2*sd(cohort_12month_mechAxis[blounts_indicies & tib_indicies], na.rm = TRUE)

data_mechAxis$Other_means[1] <- mean(cohort_preop_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_means[2] <- mean(cohort_discharge_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_means[3] <- mean(cohort_12month_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_upper2sd[1] <- data_mechAxis$Other_means[1] + 2*sd(cohort_preop_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_upper2sd[2] <- data_mechAxis$Other_means[2] + 2*sd(cohort_discharge_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_upper2sd[3] <- data_mechAxis$Other_means[3] + 2*sd(cohort_12month_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_lower2sd[1] <- data_mechAxis$Other_means[1] - 2*sd(cohort_preop_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_lower2sd[2] <- data_mechAxis$Other_means[2] - 2*sd(cohort_discharge_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_mechAxis$Other_lower2sd[3] <- data_mechAxis$Other_means[3] - 2*sd(cohort_12month_mechAxis[blounts_indicies & !(tib_indicies)], na.rm = TRUE)



# Plotting data from table

ggplot() +
  geom_line(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_mechAxis, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Mech Axis (degrees)")+
  scale_x_discrete(limits = rev)


change_mechAxis_tib <- cohort_12month_mechAxis[blounts_indicies & tib_indicies] - cohort_discharge_mechAxis[blounts_indicies & tib_indicies]
change_mechAxis_not_tib <- cohort_12month_mechAxis[blounts_indicies & !(tib_indicies)] - cohort_discharge_mechAxis[blounts_indicies & !(tib_indicies)]
boxplot(abs(change_mechAxis_tib), abs(change_mechAxis_not_tib),
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Absolute Change in Mech Axis from Discharge to 12Month (mm)")
t.test(change_mechAxis_tib, change_mechAxis_not_tib)
# GIVEN THE PLOT, BOXPLOT, AND T-TEST, IT IS POSSIBLE THAT TIB ELEVATION COULD HAVE LED TO 
# BETTER MAINTAINED CHANGES, BUT IT IS NOT STATISTICALLY SIGNIFICANT.




# DID BMI AFFECT CHANGE IN MECH AXIS?
#######################################
# raw values
plot(cohort_BMI, cohort_12month_mechAxis - cohort_discharge_mechAxis,
     xlab = "BMI",
     ylab = "Change in Mech Axis from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_BMI, abs(cohort_12month_mechAxis - cohort_discharge_mechAxis),
     xlab = "BMI",
     ylab = "Absolute Change in Mech Axis from Discharge to 12Month (degrees)")
m_BMI_mechAxis <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ cohort_BMI)
summary(m_BMI_mechAxis)
# GIVEN THE PLOT AND MODEL, I WOULD SAY NO.





# DID AGE AFFECT CHANGE IN MECH AXIS?
#######################################
# raw values
plot(cohort_AGE, cohort_12month_mechAxis - cohort_discharge_mechAxis,
     xlab = "AGE",
     ylab = "Absolute Change in Mech Axis from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_AGE, abs(cohort_12month_mechAxis - cohort_discharge_mechAxis),
     xlab = "AGE",
     ylab = "Change in Mech Axis from Discharge to 12 Month (degrees)")
m_AGE_mechAxis <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ as.numeric(cohort_AGE))
summary(m_AGE_mechAxis)
# GIVEN THE PLOT IT LOOKS LIKE YOUNGER PEOPLE TENDED TO HAVE MORE EXTREME CHANGES.
# HOWEVER, LINEAR MODEL IS NOT STATISTICALLY SIGNIFICANT.





# DID SEX AFFECT CHANGE IN MECH AXIS?
######################################
boxplot((cohort_12month_mechAxis - cohort_discharge_mechAxis)[cohort_assess_preop$Sex == "M"],
        (cohort_12month_mechAxis - cohort_discharge_mechAxis)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in Mech Axis from Discharge to 12Month (degrees)")
# GIVEN THE BOXPLOT, I WOULD DEFINITELY SAY NO




# HOW DO BMI, AGE, AND SEX INTERACT IN A LINAER MODEL WITH CHANGE IN MECH AXIS?
################################################################################
m_DEMO_mechAxis <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_assess_preop$Sex) + cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$DIagnosis))
summary(m_DEMO_mechAxis)
# NOT STATISTICALLY SIGNIFICANT.




####################################################################################################
# Step 14: Were changes in MAD sustained from discharge to 12month?
####################################################################################################


# PLOTTING CHANGE IN MAD FOR EACH PATIENT
################################################
discharge_12month_MAD <- data.frame(MAD = c(cohort_discharge_MAD, cohort_12month_MAD), 
                                    StagesOfCare = c( c(rep("Discharge", length(cohort_discharge_MAD))), c(rep("12Month", length(cohort_12month_MAD)))),
                                    paired_points = c(1:length(cohort_discharge_MAD)), 1:length(cohort_12month_MAD))
discharge_12month_MAD$StagesOfCare <- factor(discharge_12month_MAD$StagesOfCare, levels = c("Discharge", "12Month"))
ggplot(discharge_12month_MAD, aes(x = StagesOfCare, y = MAD)) +
  geom_point(aes(color = StagesOfCare)) +
  geom_line(aes(group = paired_points)) +
  labs(x = "Stage of Care", y = "MAD (cm)")




# MEAN CHANGE IN MAD
#############################
# raw values
mean(cohort_12month_MAD - cohort_discharge_MAD, na.rm = TRUE)
# absolute value
mean(abs(cohort_12month_MAD - cohort_discharge_MAD), na.rm = TRUE)






# DID THE DIAGNOSIS IMPACT CHANGE IN MAD?
################################################
#12 Month
boxplot((MAD_12month_dist)[blounts_indicies],
        (MAD_12month_dist)[rickets_indicies],
        (MAD_12month_dist)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Absolute Distance From Normal MAD (mm)")
# raw values
boxplot((cohort_12month_MAD - cohort_discharge_MAD)[blounts_indicies],
        (cohort_12month_MAD - cohort_discharge_MAD)[rickets_indicies],
        (cohort_12month_MAD - cohort_discharge_MAD)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Change in MAD from Discharge to 12month (mm)")
# absolute values
boxplot(abs(cohort_12month_MAD - cohort_discharge_MAD)[blounts_indicies],
        abs(cohort_12month_MAD - cohort_discharge_MAD)[rickets_indicies],
        abs(cohort_12month_MAD - cohort_discharge_MAD)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        ylab = "Absolute Change in MAD from Discharge to 12month (cm)")
# BLOUNTS HAD MORE POSITIVE CHANGE WHILE RICKETS HAD MORE NEGATIVE
# IN TERMS OF ABSOLUTE CHANGE, THE TWO GROUPS LOOK ABOUT THE SAME




# DID LENGTH OF TIME IN CAST AFFECT CHANGE IN MAD?
#########################################################
# raw values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MAD - cohort_discharge_MAD,
     xlab = "Days in Cast From ",
     ylab = "Change in MPTA from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, abs(cohort_12month_MAD - cohort_discharge_MAD),
     ylab = "Absolute Change in MAD from Discharge to 12Month (cm)",
     xlab = "Days in Cast")
# linear model
m_dayscast_MAD <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
summary(m_dayscast_MAD)
# GIVEN THE PLOT, I WOULD SAY NO.





# DID SURGICAL TECHNIQUE AFFECT CHANGE IN MAD? (TIB ELEVATION VS OTHER - BLOUNTS ONLY)
############################################################################################
data_MAD <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                       Tib_means <- rep(c(0), 3),
                       Tib_upper2sd <- rep(c(0), 3),
                       Tib_lower2sd <- rep(c(0), 3),
                       Other_means <- rep(c(0), 3),
                       Other_upper2sd <- rep(c(0), 3),
                       Other_lower2sd <- rep(c(0), 3)
)


# The column names came out weird. I fix it below

colnames(data_MAD) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")


data_MAD$Tib_means[1] <- mean(cohort_preop_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_means[2] <- mean(cohort_discharge_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_means[3] <- mean(cohort_12month_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_upper2sd[1] <- data_MAD$Tib_means[1] + 2*sd(cohort_preop_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_upper2sd[2] <- data_MAD$Tib_means[2] + 2*sd(cohort_discharge_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_upper2sd[3] <- data_MAD$Tib_means[3] + 2*sd(cohort_12month_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_lower2sd[1] <- data_MAD$Tib_means[1] - 2*sd(cohort_preop_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_lower2sd[2] <- data_MAD$Tib_means[2] - 2*sd(cohort_discharge_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MAD$Tib_lower2sd[3] <- data_MAD$Tib_means[3] - 2*sd(cohort_12month_MAD[blounts_indicies & tib_indicies], na.rm = TRUE)

data_MAD$Other_means[1] <- mean(cohort_preop_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_means[2] <- mean(cohort_discharge_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_means[3] <- mean(cohort_12month_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_upper2sd[1] <- data_MAD$Other_means[1] + 2*sd(cohort_preop_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_upper2sd[2] <- data_MAD$Other_means[2] + 2*sd(cohort_discharge_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_upper2sd[3] <- data_MAD$Other_means[3] + 2*sd(cohort_12month_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_lower2sd[1] <- data_MAD$Other_means[1] - 2*sd(cohort_preop_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_lower2sd[2] <- data_MAD$Other_means[2] - 2*sd(cohort_discharge_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MAD$Other_lower2sd[3] <- data_MAD$Other_means[3] - 2*sd(cohort_12month_MAD[blounts_indicies & !(tib_indicies)], na.rm = TRUE)



# Plotting data from table

ggplot() +
  geom_line(data = data_MAD, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_MAD, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_MAD, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_MAD, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_MAD, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_MAD, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Mech Axis (degrees)")+
  scale_x_discrete(limits = rev)


change_MAD_tib <- cohort_12month_MAD[blounts_indicies & tib_indicies] - cohort_discharge_MAD[blounts_indicies & tib_indicies]
change_MAD_not_tib <- cohort_12month_MAD[blounts_indicies & !(tib_indicies)] - cohort_discharge_MAD[blounts_indicies & !(tib_indicies)]
boxplot(abs(change_MAD_tib), abs(change_MAD_not_tib),
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Absolute Change in MAD from Discharge to 12 Month (mm)")
t.test(change_MAD_tib, change_MAD_not_tib)
# GIVEN THE PLOT, BOXPLOT, AND T-TEST, IT IS POSSIBLE THAT TIB ELEVATION COULD HAVE LED TO 
# BETTER MAINTAINED CHANGES, BUT IT IS NOT STATISTICALLY SIGNIFICANT.




# DID BMI AFFECT CHANGE IN MAD?
#######################################
# raw values
plot(cohort_BMI, cohort_12month_MAD - cohort_discharge_MAD,
     xlab = "BMI",
     ylab = "Change in MAD from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_BMI, abs(cohort_12month_MAD - cohort_discharge_MAD),
     xlab = "BMI",
     ylab = "Change in MAD from Discharge to 12 Month (degrees)")
m_BMI_MAD <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ cohort_BMI)
summary(m_BMI_MAD)
# GIVEN THE PLOT AND MODEL, I WOULD SAY NO.





# DID AGE AFFECT CHANGE IN MAD?
#######################################
# raw values
plot(cohort_AGE, cohort_12month_MAD - cohort_discharge_MAD,
     xlab = "AGE",
     ylab = "Change in MPTA from Discharge to 12 Month (degrees)")
# absolute values
plot(cohort_AGE, abs(cohort_12month_MAD - cohort_discharge_MAD),
     xlab = "AGE",
     ylab = "Change in MPTA from Discharge to 12 Month (degrees)")
m_AGE_MAD <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ as.numeric(cohort_AGE))
summary(m_AGE_MAD)
# GIVEN THE PLOT AND MODEL, I WOULD SAY NO





# DID SEX AFFECT CHANGE IN MAD?
######################################
boxplot((cohort_12month_MAD - cohort_discharge_MAD)[cohort_assess_preop$Sex == "M"],
        (cohort_12month_MAD - cohort_discharge_MAD)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in MAD from Discharge to 12Month (cm)")
# GIVEN THE BOXPLOT, I WOULD DEFINITELY SAY NO




# HOW DO BMI, AGE, AND SEX INTERACT IN A LINAER MODEL WITH CHANGE IN MAD?
################################################################################
m_DEMO_MAD <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) +  factor(cohort_assess_preop$Sex) + cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$DIagnosis))
summary(m_DEMO_MAD)
# NOT WELL, DOESN'T LOOK LIKE THEY HAVE ANY AFFECT ON CHANGE IN MAD






####################################################################################################
# Step 15: Were changes in MPTA sustained from discharge to 12month?
####################################################################################################


# PLOTTING CHANGE IN MPTA FOR EACH PATIENT
################################################
discharge_12month_MPTA <- data.frame(MPTA = c(cohort_discharge_MPTA, cohort_12month_MPTA), 
                                     StagesOfCare = c( c(rep("Discharge", length(cohort_discharge_MPTA))), c(rep("12Month", length(cohort_12month_MPTA)))),
                                     paired_points = c(1:length(cohort_discharge_MPTA)), 1:length(cohort_12month_MPTA))
discharge_12month_MPTA$StagesOfCare <- factor(discharge_12month_MPTA$StagesOfCare, levels = c("Discharge", "12Month"))
ggplot(discharge_12month_MPTA, aes(x = StagesOfCare, y = MPTA)) +
  geom_point(aes(color = StagesOfCare)) +
  geom_line(aes(group = paired_points)) +
  labs(x = "Stage of Care", y = "MPTA (degrees)")




# MEAN CHANGE IN MPTA
#############################
# raw value
mean(cohort_12month_MPTA - cohort_discharge_MPTA, na.rm = TRUE)
# absolute value
mean(abs(cohort_12month_MPTA - cohort_discharge_MPTA), na.rm = TRUE)





# DID THE DIAGNOSIS IMPACT CHANGE IN MPTA?
################################################
# raw values
boxplot((cohort_12month_MPTA - cohort_discharge_MPTA)[blounts_indicies],
        (cohort_12month_MPTA - cohort_discharge_MPTA)[rickets_indicies],
        (cohort_12month_MPTA - cohort_discharge_MPTA)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Change in MPTA from Discharge to 12Month (degrees)")
# absolute values
boxplot(abs(cohort_12month_MPTA - cohort_discharge_MPTA)[blounts_indicies],
        abs(cohort_12month_MPTA - cohort_discharge_MPTA)[rickets_indicies],
        abs(cohort_12month_MPTA - cohort_discharge_MPTA)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        ylab = "Absolute Change in MPTA from Discharge to 12months (degrees)")
# BLOUNTS HAD MORE NEGATIVE CHANGE WHILE RICKETS HAD MORE POSITIVE CHANGE.
# IN TERMS OF ABSOLUTE CHANGE, BLOUNTS AND RICKETS WERE SIMILAR.



# DID LENGTH OF TIME IN CAST AFFECT CHANGE IN MPTA?
#########################################################
# raw values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MPTA - cohort_discharge_MPTA,
     xlab = "Days in Cast From First Day of Surgery",
     ylab = "Change in MPTA from Discharge to 12Month (degrees)")
# absolute values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, abs(cohort_12month_MPTA - cohort_discharge_MPTA),
     xlab = "Days in Cast",
     ylab = "Absolute Change in MPTA from Discharge to 12 Month (degrees)")
m_dayscast_MPTA <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
summary(m_dayscast_MPTA)
# GIVEN THE PLOT, I WOULD SAY NO.





# DID SURGICAL TECHNIQUE AFFECT CHANGE IN MPTA? (TIB ELEVATION VS OTHER - BLOUNTS ONLY)
############################################################################################
data_MPTA <- data.frame(StagesOfCare = c("Preop", "Discharge", "12Month"),
                        Tib_means <- rep(c(0), 3),
                        Tib_upper2sd <- rep(c(0), 3),
                        Tib_lower2sd <- rep(c(0), 3),
                        Other_means <- rep(c(0), 3),
                        Other_upper2sd <- rep(c(0), 3),
                        Other_lower2sd <- rep(c(0), 3)
)


# The column names came out weird. I fix it below

colnames(data_MPTA) <- c("StagesOfCare", "Tib_means", "Tib_upper2sd", "Tib_lower2sd", "Other_means", "Other_upper2sd", "Other_lower2sd")


data_MPTA$Tib_means[1] <- mean(cohort_preop_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_means[2] <- mean(cohort_discharge_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_means[3] <- mean(cohort_12month_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_upper2sd[1] <- data_MPTA$Tib_means[1] + 2*sd(cohort_preop_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_upper2sd[2] <- data_MPTA$Tib_means[2] + 2*sd(cohort_discharge_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_upper2sd[3] <- data_MPTA$Tib_means[3] + 2*sd(cohort_12month_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_lower2sd[1] <- data_MPTA$Tib_means[1] - 2*sd(cohort_preop_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_lower2sd[2] <- data_MPTA$Tib_means[2] - 2*sd(cohort_discharge_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)
data_MPTA$Tib_lower2sd[3] <- data_MPTA$Tib_means[3] - 2*sd(cohort_12month_MPTA[blounts_indicies & tib_indicies], na.rm = TRUE)

data_MPTA$Other_means[1] <- mean(cohort_preop_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_means[2] <- mean(cohort_discharge_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_means[3] <- mean(cohort_12month_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_upper2sd[1] <- data_MPTA$Other_means[1] + 2*sd(cohort_preop_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_upper2sd[2] <- data_MPTA$Other_means[2] + 2*sd(cohort_discharge_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_upper2sd[3] <- data_MPTA$Other_means[3] + 2*sd(cohort_12month_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_lower2sd[1] <- data_MPTA$Other_means[1] - 2*sd(cohort_preop_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_lower2sd[2] <- data_MPTA$Other_means[2] - 2*sd(cohort_discharge_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)
data_MPTA$Other_lower2sd[3] <- data_MPTA$Other_means[3] - 2*sd(cohort_12month_MPTA[blounts_indicies & !(tib_indicies)], na.rm = TRUE)



# Plotting data from table

ggplot() +
  geom_line(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_MPTA, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "MPTA (degrees)")+
  scale_x_discrete(limits = rev)



change_MPTA_tib <- cohort_12month_MPTA[blounts_indicies & tib_indicies] - cohort_discharge_MPTA[blounts_indicies & tib_indicies]
change_MPTA_not_tib <- cohort_12month_MPTA[blounts_indicies & !(tib_indicies)] - cohort_discharge_MPTA[blounts_indicies & !(tib_indicies)]
boxplot(abs(change_MPTA_tib), abs(change_MPTA_not_tib),
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Absolute Change in MPTA from Discharge to 12Month (degrees)")
t.test(change_MPTA_tib, change_MPTA_not_tib)
# GIVEN THE PLOT, BOXPLOT, AND T-TEST, IT IS POSSIBLE THAT TIB ELEVATION COULD HAVE LED TO 
# BETTER MAINTAINED CHANGES, BUT IT IS NOT STATISTICALLY SIGNIFICANT.




# DID BMI AFFECT CHANGE IN MPTA?
#######################################
# raw values
plot(cohort_BMI, cohort_12month_MPTA - cohort_discharge_MPTA,
     xlab = "BMI",
     ylab = "Change in MPTA from Discharge to 12Month (degrees)")
# absolute values
plot(cohort_BMI, abs(cohort_12month_MPTA - cohort_discharge_MPTA),
     xlab = "BMI",
     ylab = "Absolute Change in MPTA from Discharge to 12Month (degrees)")
m_BMI_MPTA <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ cohort_BMI)
summary(m_BMI_MPTA)
# GIVEN THE PLOT AND MODEL, I WOULD SAY NO.





# DID AGE AFFECT CHANGE IN MPTA?
#######################################
# raw values
plot(cohort_AGE, cohort_12month_MPTA - cohort_discharge_MPTA,
     xlab = "AGE",
     ylab = "Change in MPTA from Discharge to 12Month (degrees)")
# absolute values
plot(cohort_AGE, abs(cohort_12month_MPTA - cohort_discharge_MPTA),
     xlab = "AGE",
     ylab = "Change in MPTA from Discharge to 12Month (degrees)")
m_AGE_MPTA <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ as.numeric(cohort_AGE))
summary(m_AGE_MPTA)
# GIVEN THE PLOT AND MODEL, I WOULD SAY THERE IS SOME IMPACT FROM AGE.
# THE MODEL IS NEARLY SIGNIFICANT, SUGGESTING AS AGE INCREASES, ABSOLUTE CHANGE DECREASES





# DID SEX AFFECT CHANGE IN MPTA?
######################################
boxplot((cohort_12month_MPTA - cohort_discharge_MPTA)[cohort_assess_preop$Sex == "M"],
        (cohort_12month_MPTA - cohort_discharge_MPTA)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in MPTA from Discharge to 12Month (degrees)")
# It looks like females could have had more drastic changes. In this instance, 
# I want to do a boxplot for absolute values
boxplot(abs(cohort_12month_MPTA - cohort_discharge_MPTA)[cohort_assess_preop$Sex == "M"],
        abs(cohort_12month_MPTA - cohort_discharge_MPTA)[cohort_assess_preop$Sex == "F"],
        names = c("Male", "Female"),
        xlab = "Sex",
        ylab = "Change in MPTA from Discharge to 12Month (degrees)")
# OVERALL, IT DOESN'T LOOK LIKE MALES AND FEMALES DIFFERED SIGNIFICANTLY IN MPTA CHANGES





# HOW DO BMI, AGE, AND SEX INTERACT IN A LINAER MODEL WITH CHANGE IN MPTA?
################################################################################
m_DEMO_MPTA <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ + as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) +  factor(cohort_assess_preop$Sex) + cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$DIagnosis))
summary(m_DEMO_MPTA)
# NOT WELL, DOESN'T LOOK LIKE THEY HAVE ANY AFFECT ON CHANGE IN MPTA



# T-Tests for Preop and 12 month
################################################################################
# Alignment
t.test(alignment_preop_dist[blounts_indicies], alignment_preop_dist[rickets_indicies], var.equal = TRUE)
# Mech Axis
t.test(mechAxis_preop_dist[blounts_indicies], mechAxis_preop_dist[rickets_indicies], var.equal = TRUE)
# Mech Axis Deviation
t.test(MAD_preop_dist[blounts_indicies], MAD_preop_dist[rickets_indicies], var.equal= TRUE)
# MPTA 
t.test(MPTA_preop_dist[blounts_indicies], MPTA_preop_dist[rickets_indicies], var.equal= TRUE)



# Alignment
t.test(alignment_12month_dist[blounts_indicies], alignment_discharge_dist[rickets_indicies], var.equal = TRUE)
# Mech Axis
t.test(mechAxis_12month_dist[blounts_indicies], mechAxis_discharge_dist[rickets_indicies], var.equal = TRUE)
# Mech Axis Deviation
t.test(MAD_12month_dist[blounts_indicies], MAD_discharge_dist[rickets_indicies], var.equal= TRUE)
# MPTA 
t.test(MPTA_12month_dist[blounts_indicies], MPTA_discharge_dist[rickets_indicies], var.equal= TRUE)




# Final Regressions
###################################################################################
cohort_DAYSINCAST <- as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
library(car)

cohort_BMI <- as.numeric(cohort_assess_12month$BMI..V11)
cohort_BMI <- ifelse(cohort_BMI == 0, NA, cohort_BMI)


# DIAGNOSIS - ALIGNMENT - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_daad <- lm(alignment_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + alignment_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model_daad)
qqnorm(model_daad$residuals)
summary(model_daad)

# DIAGNOSIS - MECH AXIS - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_maad <- lm(mechAxis_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + mechAxis_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model_maad)
qqnorm(model_maad$residuals)
summary(model_maad)

# DIAGNOSIS - MECH AXIS DEVIATION - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_madd <- lm(MAD_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MAD_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model_madd)
qqnorm(model_madd$residuals)
summary(model_madd)

# DIAGNOSIS - MPTA - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_mpta <- lm(MPTA_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MPTA_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model_mpta)
summary(model_mpta)




# DIAGNOSIS - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE

# DIAGNOSIS - ALIGNMENT - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# AGE WAS SIGNIFICANT MODEL WAS SIGNIFICANT TABLE 9
model <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + alignment_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model)
summary(model)


# DIAGNOSIS - MECH AXIS - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# UNCHANGED
model <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + mechAxis_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model)
summary(model)


# DIAGNOSIS - MECH AXIS DEVIATION - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# NOTHING SIGNIFICANT TABLE 11 DID CHANGE THO
model <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MAD_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model)
summary(model)


# DIAGNOSIS - MPTA - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# SLIGHTLY LESS SIGNIFICANT P-VALUE. AGE AND RICKETS STILL SIGNIFICANT TABLE 12. DID CHANGE
model <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MPTA_preop_dist + factor(cohort_assess_preop$DIagnosis))
vif(model)
summary(model)





# LENGTH OF TIME IN CAST - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS


# LENGTH OF TIME IN CAST - ALIGNMENT - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_ltca <- lm(alignment_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + alignment_preop_dist + cohort_DAYSINCAST)
vif(model_ltca)
summary(model_ltca)

# LENGTH OF TIME IN CAST - MECH AXIS - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_ltcma <- lm(mechAxis_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + mechAxis_preop_dist + cohort_DAYSINCAST)
vif(model_ltcma)
summary(model_ltcma)

# LENGTH OF TIME IN CAST - MECH AXIS DEVIATION - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_ltcmad <- lm(MAD_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MAD_preop_dist + cohort_DAYSINCAST)
vif(model_ltcmad)
summary(model_ltcmad)

# LENGTH OF TIME IN CAST - MPTA - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_ltcmpta <- lm(MPTA_12month_dist ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MPTA_preop_dist + cohort_DAYSINCAST)
vif(model_ltcmpta)
summary(model_ltcmpta)





# LENGTH OF TIME IN CAST - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE

# LENGTH OF TIME IN CAST - ALIGNMENT - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# DID CHANGE, BECAME MORE SIGNIFICANT TABLE 17. AGE IS SIGNIFICANT
model_ltca <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + alignment_preop_dist + cohort_DAYSINCAST)
vif(model_ltca)
summary(model_ltca)

# LENGTH OF TIME IN CAST - MECH AXIS - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# DID CHANGE, BECAME WAY LESS SIGNIFICANT (NOT SIGNIFICANT TO BEGIN WITH THO) TABLE 18
model_ltcma <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + mechAxis_preop_dist + cohort_DAYSINCAST)
vif(model_ltcma)
summary(model_ltcma)

# LENGTH OF TIME IN CAST - MECH AXIS DEVIATION - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# WENT FROM KINDA SIGNIFICANT (0.14) TO NOT AT ALL 0.95 TABLE 19
model_ltcmad <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MAD_preop_dist + cohort_DAYSINCAST)
vif(model_ltcmad)
summary(model_ltcmad)

# LENGTH OF TIME IN CAST - MPTA - ABSOLUTE CHANGE FROM 12 MONTH TO DISCHARGE
# WENT FROM KINDA SIGNIFICANT (0.16) TO NOT AT ALL 0.4763 TABLE 20
model_ltcmpta <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ cohort_BMI + cohort_AGE + factor(cohort_assess_preop$Sex) + MPTA_preop_dist + cohort_DAYSINCAST)
vif(model_ltcmpta)
summary(model_ltcmpta)





# SURGICAL TECHNIQUE - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS

tib_or_no <- factor(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies])

# SURGICAL TECHNIQUE - ALIGNMENT - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_sta <- lm(alignment_12month_dist[blounts_indicies] ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + alignment_preop_dist[blounts_indicies] + tib_or_no)
vif(model_sta)
summary(model_sta)

# SURGICAL TECHNIQUE - MECH AXIS - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_stma <- lm(mechAxis_12month_dist[blounts_indicies] ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + mechAxis_preop_dist[blounts_indicies] + tib_or_no)
vif(model_stma)
summary(model_stma)

# SURGICAL TECHNIQUE - MECH AXIS DEVIATION - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_stmad <- lm(MAD_12month_dist[blounts_indicies] ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + MAD_preop_dist[blounts_indicies] + tib_or_no)
vif(model_stmad)
summary(model_stmad)

# SURGICAL TECHNIQUE - MPTA - ABSOLUTE DISTANCE FROM NORMAL AT 12 MONTHS
model_stmpta <- lm(MPTA_12month_dist[blounts_indicies] ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + MPTA_preop_dist[blounts_indicies] + tib_or_no)
vif(model_stmpta)
summary(model_stmpta)






# SURGICAL TECHNIQUE - ABSOLUTE CHANGE FROM 12MONTH TO DISCHARGE

# SURGICAL TECHNIQUE - ALIGNMENT - ABSOLUTE CHANGE FROM 12MONTH TO DISCHARGE
# BECAME MUCH MORE SIGNIFICANT, TABLE 27, AGE AND MALE SEX WERE SIGNIFICANT
model_sta <- lm(abs(cohort_12month_alignment[blounts_indicies] - cohort_discharge_alignment[blounts_indicies]) ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + alignment_preop_dist[blounts_indicies] + tib_or_no)
vif(model_sta)
summary(model_sta)

# SURGICAL TECHNIQUE - MECH AXIS - ABSOLUTE CHANGE FROM 12MONTH TO DISCHARGE
# NO CHANGE TABLE 28
model_stma <- lm(abs(cohort_12month_mechAxis[blounts_indicies] - cohort_discharge_mechAxis[blounts_indicies]) ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + mechAxis_preop_dist[blounts_indicies] + tib_or_no)
vif(model_stma)
summary(model_stma)

# SURGICAL TECHNIQUE - MECH AXIS DEVIATION - ABSOLUTE CHANGE FROM 12MONTH TO DISCHARGE
# BECAME SLIGHTLY MORE SIGNIFICANT (.23 to 0.12), TABLE 29, TIB EVEN MORE SIGNIFICANT
model_stmad <- lm(abs(cohort_12month_MAD[blounts_indicies] - cohort_discharge_MAD[blounts_indicies]) ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + MAD_preop_dist[blounts_indicies] + tib_or_no)
vif(model_stmad)
summary(model_stmad)

# SURGICAL TECHNIQUE - MPTA - ABSOLUTE CHANGE FROM 12MONTH TO DISCHARGE
# WENT FROM VERY SIGNIFICANT, TO SLIGHTLY NOT SIGNIFICANT (TABLE 30)
model_stmpta <- lm(abs(cohort_12month_MPTA[blounts_indicies] - cohort_discharge_MPTA[blounts_indicies]) ~ cohort_BMI[blounts_indicies] + cohort_AGE[blounts_indicies] + factor(cohort_assess_preop$Sex)[blounts_indicies] + MPTA_preop_dist[blounts_indicies] + tib_or_no)
vif(model_stmpta)
summary(model_stmpta)


sex_tib <- cohort_assess_preop$Sex[blounts_indicies & tib_indicies]
sex_not_tib <- cohort_assess_preop$Sex[blounts_indicies & !tib_indicies]

sex_tib_binary <- ifelse(sex_tib == "M", 1, 0)
sex_not_tib_binary <- ifelse(sex_not_tib == "M", 1, 0)
t.test(sex_tib_binary, sex_not_tib_binary)

mean(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[blounts_indicies & tib_indicies])), na.rm = TRUE)
mean(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[blounts_indicies & !tib_indicies])), na.rm = TRUE)
t.test(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[blounts_indicies & tib_indicies])),
       abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[blounts_indicies & !tib_indicies])))


plot(alignment_preop_dist[blounts_indicies], abs(alignment_12month_dist - alignment_discharge_dist)[blounts_indicies])

male_indicies <- cohort_SEX == "M"
female_indicies <- !male_indicies

df1 <- abs(alignment_12month_dist - alignment_discharge_dist)[male_indicies & blounts_indicies]
df2 <- abs(alignment_12month_dist - alignment_discharge_dist)[!male_indicies& blounts_indicies]
boxplot(df1, df2)

plot(cohort_AGE[blounts_indicies], abs(alignment_12month_dist - alignment_discharge_dist)[blounts_indicies])


boxplot(abs(alignment_12month_dist - alignment_discharge_dist)[tib_indicies & blounts_indicies],
        abs(alignment_12month_dist - alignment_discharge_dist)[!tib_indicies & blounts_indicies],
        names = c("Plateau Elevation", "No Plateau Elevation"),
        ylab = "Absolute Change in Alignment from Discharge to 12 Months (cm)")

boxplot(abs(mechAxis_12month_dist - mechAxis_discharge_dist)[tib_indicies & blounts_indicies],
        abs(mechAxis_12month_dist - mechAxis_discharge_dist)[!tib_indicies & blounts_indicies],
        names = c("Plateau Elevation", "No Plateau Elevation"),
        ylab = "Absolute Change in Mech Axis from Discharge to 12 Months (mm)")

boxplot(abs(MAD_12month_dist - MAD_discharge_dist)[tib_indicies & blounts_indicies],
        abs(MAD_12month_dist - MAD_discharge_dist)[!tib_indicies & blounts_indicies],
        names = c("Plateau Elevation", "No Plateau Elevation"),
        ylab = "Absolute Change in MAD from Discharge to 12 Months (cm)")

# QOL STUFF
##################################################################################
cohort_assess_preop$AVERAGE.MS.Special.QOL 
cohort_assess_preop$unweighted.WHODAS.score



t1 <- (cohort_assess_12month$AVERAGE.MS.Special.QOL != "")
t2 <- (cohort_assess_12month$unweighted.WHODAS.score != "")
QOL_indicies <- t1 & t2
QOL_indicies <- t2

length(QOL_indicies[QOL_indicies == TRUE])
length(QOL_indicies)

preop_MSaverage_QOL <- as.numeric(cohort_assess_preop$AVERAGE.MS.Special.QOL)[QOL_indicies]
preop_unweighted_QOL <-as.numeric(cohort_assess_preop$unweighted.WHODAS.score)[QOL_indicies]

discharge_MSaverage_QOL <- as.numeric(cohort_assess_discharge$AVERAGE.MS.Special.QOL)[QOL_indicies]
discharge_unweighted_QOL <-as.numeric(cohort_assess_discharge$unweighted.WHODAS.score)[QOL_indicies]

Twelvemonth_MSaverage_QOL <- as.numeric(cohort_assess_12month$AVERAGE.MS.Special.QOL)[QOL_indicies]
Twelvemonth_unweighted_QOL <-as.numeric(cohort_assess_12month$unweighted.WHODAS.score)[QOL_indicies]

mean(preop_MSaverage_QOL)
mean(preop_unweighted_QOL, na.rm = TRUE)
range(preop_MSaverage_QOL)
range(preop_unweighted_QOL, na.rm= TRUE)
sd(preop_MSaverage_QOL)
sd(preop_unweighted_QOL, na.rm = TRUE)


mean(discharge_MSaverage_QOL)
mean(discharge_unweighted_QOL, na.rm = TRUE)
range(discharge_MSaverage_QOL)
range(discharge_unweighted_QOL, na.rm = TRUE)
sd(discharge_MSaverage_QOL)
sd(discharge_unweighted_QOL, na.rm = TRUE)


mean(Twelvemonth_MSaverage_QOL)
mean(Twelvemonth_unweighted_QOL, na.rm = TRUE)
range(Twelvemonth_MSaverage_QOL, na.rm = TRUE)
range(Twelvemonth_unweighted_QOL, na.rm=TRUE)
sd(Twelvemonth_MSaverage_QOL)
sd(Twelvemonth_unweighted_QOL, na.rm = TRUE)



preop_unweighted_QOL <-as.numeric(cohort_assess_preop$unweighted.WHODAS.score)
Twelvemonth_unweighted_QOL <-as.numeric(cohort_assess_12month$unweighted.WHODAS.score)

QOL_indicies <- !is.na(Twelvemonth_unweighted_QOL- preop_unweighted_QOL)
change_WHODAS <- (Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[!is.na(Twelvemonth_unweighted_QOL- preop_unweighted_QOL)]
length(change_WHODAS)

neg_change_indicies <- (Twelvemonth_unweighted_QOL- preop_unweighted_QOL) <= 0
neg_change_indicies <- ifelse(is.na(neg_change_indicies) | neg_change_indicies == FALSE, FALSE, TRUE)

cohort_assess_preop$unweighted.WHODAS.score[neg_change_indicies]
cohort_assess_12month$unweighted.WHODAS.score[neg_change_indicies]
cohort_assess_preop$BMI..V11.[neg_change_indicies]
cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns[neg_change_indicies]
cohort_assess_preop$cleaned_Alignment..V2..in.cm[neg_change_indicies]
cohort_assess_12month$cleaned_Alignment..V2..in.cm[neg_change_indicies]
# looking at outcomes for patients with negative QOL score changes




change_MSaverage_QOL <- Twelvemonth_MSaverage_QOL - preop_MSaverage_QOL
change_unweighted_QOL <- Twelvemonth_unweighted_QOL - preop_unweighted_QOL

# MS QOL AND ALIGNMENT
plot(abs((cohort_preop_alignment - cohort_12month_alignment)[QOL_indicies]), change_MSaverage_QOL, 
     ylab = "Change in MS.Average QOL (Preop - 12Month)",
     xlab = "Absolute Change in Alignment (Preop - 12Month)")


# WHODAS AND ALIGNMENT
plot(abs((cohort_preop_alignment - cohort_12month_alignment)[QOL_indicies]), change_unweighted_QOL, 
     ylab = "Change in WHODAS QOL (Preop - 12Month)",
     xlab = "Absolute Change in Alignment (Preop - 12Month)")



# MS QOL AND MECH AXIS
plot(abs((cohort_preop_mechAxis - cohort_12month_mechAxis)[QOL_indicies]), change_MSaverage_QOL, 
     ylab = "Change in MS.Average QOL (Preop - 12Month)",
     xlab = "Absolute Change in Mech Axis (Preop - 12Month)")


# WHODAS AND MECH AXIS
plot(abs((cohort_preop_mechAxis - cohort_12month_mechAxis)[QOL_indicies]), change_unweighted_QOL, 
     ylab = "Change in WHODAS QOL (Preop - 12Month)",
     xlab = "Absolute Change in Mech Axis (Preop - 12Month)")

summary(lm(change_unweighted_QOL ~ abs((cohort_preop_mechAxis - cohort_12month_mechAxis)[QOL_indicies])))

# MS QOL AND MAD
plot(abs((cohort_preop_MAD - cohort_12month_MAD)[QOL_indicies]), change_MSaverage_QOL, 
     ylab = "Change in MS.Average QOL (Preop - 12Month)",
     xlab = "Absolute Change in MAD (Preop - 12Month)")


# WHODAS AND MAD
plot(abs((cohort_preop_MAD - cohort_12month_MAD)[QOL_indicies]), change_unweighted_QOL, 
     ylab = "Change in WHODAS QOL (Preop - 12Month)",
     xlab = "Absolute Change in MAD (Preop - 12Month)")


# Change in individual QOL answers
cohort_QOL_preop <- cohort_assess_preop[,107:120]
cohort_QOL_preop <- as.numeric(cohort_QOL_preop)
colSums(cohort_QOL_preop)

cohort_QOL_preop




plot(Twelvemonth_MSaverage_QOL[QOL_indicies], alignment_12month_dist[QOL_indicies])
plot(Twelvemonth_MSaverage_QOL[QOL_indicies], mechAxis_12month_dist[QOL_indicies])
plot(Twelvemonth_MSaverage_QOL[QOL_indicies], MAD_12month_dist[QOL_indicies])
plot(Twelvemonth_MSaverage_QOL[QOL_indicies], MPTA_12month_dist[QOL_indicies])


plot(Twelvemonth_unweighted_QOL[QOL_indicies], alignment_12month_dist[QOL_indicies])
plot(Twelvemonth_unweighted_QOL[QOL_indicies], mechAxis_12month_dist[QOL_indicies])
plot(Twelvemonth_unweighted_QOL[QOL_indicies], MAD_12month_dist[QOL_indicies])
plot(Twelvemonth_unweighted_QOL[QOL_indicies], MPTA_12month_dist[QOL_indicies])



model_who_mad <- lm(change_unweighted_QOL ~ (abs(cohort_preop_MAD - cohort_12month_MAD)[QOL_indicies]))
summary(model_who_mad)

change_MSaverage_QOL
(cohort_preop - alignment_preop_dist)[QOL_indicies]

cohort_preop_alignment


t1 <- abs((MAD_12month_dist - MAD_discharge_dist)[blounts_indicies & tib_indicies])
t2 <- abs((MAD_12month_dist - MAD_discharge_dist)[blounts_indicies & !tib_indicies])
boxplot(t1, t2, names = c("Tibial Elevation", "Other Technique"), ylab = "Absolute Change in MAD from Discharge to 12 Months (cm)")    



t3 <- abs((MPTA_12month_dist - MPTA_discharge_dist)[blounts_indicies & tib_indicies])
t4 <- abs((MPTA_12month_dist - MPTA_discharge_dist)[blounts_indicies & !tib_indicies])
boxplot(t3, t4, names = c("Tibial Elevation", "Other Technique"), ylab = "Absolute Change in MPTA from Discharge to 12 Months (degrees)")    



xvals <- c(1, 2, 3, 4, 5, 6, 7, 8,9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
yvals <- c(30, 10, 28, 12, 26, 13, 26, 17, 25, 17, 24, 18, 22, 18,  21, 19, 21, 20, 20, 18)
plot(xvals, yvals, xlab = "Days in Cast", ylab = "Outcome A (cm)")
abline(a = 20, b = 0)
A_model <- lm(yvals ~ xvals)
summary(A_model)
abs_distance_yvals
plot(xvals, abs_distance_yvals, xlab = "Days in Cast", ylab = "Absolute Distance from Normal Range for Outcome A (cm)")
A_other_model <- lm(abs_distance_yvals ~ xvals)
summary(A_other_model)



# FOOTDROP AT DISCHARGE?

foot_drop_indicies_discharge <- cohort_assess_discharge$Assess.for.Foot.Drop..y...yes..n...no..NA...blank. == "Y"
foot_drop_indicies_12month <- cohort_assess_12month$Assess.for.Foot.Drop..y...yes..n...no..NA...blank. == "Y"

cohort_assess_preop$SLF..[foot_drop_indicies_discharge]
cohort_assess_preop$Which.leg.short..V1.5...L...R...NA.[foot_drop_indicies_discharge]

cohort_assess_preop$SLF..[foot_drop_indicies_12month]
cohort_assess_preop$Which.leg.short..V1.5...L...R...NA.[foot_drop_indicies_12month]

foot_drop_indicies_discharge
foot_drop_indicies_12month

assess12Mo <- assess1[assess1$Stage.of.Care == "Assess_12_month_postop",]
indicies_y <- grepl("y", tolower(assess12Mo$attended.12.mo.follow.up))
length(assess12Mo$attended.12.mo.follow.up[indicies_y])

length(unique(assess12Mo$SLF..[indicies_y]))




model_alignment_1 <- lm(alignment_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
summary(model_alignment_1)

length(model_alignment_1$residuals)
length(model_alignment_1$residuals[model_alignment_1$residuals > 0])
length(model_alignment_1$residuals[model_alignment_1$residuals < 0])

plot(model_alignment_1$residuals)
abline(a = 0, b = 0, col = "red")


less_100 <- as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) <= 100

model_alignment_1 <- lm(alignment_12month_dist[less_100] ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)[less_100])
summary(model_alignment_1)

length(model_alignment_1$residuals)
length(model_alignment_1$residuals[model_alignment_1$residuals > 0])
length(model_alignment_1$residuals[model_alignment_1$residuals < 0])

plot(model_alignment_1$residuals)
abline(a = 0, b = 0, col = "red")

plot(alignment_12month_dist[less_100] ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)[less_100])


hist(log(as.numeric(cohort_assess_preop$BMI..V11.)))
hist(log(as.numeric(cohort_assess_preop$Age.at.First.Surgery)))
hist(log(as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)))

BMI_L <- log(as.numeric(cohort_assess_preop$BMI..V11.))
AGE_L <- log(as.numeric(cohort_assess_preop$Age.at.First.Surgery))
DAYS_L <- log(as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))

log_alignment_12month_dist <- ifelse(alignment_12month_dist > 0, log(alignment_12month_dist), 0)
mod <- lm(log_alignment_12month_dist ~ BMI_L + AGE_L)
summary(mod)

mod <- lm(log_alignment_12month_dist ~ BMI_L)
summary(mod)


mod <- lm(log_alignment_12month_dist ~ AGE_L)
summary(mod)

hist(log(alignment_12month_dist))

log(alignment_12month_dist)
log_alignment


# SHORT REPORT
# alignment
mean(alignment_preop_dist)
range(alignment_preop_dist)

mean(alignment_discharge_dist, na.rm = TRUE)
range(alignment_discharge_dist, na.rm = TRUE)

mean(alignment_12month_dist, na.rm = TRUE)
range(alignment_12month_dist, na.rm = TRUE)
# alignment


# mech axis
mean(mechAxis_preop_dist)
range(mechAxis_preop_dist)

mean(mechAxis_discharge_dist, na.rm = TRUE)
range(mechAxis_discharge_dist, na.rm = TRUE)

mean(mechAxis_12month_dist, na.rm = TRUE)
range(mechAxis_12month_dist, na.rm = TRUE)
# mech axis



# MAD
mean(MAD_preop_dist)
range(MAD_preop_dist)

mean(MAD_discharge_dist, na.rm = TRUE)
range(MAD_discharge_dist, na.rm = TRUE)

mean(MAD_12month_dist, na.rm = TRUE)
range(MAD_12month_dist, na.rm = TRUE)
# MAD

summary(lm(alignment_12month_dist ~ alignment_preop_dist + cohort_assess_preop$Tibial.Plateau.Elevation.))
summary(lm(mechAxis_12month_dist ~ mechAxis_preop_dist + cohort_assess_preop$Tibial.Plateau.Elevation.))
summary(lm(MAD_12month_dist ~ MAD_preop_dist + cohort_assess_preop$Tibial.Plateau.Elevation.))
summary(lm(MPTA_12month_dist ~ MPTA_preop_dist + cohort_assess_preop$Tibial.Plateau.Elevation.))

plot(cohort_AGE, mechAxis_12month_dist, xlab = "AGE", ylab = "Distance from Normal Mech Axis at 12 Months")
plot(cohort_AGE, MAD_12month_dist, xlab = "AGE", ylab = "Distance from Normal MAD at 12 Months")
plot(cohort_AGE, alignment_12month_dist)

alignment_whisker_data <- data.frame(Diagnosis = cohort_assess_preop$DIagnosis,
                                     Preop = alignment_preop_dist,
                                     Discharge = alignment_discharge_dist,
                                     TwelveMonth = alignment_12month_dist)

write.csv(alignment_whisker_data, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\alignmentwhisker.csv")

b_whisker_preop <- data.frame(Blounts = alignment_preop_dist[blounts_indicies], StagesOfCare = rep(c("Preop"), length(alignment_preop_dist[blounts_indicies])))
b_whisker_discharge <- data.frame(Blounts = alignment_discharge_dist[blounts_indicies], StagesOfCare = rep(c("Discharge"), length(alignment_discharge_dist[blounts_indicies])))
b_whisker_12month <- data.frame(Blounts = alignment_12month_dist[blounts_indicies], StagesOfCare = rep(c("12Month"), length(alignment_12month_dist[blounts_indicies])))

b_whisker <- rbind(b_whisker_preop, b_whisker_discharge)
b_whisker <- rbind(b_whisker, b_whisker_12month)
b_whisker
write.csv(b_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\b_whisker.csv")

r_whisker_preop <- data.frame(rickets = alignment_preop_dist[rickets_indicies], StagesOfCare = rep(c("Preop"), length(alignment_preop_dist[rickets_indicies])))
r_whisker_discharge <- data.frame(rickets = alignment_discharge_dist[rickets_indicies], StagesOfCare = rep(c("Discharge"), length(alignment_discharge_dist[rickets_indicies])))
r_whisker_12month <- data.frame(rickets = alignment_12month_dist[rickets_indicies], StagesOfCare = rep(c("12Month"), length(alignment_12month_dist[rickets_indicies])))

r_whisker <- rbind(r_whisker_preop, r_whisker_discharge)
r_whisker <- rbind(r_whisker, r_whisker_12month)
r_whisker
write.csv(r_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\r_whisker.csv")


o_whisker_preop <- data.frame(others = alignment_preop_dist[others_indicies], StagesOfCare = rep(c("Preop"), length(alignment_preop_dist[others_indicies])))
o_whisker_discharge <- data.frame(others = alignment_discharge_dist[others_indicies], StagesOfCare = rep(c("Discharge"), length(alignment_discharge_dist[others_indicies])))
o_whisker_12month <- data.frame(others = alignment_12month_dist[others_indicies], StagesOfCare = rep(c("12Month"), length(alignment_12month_dist[others_indicies])))

o_whisker <- rbind(o_whisker_preop, o_whisker_discharge)
o_whisker <- rbind(o_whisker, o_whisker_12month)
o_whisker
write.csv(o_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\o_whisker.csv")





b_whisker_preop <- data.frame(Blounts = mechAxis_preop_dist[blounts_indicies], StagesOfCare = rep(c("Preop"), length(mechAxis_preop_dist[blounts_indicies])))
b_whisker_afterwedge <- data.frame(Blounts = mechAxis_xafterwedge_dist[blounts_indicies], StagesOfCare = rep(c("XAfterWedge"), length(mechAxis_xafterwedge_dist[blounts_indicies])))
b_whisker_outcast <- data.frame(Blounts = mechAxis_xoutofcast_dist[blounts_indicies], StagesOfCare = rep(c("XOutOfCast"), length(mechAxis_xoutofcast_dist[blounts_indicies])))
b_whisker_discharge <- data.frame(Blounts = mechAxis_discharge_dist[blounts_indicies], StagesOfCare = rep(c("Discharge"), length(mechAxis_discharge_dist[blounts_indicies])))
b_whisker_12month <- data.frame(Blounts = mechAxis_12month_dist[blounts_indicies], StagesOfCare = rep(c("12Month"), length(mechAxis_12month_dist[blounts_indicies])))

b_whisker <- rbind(b_whisker_preop, b_whisker_afterwedge)
b_whisker <- rbind(b_whisker, b_whisker_outcast)
b_whisker <- rbind(b_whisker, b_whisker_discharge)
b_whisker <- rbind(b_whisker, b_whisker_12month)


write.csv(b_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\bm_whisker.csv")


r_whisker_preop <- data.frame(rickets = mechAxis_preop_dist[rickets_indicies], StagesOfCare = rep(c("Preop"), length(mechAxis_preop_dist[rickets_indicies])))
r_whisker_afterwedge <- data.frame(rickets = mechAxis_xafterwedge_dist[rickets_indicies], StagesOfCare = rep(c("XAfterWedge"), length(mechAxis_xafterwedge_dist[rickets_indicies])))
r_whisker_outcast <- data.frame(rickets = mechAxis_xoutofcast_dist[rickets_indicies], StagesOfCare = rep(c("XOutOfCast"), length(mechAxis_xoutofcast_dist[rickets_indicies])))
r_whisker_discharge <- data.frame(rickets = mechAxis_discharge_dist[rickets_indicies], StagesOfCare = rep(c("Discharge"), length(mechAxis_discharge_dist[rickets_indicies])))
r_whisker_12month <- data.frame(rickets = mechAxis_12month_dist[rickets_indicies], StagesOfCare = rep(c("12Month"), length(mechAxis_12month_dist[rickets_indicies])))

r_whisker <- rbind(r_whisker_preop, r_whisker_afterwedge)
r_whisker <- rbind(r_whisker, r_whisker_outcast)
r_whisker <- rbind(r_whisker, r_whisker_discharge)
r_whisker <- rbind(r_whisker, r_whisker_12month)

write.csv(r_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\rm_whisker.csv")



o_whisker_preop <- data.frame(others = mechAxis_preop_dist[others_indicies], StagesOfCare = rep(c("Preop"), length(mechAxis_preop_dist[others_indicies])))
o_whisker_afterwedge <- data.frame(others = mechAxis_xafterwedge_dist[others_indicies], StagesOfCare = rep(c("XAfterWedge"), length(mechAxis_xafterwedge_dist[others_indicies])))
o_whisker_outcast <- data.frame(others = mechAxis_xoutofcast_dist[others_indicies], StagesOfCare = rep(c("XOutOfCast"), length(mechAxis_xoutofcast_dist[others_indicies])))
o_whisker_discharge <- data.frame(others = mechAxis_discharge_dist[others_indicies], StagesOfCare = rep(c("Discharge"), length(mechAxis_discharge_dist[others_indicies])))
o_whisker_12month <- data.frame(others = mechAxis_12month_dist[others_indicies], StagesOfCare = rep(c("12Month"), length(mechAxis_12month_dist[others_indicies])))

o_whisker <- rbind(o_whisker_preop, o_whisker_afterwedge)
o_whisker <- rbind(o_whisker, o_whisker_outcast)
o_whisker <- rbind(o_whisker, o_whisker_discharge)
o_whisker <- rbind(o_whisker, o_whisker_12month)



write.csv(o_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\om_whisker.csv")





b_whisker_preop <- data.frame(Blounts = MAD_preop_dist[blounts_indicies], StagesOfCare = rep(c("Preop"), length(MAD_preop_dist[blounts_indicies])))
b_whisker_afterwedge <- data.frame(Blounts = MAD_xafterwedge_dist[blounts_indicies], StagesOfCare = rep(c("XAfterWedge"), length(MAD_xafterwedge_dist[blounts_indicies])))
b_whisker_outcast <- data.frame(Blounts = MAD_xoutofcast_dist[blounts_indicies], StagesOfCare = rep(c("XOutOfCast"), length(MAD_xoutofcast_dist[blounts_indicies])))
b_whisker_discharge <- data.frame(Blounts = MAD_discharge_dist[blounts_indicies], StagesOfCare = rep(c("Discharge"), length(MAD_discharge_dist[blounts_indicies])))
b_whisker_12month <- data.frame(Blounts = MAD_12month_dist[blounts_indicies], StagesOfCare = rep(c("12Month"), length(MAD_12month_dist[blounts_indicies])))

b_whisker <- rbind(b_whisker_preop, b_whisker_afterwedge)
b_whisker <- rbind(b_whisker, b_whisker_outcast)
b_whisker <- rbind(b_whisker, b_whisker_discharge)
b_whisker <- rbind(b_whisker, b_whisker_12month)


quantile(b_whisker_outcast$Blounts, na.rm = TRUE)

b_whisker


write.csv(b_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\bm_whisker.csv")


r_whisker_preop <- data.frame(rickets = MAD_preop_dist[rickets_indicies], StagesOfCare = rep(c("Preop"), length(MAD_preop_dist[rickets_indicies])))
r_whisker_afterwedge <- data.frame(rickets = MAD_xafterwedge_dist[rickets_indicies], StagesOfCare = rep(c("XAfterWedge"), length(MAD_xafterwedge_dist[rickets_indicies])))
r_whisker_outcast <- data.frame(rickets = MAD_xoutofcast_dist[rickets_indicies], StagesOfCare = rep(c("XOutOfCast"), length(MAD_xoutofcast_dist[rickets_indicies])))
r_whisker_discharge <- data.frame(rickets = MAD_discharge_dist[rickets_indicies], StagesOfCare = rep(c("Discharge"), length(MAD_discharge_dist[rickets_indicies])))
r_whisker_12month <- data.frame(rickets = MAD_12month_dist[rickets_indicies], StagesOfCare = rep(c("12Month"), length(MAD_12month_dist[rickets_indicies])))

r_whisker <- rbind(r_whisker_preop, r_whisker_afterwedge)
r_whisker <- rbind(r_whisker, r_whisker_outcast)
r_whisker <- rbind(r_whisker, r_whisker_discharge)
r_whisker <- rbind(r_whisker, r_whisker_12month)

write.csv(r_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\rm_whisker.csv")

quantile(r_whisker_outcast$rickets, na.rm = TRUE)



o_whisker_preop <- data.frame(others = MAD_preop_dist[others_indicies], StagesOfCare = rep(c("Preop"), length(MAD_preop_dist[others_indicies])))
o_whisker_afterwedge <- data.frame(others = MAD_xafterwedge_dist[others_indicies], StagesOfCare = rep(c("XAfterWedge"), length(MAD_xafterwedge_dist[others_indicies])))
o_whisker_outcast <- data.frame(others = MAD_xoutofcast_dist[others_indicies], StagesOfCare = rep(c("XOutOfCast"), length(MAD_xoutofcast_dist[others_indicies])))
o_whisker_discharge <- data.frame(others = MAD_discharge_dist[others_indicies], StagesOfCare = rep(c("Discharge"), length(MAD_discharge_dist[others_indicies])))
o_whisker_12month <- data.frame(others = MAD_12month_dist[others_indicies], StagesOfCare = rep(c("12Month"), length(MAD_12month_dist[others_indicies])))

o_whisker <- rbind(o_whisker_preop, o_whisker_afterwedge)
o_whisker <- rbind(o_whisker, o_whisker_outcast)
o_whisker <- rbind(o_whisker, o_whisker_discharge)
o_whisker <- rbind(o_whisker, o_whisker_12month)

quantile(o_whisker_outcast$others, na.rm = TRUE)

write.csv(o_whisker, "C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\om_whisker.csv")



quantile(MAD_12month_dist[blounts_indicies & !tib_indicies], na.rm = TRUE)


# WHODAS GREATEST CHANGE IN SCORE FOR REPORT


colnames(cohort_assess_preop)

whodas_adult <- cohort_assess_12month[,107:122]
whodas_pediatric <- cohort_assess_12month[,55:67]

whodas_adult <- lapply(whodas_adult, as.numeric)
whodas_pediatric <- lapply(whodas_pediatric, as.numeric)

whodas_adult <- as.data.frame(whodas_adult)
whodas_pediatric <- as.data.frame(whodas_pediatric)

colMeans(whodas_adult, na.rm = TRUE)
colMeans(whodas_pediatric, na.rm = TRUE)


na_count_adult <-sapply(whodas_adult, function(y) sum(length(which(is.na(y)))))
na_count_pediatric <-sapply(whodas_pediatric, function(y) sum(length(which(is.na(y)))))

whodas_adult

nrow(whodas_adult)

t.test(whodas_adult$WA.1.Standing.up.for.long.periods.of.time.such.as.30.minutes.,
       whodas_pediatric$WP.1..Standing.for.reasonable.periods.of.time.for.example.in.school.or.church.temple.,)


t.test(whodas_adult$WA..2.Taking.care.of.your.household.responsibilities.,
       whodas_pediatric$WP.2..Doing.chores.or.other.things.you.are.expected.to.do.at.home.to.help.out.)


t.test(whodas_adult$WA.4.How.much.of.a.problem.did.you.have.joining.in.community.activities..for.example..festivities..religious.or.other.activities..in.the.same.way.as.anyone.else.can.,
       whodas_pediatric$WP.4..Joining.in.on.community.activities..for.example..clubs..religious.groups..or.after.school.activites..than.you.thought.you.should.)



t.test(whodas_adult$WA.5.How.much.have.you.been.emotionally.affected.by.your.health.problems.,
       whodas_pediatric$WP.5..How.much.have.you.been.upset.by.your.health.condition.)




t.test(whodas_adult$WA.6.Concentrating.on.doing.something.for.ten.minutes.,
       whodas_pediatric$WP.6..Concentrating.for.10.minutes.at.a.time.or.more.while.doing.homework..playing.a.game..or.doing.something.you.were.asked.to.do.)


t.test(whodas_adult$WA.9.Getting.dressed.,
       whodas_pediatric$WP.9..Getting.dressed.on.your.own.)


t.test(whodas_adult$WA.10.Dealing.with.people.you.do.not.know.,
       whodas_pediatric$WP.10..Getting.along.with.people.you.do.not.know.well.)


t.test(whodas_adult$WA.12.Your.day.to.day.work.,
       whodas_pediatric$WP.12..Finishing.chores.or.home.activities.that.you.are.supposed.to.do.)

t.test(whodas_adult$WA.16.We.would.like.to.know.how.good.or.bad.your.health.is.TODAY,
       whodas_pediatric$WP..Health.Rating..How.good.or.bad.is.your.health.today...scale.of.0.10..with.0.being.worst.health.and.10.being.best.health..which.varies.from.the.paper.copy.)

colnames(cohort_assess_12month)
cohort_assess_12month$Column3

whodas_adult

whodas_adult_12month <- whodas_adult
whodas_adult_preop <- cohort_assess_preop[,107:122]
whodas_adult_preop <- lapply(whodas_adult_preop, as.numeric)
whodas_adult_preop <- as.data.frame(whodas_adult_preop)


whodas_adult_12month
whodas_adult_preop

whodas_indicies <- !(is.na(whodas_adult_12month$WA.1.Standing.up.for.long.periods.of.time.such.as.30.minutes.))



whodas_mean_diffs <- colMeans(whodas_adult_preop[whodas_indicies,]) - colMeans(whodas_adult_12month[whodas_indicies,])

sort(whodas_mean_diffs)




d1 <- data.frame(TwelveMonth = as.numeric(whodas_adult_12month$WA.13.How.many.days.were.these.difficulties.present.[whodas_indicies]))
d2 <- data.frame(preop = as.numeric(whodas_adult_preop$WA.13.How.many.days.were.these.difficulties.present.[whodas_indicies]))
d3 <- cbind(d2, d1)


boxplot(d3)


colnames(cohort_assess_preop)

cohort_assess_preop$Functional.AVERAGE.MS.QOL

d3

sort(whodas_mean_diffs)


cohort_assess_preop$SUM.WHODAS.PEAEDS

d1 <- data.frame(TwelveMonth = as.numeric(whodas_adult_12month$WA.16.We.would.like.to.know.how.good.or.bad.your.health.is.TODAY[whodas_indicies]))
d2 <- data.frame(preop = as.numeric(whodas_adult_preop$WA.16.We.would.like.to.know.how.good.or.bad.your.health.is.TODAY[whodas_indicies]))
d3 <- cbind(d2, d1)

boxplot(d3)




d1 <- data.frame(TwelveMonth = as.numeric(whodas_adult_12month$WA.15.Not.counting.the.days.you.were.totally.unable..for.how.many.days.did.you.cut.back.or.reduce.your.usual.activities.or.work.because.of.any.health.condition.[whodas_indicies]))
d2 <- data.frame(preop = as.numeric(whodas_adult_preop$WA.15.Not.counting.the.days.you.were.totally.unable..for.how.many.days.did.you.cut.back.or.reduce.your.usual.activities.or.work.because.of.any.health.condition.[whodas_indicies]))
d3 <- cbind(d2, d1)

boxplot(d3)

d3

length(unique(cohort_assess_preop$SLF..[whodas_indicies]))
length(unique(cohort_assess_preop$SLF..))


mean(as.numeric(cohort_assess_preop$unweighted.WHODAS.score), na.rm = TRUE)

mean(as.numeric(cohort_assess_12month$unweighted.WHODAS.score), na.rm = TRUE)
cohort_assess_12month$unweighted.WHODAS.score

as.data.frame(rowSums(whodas_pediatric[whodas_indicies,1:12], na.rm=TRUE) / 48)
 
as.numeric(cohort_assess_12month[whodas_indicies, 123])


t.test(as.data.frame(rowSums(whodas_pediatric[whodas_indicies,1:12], na.rm=TRUE) / 48),
       as.data.frame(as.numeric(cohort_assess_12month[whodas_indicies, 123])))


whodas_adult[whodas_indicies,2]
whodas_pediatric[whodas_indicies,2]


as.data.frame(rowSums(whodas_pediatric[whodas_indicies,1:12], na.rm=TRUE) / 48)
rowSums(whodas_adult[whodas_indicies, 1:15]) / 60
(as.numeric(cohort_assess_12month[whodas_indicies, 123]))
rowSums(whodas_adult[whodas_indicies,1:15]) / 48

t.test(rowSums(whodas_pediatric[whodas_indicies,1:12], na.rm=TRUE) / 48,
       rowSums(whodas_adult[whodas_indicies,1:12]) / 48)


whodas_adult_preop[whodas_indicies, 1] - whodas_adult_12month[whodas_indicies, 1]
whodas_adult_preop[whodas_indicies, 2] - whodas_adult_12month[whodas_indicies, 2]
whodas_adult_preop[whodas_indicies, 3] - whodas_adult_12month[whodas_indicies, 3]



whodas_mean_diffs <- colMeans(whodas_adult_preop[whodas_indicies,]) - colMeans(whodas_adult_12month[whodas_indicies,])
sort(whodas_mean_diffs)


whodas_adult_preop[whodas_indicies,1:12] - whodas_adult_12month[whodas_indicies,1:12]


whodas_adult_preop[whodas_indicies, 1:12]


(1 - (rowMeans(whodas_adult_preop[whodas_indicies, 1:12]) / 4)) - (1 - (rowMeans(whodas_adult_12month[whodas_indicies, 1:12]) / 4))


mean((1 - (rowMeans(whodas_adult_preop[whodas_indicies, 1:12]) / 4)))

mean((1 - (rowMeans(whodas_adult_12month[whodas_indicies, 1:12]) / 4)))

min(1 - (rowMeans(whodas_adult_12month[whodas_indicies, 1:12]) / 4))
min(1 - (rowMeans(whodas_adult_preop[whodas_indicies, 1:12]) / 4))

max(1 - (rowMeans(whodas_adult_12month[whodas_indicies, 1:12]) / 4))
max(1 - (rowMeans(whodas_adult_preop[whodas_indicies, 1:12]) / 4))


(cohort_assess_preop$DIagnosis[whodas_unique_indicies])[regression_indicies_unique]
(cohort_assess_preop$SLF..[whodas_unique_indicies])[regression_indicies_unique]

(cohort_assess_preop$DIagnosis[whodas_unique_indicies])[!regression_indicies_unique]
(cohort_assess_preop$SLF..[whodas_unique_indicies])[!regression_indicies_unique]


#####################################################################################
# COMPLICATION ANALYSIS
#####################################################################################

# Questions to answer
# 1. Did patients with complications have worse outcomes? (farther from normal ranges)
# 2. Did the preoperative measurement values differ between patients with/without complications?
# 3. Did BMI, Age or Sex distribution differ between patients with/without complications?
# 4. Did patients with complications have worse reduction to normal range?
# 5. Did patients with complications have worse WHODAS outcomes?




# Did patients with complications have worse outcomes?
# Seems to be no.

# No for alignment
alignment_12month_dist[complication_indicies]
alignment_12month_dist[!complication_indicies]
mean(alignment_12month_dist[complication_indicies], na.rm = TRUE)
mean(alignment_12month_dist[!complication_indicies], na.rm = TRUE)
boxplot(alignment_12month_dist[complication_indicies], alignment_12month_dist[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Distance from Normal Alignment at 12 Months (cm)")
t.test(alignment_12month_dist[complication_indicies], alignment_12month_dist[!complication_indicies])



# No for Mech Axis
mechAxis_12month_dist[complication_indicies]
mechAxis_12month_dist[!complication_indicies]
mean(mechAxis_12month_dist[complication_indicies], na.rm = TRUE)
mean(mechAxis_12month_dist[!complication_indicies], na.rm = TRUE)
boxplot(mechAxis_12month_dist[complication_indicies], mechAxis_12month_dist[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Distance from Normal Mech Axis at 12 Months (mm)")
t.test(mechAxis_12month_dist[complication_indicies], mechAxis_12month_dist[!complication_indicies])



# No for MAD
MAD_12month_dist[complication_indicies]
MAD_12month_dist[!complication_indicies]
mean(MAD_12month_dist[complication_indicies], na.rm = TRUE)
mean(MAD_12month_dist[!complication_indicies], na.rm = TRUE)
boxplot(MAD_12month_dist[complication_indicies], MAD_12month_dist[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Distance from Normal Mech Axis at 12 Months (cm)")
t.test(MAD_12month_dist[complication_indicies], MAD_12month_dist[!complication_indicies])





# 2. Did the preoperative measurement values differ between patients with/without complications?
# Seems to be yes - patients with complications were farther from normal range.


# Yes for alignment. Patients with complications were typically farther from normal range.
alignment_preop_dist[complication_indicies]
alignment_preop_dist[!complication_indicies]
mean(alignment_preop_dist[complication_indicies], na.rm = TRUE)
mean(alignment_preop_dist[!complication_indicies], na.rm = TRUE)
boxplot(alignment_preop_dist[complication_indicies], alignment_preop_dist[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Distance from Normal Alignment at Preop (cm)")
t.test(alignment_preop_dist[complication_indicies], alignment_preop_dist[!complication_indicies])



# Not quite for Mech Axis. Patients with complications seem farther from normal range, just not statistically significant.
mechAxis_preop_dist[complication_indicies]
mechAxis_preop_dist[!complication_indicies]
mean(mechAxis_preop_dist[complication_indicies], na.rm = TRUE)
mean(mechAxis_preop_dist[!complication_indicies], na.rm = TRUE)
boxplot(mechAxis_preop_dist[complication_indicies], mechAxis_preop_dist[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Distance from Normal Mech Axis at 12 Months (mm)")
t.test(mechAxis_preop_dist[complication_indicies], mechAxis_preop_dist[!complication_indicies])



# Yes for MAD. Patients with complications were farther from normal range.
MAD_preop_dist[complication_indicies]
MAD_preop_dist[!complication_indicies]
mean(MAD_preop_dist[complication_indicies], na.rm = TRUE)
mean(MAD_preop_dist[!complication_indicies], na.rm = TRUE)
boxplot(MAD_preop_dist[complication_indicies], MAD_preop_dist[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Distance from Normal Mech Axis at 12 Months (cm)")
t.test(MAD_preop_dist[complication_indicies], MAD_preop_dist[!complication_indicies])





# 3. Did BMI, Age or Sex distribution differ between patients with/without complications?


# No for Age (though its possible patients with compilations end up older with more data)
cohort_AGE[complication_indicies]
cohort_AGE[!complication_indicies]
mean(cohort_AGE[complication_indicies])
mean(cohort_AGE[!complication_indicies])
boxplot(cohort_AGE[complication_indicies], cohort_AGE[!complication_indicies], 
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Age (years)")
t.test(cohort_AGE[complication_indicies], cohort_AGE[!complication_indicies])


# No for BMI 
cohort_BMI[complication_indicies]
cohort_BMI[!complication_indicies]
mean(cohort_BMI[complication_indicies], na.rm = TRUE)
mean(cohort_BMI[!complication_indicies], na.rm = TRUE)
boxplot(cohort_BMI[complication_indicies], cohort_BMI[!complication_indicies], 
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "BMI")
t.test(cohort_BMI[complication_indicies], cohort_BMI[!complication_indicies])


# No for Sex
cohort_SEX[complication_indicies]
cohort_SEX[!complication_indicies]
table(unlist(cohort_SEX[complication_indicies]))
table(unlist(cohort_SEX[!complication_indicies]))
data <- matrix(c(15, 26, 14, 21), nrow = 2)
fisher.test(data)



# 4. Did patients with complications have worse reduction to normal range?

# No for alignment (if anything they had better)
table(unlist(cohort_assess_preop$Complication.Count[poor_alignment_reduction]))
table(unlist(cohort_assess_preop$Complication.Count[!poor_alignment_reduction]))
data <- matrix(c(13, 33, 5, 17), nrow = 2)
fisher.test(data)


# No for mech axis (again if anything they had better)
table(unlist(cohort_assess_preop$Complication.Count[poor_mechAxis_reduction]))
table(unlist(cohort_assess_preop$Complication.Count[!poor_mechAxis_reduction]))
data <- matrix(c(16, 31, 8, 21), nrow = 2)
fisher.test(data)


# No for MAD (again if anything they had better)
table(unlist(cohort_assess_preop$Complication.Count[poor_MAD_reduction]))
table(unlist(cohort_assess_preop$Complication.Count[!poor_MAD_reduction]))
data <- matrix(c(8, 39, 4, 25), nrow = 2)
fisher.test(data)





# 5. Did patients with complications have worse WHODAS outcomes?


# Almost for Preop WHODAS Score (patients with complication are nearly significantly lower)
cohort_assess_preop$unweighted.WHODAS.score[complication_indicies]
cohort_assess_preop$unweighted.WHODAS.score[!complication_indicies]
mean(as.numeric(cohort_assess_preop$unweighted.WHODAS.score[complication_indicies]), na.rm = TRUE)
mean(as.numeric(cohort_assess_preop$unweighted.WHODAS.score[!complication_indicies]), na.rm = TRUE)
boxplot(as.numeric(cohort_assess_preop$unweighted.WHODAS.score[complication_indicies]),
        as.numeric(cohort_assess_preop$unweighted.WHODAS.score[!complication_indicies]),
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Unweighted WHODAS preop Score")
t.test(as.numeric(cohort_assess_preop$unweighted.WHODAS.score[complication_indicies]),
       as.numeric(cohort_assess_preop$unweighted.WHODAS.score[!complication_indicies]))


# No for 12 month WHODAS score
cohort_assess_12month$unweighted.WHODAS.score[complication_indicies]
cohort_assess_12month$unweighted.WHODAS.score[!complication_indicies]
mean(as.numeric(cohort_assess_12month$unweighted.WHODAS.score[complication_indicies]), na.rm = TRUE)
mean(as.numeric(cohort_assess_12month$unweighted.WHODAS.score[!complication_indicies]), na.rm = TRUE)
boxplot(as.numeric(cohort_assess_12month$unweighted.WHODAS.score[complication_indicies]),
        as.numeric(cohort_assess_12month$unweighted.WHODAS.score[!complication_indicies]),
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Unweighted WHODAS 12month Score")
t.test(as.numeric(cohort_assess_12month$unweighted.WHODAS.score[complication_indicies]),
       as.numeric(cohort_assess_12month$unweighted.WHODAS.score[!complication_indicies]))



# No for change in WHODAS score between preop and 12month (patients with complication potentially had less change, but more data is needed to confirm it)
(Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[complication_indicies]
(Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[!complication_indicies]
mean((Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[complication_indicies], na.rm = TRUE)
mean((Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[!complication_indicies], na.rm = TRUE)
boxplot((Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[complication_indicies],
       (Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[!complication_indicies],
        names = c("Patients With Complication", "Patients Without Complication"),
        ylab = "Change in WHODAS Score (Preop to 12Month)")
t.test((Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[complication_indicies],
       (Twelvemonth_unweighted_QOL- preop_unweighted_QOL)[!complication_indicies])


# None of the 8/19 patients, with >1 WHODAS questions with worse 12 month score than preop, had a complication
(whodas_adult_12month[whodas_unique_indicies,1])[regression_indicies_unique] %in% SLF_complication_count$PatientID
SLF_complication_count$PatientID
(whodas_adult_12month[whodas_unique_indicies,1])[regression_indicies_unique]


cohort_preop_mechAxis[less_9_indicies]
cohort_preop_mechAxis[!less_9_indicies]

young_indicies <- cohort_AGE < 9

boxplot(abs(cohort_12month_mechAxis[young_indicies] - cohort_discharge_mechAxis[young_indicies]),
        abs(cohort_12month_mechAxis[!young_indicies] - cohort_discharge_mechAxis[!young_indicies]),
        names = c("Younger than 8", "8 or Older"),
        ylab = "Absolute Change in Mech Axis mm")

plot( cohort_AGE[blounts_indicies], abs(cohort_12month_mechAxis - cohort_discharge_mechAxis)[blounts_indicies])
plot( cohort_AGE, abs(cohort_12month_mechAxis - cohort_discharge_mechAxis))


boxplot(abs(cohort_12month_alignment[young_indicies] - cohort_discharge_alignment[young_indicies]),
        abs(cohort_12month_alignment[!young_indicies] - cohort_discharge_alignment[!young_indicies]),
        names = c("Younger than 8", "8 or Older"),
        ylab = "Absolute Change in Alignment cm")


boxplot(abs(cohort_12month_MAD[young_indicies] - cohort_discharge_MAD[young_indicies]),
        abs(cohort_12month_MAD[!young_indicies] - cohort_discharge_MAD[!young_indicies]),
        names = c("Younger than 8", "8 or Older"),
        ylab = "Absolute Change in MAD cm")


(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies & young_indicies])
(cohort_assess_preop$Tibial.Plateau.Elevation.[blounts_indicies & !young_indicies])

  
