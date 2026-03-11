library(dplyr)




# Getting data
assess1 <- read.csv("C:\\MercyShips\\Coding\\Projects\\OrthoAnalysis\\Assess_Cleaned_8month.csv")



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


#################
#Clean IM measurements
################

#Insert a column to store the original measurements
assess1$cleaned_Alignment..V2..in.cm_ORIGINAL<-assess1$cleaned_Alignment..V2..in.cm

# Making IM measurements negative
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



# Tibial Plateau Elevation = 26
nrow(cohort_assess_preop[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y", ])

# Number of starting valgus (this one we actually need to be on preop) = 12
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "valgus", ])
# Number of starting varus (this one we actually need to be on preop) = 49
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "varus", ])






#############################################################################################
# Step 6: PREPARING Data
#
#############################################################################################

# Getting indicies for dissagregating data in our cohort
tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "Y"
not_tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "N"
IC_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
IM_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"


cohort_preop_MPTA <- as.numeric(cohort_assess_preop$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_discharge_MPTA <-as.numeric(cohort_assess_discharge$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_12month_MPTA <- as.numeric(cohort_assess_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_preop_MAD <- as.numeric(cohort_assess_preop$Mech.Axis.Deviation..mm.)
cohort_xafterwedge_MAD <- as.numeric(cohort_assess_xafterwedge$Mech.Axis.Deviation..mm.)
cohort_xoutofcast_MAD <- as.numeric(cohort_assess_xoutofcast$Mech.Axis.Deviation..mm.)
cohort_discharge_MAD <-as.numeric(cohort_assess_discharge$Mech.Axis.Deviation..mm.)
cohort_12month_MAD <- as.numeric(cohort_assess_12month$Mech.Axis.Deviation..mm.)
cohort_preop_mechAxis <- as.numeric(cohort_assess_preop$cleaned_mech_axis_degrees)
cohort_xafterwedge_mechAxis <- as.numeric(cohort_assess_xafterwedge$cleaned_mech_axis_degrees)
cohort_xoutofcast_mechAxis <- as.numeric(cohort_assess_xoutofcast$cleaned_mech_axis_degrees)
cohort_discharge_mechAxis <-as.numeric(cohort_assess_discharge$cleaned_mech_axis_degrees)
cohort_12month_mechAxis <- as.numeric(cohort_assess_12month$cleaned_mech_axis_degrees)
cohort_preop_alignment <- as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm)
cohort_discharge_alignment <-as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm)
cohort_12month_alignment <- as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)
cohort_BMI <- as.numeric(cohort_assess_preop$BMI..V11)
cohort_SEX <- cohort_assess_preop$Sex
cohort_AGE <- cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns
cohort_alignment_IC_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
cohort_alignment_IM_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"
blounts_indicies <- cohort_assess_preop$DIagnosis == "blounts"
rickets_indicies <- cohort_assess_preop$DIagnosis == "rickets"
others_indicies <- cohort_assess_preop$DIagnosis == "others"

# Getting distance from "normal" range for each outcome at each stage of care
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
MAD_temp <- ifelse(abs(3-MAD_12month) < abs(17-MAD_12month), abs(3-MAD_12month), abs(17-MAD_12month))
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




### Preparing data ###







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


# Plotting data from table
# This is a bit easier to digest. 
# blue = blounts
# orange = rickets
# red = others

ggplot() +
  
  geom_line(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, group = 1), color = 'blue')+
  geom_point(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means), color = 'blue')+
  geom_errorbar(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_alignment_abs, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Absolute Alignment (cm)")+
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
  geom_errorbar(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
  geom_errorbar(data = diagnosis_mechAxis_abs, mapping = aes(x = StagesOfCare, y = Others_means, ymin = Others_lower2sd, ymax = Others_upper2sd, width = 0.1), color='red')+
  
  labs(x = "Stage of Care", y = "Absolute distance from normal Mech Axis range (degrees)")+
  scale_x_discrete(limits = rev)




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
  geom_errorbar(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Blounts_means, ymin = Blounts_lower2sd, ymax = Blounts_upper2sd, width = 0.1), color='blue')+
  
  geom_line(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, group = 1), color = 'orange')+
  geom_point(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means), color = 'orange')+
  geom_errorbar(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Rickets_means, ymin = Rickets_lower2sd, ymax = Rickets_upper2sd, width = 0.1), color='orange')+
  
  geom_line(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Others_means, group = 1), color = 'red')+
  geom_point(data = diagnosis_MAD_abs, mapping = aes(x = StagesOfCare, y = Others_means), color = 'red')+
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
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_alignment, xlab = "Days in Cast", ylab = "Alignment at 12 months (cm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_mechAxis, xlab = "Days in Cast", ylab = "Mech Axis at 12 months (degrees)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MAD, xlab = "Days in Cast", ylab = "Mech Axis Deviation at 12 months (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MPTA,  xlab = "Days in Cast", ylab = "MPTA at 12 months (degrees)")



# Visually plotting days in cast vs outcomes
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, alignment_12month_dist, xlab = "Days in Cast", ylab = "Distance from normal Alignment at 12 months (cm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, mechAxis_12month_dist, xlab = "Days in Cast", ylab = "Distance from normal Mech Axis at 12 months (degrees)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, MAD_12month_dist , xlab = "Days in Cast", ylab = "Distance from normal Mech Axis Deviation at 12 months (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, MPTA_12month_dist,  xlab = "Days in Cast", ylab = "Distance from normal MPTA at 12 months (degrees)")



# Linear Regression - ALIGNMENT
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop Alignment
# Results: Model 1: Not significant
#          Model 2: Significant, but Preop Alignment was the only significant variable
model_alignment_1 <- lm(alignment_12month_dist[blounts_indicies | rickets_indicies] ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery[blounts_indicies | rickets_indicies]))
model_alignment_2 <- lm(alignment_12month_dist[blounts_indicies | rickets_indicies] ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery[blounts_indicies | rickets_indicies]) + factor(cohort_SEX[blounts_indicies | rickets_indicies]) + cohort_BMI[blounts_indicies | rickets_indicies] + as.numeric(cohort_AGE[blounts_indicies | rickets_indicies]) + alignment_preop_dist[blounts_indicies | rickets_indicies] + factor(cohort_assess_preop$DIagnosis[blounts_indicies | rickets_indicies])) 
summary(model_alignment_1)
summary(model_alignment_2)


# Linear Regression - ALIGNMENT (distance from discharge)
# Model 1:
# Model 2: 
# Results: 
model_alignment_1 <- lm(alignment_change_after_discharge ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_alignment_2 <- lm(alignment_change_after_discharge ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + as.numeric(cohort_AGE) + alignment_preop_dist + factor(cohort_assess_preop$DIagnosis)) 
summary(model_alignment_1)
summary(model_alignment_2)



# Linear Regression - MECH AXIS
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis
# Results: Model 1: Not significant
#          Model 2: Significant, but only  Preop mech Axis were significant. BMI and Age were almost significant.
model_mechAxis_1 <- lm(mechAxis_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_mechAxis_2 <- lm(mechAxis_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + as.numeric(cohort_AGE) + mechAxis_preop_dist + factor(cohort_assess_preop$DIagnosis)) 
summary(model_mechAxis_1)
summary(model_mechAxis_2)



# Linear Regression - MECH AXIS DEVIATION
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis deviation
# Results: Model 1: Significant, days in cast was positively correlated with distance from normal range
#          Model 2: Significant, but Preop mech Axis was the only significant variable. Days in cast no longer significant.
model_MAD_1 <- lm(MAD_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery))
model_MAD_2 <- lm(MAD_12month_dist ~ as.numeric(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery) + factor(cohort_SEX) + cohort_BMI + as.numeric(cohort_AGE) + MAD_preop_dist+ factor(cohort_assess_preop$DIagnosis)) 
summary(model_MAD_1)
summary(model_MAD_2)



# Linear Regression - MPTA
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis deviation
# Results: Model 1: Not significant
# Results: Model 2: Not significant
model_MPTA_1 <- lm(MPTA_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_MPTA_2 <- lm(MPTA_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery + factor(cohort_SEX) + cohort_BMI + cohort_AGE + MPTA_preop_dist) 
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
  labs(x = "Stage of Care", y = "Alignment (cm)")+
  scale_x_discrete(limits = rev)





# We will do the same thing, but:
# 1. We will use absolute values.
# 2. We will use max/min instead of 2sds

data_alignment$Tib_means[1] <- mean(alignment_preop_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_means[2] <- mean(alignment_discharge_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_means[3] <- mean(alignment_12month_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_upper2sd[1] <- max(alignment_preop_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_upper2sd[2] <- max(alignment_discharge_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_upper2sd[3] <- max(alignment_12month_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_lower2sd[1] <- min(alignment_preop_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_lower2sd[2] <- min(alignment_discharge_dist[blounts_indicies & tib_indicies])
data_alignment$Tib_lower2sd[3] <- min(alignment_12month_dist[blounts_indicies & tib_indicies])

data_alignment$Other_means[1] <- mean(alignment_preop_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_means[2] <- mean(alignment_discharge_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_means[3] <- mean(alignment_12month_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_upper2sd[1] <- max(alignment_preop_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_upper2sd[2] <- max(alignment_discharge_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_upper2sd[3] <- max(alignment_12month_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_lower2sd[1] <- min(alignment_preop_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_lower2sd[2] <- min(alignment_discharge_dist[blounts_indicies & !(tib_indicies)])
data_alignment$Other_lower2sd[3] <- min(alignment_12month_dist[blounts_indicies & !(tib_indicies)])



# Plotting data from table, but with absolute values.

ggplot() +
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, group = 1), color = 'blue')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means), color = 'blue')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Tib_means, ymin = Tib_lower2sd, ymax = Tib_upper2sd, width = 0.1), color='blue')+
  geom_line(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, group = 1), color = 'orange')+
  geom_point(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means), color = 'orange')+
  geom_errorbar(data = data_alignment, mapping = aes(x = StagesOfCare, y = Other_means, ymin = Other_lower2sd, ymax = Other_upper2sd, width = 0.1), color='orange')+
  labs(x = "Stage of Care", y = "Alignment (cm)")+
  scale_x_discrete(limits = rev)






# In both plots, its clear that there was more variability at 12month for blounts treated with 
# tib elevation than other techniques.






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
model_alignment_dist <- lm(alignment_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
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

model_MPTA_dist_2 <- lm(MPTA_12month_dist ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE + MPTA_preop_dist)
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
# raw values
boxplot((cohort_12month_alignment - cohort_discharge_alignment)[blounts_indicies],
        (cohort_12month_alignment - cohort_discharge_alignment)[rickets_indicies],
        (cohort_12month_alignment - cohort_discharge_alignment)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Change in Alignment from Discharge to 12month (cm)")
# absolute values
boxplot(abs(cohort_12month_alignment - cohort_discharge_alignment)[blounts_indicies],
        abs(cohort_12month_alignment - cohort_discharge_alignment)[rickets_indicies],
        abs(cohort_12month_alignment - cohort_discharge_alignment)[others_indicies], 
        names = c("Blounts", "Rickets", "Others"),
        xlab = "Diagnosis",
        ylab = "Absolute Change in Alignment from Discharge to 12month (cm)")
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
     ylab = "Absolute Change in Alignment from Discharge to 12month (mm)")


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
        ylab = "Change in Alignment from Discharge to 12Month (mm)")
t.test(change_alignment_tib, change_alignment_not_tib)
# GIVEN THE PLOT, BOXPLOT, AND T-TEST, IT IS POSSIBLE THAT TIB ELEVATION COULD HAVE LED TO 
# BETTER MAINTAINED CHANGES, BUT IT IS NOT STATISTICALLY SIGNIFICANT.




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




# HOW DO BMI, AGE, AND SEX INTERACT IN A LINAER MODEL WITH CHANGE IN ALIGNMENT?
################################################################################
# absolute values
m_DEMO_alignment <- lm(abs(cohort_12month_alignment - cohort_discharge_alignment) ~ cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$Sex))
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
        xlab = "Diagnosis",
        ylab = "Absolute Change in Mech Axis from Discharge to 12month (degrees)")
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
     ylab = "Absolute Change in Mech Axis from Discharge to 12 Month (degrees)")
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
boxplot(change_mechAxis_tib, change_mechAxis_not_tib,
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Change in Mech Axis from Discharge to 12Month (degrees)")
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
m_DEMO_mechAxis <- lm(abs(cohort_12month_mechAxis - cohort_discharge_mechAxis) ~ cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$Sex))
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
        xlab = "Diagnosis",
        ylab = "Change in MAD from Discharge to 12month (mm)")
# BLOUNTS HAD MORE POSITIVE CHANGE WHILE RICKETS HAD MORE NEGATIVE
# IN TERMS OF ABSOLUTE CHANGE, THE TWO GROUPS LOOK ABOUT THE SAME




# DID LENGTH OF TIME IN CAST AFFECT CHANGE IN MAD?
#########################################################
# raw values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, cohort_12month_MAD - cohort_discharge_MAD,
     xlab = "Days in Cast From ",
     ylab = "Change in MPTA from Discharge to 12 Month (degrees)"))
# absolute values
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, abs(cohort_12month_MAD - cohort_discharge_MAD))
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
boxplot(change_MAD_tib, change_MAD_not_tib,
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Change in Mech Axis from Discharge to 12 Month (degrees)")
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
m_DEMO_MAD <- lm(abs(cohort_12month_MAD - cohort_discharge_MAD) ~ cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$Sex))
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
        xlab = "Diagnosis",
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
     xlab = "Days in Cast From First Day of Surgery",
     ylab = "Absolute Change in MPTA from Discharge to 12Month (degrees)")
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
boxplot(change_MPTA_tib, change_MPTA_not_tib,
        names = c("Tibial Elevation", "Other Technique"),
        xlab = "Surgical Technique",
        ylab = "Change in MPTA from Discharge to 12Month (degrees)")
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
m_DEMO_MPTA <- lm(abs(cohort_12month_MPTA - cohort_discharge_MPTA) ~ cohort_BMI + as.numeric(cohort_AGE) + factor(cohort_assess_preop$Sex))
summary(m_DEMO_MPTA)
# NOT WELL, DOESN'T LOOK LIKE THEY HAVE ANY AFFECT ON CHANGE IN MPTA
