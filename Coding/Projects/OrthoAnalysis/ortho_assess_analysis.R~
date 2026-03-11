#############################################################################################
# Section 1: Acquiring cohorts/demographics and preparing data for analysis
#
#############################################################################################

# Getting data
assess <- read.csv("Assess_Cleaned.csv")
# Filtering for legs operated on
assess <- assess[assess$Operated.on. == "Y",]
# Removing SLF 8326 (Sesay)
assess <- assess[!(assess$SLF.. == 8326),]
# SLF 8216 has both rickets and blounts listed in the Diagnosis column. Rachel says he has blounts
assess$DIagnosis[(assess$SLF.. == "8216")] <- "blounts"
# Making IM measurements negative
assess$cleaned_Alignment..V2..in.cm <- ifelse(assess$cleaned_Alignment..V2..measurement.type == "IM", -as.numeric(assess$cleaned_Alignment..V2..in.cm), as.numeric(assess$cleaned_Alignment..V2..in.cm))
#nrow(assess[grepl("blounts", assess$DIagnosis) & grepl("rickets", assess$DIagnosis), ])
# Cleaning diagnosis column (standardizing to blounts, rickets or other)
assess$DIagnosis[grepl("blounts", tolower(assess$DIagnosis))] <- "blounts"
assess$DIagnosis[grepl("rickets", tolower(assess$DIagnosis))] <- "rickets"
assess$DIagnosis[!(grepl("blounts", tolower(assess$DIagnosis))) & !(grepl("rickets", assess$DIagnosis))] <- "others"
# Good! All rows have been standardized
nrow(assess[!(assess$DIagnosis == "blounts" | assess$DIagnosis == "rickets" | assess$DIagnosis == "others"),])
# Cleaning sex column
assess$Sex <- ifelse(grepl("m", tolower(assess$Sex)), "M", "F")
# Replacing NAs with empty strings (coding is simpler when I don't have both NAs and "" to worry about)
assess[is.na(assess)] <- "" 
# Creating Weeks.In.Cast value. 
assess$Weeks.in.Cast <- ifelse(as.numeric(assess$Number.of.days.cast.was.on.from.first.day.of.surgery) %% 7 >= 4, 
                               ceiling(as.numeric(assess$Number.of.days.cast.was.on.from.first.day.of.surgery) / 7), 
                               floor(as.numeric(assess$Number.of.days.cast.was.on.from.first.day.of.surgery) / 7))



# Partitioning data into Preop, Discharge and 12 Month Postop
assess_preop <- assess[assess$Stage.of.Care == "Assess_Preop",]
assess_xafterwedge <- assess[assess$Stage.of.Care == "XAssess_After_Wedge",]
assess_xoutofcast <- assess[assess$Stage.of.Care == "XAssess_Immediately_Out_of_Cast",]
assess_discharge <- assess[assess$Stage.of.Care == "Assess_Initial_Discharge",]
assess_12month <- assess[assess$Stage.of.Care == "Assess_12_month_postop", ]



###########################################
# Start Loop

# Columns we are checking:
# [24] "cleaned_Alignment..V2..in.cm"                                                                                              
# [25] "cleaned_Alignment..V2..measurement.type"
# [39] "cleaned_mech_axis_degrees"                                                                                                 
# [40] "cleaned_mech_axis_varus_valgus" 
# [41] "Mech.Axis.Deviation..mm."
# [45] "cleaned_MPTA..measured.from.the.lateral.tibial.plateau.for.blouts."  

Index_removal_list <- rep(c(TRUE), nrow(assess_preop))
for(i in 1:nrow(assess_preop)){
  # counter
  print(i)
  
  
  
  ### PREOP: If alignment values, Mech axis values or MAD are empty, remove leg###
  for(j in c(24, 39, 41)){
    if(assess_preop[i,j] == "" ){
      Index_removal_list[i] <- FALSE
    }
  }
  ### PREOP: If missing varus/valgus AND Alignment does not equal 0, remove leg ###
  if(assess_preop[i,25] == "" && assess_preop[i,24] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  ### PREOP: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(assess_preop[i,40] == "" && assess_preop[i,39] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  
  
  ### XAFTERWEDGE: (NOT CHECKING ALIGNMENT)
  ### If Mech axis values or MAD or MPTA are empty, remove leg
  for(j in c(39, 41)){
    if(assess_xafterwedge[i,j] == "" ){
      Index_removal_list[i] <- FALSE
    }
  }
  
  ### XAFTERWEDGE: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(assess_xafterwedge[i,40] == "" && assess_xafterwedge[i,39] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  
  ### XOUTOFCAST: (NOT CHECKING ALIGNMENT)
  ### Mech axis values, MAD or MPTA are empty, remove leg###
  for(j in c(39, 41)){
    if(assess_xoutofcast[i,j] == "" ){
      Index_removal_list[i] <- FALSE
    }
  }
  
  ### XOUTOFCAST: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(assess_xoutofcast[i,40] == "" && assess_xoutofcast[i,39] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  
  
  
  ### DISCHARGE: If alignment values, Mech axis values, MAD or MPTA are empty, remove leg ###
  for(j in c(24, 39, 41)){
    if(assess_discharge[i,j] == "" ){
      Index_removal_list[i] <- FALSE
    }
  }
  ### DISCHARGE: If missing varus/valgus AND Alignment does not equal 0, remove leg ###
  if(assess_discharge[i,25] == "" && assess_discharge[i,24] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  ### DISCHARGE: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(assess_discharge[i,40] == "" && assess_discharge[i,39] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  
  
  ### 12 MONTH: If alignment values, Mech axis values, MAD or MPTA are empty, remove leg ###
  for(j in c(24, 39, 41)){
    if(assess_12month[i,j] == "" ){
      Index_removal_list[i] <- FALSE
    }
  }
  ### 12 MONTH: If missing varus/valgus AND Alignment does not equal 0, remove leg ###
  if(assess_12month[i,25] == "" && assess_12month[i,24] != 0){
    Index_removal_list[i] <- FALSE
  }
  
  ### 12 MONTH: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(assess_12month[i,40] == "" && assess_12month[i,39] != 0){
    Index_removal_list[i] <- FALSE
  }
}

###########################################
# End Loop





# Acquiring cohorts and partitioning by stage of care
cohort_assess_preop <- assess_preop[Index_removal_list,]
cohort_assess_xafterwedge <- assess_xafterwedge[Index_removal_list,]
cohort_assess_xoutofcast <- assess_xoutofcast[Index_removal_list,]
cohort_assess_discharge <- assess_discharge[Index_removal_list,]
cohort_assess_12month <- assess_12month[Index_removal_list, ]


# Getting cohort demographics
# Using cohort_assess_preop, because these variables are consistent across all stages

# Number of patients = 39
length(unique(cohort_assess_preop$SLF..))
# Number of knees = 61
nrow(cohort_assess_preop)
# Blounts = 38
nrow(cohort_assess_preop[cohort_assess_preop$DIagnosis == "blounts",])
# Rickets = 11
nrow(cohort_assess_preop[cohort_assess_preop$DIagnosis == "rickets",])
# Other = 12
nrow(cohort_assess_preop[cohort_assess_preop$DIagnosis == "others",])
# Average Age = 8.95082
mean(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Range 4-14
range(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Female to Male (legs) Ratio = 34:27 - 63%
nrow(cohort_assess_preop[cohort_assess_preop$Sex == "M" | cohort_assess_preop$Sex == "m" | cohort_assess_preop$Sex == "M ",] )
# Tibial Plateau Elevation = 26
nrow(cohort_assess_preop[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y", ])
# Number of starting valgus (this one we actually need to be on preop) = 12
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "valgus", ])
# Number of starting varus (this one we actually need to be on preop) = 49
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "varus", ])





### Preparing Data ###

# Getting indicies for dissagregating data in our cohort
tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "Y"
not_tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "N"
IC_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
IM_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"


cohort_preop_MPTA <- as.numeric(cohort_assess_preop$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_discharge_MPTA <-as.numeric(cohort_assess_discharge$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_12month_MPTA <- as.numeric(cohort_assess_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_preop_MAD <- as.numeric(cohort_assess_preop$Mech.Axis.Deviation..mm.)
cohort_discharge_MAD <-as.numeric(cohort_assess_discharge$Mech.Axis.Deviation..mm.)
cohort_12month_MAD <- as.numeric(cohort_assess_12month$Mech.Axis.Deviation..mm.)
cohort_preop_mechAxis <- as.numeric(cohort_assess_preop$cleaned_mech_axis_degrees)
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
mechAxis_discharge_dist <- abs(as.numeric(cohort_assess_discharge$cleaned_mech_axis_degrees))
mechAxis_12month_dist <- abs(as.numeric(cohort_assess_12month$cleaned_mech_axis_degrees))

# MAD normal = 3-17 varus (positive)
MAD_preop <- as.numeric(cohort_assess_preop$Mech.Axis.Deviation..mm.)
MAD_12month <- as.numeric(cohort_assess_12month$Mech.Axis.Deviation..mm.)
MAD_temp <- ifelse(abs(3-MAD_12month) < abs(17-MAD_12month), abs(3-MAD_12month), abs(17-MAD_12month))
MAD_preop_dist <- ifelse(MAD_preop <= 17 & MAD_preop >= 3, 0, abs(17 - MAD_preop)) # all values are greater than 17
MAD_12month_dist <- ifelse(MAD_12month <= 17 & MAD_12month >= 3, 0, MAD_temp)

# MPTA normal = 85-90 degrees 
MPTA_preop <- as.numeric(cohort_assess_preop$cleaned_MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
MPTA_12month <- as.numeric(cohort_assess_12month$cleaned_MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
MPTA_temp_preop <- ifelse(abs(85-MPTA_preop) < abs(90 - MPTA_preop), abs(85-MPTA_preop), abs(90-MPTA_preop))
MPTA_temp_12month <- ifelse(abs(85-MPTA_12month) < abs(90 - MPTA_12month), abs(85-MPTA_12month), abs(90-MPTA_12month))
MPTA_preop_dist <- ifelse(MPTA_preop <= 90 & MPTA_preop >= 85, 0, MPTA_temp_preop)
MPTA_12month_dist <- ifelse(MPTA_12month <= 90 & MPTA_12month >= 85, 0, MPTA_temp_12month)


### Preparing data ###







#############################################################################################
# Section 2: How did diagnosis impact outcomes (alignment)?
#
#############################################################################################
library(ggplot2)



# We will make a table to contain mean and sd values for Alignment for each type of surgery

# normal values
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

# absolute distance from normal (0)
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



# The column names came out weird. I fix it below

colnames(diagnosis_alignment) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                   "Others_upper2sd", "Others_lower2sd")

colnames(diagnosis_alignment_abs) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                   "Others_upper2sd", "Others_lower2sd")



# Filling data table (could write a loop but I think this will be more clear)

diagnosis_alignment$Blounts_means[1] <- mean(cohort_preop_alignment[blounts_indicies])
diagnosis_alignment$Blounts_means[2] <-  mean(cohort_discharge_alignment[blounts_indicies])
diagnosis_alignment$Blounts_means[3] <-  mean(cohort_12month_alignment[blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[1] <- diagnosis_alignment$Blounts_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[2] <- diagnosis_alignment$Blounts_means[2] + 2*sd(cohort_discharge_alignment[blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[3] <- diagnosis_alignment$Blounts_means[3] + 2*sd(cohort_12month_alignment[blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[1] <- diagnosis_alignment$Blounts_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[2] <- diagnosis_alignment$Blounts_means[2] - 2*sd(cohort_discharge_alignment[blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[3] <- diagnosis_alignment$Blounts_means[3] - 2*sd(cohort_12month_alignment[blounts_indicies])


diagnosis_alignment$Rickets_means[1] <- mean(cohort_preop_alignment[rickets_indicies])
diagnosis_alignment$Rickets_means[2] <-  mean(cohort_discharge_alignment[rickets_indicies])
diagnosis_alignment$Rickets_means[3] <-  mean(cohort_12month_alignment[rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[1] <- diagnosis_alignment$Rickets_means[1] + 2*sd(cohort_preop_alignment[rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[2] <- diagnosis_alignment$Rickets_means[2] + 2*sd(cohort_discharge_alignment[rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[3] <- diagnosis_alignment$Rickets_means[3] + 2*sd(cohort_12month_alignment[rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[1] <- diagnosis_alignment$Rickets_means[1] - 2*sd(cohort_preop_alignment[rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[2] <- diagnosis_alignment$Rickets_means[2] - 2*sd(cohort_discharge_alignment[rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[3] <- diagnosis_alignment$Rickets_means[3] - 2*sd(cohort_12month_alignment[rickets_indicies])


diagnosis_alignment$Others_means[1] <- mean(cohort_preop_alignment[others_indicies])
diagnosis_alignment$Others_means[2] <-  mean(cohort_discharge_alignment[others_indicies])
diagnosis_alignment$Others_means[3] <-  mean(cohort_12month_alignment[others_indicies])
diagnosis_alignment$Others_upper2sd[1] <- diagnosis_alignment$Others_means[1] + 2*sd(cohort_preop_alignment[others_indicies])
diagnosis_alignment$Others_upper2sd[2] <- diagnosis_alignment$Others_means[2] + 2*sd(cohort_discharge_alignment[others_indicies])
diagnosis_alignment$Others_upper2sd[3] <- diagnosis_alignment$Others_means[3] + 2*sd(cohort_12month_alignment[others_indicies])
diagnosis_alignment$Others_lower2sd[1] <- diagnosis_alignment$Others_means[1] - 2*sd(cohort_preop_alignment[others_indicies])
diagnosis_alignment$Others_lower2sd[2] <- diagnosis_alignment$Others_means[2] - 2*sd(cohort_discharge_alignment[others_indicies])
diagnosis_alignment$Others_lower2sd[3] <- diagnosis_alignment$Others_means[3] - 2*sd(cohort_12month_alignment[others_indicies])





# Plotting data from table
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





# The above plot is potentially skewed because the majority of rickets are IM and preop and
# vice versa for blounts. Another issue is that, since our sample size is small for rickets 
# (n = 10) and others (n = 8), the 2sds from the mean will create a huge range that, while very likely to capture
# 95% of the population data, may not be representative of the actual spread. We address this 
# problem by replacing 2sds with simply max and mins.

diagnosis_alignment$Blounts_upper2sd[1] <- max(cohort_preop_alignment[blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[2] <- max(cohort_discharge_alignment[blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[3] <- max(cohort_12month_alignment[blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[1] <- min(cohort_preop_alignment[blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[2] <- min(cohort_discharge_alignment[blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[3] <- min(cohort_12month_alignment[blounts_indicies])


diagnosis_alignment$Rickets_upper2sd[1] <- max(cohort_preop_alignment[rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[2] <- max(cohort_discharge_alignment[rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[3] <- max(cohort_12month_alignment[rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[1] <- min(cohort_preop_alignment[rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[2] <- min(cohort_discharge_alignment[rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[3] <- min(cohort_12month_alignment[rickets_indicies])


diagnosis_alignment$Others_upper2sd[1] <- max(cohort_preop_alignment[others_indicies])
diagnosis_alignment$Others_upper2sd[2] <- max(cohort_discharge_alignment[others_indicies])
diagnosis_alignment$Others_upper2sd[3] <- max(cohort_12month_alignment[others_indicies])
diagnosis_alignment$Others_lower2sd[1] <- min(cohort_preop_alignment[others_indicies])
diagnosis_alignment$Others_lower2sd[2] <- min(cohort_discharge_alignment[others_indicies])
diagnosis_alignment$Others_lower2sd[3] <- min(cohort_12month_alignment[others_indicies])



# Plotting data from table
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




# Now, lets try taking the absolute value distance from 0 (normal)
# and plotting rickets vs blounts vs others. (still using min and max instead of 2sd)

# fills table with absolute values. I know this is a lot of code, but hopefully
# what I'm doing is clear.
diagnosis_alignment_abs$Blounts_means[1] <- mean(abs(cohort_preop_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_means[2] <-  mean(abs(cohort_discharge_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_means[3] <-  mean(abs(cohort_12month_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_upper2sd[1] <- max(abs(cohort_preop_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_upper2sd[2] <- max(abs(cohort_discharge_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_upper2sd[3] <- max(abs(cohort_12month_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_lower2sd[1] <- min(abs(cohort_preop_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_lower2sd[2] <- min(abs(cohort_discharge_alignment[blounts_indicies]))
diagnosis_alignment_abs$Blounts_lower2sd[3] <- min(abs(cohort_12month_alignment[blounts_indicies]))


diagnosis_alignment_abs$Rickets_means[1] <- mean(abs(cohort_preop_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_means[2] <-  mean(abs(cohort_discharge_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_means[3] <-  mean(abs(cohort_12month_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_upper2sd[1] <- max(abs(cohort_preop_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_upper2sd[2] <- max(abs(cohort_discharge_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_upper2sd[3] <- max(abs(cohort_12month_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_lower2sd[1] <- min(abs(cohort_preop_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_lower2sd[2] <- min(abs(cohort_discharge_alignment[rickets_indicies]))
diagnosis_alignment_abs$Rickets_lower2sd[3] <- min(abs(cohort_12month_alignment[rickets_indicies]))


diagnosis_alignment_abs$Others_means[1] <- mean(abs(cohort_preop_alignment[others_indicies]))
diagnosis_alignment_abs$Others_means[2] <-  mean(abs(cohort_discharge_alignment[others_indicies]))
diagnosis_alignment_abs$Others_means[3] <-  mean(abs(cohort_12month_alignment[others_indicies]))
diagnosis_alignment_abs$Others_upper2sd[1] <- max(abs(cohort_preop_alignment[others_indicies]))
diagnosis_alignment_abs$Others_upper2sd[2] <- max(abs(cohort_discharge_alignment[others_indicies]))
diagnosis_alignment_abs$Others_upper2sd[3] <- max(abs(cohort_12month_alignment[others_indicies]))
diagnosis_alignment_abs$Others_lower2sd[1] <- min(abs(cohort_preop_alignment[others_indicies]))
diagnosis_alignment_abs$Others_lower2sd[2] <- min(abs(cohort_discharge_alignment[others_indicies]))
diagnosis_alignment_abs$Others_lower2sd[3] <- min(abs(cohort_12month_alignment[others_indicies]))


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



# Here's another plot of blounts vs rickets vs others using absolute value
plot(factor(cohort_assess_preop$DIagnosis), abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)))



# Proportion of blounts legs in cohort in normal range (0)
# ANSWER: 10/38
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
# Section 3: Checking for correlation with outcomes and weeks in cast
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



# Visually plotting days in cast VS outcomes (raw value)
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
model_alignment_1 <- lm(alignment_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_alignment_2 <- lm(alignment_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery + factor(cohort_SEX) + cohort_BMI + cohort_AGE + alignment_preop_dist) 
summary(model_alignment_1)
summary(model_alignment_2)




# Linear Regression - MECH AXIS
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis
# Results: Model 1: Not significant
#          Model 2: Significant, but only  Preop mech Axis were significant. BMI and Age were almost significant.
model_mechAxis_1 <- lm(mechAxis_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_mechAxis_2 <- lm(mechAxis_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery + factor(cohort_SEX) + cohort_BMI + cohort_AGE + mechAxis_preop_dist) 
summary(model_mechAxis_1)
summary(model_mechAxis_2)



# Linear Regression - MECH AXIS DEVIATION
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis deviation
# Results: Model 1: Significant, days in cast was positively correlated with distance from normal range
#          Model 2: Significant, but Preop mech Axis was the only significant variable. Days in cast no longer significant.
model_MAD_1 <- lm(MAD_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_MAD_2 <- lm(MAD_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery + factor(cohort_SEX) + cohort_BMI + cohort_AGE + MAD_preop_dist) 
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
# Section 4: Understand whether the surgical technique impacted long term outcomes 
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

data_alignment$Tib_means[1] <- mean(cohort_preop_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_means[2] <- mean(cohort_discharge_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_means[3] <- mean(cohort_12month_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_upper2sd[1] <- data_alignment$Tib_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_upper2sd[2] <- data_alignment$Tib_means[2] + 2*sd(cohort_discharge_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_upper2sd[3] <- data_alignment$Tib_means[3] + 2*sd(cohort_12month_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_lower2sd[1] <- data_alignment$Tib_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_lower2sd[2] <- data_alignment$Tib_means[2] - 2*sd(cohort_discharge_alignment[blounts_indicies & tib_indicies])
data_alignment$Tib_lower2sd[3] <- data_alignment$Tib_means[3] - 2*sd(cohort_12month_alignment[blounts_indicies & tib_indicies])

data_alignment$Other_means[1] <- mean(cohort_preop_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_means[2] <- mean(cohort_discharge_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_means[3] <- mean(cohort_12month_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_upper2sd[1] <- data_alignment$Other_means[1] + 2*sd(cohort_preop_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_upper2sd[2] <- data_alignment$Other_means[2] + 2*sd(cohort_discharge_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_upper2sd[3] <- data_alignment$Other_means[3] + 2*sd(cohort_12month_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_lower2sd[1] <- data_alignment$Other_means[1] - 2*sd(cohort_preop_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_lower2sd[2] <- data_alignment$Other_means[2] - 2*sd(cohort_discharge_alignment[blounts_indicies & !(tib_indicies)])
data_alignment$Other_lower2sd[3] <- data_alignment$Other_means[3] - 2*sd(cohort_12month_alignment[blounts_indicies & !(tib_indicies)])



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
# Section 5: How did BMI, age, and sex affect outcomes?
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
# Section 6: How did QOL correlate with (absolute distance from normal outcome) OR (absolute change in outcome)
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



#############################################################################################
# Section 7: A closer look at legs with poor outcomes
#############################################################################################




### Legs with poor outcomes = without 70% reduction in distance to normal range ###



# Getting indices of legs without 70% reduction in distance to normal range in at least one outcome
poor_alignment_reduction <- alignment_12month_dist >= (alignment_preop_dist - (alignment_preop_dist * 0.7))
poor_mechAxis_reduction <- mechAxis_12month_dist >= (mechAxis_preop_dist - (mechAxis_preop_dist* 0.7))
poor_MAD_reduction <- MAD_12month_dist >= (MAD_preop_dist - (MAD_preop_dist * 0.7))
poor_MPTA_reduction <- MPTA_12month_dist >= (MPTA_preop_dist - (MPTA_preop_dist * 0.7))
poor_reduction_indicies <- poor_alignment_reduction | poor_mechAxis_reduction | poor_MAD_reduction | poor_MPTA_reduction



# Did patients with poor alignment reduction have worse starting alignment than patients with good alignment
# reduction?
# Doesn't look like it.
alignment_preop_dist[poor_alignment_reduction]
alignment_preop_dist[!(poor_alignment_reduction)]
mean(alignment_preop_dist[poor_alignment_reduction])
mean(alignment_preop_dist[!(poor_alignment_reduction)])
data_alignment <- data.frame(Alignment = alignment_preop_dist, Cohort = ifelse(poor_alignment_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(Alignment ~ Cohort, data = data_alignment)
stripchart(Alignment ~ Cohort, data = data_alignment, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(alignment_preop_dist[poor_alignment_reduction], alignment_preop_dist[!(poor_alignment_reduction)])




# Did the poor alignment reduction cohort and the not poor alignment reduction cohort differ
# in proportion of IC/IM?
# Doesn't look that way
# 12/17 were IC for the poor reduction cohort
# 34/44 were IC for the poor reduction cohort
cohort_assess_preop$cleaned_Alignment..V2..measurement.type[poor_alignment_reduction]
cohort_assess_preop$cleaned_Alignment..V2..measurement.type[!(poor_alignment_reduction)]



# How did BMI differ from the poor alignment reduction cohort and the not poor alignment
# reduction cohort?
# They don't seem to differ. This is in accordance with findings in Section 5 
cohort_BMI[poor_alignment_reduction]
cohort_BMI[!(poor_alignment_reduction)]
mean(cohort_BMI[poor_alignment_reduction])
mean(cohort_BMI[!(poor_alignment_reduction)])
data_alignment <- data.frame(BMI = cohort_BMI, Cohort = ifelse(poor_alignment_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(BMI ~ Cohort, data = data_alignment)
stripchart(BMI ~ Cohort, data = data_alignment, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_BMI[poor_alignment_reduction], cohort_BMI[!(poor_alignment_reduction)])



# How did AGE differ from the poor alignment reduction cohort and the not poor alignment
# reduction cohort?
# They differ significantly. The poor alignment reduction cohort appears to be younger
# than the non poor alignment cohort. See t.test.
# In section 5, age was not seen as significant in most test
cohort_AGE[poor_alignment_reduction]
cohort_AGE[!(poor_alignment_reduction)]
mean(cohort_AGE[poor_alignment_reduction])
mean(cohort_AGE[!(poor_alignment_reduction)])
data_alignment <- data.frame(AGE = cohort_AGE, Cohort = ifelse(poor_alignment_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(AGE ~ Cohort, data = data_alignment)
stripchart(AGE ~ Cohort, data = data_alignment, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_AGE[poor_alignment_reduction], cohort_AGE[!(poor_alignment_reduction)])



# How did SEX differ from the poor alignment reduction cohort and the not poor 
# alignment reduction cohort
# 47% female in poor alignment cohort
# 59% female in not poor alignment cohort
# However, cohorts do not differ significantly on Sex (fishers exact test)
cohort_SEX[poor_alignment_reduction]
cohort_SEX[!(poor_alignment_reduction)]
length(cohort_SEX[cohort_SEX == "F" & poor_alignment_reduction]) / length(cohort_SEX[poor_alignment_reduction])
length(cohort_SEX[cohort_SEX == "F" & !(poor_alignment_reduction)]) / length(cohort_SEX[!(poor_alignment_reduction)])
num_poor_Females <- length(cohort_SEX[cohort_SEX == "F" & poor_alignment_reduction])
num_poor_Males <- length(cohort_SEX[poor_alignment_reduction]) - length(cohort_SEX[cohort_SEX == "F" & poor_alignment_reduction])
num_not_poor_Females <- length(cohort_SEX[cohort_SEX == "F" & !(poor_alignment_reduction)])
num_not_poor_Males <- length(cohort_SEX[!(poor_alignment_reduction)]) - length(cohort_SEX[cohort_SEX == "F" & !(poor_alignment_reduction)])
data <- matrix(c(num_poor_Females, num_not_poor_Females, num_poor_Males, num_not_poor_Males), nrow = 2)
fisher.test(data)



# How did days/weeks in cast differ from the poor alignment reduction cohort and 
# the not poor alignment reduction cohort?
# Given the t.test, there isn't strong evidence to say length in cast played a roll.
cohort_assess_preop$Weeks.in.Cast[poor_alignment_reduction]
cohort_assess_preop$Weeks.in.Cast[!(poor_alignment_reduction)]
mean(cohort_assess_preop$Weeks.in.Cast[poor_alignment_reduction])
mean(cohort_assess_preop$Weeks.in.Cast[!(poor_alignment_reduction)])
data_alignment <- data.frame(Weeks_In_Cast = cohort_assess_preop$Weeks.in.Cast, Cohort = ifelse(poor_alignment_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(Weeks_In_Cast ~ Cohort, data = data_alignment)
stripchart(Weeks_In_Cast ~ Cohort, data = data_alignment, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_AGE[poor_alignment_reduction], cohort_AGE[!(poor_alignment_reduction)])



# Did the proportion of blounts/rickets cases differ significantly from the poor alignment
# reduction cohort and the not poor alignment reduction cohort.
# poor alignment cohort: 10/17 blounts, 4/17 rickets, 3/17 other
# not poor alignment cohort: 28/44 blounts, 7/44 rickets, 9/44 other
length(alignment_12month_dist[poor_alignment_reduction & blounts_indicies])
length(alignment_12month_dist[poor_alignment_reduction & rickets_indicies])
length(alignment_12month_dist[!(poor_alignment_reduction) & blounts_indicies])
length(alignment_12month_dist[!(poor_alignment_reduction) & rickets_indicies])



# How did the QOL scores differ from the poor alignment reduction cohort and the 
# not poor alignment reduction cohort?
# This is odd: The cohorts do not differ significantly in QOL scores at preop/12month.
#              In fact (though not statistically significant) the mean QOL score was 
#              lower for the poor alignment reduction cohort at preop, but higher at
#              12 months! We would not expect this. They also do not differ significantly
#              in QOL score change from preop to 12months. However, the change was obviously
#              greater in the poor alignment reduction cohort.
pa_scores_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL[poor_alignment_reduction])
npa_scores_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL[!(poor_alignment_reduction)])
pa_scores_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL[poor_alignment_reduction])
npa_scores_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL[!(poor_alignment_reduction)])
mean(pa_scores_preop, na.rm = TRUE)
mean(npa_scores_preop, na.rm = TRUE)
mean(pa_scores_12month, na.rm = TRUE)
mean(npa_scores_12month, na.rm = TRUE)
data_alignment_preop <- data.frame(QOLSUM = as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL), Cohort = ifelse(poor_alignment_reduction, "poor_reduction", "not_poor_reduction"))
data_alignment_12month <- data.frame(QOLSUM = as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL), Cohort = ifelse(poor_alignment_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(QOLSUM ~ Cohort, data = data_alignment_preop)
stripchart(QOLSUM ~ Cohort, data = data_alignment_preop, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
boxplot(QOLSUM ~ Cohort, data = data_alignment_12month)
stripchart(QOLSUM ~ Cohort, data = data_alignment_12month, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(pa_scores_preop, npa_scores_preop)
t.test(pa_scores_12month, npa_scores_12month)
boxplot(pa_scores_12month - pa_scores_preop, npa_scores_12month - npa_scores_preop)
t.test(pa_scores_12month - pa_scores_preop, npa_scores_12month - npa_scores_preop)



# Did patients with poor mech Axis reduction have worse starting mech Axis than patients with good mech Axis
# reduction?
# Doesn't look like it.
mechAxis_preop_dist[poor_mechAxis_reduction]
mechAxis_preop_dist[!(poor_mechAxis_reduction)]
mean(mechAxis_preop_dist[poor_mechAxis_reduction])
mean(mechAxis_preop_dist[!(poor_mechAxis_reduction)])
data_mechAxis <- data.frame(mechAxis = mechAxis_preop_dist, Cohort = ifelse(poor_mechAxis_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(mechAxis ~ Cohort, data = data_mechAxis)
stripchart(mechAxis ~ Cohort, data = data_mechAxis, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(mechAxis_preop_dist[poor_mechAxis_reduction], mechAxis_preop_dist[!(poor_mechAxis_reduction)])




# How did BMI differ from the poor mechAxis reduction cohort and the not poor mechAxis
# reduction cohort?
# They don't differ significantly.
# However, the not poor reduction cohort seems to be more clustered around 20.
cohort_BMI[poor_mechAxis_reduction]
cohort_BMI[!(poor_mechAxis_reduction)]
mean(cohort_BMI[poor_mechAxis_reduction])
mean(cohort_BMI[!(poor_mechAxis_reduction)])
data_mechAxis <- data.frame(BMI = cohort_BMI, Cohort = ifelse(poor_mechAxis_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(BMI ~ Cohort, data = data_mechAxis)
stripchart(BMI ~ Cohort, data = data_mechAxis, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_BMI[poor_mechAxis_reduction], cohort_BMI[!(poor_mechAxis_reduction)])



# How did AGE differ from the poor mechAxis reduction cohort and the not poor mechAxis
# reduction cohort?
# They do not differ significantly. However, the poor reduction cohort has a lower average
# age.
cohort_AGE[poor_mechAxis_reduction]
cohort_AGE[!(poor_mechAxis_reduction)]
mean(cohort_AGE[poor_mechAxis_reduction])
mean(cohort_AGE[!(poor_mechAxis_reduction)])
data_mechAxis <- data.frame(AGE = cohort_AGE, Cohort = ifelse(poor_mechAxis_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(AGE ~ Cohort, data = data_mechAxis)
stripchart(AGE ~ Cohort, data = data_mechAxis, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_AGE[poor_mechAxis_reduction], cohort_AGE[!(poor_mechAxis_reduction)])



# How did SEX differ from the poor mechAxis reduction cohort and the not poor 
# mechAxis reduction cohort
# 72.2% female in poor mechAxis cohort
# 48.8% female in not poor mechAxis cohort
# However, cohorts do not differ significantly on Sex (fishers exact test)
cohort_SEX[poor_mechAxis_reduction]
cohort_SEX[!(poor_mechAxis_reduction)]
length(cohort_SEX[cohort_SEX == "F" & poor_mechAxis_reduction]) / length(cohort_SEX[poor_mechAxis_reduction])
length(cohort_SEX[cohort_SEX == "F" & !(poor_mechAxis_reduction)]) / length(cohort_SEX[!(poor_mechAxis_reduction)])
num_poor_Females <- length(cohort_SEX[cohort_SEX == "F" & poor_mechAxis_reduction])
num_poor_Males <- length(cohort_SEX[poor_mechAxis_reduction]) - length(cohort_SEX[cohort_SEX == "F" & poor_mechAxis_reduction])
num_not_poor_Females <- length(cohort_SEX[cohort_SEX == "F" & !(poor_mechAxis_reduction)])
num_not_poor_Males <- length(cohort_SEX[!(poor_mechAxis_reduction)]) - length(cohort_SEX[cohort_SEX == "F" & !(poor_mechAxis_reduction)])
data <- matrix(c(num_poor_Females, num_not_poor_Females, num_poor_Males, num_not_poor_Males), nrow = 2)
fisher.test(data)



# How did days/weeks in cast differ from the poor mechAxis reduction cohort and 
# the not poor mechAxis reduction cohort?
# Do not differ significantly.
# Given the t.test, there isn't strong evidence to say length in cast played a roll.
cohort_assess_preop$Weeks.in.Cast[poor_mechAxis_reduction]
cohort_assess_preop$Weeks.in.Cast[!(poor_mechAxis_reduction)]
mean(cohort_assess_preop$Weeks.in.Cast[poor_mechAxis_reduction])
mean(cohort_assess_preop$Weeks.in.Cast[!(poor_mechAxis_reduction)])
data_mechAxis <- data.frame(Weeks_In_Cast = cohort_assess_preop$Weeks.in.Cast, Cohort = ifelse(poor_mechAxis_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(Weeks_In_Cast ~ Cohort, data = data_mechAxis)
stripchart(Weeks_In_Cast ~ Cohort, data = data_mechAxis, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_assess_preop$Weeks.in.Cast[poor_mechAxis_reduction],cohort_assess_preop$Weeks.in.Cast[!(poor_mechAxis_reduction)])



# Did the proportion of blounts/rickets cases differ significantly from the poor mechAxis
# reduction cohort and the not poor mechAxis reduction cohort.
# poor mechAxis cohort: 10/18 blounts, 6/18 rickets, 2/18 other
# not poor mechAxis cohort: 28/43 blounts, 5/43 rickets, 10/43 other
length(mechAxis_12month_dist[poor_mechAxis_reduction & blounts_indicies])
length(mechAxis_12month_dist[poor_mechAxis_reduction & rickets_indicies])
length(mechAxis_12month_dist[!(poor_mechAxis_reduction) & blounts_indicies])
length(mechAxis_12month_dist[!(poor_mechAxis_reduction) & rickets_indicies])
data <- matrix(c(10, 28, 6, 5), nrow = 2)
fisher.test(data)



# How did the QOL scores differ from the poor mechAxis reduction cohort and the 
# not poor mechAxis reduction cohort?
# This is odd: The cohorts do not differ significantly in QOL scores at preop/12month.
#              In fact (though not statistically significant) the mean QOL score was 
#              lower for the poor mechAxis reduction cohort at preop, but higher at
#              12 months! We would not expect this. They also do not differ significantly
#              in QOL score change from preop to 12months. However, the change was obviously
#              greater in the poor mechAxis reduction cohort.
pa_scores_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL[poor_mechAxis_reduction])
npa_scores_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL[!(poor_mechAxis_reduction)])
pa_scores_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL[poor_mechAxis_reduction])
npa_scores_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL[!(poor_mechAxis_reduction)])
mean(pa_scores_preop, na.rm = TRUE)
mean(npa_scores_preop, na.rm = TRUE)
mean(pa_scores_12month, na.rm = TRUE)
mean(npa_scores_12month, na.rm = TRUE)
data_mechAxis_preop <- data.frame(QOLSUM = as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL), Cohort = ifelse(poor_mechAxis_reduction, "poor_reduction", "not_poor_reduction"))
data_mechAxis_12month <- data.frame(QOLSUM = as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL), Cohort = ifelse(poor_mechAxis_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(QOLSUM ~ Cohort, data = data_mechAxis_preop, xlab = "Preop")
stripchart(QOLSUM ~ Cohort, data = data_mechAxis_preop, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
boxplot(QOLSUM ~ Cohort, data = data_mechAxis_12month, xlab = "12month")
stripchart(QOLSUM ~ Cohort, data = data_mechAxis_12month, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(pa_scores_preop, npa_scores_preop)
t.test(pa_scores_12month, npa_scores_12month)
boxplot(pa_scores_12month - pa_scores_preop, npa_scores_12month - npa_scores_preop)
t.test(pa_scores_12month - pa_scores_preop, npa_scores_12month - npa_scores_preop)




# Did patients with poor MPTA reduction have worse starting MPTA than patients with good MPTA reduction?
# Doesn't look like it.
# also consider small sample size
# WHAT. The starting distance from the normal range for the poor reduction cohort was less than the not poor
# reduction cohort. This is statistically significant.
temp <- data.frame(MPTA_p = MPTA_preop, MTPA_p_dist = MPTA_preop_dist, MPTA_12 = MPTA_12month, MPTA_12_dist = MPTA_12month_dist)
MPTA_preop_dist[poor_MPTA_reduction]
MPTA_preop_dist[!(poor_MPTA_reduction)]
mean(MPTA_preop_dist[poor_MPTA_reduction], na.rm = TRUE)
mean(MPTA_preop_dist[!(poor_MPTA_reduction)], na.rm = TRUE)
data_MPTA <- data.frame(MPTA = MPTA_preop_dist, Cohort = ifelse(poor_MPTA_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(MPTA ~ Cohort, data = data_MPTA)
stripchart(MPTA ~ Cohort, data = data_MPTA, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(MPTA_preop_dist[poor_MPTA_reduction], MPTA_preop_dist[!(poor_MPTA_reduction)])




# How did BMI differ from the poor MPTA reduction cohort and the not poor MPTA
# reduction cohort?
# They don't differ significantly.
cohort_BMI[poor_MPTA_reduction]
cohort_BMI[!(poor_MPTA_reduction)]
mean(cohort_BMI[poor_MPTA_reduction], na.rm = TRUE)
mean(cohort_BMI[!(poor_MPTA_reduction)], na.rm = TRUE)
data_MPTA <- data.frame(BMI = cohort_BMI, Cohort = ifelse(poor_MPTA_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(BMI ~ Cohort, data = data_MPTA)
stripchart(BMI ~ Cohort, data = data_MPTA, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_BMI[poor_MPTA_reduction], cohort_BMI[!(poor_MPTA_reduction)])



# How did AGE differ from the poor MPTA reduction cohort and the not poor MPTA
# reduction cohort?
# They do differ significantly. The poor reduction cohort was younger than the not poor reduction cohort
cohort_AGE[poor_MPTA_reduction]
cohort_AGE[!(poor_MPTA_reduction)]
mean(cohort_AGE[poor_MPTA_reduction], na.rm = TRUE)
mean(cohort_AGE[!(poor_MPTA_reduction)], na.rm = TRUE)
data_MPTA <- data.frame(AGE = cohort_AGE, Cohort = ifelse(poor_MPTA_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(AGE ~ Cohort, data = data_MPTA)
stripchart(AGE ~ Cohort, data = data_MPTA, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_AGE[poor_MPTA_reduction], cohort_AGE[!(poor_MPTA_reduction)])



# How did SEX differ from the poor MPTA reduction cohort and the not poor 
# MPTA reduction cohort
# 50% female in poor MPTA cohort
# 60% female in not poor MPTA cohort
# However, cohorts do not differ significantly on Sex (fishers exact test)
cohort_SEX[poor_MPTA_reduction]
cohort_SEX[!(poor_MPTA_reduction)]
length(cohort_SEX[cohort_SEX == "F" & poor_MPTA_reduction]) / length(cohort_SEX[poor_MPTA_reduction])
length(cohort_SEX[cohort_SEX == "F" & !(poor_MPTA_reduction)]) / length(cohort_SEX[!(poor_MPTA_reduction)])
num_poor_Females <- length(cohort_SEX[cohort_SEX == "F" & poor_MPTA_reduction])
num_poor_Males <- length(cohort_SEX[poor_MPTA_reduction]) - length(cohort_SEX[cohort_SEX == "F" & poor_MPTA_reduction])
num_not_poor_Females <- length(cohort_SEX[cohort_SEX == "F" & !(poor_MPTA_reduction)])
num_not_poor_Males <- length(cohort_SEX[!(poor_MPTA_reduction)]) - length(cohort_SEX[cohort_SEX == "F" & !(poor_MPTA_reduction)])
data <- matrix(c(num_poor_Females, num_not_poor_Females, num_poor_Males, num_not_poor_Males), nrow = 2)
fisher.test(data)



# How did days/weeks in cast differ from the poor MPTA reduction cohort and 
# the not poor MPTA reduction cohort?
# Do not differ significantly.
# Given the t.test, there isn't strong evidence to say length in cast played a roll.
cohort_assess_preop$Weeks.in.Cast[poor_MPTA_reduction]
cohort_assess_preop$Weeks.in.Cast[!(poor_MPTA_reduction)]
mean(cohort_assess_preop$Weeks.in.Cast[poor_MPTA_reduction], na.rm = TRUE)
mean(cohort_assess_preop$Weeks.in.Cast[!(poor_MPTA_reduction)], na.rm = TRUE)
data_MPTA <- data.frame(Weeks_In_Cast = cohort_assess_preop$Weeks.in.Cast, Cohort = ifelse(poor_MPTA_reduction, "poor_reduction", "not_poor_reduction"))
boxplot(Weeks_In_Cast ~ Cohort, data = data_MPTA)
stripchart(Weeks_In_Cast ~ Cohort, data = data_MPTA, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_assess_preop$Weeks.in.Cast[poor_MPTA_reduction], cohort_assess_preop$Weeks.in.Cast[!(poor_MPTA_reduction)])



# Did the proportion of blounts/rickets cases differ significantly from the poor MPTA
# reduction cohort and the not poor MPTA reduction cohort.
# poor MPTA cohort: 18/32 blounts, 8/32 rickets, 6/32 other
# not poor MPTA cohort: 20/30 blounts, 3/30 rickets, 7/30 other
# also consider small sample size
length(MPTA_12month_dist[poor_MPTA_reduction & blounts_indicies])
length(MPTA_12month_dist[poor_MPTA_reduction & rickets_indicies])
length(MPTA_12month_dist[!(poor_MPTA_reduction) & blounts_indicies])
length(MPTA_12month_dist[!(poor_MPTA_reduction) & rickets_indicies])
data <- matrix(c(18, 20, 8, 3), nrow = 2)
fisher.test(data)



# How did the QOL scores differ from the poor MPTA reduction cohort and the 
# not poor MPTA reduction cohort?
# This is odd: The preop scores did not differ significantly. However the 12month scores 
# did differ significantly with the poor reduction cohort having better QOL.

pa_scores_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL[poor_MPTA_reduction])
npa_scores_preop <- as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL[!(poor_MPTA_reduction)])
pa_scores_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL[poor_MPTA_reduction])
npa_scores_12month <- as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL[!(poor_MPTA_reduction)])
mean(pa_scores_preop, na.rm = TRUE)
mean(npa_scores_preop, na.rm = TRUE)
mean(pa_scores_12month, na.rm = TRUE)
mean(npa_scores_12month, na.rm = TRUE)
data_MPTA_preop <- data.frame(QOLSUM = as.numeric(cohort_assess_preop$SUM.TOTAL.MS.QOL), Cohort = ifelse(poor_MPTA_reduction, "poor_reduction", "not_poor_reduction"))
data_MPTA_12month <- data.frame(QOLSUM = as.numeric(cohort_assess_12month$SUM.TOTAL.MS.QOL), Cohort = ifelse(poor_MPTA_reduction, "poor_reduction", "not_poor_reduction"))
par(mfrow=c(1,2))
boxplot(QOLSUM ~ Cohort, data = data_MPTA_preop, xlab = "Preop")
stripchart(QOLSUM ~ Cohort, data = data_MPTA_preop, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
boxplot(QOLSUM ~ Cohort, data = data_MPTA_12month, xlab = "12month")
stripchart(QOLSUM ~ Cohort, data = data_MPTA_12month, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(pa_scores_preop, npa_scores_preop)
t.test(pa_scores_12month, npa_scores_12month)
boxplot(pa_scores_12month - pa_scores_preop, npa_scores_12month - npa_scores_preop)
par(mfrow=c(1,1))
t.test(pa_scores_12month - pa_scores_preop, npa_scores_12month - npa_scores_preop)



# How many patients had at least one outcome without a 70% reduction in distance to normal range?
nrow(cohort_assess_preop[poor_reduction_indicies,])
poor_reduction_cohort <- cohort_assess_preop[poor_reduction_indicies,]



# How many patients had a reduction of 70% in all outcomes?
nrow(cohort_assess_preop[!(poor_reduction_indicies),])
not_poor_reduction_cohort <- cohort_assess_preop[!(poor_reduction_indicies),]



# How did the BMI of the poor reduction cohort compare to the other cohort?
mean(cohort_BMI[poor_reduction_indicies])
mean(cohort_BMI[!(poor_reduction_indicies)])
t.test(cohort_BMI[poor_reduction_indicies], cohort_BMI[!(poor_reduction_indicies)])



# How did the AGE of the poor reduction cohort compare to the other cohort?
# THE POOR REDUCTION COHORT IS YOUNGER!
mean(cohort_AGE[poor_reduction_indicies])
mean(cohort_AGE[!(poor_reduction_indicies)])
data_alignment <- data.frame(Age = cohort_AGE, Cohort = ifelse(poor_reduction_indicies, "poor_reduction", "not_poor_reduction"))
boxplot(Age ~ Cohort, data = data_alignment)
stripchart(Age ~ Cohort, data = data_alignment, method = "jitter", vertical = TRUE, add = TRUE, pch = 19)
t.test(cohort_AGE[poor_reduction_indicies], cohort_AGE[!(poor_reduction_indicies)])



# How did the SEX of the poor reduction cohort compare to the other cohort?
# poor reduction cohort was 48% MEN
# not poor reduction cohort was 35% MEN
# chisquare test did not find significant difference
cohort_SEX[poor_reduction_indicies]
cohort_SEX[!(poor_reduction_indicies)]
length(cohort_SEX[poor_reduction_indicies & cohort_SEX == "M"]) / length(cohort_SEX[poor_reduction_indicies])
length(cohort_SEX[!(poor_reduction_indicies) & cohort_SEX == "M"]) / length(cohort_SEX[!(poor_reduction_indicies)])
cohort_SEX_binary <- ifelse(cohort_SEX == "M", 1, 0)
num_poor_Males <- sum(cohort_SEX_binary[poor_reduction_indicies & cohort_SEX_binary == 1])
num_poor_Females <- length(cohort_SEX_binary[poor_reduction_indicies]) - num_poor_Males
num_not_poor_Males <- sum(cohort_SEX_binary[!(poor_reduction_indicies) & cohort_SEX_binary == 1])
num_not_poor_Females <- length(cohort_SEX_binary[!(poor_reduction_indicies)]) - num_not_poor_Males
data <- matrix(c(num_poor_Males, num_not_poor_Males, num_poor_Females, num_not_poor_Females), nrow = 2)
chisq.test(data)



# For each leg, let's count the number of poor outcomes they had (0 - 4)
cohort_assess_preop$sumPoorReduction <- rep(c(0), nrow(cohort_assess_preop))
cohort_assess_preop$sumPoorReduction <- ifelse(poor_alignment_reduction, cohort_assess_preop$sumPoorReduction + 1, cohort_assess_preop$sumPoorReduction)
cohort_assess_preop$sumPoorReduction <- ifelse(poor_mechAxis_reduction, cohort_assess_preop$sumPoorReduction + 1, cohort_assess_preop$sumPoorReduction)
cohort_assess_preop$sumPoorReduction <- ifelse(poor_MAD_reduction, cohort_assess_preop$sumPoorReduction + 1, cohort_assess_preop$sumPoorReduction)
# Ignoring MPTA for now
# cohort_assess_preop$sumPoorReduction <- ifelse(poor_MPTA_reduction, cohort_assess_preop$sumPoorReduction + 1, cohort_assess_preop$sumPoorReduction)



# Getting indices with 2 or more poor reduction outcomes
more_2_poor_reduction_indicies <- cohort_assess_preop$sumPoorReduction >= 2



# What was the SEX distribution of those with 2 or more poor reduction outcomes?
# 9/13 were female
cohort_assess_preop$Sex[more_2_poor_reduction_indicies]



# What was the AGE distribution of those with 2 or more poor reduction outcomes?
# 7/12 were 6 or less
cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns[more_2_poor_reduction_indicies]
mean(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns[more_2_poor_reduction_indicies])



# What was the SEX distribution of those with 1 or less poor reduction outcomes?
# 25/48 were female
length(cohort_assess_preop$Sex[!(more_2_poor_reduction_indicies) & cohort_assess_preop$Sex == "F"])
length(cohort_assess_preop$Sex[!(more_2_poor_reduction_indicies)])



# What was the AGE distribution of those with 1 or less poor reduction outcomes?
# 7/48 were 6 or less
cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns[!(more_2_poor_reduction_indicies)]
mean(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns[!(more_2_poor_reduction_indicies)])



# What was the QOL scores of those with 2 or more poor reduction outcomes?
# Lowest was 80
cohort_assess_12month$SUM.TOTAL.MS.QOL[more_2_poor_reduction_indicies]



# What was the QOL scores of those with 1 or less poor reduction outcomes?
# Lowest was 71
cohort_assess_12month$SUM.TOTAL.MS.QOL[!(more_2_poor_reduction_indicies)]



# This is odd. QOL scores don't seem to correlate much with poor reduction outcomes.
# This leads to the next analysis.



# QUICK THOUGHTS
# It looks like being young and female resulted in poor outcomes


### Second Analysis: Legs with poor outcomes = without 70% reduction in distance to normal range ###
