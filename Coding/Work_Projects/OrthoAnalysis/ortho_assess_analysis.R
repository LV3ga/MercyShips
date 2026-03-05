#############################################################################################
# Step 1: Acquring cohorts and demograpics
#
#############################################################################################

# Getting data
assess <- read.csv("Cleaned_Assess.csv")
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
  
  
  
  ### PREOP: If alignment values, Mech axis values, MAD or MPTA are empty, remove leg###
  for(j in c(24, 39, 41, 45)){
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
  ### If Mech axis values, MAD or MPTA are empty, remove leg
  for(j in c(39, 41, 45)){
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
  for(j in c(39, 41, 45)){
    if(assess_xoutofcast[i,j] == "" ){
      Index_removal_list[i] <- FALSE
    }
  }

  ### XOUTOFCAST: If missing IC/IM AND Mech Axis does not equal 0, remove leg ###
  if(assess_xoutofcast[i,40] == "" && assess_xoutofcast[i,39] != 0){
    Index_removal_list[i] <- FALSE
  }


  
  
  ### DISCHARGE: If alignment values, Mech axis values, MAD or MPTA are empty, remove leg ###
  for(j in c(24, 39, 41, 45)){
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
  for(j in c(24, 39, 41, 45)){
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


# Adding rickets patients only missing MPTA at XAfterWedge back into the cohort
SLF_list <- c("9256", "9285", "9438", "8190", "9256", "9285", "9438")
Leg_list <- c("right","right","right","left","left","left","left")

j <- 1
for(i in 1:nrow(assess_preop)){
  print(i)
  if(assess_preop$SLF..[i] == SLF_list[j] & assess_preop$Right.or.Left.Leg.[i] == Leg_list[j]){
    print(j)
    Index_removal_list[i] <- TRUE
    j <- j + 1
  }
  
  if(j > 7){
    break
  }
}



# Acquiring cohorts and partitioning by stage of care
cohort_assess_preop <- assess_preop[Index_removal_list,]
cohort_assess_xafterwedge <- assess_xafterwedge[Index_removal_list,]
cohort_assess_xoutofcast <- assess_xoutofcast[Index_removal_list,]
cohort_assess_discharge <- assess_discharge[Index_removal_list,]
cohort_assess_12month <- assess_12month[Index_removal_list, ]


# Getting cohort demographics
# Using cohort_assess_preop, because these variables are consistent across all stages

# Number of patients = 31
length(unique(cohort_assess_preop$SLF..))
# Number of knees = 48
nrow(cohort_assess_preop)
# Blounts = 31
nrow(cohort_assess_preop[cohort_assess_preop$DIagnosis == "blounts",])
# Rickets = 10
nrow(cohort_assess_preop[cohort_assess_preop$DIagnosis == "rickets",])
# Other = 7
nrow(cohort_assess_preop[cohort_assess_preop$DIagnosis == "others",])
# Average Age = 9.167
mean(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Range 4-14
range(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Female to Male Ratio = 28:20 - 63%
nrow(cohort_assess_preop[cohort_assess_preop$Sex == "M" | cohort_assess_preop$Sex == "m" | cohort_assess_preop$Sex == "M ",] )
# Tibial Plateau Elevation = 21
nrow(cohort_assess_preop[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y", ])
# Number of starting valgus (this one we actually need to be on preop) = 10
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "valgus", ])
# Number of starting varus (this one we actually need to be on preop) = 38
nrow(cohort_assess_preop[cohort_assess_preop$cleaned_mech_axis_varus_valgus == "varus", ])




# Getting indicies for dissagregating data in our cohort
blounts_indicies <- cohort_assess_preop$DIagnosis == "blounts"
rickets_indicies <- cohort_assess_preop$DIagnosis == "rickets"
others_indicies <- cohort_assess_preop$DIagnosis == "others"
tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "Y"
not_tib_indicies <- cohort_assess_preop$Tibial.Plateau.Elevation. == "N"
IC_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
IM_preop_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"






#############################################################################################
# Step 2: How did diagnosis impact outcomes (alignment)?
#
#############################################################################################
library(ggplot2)



### Preparing data ###
cohort_preop_alignment <- as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm)
cohort_discharge_alignment <- as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm)
cohort_12month_alignment <- as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)

cohort_alignment_IC_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
cohort_alignment_IM_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"
cohort_blounts_indicies <- cohort_assess_preop$DIagnosis == "blounts"
cohort_rickets_indicies <- cohort_assess_preop$DIagnosis == "rickets"
cohort_others_indicies <- cohort_assess_preop$DIagnosis == "others"
### Preparing data ###




# We will make a table to contain mean and sd values for Alignment for each type of surgery

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


# The column names came out weird. I fix it below

colnames(diagnosis_alignment) <- c("StagesOfCare", "Blounts_means", "Blounts_upper2sd", "Blounts_lower2sd", "Rickets_means", "Rickets_upper2sd", "Rickets_lower2sd", "Others_means", 
                                   "Others_upper2sd", "Others_lower2sd")



# Filling data table (could write a loop but I think this will be more clear)

diagnosis_alignment$Blounts_means[1] <- mean(cohort_preop_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_means[2] <-  mean(cohort_discharge_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_means[3] <-  mean(cohort_12month_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[1] <- diagnosis_alignment$Blounts_means[1] + 2*sd(cohort_preop_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[2] <- diagnosis_alignment$Blounts_means[2] + 2*sd(cohort_discharge_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[3] <- diagnosis_alignment$Blounts_means[3] + 2*sd(cohort_12month_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[1] <- diagnosis_alignment$Blounts_means[1] - 2*sd(cohort_preop_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[2] <- diagnosis_alignment$Blounts_means[2] - 2*sd(cohort_discharge_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[3] <- diagnosis_alignment$Blounts_means[3] - 2*sd(cohort_12month_alignment[cohort_blounts_indicies])


diagnosis_alignment$Rickets_means[1] <- mean(cohort_preop_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_means[2] <-  mean(cohort_discharge_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_means[3] <-  mean(cohort_12month_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[1] <- diagnosis_alignment$Rickets_means[1] + 2*sd(cohort_preop_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[2] <- diagnosis_alignment$Rickets_means[2] + 2*sd(cohort_discharge_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[3] <- diagnosis_alignment$Rickets_means[3] + 2*sd(cohort_12month_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[1] <- diagnosis_alignment$Rickets_means[1] - 2*sd(cohort_preop_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[2] <- diagnosis_alignment$Rickets_means[2] - 2*sd(cohort_discharge_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[3] <- diagnosis_alignment$Rickets_means[3] - 2*sd(cohort_12month_alignment[cohort_rickets_indicies])


diagnosis_alignment$Others_means[1] <- mean(cohort_preop_alignment[cohort_others_indicies])
diagnosis_alignment$Others_means[2] <-  mean(cohort_discharge_alignment[cohort_others_indicies])
diagnosis_alignment$Others_means[3] <-  mean(cohort_12month_alignment[cohort_others_indicies])
diagnosis_alignment$Others_upper2sd[1] <- diagnosis_alignment$Others_means[1] + 2*sd(cohort_preop_alignment[cohort_others_indicies])
diagnosis_alignment$Others_upper2sd[2] <- diagnosis_alignment$Others_means[2] + 2*sd(cohort_discharge_alignment[cohort_others_indicies])
diagnosis_alignment$Others_upper2sd[3] <- diagnosis_alignment$Others_means[3] + 2*sd(cohort_12month_alignment[cohort_others_indicies])
diagnosis_alignment$Others_lower2sd[1] <- diagnosis_alignment$Others_means[1] - 2*sd(cohort_preop_alignment[cohort_others_indicies])
diagnosis_alignment$Others_lower2sd[2] <- diagnosis_alignment$Others_means[2] - 2*sd(cohort_discharge_alignment[cohort_others_indicies])
diagnosis_alignment$Others_lower2sd[3] <- diagnosis_alignment$Others_means[3] - 2*sd(cohort_12month_alignment[cohort_others_indicies])





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

diagnosis_alignment$Blounts_upper2sd[1] <- max(cohort_preop_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[2] <- max(cohort_discharge_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_upper2sd[3] <- max(cohort_12month_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[1] <- min(cohort_preop_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[2] <- min(cohort_discharge_alignment[cohort_blounts_indicies])
diagnosis_alignment$Blounts_lower2sd[3] <- min(cohort_12month_alignment[cohort_blounts_indicies])


diagnosis_alignment$Rickets_upper2sd[1] <- max(cohort_preop_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[2] <- max(cohort_discharge_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_upper2sd[3] <- max(cohort_12month_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[1] <- min(cohort_preop_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[2] <- min(cohort_discharge_alignment[cohort_rickets_indicies])
diagnosis_alignment$Rickets_lower2sd[3] <- min(cohort_12month_alignment[cohort_rickets_indicies])


diagnosis_alignment$Others_upper2sd[1] <- max(cohort_preop_alignment[cohort_others_indicies])
diagnosis_alignment$Others_upper2sd[2] <- max(cohort_discharge_alignment[cohort_others_indicies])
diagnosis_alignment$Others_upper2sd[3] <- max(cohort_12month_alignment[cohort_others_indicies])
diagnosis_alignment$Others_lower2sd[1] <- min(cohort_preop_alignment[cohort_others_indicies])
diagnosis_alignment$Others_lower2sd[2] <- min(cohort_discharge_alignment[cohort_others_indicies])
diagnosis_alignment$Others_lower2sd[3] <- min(cohort_12month_alignment[cohort_others_indicies])



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
diagnosis_alignment$Blounts_means[1] <- mean(abs(cohort_preop_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_means[2] <-  mean(abs(cohort_discharge_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_means[3] <-  mean(abs(cohort_12month_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_upper2sd[1] <- max(abs(cohort_preop_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_upper2sd[2] <- max(abs(cohort_discharge_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_upper2sd[3] <- max(abs(cohort_12month_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_lower2sd[1] <- min(abs(cohort_preop_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_lower2sd[2] <- min(abs(cohort_discharge_alignment[cohort_blounts_indicies]))
diagnosis_alignment$Blounts_lower2sd[3] <- min(abs(cohort_12month_alignment[cohort_blounts_indicies]))


diagnosis_alignment$Rickets_means[1] <- mean(abs(cohort_preop_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_means[2] <-  mean(abs(cohort_discharge_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_means[3] <-  mean(abs(cohort_12month_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_upper2sd[1] <- max(abs(cohort_preop_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_upper2sd[2] <- max(abs(cohort_discharge_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_upper2sd[3] <- max(abs(cohort_12month_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_lower2sd[1] <- min(abs(cohort_preop_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_lower2sd[2] <- min(abs(cohort_discharge_alignment[cohort_rickets_indicies]))
diagnosis_alignment$Rickets_lower2sd[3] <- min(abs(cohort_12month_alignment[cohort_rickets_indicies]))


diagnosis_alignment$Others_means[1] <- mean(abs(cohort_preop_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_means[2] <-  mean(abs(cohort_discharge_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_means[3] <-  mean(abs(cohort_12month_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_upper2sd[1] <- max(abs(cohort_preop_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_upper2sd[2] <- max(abs(cohort_discharge_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_upper2sd[3] <- max(abs(cohort_12month_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_lower2sd[1] <- min(abs(cohort_preop_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_lower2sd[2] <- min(abs(cohort_discharge_alignment[cohort_others_indicies]))
diagnosis_alignment$Others_lower2sd[3] <- min(abs(cohort_12month_alignment[cohort_others_indicies]))


# Plotting data from table
# This is a bit easier to digest. 
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
  
  labs(x = "Stage of Care", y = "Absolute Alignment (cm)")+
  scale_x_discrete(limits = rev)



# Here's another plot of blounts vs rickets vs others using absolute value
plot(factor(cohort_assess_preop$DIagnosis), abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)))



# Proportion of blounts legs in cohort in normal range (0)
# ANSWER: 10/31
cohort_12month_alignment[blounts_indicies]



# Proportion of rickets legs in cohort in normal range (0)
# ANSWER: 0/10
cohort_12month_alignment[rickets_indicies]




# Proportion of other (not blounts or rickets) legs in cohort in normal range (0)
# ANSWER: 2/7
cohort_12month_alignment[others_indicies]



# The following SLF numbers are patients in the other category
# (no blounts or rickets) whose legs (both right and left)
# have the exact same alignment values at all stages of care.
# This seems odd.
# 8010, 8057, 8287, 9257
other_legs <- cohort_assess_preop[others_indicies,]





# At this point I would have to get really into the weeds to figure this out.
# So, I will just report some summary statistics and move on for now.
# For each diagnosis category, I will report:
#       0. sample size
#       1. preop means, max and mins for alignment (absolute value)
#       2. discharge means, max and mins for alignment (absolute value)
#       3. 12 month means, max and mins for alignment (absolute value)
#       4. Proportion of IC at preop
#       5. Proportion of IM at preop
#       6. Number of normal alignments at 12 month post op
#       7. Male to female ratio

# We know 0 - 3 and 6 from above work in this section.

# We know 4 - 5 from Step 1.5

# 6. Male to female ratio
# 21 out of 31 female blounts legs
# 3 out of 10 female rickets legs
# 3 out of 7 female others legs
nrow(cohort_assess_12month[blounts_indicies & tolower(cohort_assess_12month$Sex) == "f",])
nrow(cohort_assess_12month[rickets_indicies & tolower(cohort_assess_12month$Sex) == "f",])
nrow(cohort_assess_12month[others_indicies & tolower(cohort_assess_12month$Sex) == "f",])

# Summarzing data 

# FOR BLOUNTS (absolute value)
#       0. sample size = 31
#       1. preop means, max and mins for alignment (absolute value) 15.016129, 26, 3.5
#       2. discharge means, max and mins for alignment (absolute value) 2.112903, 8, 0                             
#       3. 12 month means, max and mins for alignment (absolute value) 2.838710, 11, 0               
#       4. Proportion of IC at preop 30/31
#       5. Proportion of IM at preop 1/31
#       6. Number of normal alignments at 12 month post op 10/31
#       7. Male to female ratio 10:21


# FOR RICKETS (absolute value)
#       0. sample size = 10
#       1. preop means, max and mins for alignment (absolute value) 16.3, 22, 9
#       2. discharge means, max and mins for alignment (absolute value) 3.1, 7, 1                             
#       3. 12 month means, max and mins for alignment (absolute value) 4.7, 11, 2               
#       4. Proportion of IC at preop 2/10
#       5. Proportion of IM at preop 8/31
#       6. Number of normal alignments at 12 month post op 0/10
#       7. Male to female ratio 7:3


# FOR OTHERS (absolute value)
#       0. sample size = 7
#       1. preop means, max and mins for alignment (absolute value) 14.2857143, 25.5, 7
#       2. discharge means, max and mins for alignment (absolute value) 0.2857143, 1.0, 0                             
#       3. 12 month means, max and mins for alignment (absolute value) 2.8571429, 6.0, 0               
#       4. Proportion of IC at preop 5/7
#       5. Proportion of IM at preop 2/7
#       6. Number of normal alignments at 12 month post op 2/7
#       7. Male to female ratio 4:3



#############################################################################################
# Step 3: Checking for correlation with outcomes and weeks in cast
#
#############################################################################################


### Preparing Data ###
cohort_SEX <- cohort_assess_preop$Sex
cohort_BMI <- as.numeric(cohort_assess_preop$BMI..V11)
cohort_AGE <- as.numeric(cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)

# Getting distance from "normal" range for each outcome at each stage of care
# Alignment normal = 0
alignment_preop_dist <- abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm))
alignment_12month_dist <- abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm))

# Mech Axis normal = 0
mechAxis_preop_dist <- abs(as.numeric(cohort_assess_preop$cleaned_mech_axis_degrees))
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



### Preparing Data ###


# Visually plotting weeks in cast VS outcomes 
plot(cohort_assess_preop$Weeks.in.Cast, alignment_12month_dist, xlab = "Weeks in Cast", ylab = "Distance from normal Alignment (cm)")
plot(cohort_assess_preop$Weeks.in.Cast, mechAxis_12month_dist, xlab = "Weeks in Cast", ylab = "Distance from normal Mech Axis (degrees)")
plot(cohort_assess_preop$Weeks.in.Cast, MAD_12month_dist, xlab = "Weeks in Cast", ylab = "Distance from normal Mech Axis Deviation (mm)")
plot(cohort_assess_preop$Weeks.in.Cast, MPTA_12month_dist,  xlab = "Weeks in Cast", ylab = "Distance from normal MPTA (degrees)")



# Visually plotting days in cast vs outcomes
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, alignment_12month_dist, xlab = "Days in Cast", ylab = "Distance from normal Alignment (cm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, mechAxis_12month_dist, xlab = "Days in Cast", ylab = "Distance from normal Mech Axis (degrees)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, MAD_12month_dist , xlab = "Days in Cast", ylab = "Distance from normal Mech Axis Deviation (mm)")
plot(cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery, MPTA_12month_dist,  xlab = "Days in Cast", ylab = "Distance from normal MPTA (degrees)")




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
#          Model 2: Significant, but Preop mech Axis was the only significant variable
model_mechAxis_1 <- lm(mechAxis_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_mechAxis_2 <- lm(mechAxis_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery + factor(cohort_SEX) + cohort_BMI + cohort_AGE + mechAxis_preop_dist) 
summary(model_mechAxis_1)
summary(model_mechAxis_2)



# Linear Regression - MECH AXIS DEVIATION
# Model 1: Days in cast
# Model 2: Days in cast + Age + BMI + Sex + Preop mech Axis deviation
# Results: Model 1: Not significant
#          Model 2: Significant, but Preop mech Axis was the only significant variable
model_MAD_1 <- lm(MAD_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_MAD_2 <- lm(MAD_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery + factor(cohort_SEX) + cohort_BMI + cohort_AGE + MAD_preop_dist) 
summary(model_MAD_1)
summary(model_MAD_2)



model_mechAxisDeviation <- lm(MAD_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_MPTA <- lm(MPTA_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)




# Summaries of models (none are statistically significant, nor demonstrate strong linear correlation)
# Normally we would have to check for homoscedasticity or errors and the normal distribution of errors,
# but, since none of the tests were statistically significant, it won't make a difference to our analysis.
summary(model_alignment)
summary(model_mechAxis)
summary(model_mechAxisDeviation)
summary(model_MPTA)



# Fitting a linear regression model for each outcome WITH BMI, SEX and AGE
model_alignment <- lm(alignment_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_mechAxis <- lm(mechAxis_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_mechAxisDeviation <- lm(MAD_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)
model_MPTA <- lm(MPTA_12month_dist ~ cohort_assess_preop$Number.of.days.cast.was.on.from.first.day.of.surgery)




# Summaries of models (none are statistically significant, nor demonstrate strong linear correlation)
# Normally we would have to check for homoscedasticity or errors and the normal distribution of errors,
# but, since none of the tests were statistically significant, it won't make a difference to our analysis.
summary(model_alignment)
summary(model_mechAxis)
summary(model_mechAxisDeviation)
summary(model_MPTA)

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
# Step 4: Understand whether the surgical technique impacted long term outcomes 
# - Tibial plateau elevation for blounts vs ‘other surgical technique’ for blounts 
# - We will compare techniques with the 'Alignment' outcome
#############################################################################################
library(ggplot2)

# MAD normal = 3-17 varus (positive)
# MPTA normal = 85-90 degrees 
# Alignment normal = 0
alignment_preop_dist <- abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm))
alignment_discharge_dist <- abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm))
alignment_12month_dist <- abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm))

# Mech Axis normal = 0
mechAxis_preop_dist <- abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm))
alignment_discharge_dist <- abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm))
alignment_12month_dist <- abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm))



blounts_indicies <-cohort_assess_preop$DIagnosis == "blounts"



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






# Filling data table (could write a loop but I think this will be more clear)

data_alignment$Tib_means[1] <- mean(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_means[2] <- mean(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_means[3] <- mean(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_upper2sd[1] <- data_alignment$Tib_means[1] + 2*sd(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_upper2sd[2] <- data_alignment$Tib_means[2] + 2*sd(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_upper2sd[3] <- data_alignment$Tib_means[3] + 2*sd(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_lower2sd[1] <- data_alignment$Tib_means[1] - 2*sd(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_lower2sd[2] <- data_alignment$Tib_means[2] - 2*sd(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))
data_alignment$Tib_lower2sd[3] <- data_alignment$Tib_means[3] - 2*sd(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies]))

data_alignment$Other_means[1] <- mean(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_means[2] <- mean(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_means[3] <- mean(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_upper2sd[1] <- data_alignment$Other_means[1] + 2*sd(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_upper2sd[2] <- data_alignment$Other_means[2] + 2*sd(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_upper2sd[3] <- data_alignment$Other_means[3] + 2*sd(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_lower2sd[1] <- data_alignment$Other_means[1] - 2*sd(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_lower2sd[2] <- data_alignment$Other_means[2] - 2*sd(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))
data_alignment$Other_lower2sd[3] <- data_alignment$Other_means[3] - 2*sd(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies]))



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
data_alignment$Tib_means[1] <- mean(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_means[2] <- mean(abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_means[3] <- mean(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_upper2sd[1] <- max(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_upper2sd[2] <- max(abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_upper2sd[3] <- max(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_lower2sd[1] <- min(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_lower2sd[2] <- min(abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))
data_alignment$Tib_lower2sd[3] <- min(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])))

data_alignment$Other_means[1] <- mean(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_means[2] <- mean(abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_means[3] <- mean(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_upper2sd[1] <- max(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_upper2sd[2] <- max(abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_discharge$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_upper2sd[3] <- max(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_12month$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_lower2sd[1] <- min(abs(as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_lower2sd[2] <- min(abs(as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))
data_alignment$Other_lower2sd[3] <- min(abs(as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])))



length(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])
length(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])





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
#       0. sample size = 21
length(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])
#       1. preop means, max and mins for alignment (absolute value) 16.2380952, 26, 7
#       2. discharge means, max and mins for alignment (absolute value) 2.309524, 8, 0                             
#       3. 12 month means, max and mins for alignment (absolute value) 2.952381, 11, 0               
#       4. Proportion of IC at preop 21/21
length(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies & cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"])
#       5. Proportion of IM at preop 0/21
#       6. Number of normal alignments at 12 month post op 8/21
length(cohort_assess_12month[cohort_assess_12month$cleaned_Alignment..V2..in.cm == 0 & cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies])
#       7. Male to female ratio 4:21
nrow(cohort_assess_preop[cohort_assess_preop$Tibial.Plateau.Elevation. == "Y" & blounts_indicies & tolower(cohort_assess_preop$Sex) == "m",])



# FOR NOT TIB PLAT ELEVATION (absolute value)
#       0. sample size = 10
length(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])
#       1. preop means, max and mins for alignment (absolute value) 12.45, 24, 3.5
#       2. discharge means, max and mins for alignment (absolute value) 1.70, 4, 0                             
#       3. 12 month means, max and mins for alignment (absolute value) 2.60, 4.5, 0               
#       4. Proportion of IC at preop 9/10
length(cohort_assess_preop$cleaned_Alignment..V2..in.cm[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies & cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"])
#       5. Proportion of IM at preop 1/10
#       6. Number of normal alignments at 12 month post op 2/10
length(cohort_assess_12month[cohort_assess_12month$cleaned_Alignment..V2..in.cm == 0 & cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies])
#       7. Male to female ratio 5:5
nrow(cohort_assess_preop[cohort_assess_preop$Tibial.Plateau.Elevation. == "N" & blounts_indicies & grepl("m",tolower(cohort_assess_preop$Sex)),])


#############################################################################################
# Step 5: How did BMI, age, and sex affect outcomes?
#
#############################################################################################



### Preparing Data ###

#cohort_preop_MPTA <- as.numeric(cohort_assess_preop$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
#cohort_discharge_MPTA <-as.numeric(cohort_assess_discharge$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
cohort_12month_MPTA <- as.numeric(cohort_assess_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
#cohort_preop_MAD <- as.numeric(cohort_assess_preop$Mech.Axis.Deviation..mm.)
#cohort_discharge_MAD <-as.numeric(cohort_assess_discharge$Mech.Axis.Deviation..mm.)
cohort_12month_MAD <- as.numeric(cohort_assess_12month$Mech.Axis.Deviation..mm.)
#cohort_preop_mechAxis <- as.numeric(cohort_assess_preop$cleaned_mech_axis_degrees)
#cohort_discharge_mechAxis <-as.numeric(cohort_assess_discharge$cleaned_mech_axis_degrees)
cohort_12month_mechAxis <- as.numeric(cohort_assess_12month$cleaned_mech_axis_degrees)
#cohort_preop_alignment <- as.numeric(cohort_assess_preop$cleaned_Alignment..V2..in.cm)
#cohort_discharge_alignment <-as.numeric(cohort_assess_discharge$cleaned_Alignment..V2..in.cm)
cohort_12month_alignment <- as.numeric(cohort_assess_12month$cleaned_Alignment..V2..in.cm)
cohort_BMI <- as.numeric(cohort_assess_preop$BMI..V11)
cohort_SEX <- cohort_assess_preop$Sex
cohort_AGE <- cohort_assess_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns
cohort_alignment_IC_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IC"
cohort_alignment_IM_indicies <- cohort_assess_preop$cleaned_Alignment..V2..measurement.type == "IM"
cohort_blounts_indicies <- cohort_assess_preop$DIagnosis == "blounts"
cohort_rickets_indicies <- cohort_assess_preop$DIagnosis == "rickets"
cohort_others_indicies <- cohort_assess_preop$DIagnosis == "others"

### Preparing data ###



### Actual Analysis - Linear Models ###

# Alignment
#     Model is not significant (p-value = 0.1563)
#     BMI is significant (positive)
#     
model_alignment <- lm(cohort_12month_alignment ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_alignment$residuals)
plot(fitted(model_alignment), resid(model_alignment))
abline(a = 0, b = 0)
summary(model_alignment)



# Mech Axis
#     Model is significant (p-value = 0.02374)
#     BMI is significant (positive)
model_mechAxis <- lm(cohort_12month_mechAxis ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_mechAxis$residuals)
plot(fitted(model_mechAxis), resid(model_mechAxis))
abline(a = 0, b = 0)
summary(model_mechAxis)



# Mech Axis Deviation (MAD)
#     Model is almost significant (p-value = 0.07418)
#     BMI is significant (positive)
model_MAD <- lm(cohort_12month_MAD ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MAD$residuals)
plot(fitted(model_MAD), resid(model_MAD))
abline(a = 0, b = 0)
summary(model_MAD)



# MPTA
#     Model is almost significant (p-value = 0.007792)
#     BMI is significant (negative)
model_MPTA <- lm(cohort_12month_MPTA ~ cohort_BMI + factor(cohort_SEX) + cohort_AGE)
qqnorm(model_MPTA$residuals)
plot(fitted(model_MPTA), resid(model_MPTA))
abline(a = 0, b = 0)
summary(model_MPTA)

### Actual Analysis - Linear Models ###



### Thoughts ###
#
# In all linear models, BMI was associated with the outcomes


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


