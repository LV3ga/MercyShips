############################################################################################
# MECH AXIS DEVIATION OUTCOME ANALYSIS
#
# NOTE: RUN assess_Analysis.R first
#
# IN THIS DOCUMENT WE:
# 1. do analysis for Mech Axis Deviation for all stages of care
# 2. do analysis for Mech Axis Deviation for just preop and discharge
# 
############################################################################################


colnames(ortho_assess)
ortho_assess[37] # Mech Axis Deviation

############################################################################################
# GETTING COHORT DATA FOR ALL STAGES OF CARE #
############################################################################################
SLF_removal_list <- list()

# First checking ortho_assess_preop
for(i in 1:nrow(ortho_assess_preop)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 (Mech Axis Deviation)
  for(j in c(37)){
    if(!(grepl("\\d", ortho_assess_preop[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}

# Then checking ortho_assess_discharge
for(i in 1:nrow(ortho_assess_discharge)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in columns 37, 39 and 41
  for(j in c(37)){
    if(!(grepl("\\d", ortho_assess_discharge[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}


# Finally checking ortho_assess_12month
for(i in 1:nrow(ortho_assess_12month)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in columns 37, 39 and 41
  for(j in c(37)){
    if(!(grepl("\\d", ortho_assess_12month[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}


# 10 patients missing Mech Axis Deviation data in either preop OR discharge OR 12month
SLF_removal_list <- unique(SLF_removal_list)
SLF_removal_list


# Using SLF_removal_list to create cohort
cohort_MAD_allstages_preop <- ortho_assess_preop[!(ortho_assess_preop$SLF.. %in% SLF_removal_list), ]
# Assessment Ortho Data Preop w/ rickets
cohort_MAD_allstages_preop_rickets <- cohort_MAD_allstages_preop[grepl("rickets", cohort_MAD_allstages_preop$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MAD_allstages_preop_blounts <- cohort_MAD_allstages_preop[grepl("blounts", cohort_MAD_allstages_preop$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MAD_allstages_preop_other <- cohort_MAD_allstages_preop[!(cohort_MAD_allstages_preop$SLF.. %in% append(cohort_MAD_allstages_preop_rickets$SLF.., cohort_MAD_allstages_preop_blounts$SLF..)),]


# Doing the same thing for the other stages of care
cohort_MAD_allstages_discharge <- ortho_assess_discharge[!(ortho_assess_discharge$SLF.. %in% SLF_removal_list), ]
# Assessment Ortho Data Preop w/ rickets
cohort_MAD_allstages_discharge_rickets <- cohort_MAD_allstages_discharge[grepl("rickets", cohort_MAD_allstages_discharge$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MAD_allstages_discharge_blounts <- cohort_MAD_allstages_discharge[grepl("blounts", cohort_MAD_allstages_discharge$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MAD_allstages_discharge_other <- cohort_MAD_allstages_discharge[!(cohort_MAD_allstages_discharge$SLF.. %in% append(cohort_MAD_allstages_discharge_rickets$SLF.., cohort_MAD_allstages_discharge_blounts$SLF..)),]


# Doing the same thing for the other stages of care
cohort_MAD_allstages_12month <- ortho_assess_12month[!(ortho_assess_12month$SLF.. %in% SLF_removal_list), ]
# Assessment Ortho Data Preop w/ rickets
cohort_MAD_allstages_12month_rickets <- cohort_MAD_allstages_12month[grepl("rickets", cohort_MAD_allstages_12month$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MAD_allstages_12month_blounts <- cohort_MAD_allstages_12month[grepl("blounts", cohort_MAD_allstages_12month$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MAD_allstages_12month_other <- cohort_MAD_allstages_12month[!(cohort_MAD_allstages_12month$SLF.. %in% append(cohort_MAD_allstages_12month_rickets$SLF.., cohort_MAD_allstages_12month_blounts$SLF..)),]



############################################################################################
# GETTING COHORT DATA FOR JUST PREOP AND DISCHARGE #
############################################################################################

SLF_removal_list <- list()

colnames(ortho_assess)

# Alignment
# MA
# MAD
# MPTA

# First checking ortho_assess_preop
for(i in 1:nrow(ortho_assess_preop)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 (Mech Axis Deviation)
  for(j in c(23, 36, 37, 39)){
    if(!(grepl("\\d", ortho_assess_preop[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}


# Then checking ortho_assess_discharge
for(i in 1:nrow(ortho_assess_discharge)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in columns 37, 39 and 41
  for(j in c(37, 39, 41)){
    if(!(grepl("\\d", ortho_assess_discharge[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}



# Then checking ortho_assess_discharge
for(i in 1:nrow(ortho_assess_12month)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in columns 37, 39 and 41
  for(j in c(37, 39, 41)){
    if(!(grepl("\\d", ortho_assess_12month[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}



# 3 patients missing Mech Axis Deviation data in either preop OR discharge OR 12month
SLF_removal_list <- unique(SLF_removal_list)
SLF_removal_list

(unique(ortho_assess$SLF..))

# Using SLF_removal_list to create cohort
cohort_MAD_PreDis_preop <- ortho_assess_preop[!(ortho_assess_preop$SLF.. %in% SLF_removal_list), ]
# Assessment Ortho Data Preop w/ rickets
cohort_MAD_PreDis_preop_rickets <- cohort_MAD_PreDis_preop[grepl("rickets", cohort_MAD_PreDis_preop$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MAD_PreDis_preop_blounts <- cohort_MAD_PreDis_preop[grepl("blounts", cohort_MAD_PreDis_preop$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MAD_PreDis_preop_other <- cohort_MAD_PreDis_preop[!(cohort_MAD_PreDis_preop$SLF.. %in% append(cohort_MAD_PreDis_preop_rickets$SLF.., cohort_MAD_PreDis_preop_blounts$SLF..)),]


# Doing the same thing for the other stages of care
cohort_MAD_PreDis_discharge <- ortho_assess_discharge[!(ortho_assess_discharge$SLF.. %in% SLF_removal_list), ]
# Assessment Ortho Data Preop w/ rickets
cohort_MAD_PreDis_discharge_rickets <- cohort_MAD_PreDis_discharge[grepl("rickets", cohort_MAD_PreDis_discharge$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MAD_PreDis_discharge_blounts <- cohort_MAD_PreDis_discharge[grepl("blounts", cohort_MAD_PreDis_discharge$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MAD_PreDis_discharge_other <- cohort_MAD_PreDis_discharge[!(cohort_MAD_PreDis_discharge$SLF.. %in% append(cohort_MAD_PreDis_discharge_rickets$SLF.., cohort_MAD_PreDis_discharge_blounts$SLF..)),]



############################################################################################
# ANALYSIS OF MECH AXIS DEVIATION COHORT DATA FOR ALL STAGES OF CARE #
#
# NOTE: Need to double check that my script works as intended. I don't want to do it now.
#       I'm going forward. There shouldn't be much code change if something is wrong.
#
############################################################################################


# BLOUNTS BOXPLOT ANALYSIS
par(mfrow = c(2,2))

med_preop <- median(cohort_MAD_allstages_preop_blounts$Mech.Axis.Deviation..mm.)
med_discharge <- median(cohort_MAD_allstages_discharge_blounts$Mech.Axis.Deviation..mm.)
med_12month <- median(cohort_MAD_allstages_12month_blounts$Mech.Axis.Deviation..mm.)

boxplot(cohort_MAD_allstages_preop_blounts$Mech.Axis.Deviation..mm., xlab = paste("MAD Blounts PREOP - Median = ", med_preop), ylab = "mm")
boxplot(cohort_MAD_allstages_discharge_blounts$Mech.Axis.Deviation..mm., xlab = paste("MAD Blounts DISCHARGE - Median = ", med_discharge), ylab = "mm")
boxplot(cohort_MAD_allstages_12month_blounts$Mech.Axis.Deviation..mm., xlab = paste("MAD Blounts 12 MONTH - Median = ", med_12month), ylab = "mm")


# BLOUNTS BARPLOT ANALYSIS
par(mfrow = c(2,2))

barplot(sort(cohort_MAD_allstages_preop_blounts$Mech.Axis.Deviation..mm.), xlab = "MAD Blounts PREOP", ylim = c(0,150))
barplot(sort(cohort_MAD_allstages_discharge_blounts$Mech.Axis.Deviation..mm.), xlab = "MAD Blounts DISCHARGE", ylim = c(0,150))
barplot(sort(cohort_MAD_allstages_12month_blounts$Mech.Axis.Deviation..mm.), xlab = "MAD Blounts 12 MONTH", ylim = c(0,150))


# RICKETS BOXPLOT ANALYSIS
par(mfrow = c(2,2))

med_preop <- median(cohort_MAD_allstages_preop_rickets$Mech.Axis.Deviation..mm.)
med_discharge <- median(cohort_MAD_allstages_discharge_rickets$Mech.Axis.Deviation..mm.)
med_12month <- median(cohort_MAD_allstages_12month_rickets$Mech.Axis.Deviation..mm.)

boxplot(cohort_MAD_allstages_preop_rickets$Mech.Axis.Deviation..mm., xlab = paste("MAD Rickets PREOP - Median = ", med_preop), ylab = "mm")
boxplot(cohort_MAD_allstages_discharge_rickets$Mech.Axis.Deviation..mm., xlab = paste("MAD Rickets DISCHARGE - Median = ", med_discharge), ylab = "mm")
boxplot(cohort_MAD_allstages_12month_rickets$Mech.Axis.Deviation..mm., xlab = paste("MAD Rickets 12 MONTH - Median = ", med_12month), ylab = "mm")


# RICKETS BARPLOT ANALYSIS
par(mfrow = c(2,2))

barplot(sort(cohort_MAD_allstages_preop_rickets$Mech.Axis.Deviation..mm.), xlab = "MAD Rickets PREOP", ylim = c(0,150))
barplot(sort(cohort_MAD_allstages_discharge_rickets$Mech.Axis.Deviation..mm.), xlab = "MAD Rickets DISCHARGE", ylim = c(0,150))
barplot(sort(cohort_MAD_allstages_12month_rickets$Mech.Axis.Deviation..mm.), xlab = "MAD Rickets 12 MONTH", ylim = c(0,150))


# OTHER BOXPLOT ANALYSIS
par(mfrow = c(2,2))

med_preop <- median(cohort_MAD_allstages_preop_other$Mech.Axis.Deviation..mm.)
med_discharge <- median(cohort_MAD_allstages_discharge_other$Mech.Axis.Deviation..mm.)
med_12month <- median(cohort_MAD_allstages_12month_other$Mech.Axis.Deviation..mm.)

boxplot(cohort_MAD_allstages_preop_other$Mech.Axis.Deviation..mm., xlab = paste("MAD Other PREOP - Median = ", med_preop), ylab = "mm")
boxplot(cohort_MAD_allstages_discharge_other$Mech.Axis.Deviation..mm., xlab = paste("MAD Other DISCHARGE - Median = ", med_discharge), ylab = "mm")
boxplot(cohort_MAD_allstages_12month_other$Mech.Axis.Deviation..mm., xlab = paste("MAD Other 12 MONTH - Median = ", med_12month), ylab = "mm")


# OTHER BARPLOT ANALYSIS
par(mfrow = c(2,2))

barplot(sort(cohort_MAD_allstages_preop_other$Mech.Axis.Deviation..mm.), xlab = "MAD Other PREOP", ylim = c(0,150))
barplot(sort(cohort_MAD_allstages_discharge_other$Mech.Axis.Deviation..mm.), xlab = "MAD Other DISCHARGE", ylim = c(0,150))
barplot(sort(cohort_MAD_allstages_12month_other$Mech.Axis.Deviation..mm.), xlab = "MAD Other 12 MONTH", ylim = c(0,150))


# NUMBER OF SUCCESSES VS FAILURES
# NORMAL RANGE: 0-17

# TOTAL SUCCESSES AND TOTAL FAILURES
# Total: 74
# Successes: 38
# Failures: 36
# Success Proportion: 0.51%
nrow(cohort_MAD_allstages_12month)
nrow(cohort_MAD_allstages_12month[cohort_MAD_allstages_12month$Mech.Axis.Deviation..mm. <= 17,]) 


# BLOUNTS SUCCESSES AND BLOUNTS FAILURES
# Total: 42
# Successes: 19
# Failures: 23
# Success Proportion: 0.45%
nrow(cohort_MAD_allstages_12month_blounts)
nrow(cohort_MAD_allstages_12month_blounts[cohort_MAD_allstages_12month_blounts$Mech.Axis.Deviation..mm. <= 17,])


# RICKETS SUCCESSES AND RICKETS FAILURES
# Total: 18
# Successes: 11
# Failures: 7
# Success Proportion: 0.61%
nrow(cohort_MAD_allstages_12month_rickets)
nrow(cohort_MAD_allstages_12month_rickets[cohort_MAD_allstages_12month_rickets$Mech.Axis.Deviation..mm. <= 17,])


# OTHER SUCCESSES AND RICKETS FAILURES
# Total: 14
# Successes: 8
# Failures: 6
# Success Proportion: 0.57%
nrow(cohort_MAD_allstages_12month_other)
nrow(cohort_MAD_allstages_12month_other[cohort_MAD_allstages_12month_other$Mech.Axis.Deviation..mm. <= 17,])


# DO WE HAVE SUFFICIENT DATA TO CONCLUDE THAT RICKETS AND BLOUNTS HAVE DIFFERENT SUCCESS RATES?
# IF THE SUCCESS RATE FOR RICKETS IS THE SAME AS BLOUNTS, THEN THE PROBABILITY OF GETTING OUR
# NUMBER OF SUCCESSES FOR RICKETS IS AROUND 7%. CAN'T ARGUE STATISTICAL SIGNIFICANCE WITH THAT.
dbinom(x = 11, size = 18, p = 0.45)
