############################################################################################
# MPTA OUTCOME ANALYSIS
#
# NOTE: RUN assess_Analysis.R first
#
# IN THIS DOCUMENT WE:
# 1. do analysis for MPTA for all stages of care
# 2. do analysis for MPTA for just preop and discharge
# 
############################################################################################


colnames(ortho_assess)
ortho_assess[39] # MPTA


############################################################################################
# GETTING COHORT DATA FOR ALL STAGES OF CARE #
############################################################################################
SLF_removal_list <- list()

# First checking ortho_assess_preop
for(i in 1:nrow(ortho_assess_preop)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 39 (MPTA)
  for(j in c(39)){
    if(!(grepl("\\d", ortho_assess_preop[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}

# Then checking ortho_assess_discharge
for(i in 1:nrow(ortho_assess_discharge)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in columns 39
  for(j in c(39)){
    if(!(grepl("\\d", ortho_assess_discharge[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}


# Finally checking ortho_assess_12month
for(i in 1:nrow(ortho_assess_12month)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in columns 39, 39 and 41
  for(j in c(39)){
    if(!(grepl("\\d", ortho_assess_12month[i,j]))){
      SLF_removal_list <- c(SLF_removal_list, ortho_assess_preop$SLF..[i])
    }
  }
}


# 11 patients missing MPTA data in either preop OR discharge OR 12month
SLF_removal_list <- unique(SLF_removal_list)
SLF_removal_list


# Using SLF_removal_list to create cohort
cohort_MPTA_allstages_preop <- ortho_assess_preop[!(ortho_assess_preop$SLF.. %in% SLF_removal_list), ]
# Converting MPTA to numeric from string (that's what the data is encoded as for MPTA)
cohort_MPTA_allstages_preop$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <- as.numeric(cohort_MPTA_allstages_preop$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
# Assessment Ortho Data Preop w/ rickets
cohort_MPTA_allstages_preop_rickets <- cohort_MPTA_allstages_preop[grepl("rickets", cohort_MPTA_allstages_preop$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MPTA_allstages_preop_blounts <- cohort_MPTA_allstages_preop[grepl("blounts", cohort_MPTA_allstages_preop$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MPTA_allstages_preop_other <- cohort_MPTA_allstages_preop[!(cohort_MPTA_allstages_preop$SLF.. %in% append(cohort_MPTA_allstages_preop_rickets$SLF.., cohort_MPTA_allstages_preop_blounts$SLF..)),]


# Doing the same thing for the other stages of care
cohort_MPTA_allstages_discharge <- ortho_assess_discharge[!(ortho_assess_discharge$SLF.. %in% SLF_removal_list), ]
# Converting MPTA to numeric from string (that's what the data is encoded as for MPTA)
cohort_MPTA_allstages_discharge$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <- as.numeric(cohort_MPTA_allstages_discharge$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
# Assessment Ortho Data Preop w/ rickets
cohort_MPTA_allstages_discharge_rickets <- cohort_MPTA_allstages_discharge[grepl("rickets", cohort_MPTA_allstages_discharge$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MPTA_allstages_discharge_blounts <- cohort_MPTA_allstages_discharge[grepl("blounts", cohort_MPTA_allstages_discharge$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MPTA_allstages_discharge_other <- cohort_MPTA_allstages_discharge[!(cohort_MPTA_allstages_discharge$SLF.. %in% append(cohort_MPTA_allstages_discharge_rickets$SLF.., cohort_MPTA_allstages_discharge_blounts$SLF..)),]


# Doing the same thing for the other stages of care
cohort_MPTA_allstages_12month <- ortho_assess_12month[!(ortho_assess_12month$SLF.. %in% SLF_removal_list), ]
# Converting MPTA to numeric from string (that's what the data is encoded as for MPTA)
cohort_MPTA_allstages_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <- as.numeric(cohort_MPTA_allstages_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
# Assessment Ortho Data Preop w/ rickets
cohort_MPTA_allstages_12month_rickets <- cohort_MPTA_allstages_12month[grepl("rickets", cohort_MPTA_allstages_12month$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
cohort_MPTA_allstages_12month_blounts <- cohort_MPTA_allstages_12month[grepl("blounts", cohort_MPTA_allstages_12month$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
cohort_MPTA_allstages_12month_other <- cohort_MPTA_allstages_12month[!(cohort_MPTA_allstages_12month$SLF.. %in% append(cohort_MPTA_allstages_12month_rickets$SLF.., cohort_MPTA_allstages_12month_blounts$SLF..)),]



############################################################################################
# ANALYSIS OF MPTA COHORT DATA FOR ALL STAGES OF CARE #
#
# NOTE: Need to double check that my script works as intended. I don't want to do it now.
#       I'm going forward. There shouldn't be much code change if something is wrong.
#
############################################################################################


# BLOUNTS BOXPLOT ANALYSIS
par(mfrow = c(2,2))


med_preop <- median(as.numeric(cohort_MPTA_allstages_preop_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.))
med_discharge <- median(cohort_MPTA_allstages_discharge_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
med_12month <- median(cohort_MPTA_allstages_12month_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)

boxplot(cohort_MPTA_allstages_preop_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Blounts PREOP - Median = ", med_preop), ylab = "mm")
boxplot(cohort_MPTA_allstages_discharge_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Blounts DISCHARGE - Median = ", med_discharge), ylab = "mm")
boxplot(cohort_MPTA_allstages_12month_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Blounts 12 MONTH - Median = ", med_12month), ylab = "mm")


# BLOUNTS BARPLOT ANALYSIS
par(mfrow = c(2,2))

barplot(sort(cohort_MPTA_allstages_preop_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Blounts PREOP", ylim = c(0,150))
barplot(sort(cohort_MPTA_allstages_discharge_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Blounts DISCHARGE", ylim = c(0,150))
barplot(sort(cohort_MPTA_allstages_12month_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Blounts 12 MONTH", ylim = c(0,150))


# RICKETS BOXPLOT ANALYSIS
par(mfrow = c(2,2))




med_preop <- median(cohort_MPTA_allstages_preop_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
med_discharge <- median(cohort_MPTA_allstages_discharge_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
med_12month <- median(cohort_MPTA_allstages_12month_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)

boxplot(cohort_MPTA_allstages_preop_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Rickets PREOP - Median = ", med_preop), ylab = "mm")
boxplot(cohort_MPTA_allstages_discharge_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Rickets DISCHARGE - Median = ", med_discharge), ylab = "mm")
boxplot(cohort_MPTA_allstages_12month_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Rickets 12 MONTH - Median = ", med_12month), ylab = "mm")


# RICKETS BARPLOT ANALYSIS
par(mfrow = c(2,2))

barplot(sort(cohort_MPTA_allstages_preop_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Rickets PREOP", ylim = c(0,150))
barplot(sort(cohort_MPTA_allstages_discharge_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Rickets DISCHARGE", ylim = c(0,150))
barplot(sort(cohort_MPTA_allstages_12month_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Rickets 12 MONTH", ylim = c(0,150))


# OTHER BOXPLOT ANALYSIS
par(mfrow = c(2,2))

med_preop <- median(cohort_MPTA_allstages_preop_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
med_discharge <- median(cohort_MPTA_allstages_discharge_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)
med_12month <- median(cohort_MPTA_allstages_12month_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.)

boxplot(cohort_MPTA_allstages_preop_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Other PREOP - Median = ", med_preop), ylab = "mm")
boxplot(cohort_MPTA_allstages_discharge_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Other DISCHARGE - Median = ", med_discharge), ylab = "mm")
boxplot(cohort_MPTA_allstages_12month_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts., xlab = paste("MPTA Other 12 MONTH - Median = ", med_12month), ylab = "mm")


# OTHER BARPLOT ANALYSIS
par(mfrow = c(2,2))

barplot(sort(cohort_MPTA_allstages_preop_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Other PREOP", ylim = c(0,150))
barplot(sort(cohort_MPTA_allstages_discharge_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Other DISCHARGE", ylim = c(0,150))
barplot(sort(cohort_MPTA_allstages_12month_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts.), xlab = "MPTA Other 12 MONTH", ylim = c(0,150))


# NUMBER OF SUCCESSES VS FAILURES
# NORMAL RANGE: 0-17

# TOTAL SUCCESSES AND TOTAL FAILURES
# Total: 74
# Successes: 38
# Failures: 36
# Success Proportion: 0.51%
nrow(cohort_MPTA_allstages_12month)
nrow(cohort_MPTA_allstages_12month[cohort_MPTA_allstages_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <= 90 && cohort_MPTA_allstages_12month$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. >= 85,]) 


# BLOUNTS SUCCESSES AND BLOUNTS FAILURES
# Total: 42
# Successes: 19
# Failures: 23
# Success Proportion: 0.45%
nrow(cohort_MPTA_allstages_12month_blounts)
nrow(cohort_MPTA_allstages_12month_blounts[cohort_MPTA_allstages_12month_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <= 90 && cohort_MPTA_allstages_12month_blounts$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. >= 85,])


# RICKETS SUCCESSES AND RICKETS FAILURES
# Total: 18
# Successes: 11
# Failures: 7
# Success Proportion: 0.61%
nrow(cohort_MPTA_allstages_12month_rickets)
nrow(cohort_MPTA_allstages_12month_rickets[cohort_MPTA_allstages_12month_rickets$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <= 17,])


# OTHER SUCCESSES AND RICKETS FAILURES
# Total: 14
# Successes: 8
# Failures: 6
# Success Proportion: 0.57%
nrow(cohort_MPTA_allstages_12month_other)
nrow(cohort_MPTA_allstages_12month_other[cohort_MPTA_allstages_12month_other$MPTA..measured.from.the.lateral.tibial.plateau.for.blouts. <= 17,])


# DO WE HAVE SUFFICIENT DATA TO CONCLUDE THAT RICKETS AND BLOUNTS HAVE DIFFERENT SUCCESS RATES?
# IF THE SUCCESS RATE FOR RICKETS IS THE SAME AS BLOUNTS, THEN THE PROBABILITY OF GETTING OUR
# NUMBER OF SUCCESSES FOR RICKETS IS AROUND 7%. CAN'T ARGUE STATISTICAL SIGNIFICANCE WITH THAT.
dbinom(x = 11, size = 18, p = 0.45)


