############################################################################################
# GETTING COHORTS
# 
# Notes from this section:
# 1. Compile legs who have all 3 radiology outcomes across
#    all five stages of measurement, which are Mech Axis, MAD and
#    MPTA
#
# 2. Compile legs who have Alignment (clinical value) present across
#    all 3 stages of measurement (pre op, discharge, 12 mo)
#    Reminder if no value, the cell will say 'BLANK' or 'blank' or 'Blank'
#
#
#
############################################################################################

colnames(ortho_assess)


################################################################################################################
# Testing new removal method
# I was removing legs based on their SLF number which is a problem because some patients had both legs operated on,
# so one leg could have complete data and get removed if their other leg was missing data.

# First let's test to see if the 5 data tables from the 5 stages of care have the same ordering of patients
# This will allow us to use indicies to remove legs


# Checking if SLF ordering is the same
ortho_assess_preop$SLF.. == ortho_assess_xafterwedge$SLF.. 
ortho_assess_xafterwedge$SLF.. == ortho_assess_xoutcast$SLF..
ortho_assess_xoutcast$SLF.. == ortho_assess_discharge$SLF..
ortho_assess_discharge$SLF.. == ortho_assess_12month$SLF..


# Checking if right/left leg ordering is the same
ortho_assess_preop$Right.or.Left.Leg. == ortho_assess_xafterwedge$Right.or.Left.Leg.
ortho_assess_preop$Right.or.Left.Leg. == ortho_assess_xoutcast$Right.or.Left.Leg.
ortho_assess_preop$Right.or.Left.Leg. == ortho_assess_discharge$Right.or.Left.Leg.
ortho_assess_preop$Right.or.Left.Leg. == ortho_assess_12month$Right.or.Left.Leg.


# Everything checks out, let's do the removal


################################################################################################################
# 1. Compile legs who have all 3 radiology outcomes across
#    all five stages of measurement, which are Mech Axis, MAD and
#    MPTA. Additionally, find # of Blounts, # of Rickets, # of Others, 
#    # of Double Diagnosis, Average Age, Female:Male ratio, and
#    # of Tibial Plateau Elevations.

Index_removal_list <- rep(TRUE, nrow(ortho_assess_preop))


# First checking ortho_assess_preop
for(i in 1:nrow(ortho_assess_preop)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 and 39 (MAD and MPTA)
  for(j in c(37, 39)){
    if(!(grepl("\\d", ortho_assess_preop[i,j]))){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding SLF IDs for removal if they are missing data in column 36 (Mech Axis)
  for(j in c(36)){
    if(!(grepl("valgus", ortho_assess_preop[i,j]) || grepl("varus", ortho_assess_preop[i,j]) || ortho_assess_12month[i,j] == "0" )){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding indices for removal if Alignment column lacks IC/IM or 0
  for(j in c(23)){
    if(!(grepl("IC", ortho_assess_preop[i,j])) & !(grepl("IM", ortho_assess_preop[i,j])) & !(ortho_assess_preop[i,j] == "0") ){
      Index_removal_list[i] <- FALSE
    }
  }
  
}


# Then checking ortho_assess_xafterwedge
for(i in 1:nrow(ortho_assess_xafterwedge)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 and 39 (MAD and MPTA)
  for(j in c(37, 39)){
    if(!(grepl("\\d", ortho_assess_xafterwedge[i,j]))){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding SLF IDs for removal if they are missing data in column 36 (Mech Axis)
  for(j in c(36)){
    if(!(grepl("valgus", ortho_assess_xafterwedge[i,j]) || grepl("varus", ortho_assess_xafterwedge[i,j]) || ortho_assess_12month[i,j] == "0" )){
      Index_removal_list[i] <- FALSE
    }
  }
}


# Then checking ortho_assess_xoutcast
for(i in 1:nrow(ortho_assess_xoutcast)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 and 39 (MAD and MPTA)
  for(j in c(37, 39)){
    if(!(grepl("\\d", ortho_assess_xoutcast[i,j]))){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding SLF IDs for removal if they are missing data in column 36 (Mech Axis)
  for(j in c(36)){
    if(!(grepl("valgus", ortho_assess_xoutcast[i,j]) || grepl("varus", ortho_assess_xoutcast[i,j]) || ortho_assess_12month[i,j] == "0" )){
      Index_removal_list[i] <- FALSE
    }
  }
}


# Then checking ortho_assess_discharge
for(i in 1:nrow(ortho_assess_discharge)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 and 39 (MAD and MPTA)
  for(j in c(37, 39)){
    if(!(grepl("\\d", ortho_assess_discharge[i,j]))){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding SLF IDs for removal if they are missing data in column 36 (Mech Axis)
  for(j in c(36)){
    if(!(grepl("valgus", ortho_assess_discharge[i,j]) || grepl("varus", ortho_assess_discharge[i,j]) || ortho_assess_12month[i,j] == "0" )){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding indices for removal if Alignment column lacks IC/IM or 0
  for(j in c(23)){
    if(!(grepl("IC", ortho_assess_discharge[i,j])) & !(grepl("IM", ortho_assess_discharge[i,j])) & !(ortho_assess_discharge[i,j] == "0") ){
      Index_removal_list[i] <- FALSE
    }
  }
}


# Then checking ortho_assess_12month
for(i in 1:nrow(ortho_assess_12month)){
  # counter
  print(i)
  
  # Adding SLF IDs for removal if they are missing numerical data in column 37 and 39 (MAD and MPTA)
  for(j in c(37, 39)){
    if(!(grepl("\\d", ortho_assess_12month[i,j]))){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding SLF IDs for removal if they are missing data in column 36 (Mech Axis)
  for(j in c(36)){
    if(!(grepl("valgus", ortho_assess_12month[i,j]) || grepl("varus", ortho_assess_12month[i,j]) || ortho_assess_12month[i,j] == "0" )){
      Index_removal_list[i] <- FALSE
    }
  }
  
  # Adding indices for removal if Alignment column lacks IC/IM or 0
  for(j in c(23)){
    if(!(grepl("IC", ortho_assess_12month[i,j])) & !(grepl("IM", ortho_assess_12month[i,j])) & !(ortho_assess_12month[i,j] == "0" )){
      Index_removal_list[i] <- FALSE
    }
  }
}


# RESULTS
# All 5 stages - 38 out of 92 legs remain
# If we exclude xafterwedge we have 51 out of 92
# If we exclude xafterwedge, xoutcast, we have 65 out of 92
# If we exclude xafterwedge, xoutcast, discharge, we have 67 out of 92
nrow(ortho_assess_preop[Index_removal_list,])
cohort_radiology_preop <- ortho_assess_preop[Index_removal_list,]

# GETTING REST OF REQUIRED DATA
# Number of patients = 27
length(unique(cohort_radiology_preop$SLF..))
# Blounts = 25
nrow(cohort_radiology_preop[grepl("blounts", cohort_radiology_preop$DIagnosis) & !(grepl("rickets", cohort_radiology_preop$DIagnosis)),])
# Rickets = 3
nrow(cohort_radiology_preop[!(grepl("blounts", cohort_radiology_preop$DIagnosis)) & (grepl("rickets", cohort_radiology_preop$DIagnosis)),])
# Blounts and Rickets = 2
nrow(cohort_radiology_preop[grepl("blounts", cohort_radiology_preop$DIagnosis) & (grepl("rickets", cohort_radiology_preop$DIagnosis)),])
# Other = 8
nrow(cohort_radiology_preop[!(grepl("blounts", cohort_radiology_preop$DIagnosis)) & !(grepl("rickets", cohort_radiology_preop$DIagnosis)),])
# Average Age = 9.02
mean(cohort_radiology_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Range 5-14
range(cohort_radiology_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Female to Male Ratio = 24:14 - 63%
nrow(cohort_radiology_preop[cohort_radiology_preop$Sex == "M" | cohort_radiology_preop$Sex == "m", ] )
# Tibial Plateau Elevation = 19
nrow(cohort_radiology_preop[cohort_radiology_preop$Tibial.Plateau.Elevation. == "Y", ])

################################################################################################################
# 2. Compile legs who have Alignment (clinical value) present across
#    3 stages of measurement (pre op, discharge, 12 mo)
#    Reminder if no value, the cell will say 'BLANK' or 'blank' or 'Blank'

colnames(ortho_assess_preop)
Index_removal_list <- rep(TRUE, nrow(ortho_assess_preop))

ortho_assess[23]


# First checking ortho_assess_preop
for(i in 1:nrow(ortho_assess_preop)){
  # counter
  print(i)
  
  # Adding indices for removal if Alignment column lacks IC/IM or 0
  for(j in c(23)){
    if(!(grepl("IC", ortho_assess_preop[i,j])) & !(grepl("IM", ortho_assess_preop[i,j])) & !(ortho_assess_preop[i,j] == "0") ){
      Index_removal_list[i] <- FALSE
    }
  }
  
}



# Then checking ortho_assess_discharge
for(i in 1:nrow(ortho_assess_discharge)){
  # counter
  print(i)
  
  # Adding indices for removal if Alignment column lacks IC/IM or 0
  for(j in c(23)){
    if(!(grepl("IC", ortho_assess_discharge[i,j])) & !(grepl("IM", ortho_assess_discharge[i,j])) & !(ortho_assess_discharge[i,j] == "0") ){
      Index_removal_list[i] <- FALSE
    }
  }
}


# Then checking ortho_assess_12month
for(i in 1:nrow(ortho_assess_12month)){
  # counter
  print(i)
  
  # Adding indices for removal if Alignment column lacks IC/IM or 0
  for(j in c(23)){
    if(!(grepl("IC", ortho_assess_12month[i,j])) & !(grepl("IM", ortho_assess_12month[i,j])) & !(ortho_assess_12month[i, j] == "0") ){
      Index_removal_list[i] <- FALSE
    }
  }
}


# RESULTS
# All 3 stages - 65 out of 92 legs remain
# If we exclude discharge - 68 out of 92 legs remain
nrow(ortho_assess_preop[Index_removal_list,])
cohort_clinical_preop <- ortho_assess_preop[Index_removal_list,]


# GETTING REST OF REQUIRED DATA
# Number of patients = 27
length(unique(cohort_clinical_preop$SLF..))
# Blounts = 39
nrow(cohort_clinical_preop[grepl("blounts", cohort_clinical_preop$DIagnosis) & !(grepl("rickets", cohort_clinical_preop$DIagnosis)),])
# Rickets = 11
nrow(cohort_clinical_preop[!(grepl("blounts", cohort_clinical_preop$DIagnosis)) & (grepl("rickets", cohort_clinical_preop$DIagnosis)),])
# Blounts and Rickets = 2
nrow(cohort_clinical_preop[grepl("blounts", cohort_clinical_preop$DIagnosis) & (grepl("rickets", cohort_clinical_preop$DIagnosis)),])
# Other = 13
nrow(cohort_clinical_preop[!(grepl("blounts", cohort_clinical_preop$DIagnosis)) & !(grepl("rickets", cohort_clinical_preop$DIagnosis)),])
# Average Age = 9.24
mean(cohort_clinical_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Range 4-14
range(cohort_clinical_preop$Age.at.first.surgery.based.on.DOB.and.date.of.surgery.columns)
# Female to Male Ratio = 35:30 - 53%
nrow(cohort_clinical_preop[cohort_clinical_preop$Sex == "M" | cohort_clinical_preop$Sex == "m" | cohort_clinical_preop$Sex == "M ", ] )
# Tibial Plateau Elevation = 30
nrow(cohort_clinical_preop[cohort_clinical_preop$Tibial.Plateau.Elevation. == "Y", ])

unique(cohort_clinical_preop$Sex)


################################################################################################################
# Playground

ortho_assess_preop$Mech.Axis
ortho_assess_xafterwedge$
ortho_assess_discharge$Mech.Axis


ortho_assess_all_preop <- ortho_assess_all[ortho_assess_all$Stage.of.Care == "Assess_Preop", ]
SLF_double_diags <- list()

for(i in 1:nrow(ortho_assess_all_preop)){
  if(grepl("blounts", ortho_assess_all_preop$DIagnosis[i]) && grepl("rickets", ortho_assess_all_preop$DIagnosis[i])){
    
  }
}

grepl("blounts", cohort_radiology_preop$DIagnosis)
grepl("rickets", cohort_radiology_preop$DIagnosis)

intersect(grepl("blounts", cohort_radiology_preop$DIagnosis), grepl("blounts", cohort_radiology_preop$DIagnosis))

grepl("blounts", cohort_radiology_preop$DIagnosis) & grepl("rickets", cohort_radiology_preop$DIagnosis)

