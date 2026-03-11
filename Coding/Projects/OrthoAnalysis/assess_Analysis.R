############################################################################################
# ORGANIZING DATASETS FOR ANALYSIS
# 
# Notes from this section:
# 1. SLF 8216 is the only patient with both Rickets and Blounts in their diagnosis.
#    Specifically, it says: "blounts post op (rickets preop)". I AM CURRENTLY EXCLUDING
#    THE PATIENT FROM THE DATA SUMMARY, AS I DO NOT KNOW HOW TO CATEGORIZE
############################################################################################

# Getting Assessment Ortho Data
ortho_assess <- read.csv("Assess.csv")
# Removing all records from legs NOT operated on
ortho_assess <- ortho_assess[ortho_assess$Operated.on. == "Y", ]
ortho_assess_all <- ortho_assess
# SLF 8216 has both rickets and blounts listed in the Diagnosis column. They are removed from the data below.
ortho_assess <- ortho_assess[!(ortho_assess$SLF.. == "8216"),]




# Assessment Ortho Data Preop
ortho_assess_preop <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_Preop",]
# Assessment Ortho Data Preop w/ rickets
ortho_assess_preop_rickets <- ortho_assess_preop[grepl("rickets", ortho_assess_preop$DIagnosis),]
# Assessment Ortho Data Preop w/ blounts
ortho_assess_preop_blounts <- ortho_assess_preop[grepl("blounts", ortho_assess_preop$DIagnosis),]
# Assessment Ortho Data Preop other (not recorded rickets or blounts)
ortho_assess_preop_other <- ortho_assess_preop[!(ortho_assess_preop$SLF.. %in% append(ortho_assess_preop_rickets$SLF.., ortho_assess_preop_blounts$SLF..)),]


# Assessment Ortho Data XAssess_After_Wedge
ortho_assess_xafterwedge <- ortho_assess[ortho_assess$Stage.of.Care == "XAssess_After_Wedge", ]


# Assessment Ortho Data XAssess_Immediately_Out_of_Cast
ortho_assess_xoutcast <- ortho_assess[ortho_assess$Stage.of.Care == "XAssess_Immediately_Out_of_Cast", ]


# Assessment Ortho Data Discharge
ortho_assess_discharge <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_Initial_Discharge",]
# Assessment Ortho Data Discharge w/ rickets
ortho_assess_discharge_rickets <- ortho_assess_discharge[grepl("rickets", ortho_assess_discharge$DIagnosis),]
# Assessment Ortho Data Discharge w/ blounts
ortho_assess_discharge_blounts <- ortho_assess_discharge[grepl("blounts", ortho_assess_discharge$DIagnosis),]
# Assessment Ortho Data Discharge other (not recorded rickets or blounts)
ortho_assess_discharge_other <- ortho_assess_discharge[!(ortho_assess_discharge$SLF.. %in% append(ortho_assess_discharge_rickets$SLF.., ortho_assess_discharge_blounts$SLF..)),]


# Assessment Ortho Data 12 Month
ortho_assess_12month <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_12_month_postop",]
# Assessment Ortho Data 12 Month w/ rickets
ortho_assess_12month_rickets <- ortho_assess_12month[grepl("rickets", ortho_assess_12month$DIagnosis),]
# Assessment Ortho Data 12 Month w/ blounts
ortho_assess_12month_blounts <- ortho_assess_12month[grepl("blounts", ortho_assess_12month$DIagnosis),]
# Assessment Ortho Data Discharge other (not recorded rickets or blounts)
ortho_assess_12month_other <- ortho_assess_12month[!(ortho_assess_12month$SLF.. %in% append(ortho_assess_12month_rickets$SLF.., ortho_assess_12month_blounts$SLF..)),]



############################################################################################
# MPTA OUTCOME ANALYSIS
#
# do analysis for MPTA for all stages of care
# 
############################################################################################


############################################################################################
# ANGLE OF DEVIATION OUTCOME ANALYSIS
#
# do analysis for Angle of Deviation for all stages of care
# 
############################################################################################


############################################################################################
# Understand whether diagnosis impacted long term outcomes Blounts vs rickets vs other 
# 
# Notes from this section:
# 1. 
#
#
############################################################################################

# Outcome variable: Alignment (column 23 in the data)

ortho_assess_preop_rickets[23]
ortho_assess_12month_rickets[23]
ortho_assess_12month_other[23]

ortho_assess_preop_blounts[23]
ortho_assess_12month_blounts[23]

ortho_assess <- ortho_assess[!(ortho_assess$SLF.. == 8326), ]


assess[177,]
