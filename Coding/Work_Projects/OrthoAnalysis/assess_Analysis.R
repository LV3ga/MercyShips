
setwd("C:\\MercyShips\\Coding\\Work_Projects\\OrthoAnalysis")


ortho_assess <- read.csv("Assess.csv")


ortho_assess_preop <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_Preop",]
ortho_assess_preop_rickets <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_Preop",]

ortho_assess_discharge <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_Initial_Discharge",]


ortho_assess_12month <- ortho_assess[ortho_assess$Stage.of.Care == "Assess_12_month_postop",]


grepl("rickets", ortho_assess_preop$DIagnosis)


