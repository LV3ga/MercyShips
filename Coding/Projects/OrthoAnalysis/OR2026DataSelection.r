                                        # In this file, variables are cleaned and selected for analysis.
                                        # The result is stored in OR2026DataSelected.csv
###################################################################################################################

                                        # Setting directory
setwd("c:/MercyShips/Coding/Projects/OrthoAnalysis")

                                        # Reading in data
OR2026Data <- read.csv("OR2026DataTransformed.csv")

                                        # Storing data relevant for analysis
ID <- OR2026Data$PatientID
firstName <- OR2026Data$First.name
lastName <- OR2026Data$Last.Name.
age <- as.numeric(OR2026Data$Age)
preopBMI <- as.numeric(OR2026Data$Preop.BMI..V11.)
sex <- gsub(" ", "", toupper(OR2026Data$Sex))
preopAlign <-as.numeric(OR2026Data$Preop.cleaned_Alignment..V2..in.cm)
dischargeAlign <-as.numeric(OR2026Data$Discharge.cleaned_Alignment..V2..in.cm)
oneyearAlign <-as.numeric(OR2026Data$Oneyear.cleaned_Alignment..V2..in.cm)
changeAlign <- abs(oneyearAlign - dischargeAlign)

                                        # Combining relevant data into spreadsheet to be analyzed
OR2026DataAnalysis <- cbind(ID, firstName, lastName, age, preopBMI, sex, preopAlign, dischargeAlign, oneyearAlign, changeAlign)

                                        # Exporting Data
write.csv(OR2026DataAnalysis, "OR2026DataSelected.csv")

