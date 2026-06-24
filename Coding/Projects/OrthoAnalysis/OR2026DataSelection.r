
                                        # In this file, variables are cleaned and selected for analysis.
                                        # The result is stored in OR2026DataSelected.csv
###################################################################################################################

                                        # Setting directory
setwd("c:/MercyShips/Coding/Projects/OrthoAnalysis")

                                        # Reading in data
OR2026Data <- read.csv("OR2026DataTransformed.csv")


# Demographics
ID <- OR2026Data$PatientID
firstName <- OR2026Data$First.name
lastName <- OR2026Data$Last.Name.
age <- as.numeric(OR2026Data$Age)
diagnosis <- ifelse(grepl("blounts", tolower(OR2026Data$DIagnosis)), "blounts", ifelse( grepl("rickets", tolower(OR2026Data$DIagnosis)), "rickets", "other"))
preopBMI <- as.numeric(OR2026Data$Preop.BMI..V11.)
sex <- gsub(" ", "", toupper(OR2026Data$Sex))
surgeryName <- OR2026Data$Name.of.Surgery.Done


# Clinical Data
preopAlign <-as.numeric(OR2026Data$Preop.cleaned_Alignment..V2..in.cm)
dischargeAlign <-as.numeric(OR2026Data$Discharge.cleaned_Alignment..V2..in.cm)
oneyearAlign <-as.numeric(OR2026Data$Oneyear.cleaned_Alignment..V2..in.cm)
twoyearAlign <-as.numeric(OR2026Data$Twoyear.cleaned_Alignment..V2..in.cm)


# Radiological Data
preopMechAxisRaw <- OR2026Data$Preop.Mech.Axis
dischargeMechAxisRaw <-OR2026Data$Discharge.Mech.Axis
oneyearMechAxisRaw <-OR2026Data$Oneyear.Mech.Axis
twoyearMechAxisRaw <-OR2026Data$Twoyear.Mech.Axis
preopMechAxisDegrees <- OR2026Data$Preop.cleaned_mech_axis_degrees
dischargeMechAxisDegrees <-OR2026Data$Discharge.cleaned_mech_axis_degrees
oneyearMechAxisDegrees <-OR2026Data$Oneyear.cleaned_mech_axis_degrees
twoyearMechAxisDegrees <-OR2026Data$Twoyear.cleaned_mech_axis_degrees
preopMechAxisMeasurement <- OR2026Data$Preop.cleaned_mech_axis_varus_valgus
dischargeMechAxisMeasurement <-OR2026Data$Discharge.cleaned_mech_axis_varus_valgus
oneyearMechAxisMeasurement <-OR2026Data$Oneyear.cleaned_mech_axis_varus_valgus
twoyearMechAxisMeasurement <-OR2026Data$Twoyear.cleaned_mech_axis_varus_valgus
preopMAD <- as.numeric(OR2026Data$Preop.Mech.Axis.Deviation..mm.)
dischargeMAD <-as.numeric(OR2026Data$Discharge.Mech.Axis.Deviation..mm.)
oneyearMAD <- as.numeric(OR2026Data$Oneyear.Mech.Axis.Deviation..mm.)
twoyearMAD <-as.numeric(OR2026Data$Twoyear.Mech.Axis.Deviation..mm.)
 

# QOL Data
preopEQ.1 <- OR2026Data$Preop.EQ.1..Mobility..how.much.difficulty.do.you.have.walking.about.
preopEQ.2 <- OR2026Data$Preop.EQ.2..Looking.After.Myself..How.much.difficlty.do.you.have.washing.or.dressing.yourself.
preopEQ.3 <- OR2026Data$Preop.EQ.3..Doing.Usual.Activities..for.example.poing.to.school..hobbies..sports..playing...How.much.difficulty.do.you.having.doing.your.usual.activities.
preopEQ.4 <- OR2026Data$Preop.EQ.4..Pain.or.Discomfort..How.much.pain.or.discomfort.do.you.have.
preopEQ.5 <- OR2026Data$Preop.EQ.5..Feeling.worried..sad..or.unhappy..How.much.do.you.feel.worried..sad..or.unhappy.
preopEQ.final <- OR2026Data$Preop.EQ..Health.Rating..How.good.or.bad.is.your.health.today...scale.of.0.100..with.0.being.worst.health.and.100.being.best.health.
dischargeEQ.1 <- OR2026Data$Discharge.EQ.1..Mobility..how.much.difficulty.do.you.have.walking.about.
dischargeEQ.2 <- OR2026Data$Discharge.EQ.2..Looking.After.Myself..How.much.difficlty.do.you.have.washing.or.dressing.yourself.
dischargeEQ.3 <- OR2026Data$Discharge.EQ.3..Doing.Usual.Activities..for.example.poing.to.school..hobbies..sports..playing...How.much.difficulty.do.you.having.doing.your.usual.activities.
dischargeEQ.4 <- OR2026Data$Discharge.EQ.4..Pain.or.Discomfort..How.much.pain.or.discomfort.do.you.have.
dischargeEQ.5 <- OR2026Data$Discharge.EQ.5..Feeling.worried..sad..or.unhappy..How.much.do.you.feel.worried..sad..or.unhappy.
dischargeEQ.final <- OR2026Data$Discharge.EQ..Health.Rating..How.good.or.bad.is.your.health.today...scale.of.0.100..with.0.being.worst.health.and.100.being.best.health.
oneyearEQ.1 <- OR2026Data$Oneyear.EQ.1..Mobility..how.much.difficulty.do.you.have.walking.about.
oneyearEQ.2 <- OR2026Data$Oneyear.EQ.2..Looking.After.Myself..How.much.difficlty.do.you.have.washing.or.dressing.yourself.
oneyearEQ.3 <- OR2026Data$Oneyear.EQ.3..Doing.Usual.Activities..for.example.poing.to.school..hobbies..sports..playing...How.much.difficulty.do.you.having.doing.your.usual.activities.
oneyearEQ.4 <- OR2026Data$Oneyear.EQ.4..Pain.or.Discomfort..How.much.pain.or.discomfort.do.you.have.
oneyearEQ.5 <- OR2026Data$Oneyear.EQ.5..Feeling.worried..sad..or.unhappy..How.much.do.you.feel.worried..sad..or.unhappy.
oneyearEQ.final <- OR2026Data$Oneyear.EQ..Health.Rating..How.good.or.bad.is.your.health.today...scale.of.0.100..with.0.being.worst.health.and.100.being.best.health.
twoyearEQ.1 <- OR2026Data$Twoyear.EQ.1..Mobility..how.much.difficulty.do.you.have.walking.about.
twoyearEQ.2 <- OR2026Data$Twoyear.EQ.2..Looking.After.Myself..How.much.difficlty.do.you.have.washing.or.dressing.yourself.
twoyearEQ.3 <- OR2026Data$Twoyear.EQ.3..Doing.Usual.Activities..for.example.poing.to.school..hobbies..sports..playing...How.much.difficulty.do.you.having.doing.your.usual.activities.
twoyearEQ.4 <- OR2026Data$Twoyear.EQ.4..Pain.or.Discomfort..How.much.pain.or.discomfort.do.you.have.
twoyearEQ.5 <- OR2026Data$Twoyear.EQ.5..Feeling.worried..sad..or.unhappy..How.much.do.you.feel.worried..sad..or.unhappy.
twoyearEQ.final <- OR2026Data$Twoyear.EQ..Health.Rating..How.good.or.bad.is.your.health.today...scale.of.0.100..with.0.being.worst.health.and.100.being.best.health.


#Pain Data
preopBestPain <- as.numeric(OR2026Data[ , which(colnames(OR2026Data) == "Preop.Best.Pain")])
dischargeBestPain <- as.numeric(OR2026Data[ , which(colnames(OR2026Data) == "Discharge.Best.Pain")])
oneyearBestPain <- as.numeric(OR2026Data[ , which(colnames(OR2026Data) == "Oneyear.Best.Pain")])
preopWorstPain <- as.numeric(OR2026Data[ , which(colnames(OR2026Data) == "Preop.Worst.Pain")])
dischargeWorstPain <- as.numeric(OR2026Data[ , which(colnames(OR2026Data) == "Discharge.Worst.Pain")])
oneyearWorstPain <- as.numeric(OR2026Data[ , which(colnames(OR2026Data) == "Oneyear.Worst.Pain")])



                                        # Combining relevant data into spreadsheet to be analyzed










OR2026DataAnalysis <- cbind(ID, firstName, lastName, age, preopBMI, sex, diagnosis, surgeryName,
                            preopAlign, dischargeAlign, oneyearAlign, twoyearAlign,
                            preopMechAxisRaw, dischargeMechAxisRaw, oneyearMechAxisRaw, twoyearMechAxisRaw,
                            preopMechAxisDegrees, dischargeMechAxisDegrees, oneyearMechAxisDegrees, twoyearMechAxisDegrees,
                            preopMechAxisMeasurement, dischargeMechAxisMeasurement, oneyearMechAxisMeasurement, twoyearMechAxisMeasurement,
                            preopMAD, dischargeMAD, oneyearMAD, twoyearMAD,
                            preopEQ.1, preopEQ.2, preopEQ.3, preopEQ.4, preopEQ.5, preopEQ.final,
                            dischargeEQ.1, dischargeEQ.2, dischargeEQ.3, dischargeEQ.4, dischargeEQ.5,
                            dischargeEQ.final, oneyearEQ.1, oneyearEQ.2, oneyearEQ.3, oneyearEQ.4,
                            oneyearEQ.5, oneyearEQ.final, twoyearEQ.1, twoyearEQ.2, twoyearEQ.3,
                            twoyearEQ.4, twoyearEQ.5, twoyearEQ.final, preopBestPain, preopWorstPain,
                            dischargeBestPain, dischargeWorstPain, oneyearBestPain, oneyearWorstPain)

                                        # Exporting Data
write.csv(OR2026DataAnalysis, "OR2026DataSelected.csv")


