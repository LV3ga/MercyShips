                                        # This file uses OR2026DataTransformed.csv which was produced by
                                        # OR2026DataTransform.r - which transforms the row oriented dataset
                                        # into a column oriented dataset, and, selects only legs that were
                                        # operated on.

                                        # In this file, legs are further filtered to only include blounts legs
                                        # and variables are cleaned and selected for analysis.
                                        # The result is stored in OR2026DataSelectedBlounts.csv
###################################################################################################################

                                        # Setting directory
setwd("c:/MercyShips/Coding/Projects/OrthoAnalysis")



                                        # Reading in data
OR2026Data <- read.csv("OR2026DataTransformed.csv")



                                        # Selecting only legs with blounts in the diagnosis column
                                        #                                 ^^^ some legs still don't have blounts!
                                        # Also re-labeling row indicies so numbers aren't skipped
OR2026Data <- OR2026Data[grepl("blounts", tolower(OR2026Data$DIagnosis)), ]
rownames(OR2026Data) <- 1:nrow(OR2026Data)



                                        # Some patients had both legs operated on, but only one leg had blounts.
                                        # However, the diagnosis column doesn't distinguish between legs.
                                        # Therefore, we need to further filter legs by mannually looking at the diagnosis
nrow(OR2026Data)
OR2026Data[,c(1, 2, 11, 12)]



                                        # Removing legs that do not have blounts disease (there were two)
OR2026Data <- OR2026Data[-c(59, 79),]
rownames(OR2026Data) <- 1:nrow(OR2026Data)



                                        # Storing data relevant for analysis

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
changeAlign <- abs(oneyearAlign - dischargeAlign)


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


                                        # Combining relevant data into spreadsheet to be analyzed
OR2026DataAnalysis <- cbind(ID, firstName, lastName, age, preopBMI, sex, diagnosis, surgeryName,  preopAlign, dischargeAlign, oneyearAlign, changeAlign, preopEQ.1, preopEQ.2, preopEQ.3, preopEQ.4, preopEQ.5, preopEQ.final,
                            dischargeEQ.1, dischargeEQ.2, dischargeEQ.3, dischargeEQ.4, dischargeEQ.5, dischargeEQ.final,
                            oneyearEQ.1, oneyearEQ.2, oneyearEQ.3, oneyearEQ.4, oneyearEQ.5, oneyearEQ.final,
                            twoyearEQ.1, twoyearEQ.2, twoyearEQ.3, twoyearEQ.4, twoyearEQ.5, twoyearEQ.final)

                                        # Exporting Data
write.csv(OR2026DataAnalysis, "OR2026DataSelectedBlounts.csv")
