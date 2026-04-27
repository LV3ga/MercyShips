

setwd("c:/MercyShips/Coding/Projects/OrthoAnalysis")

                                        # Reading in data
data <- read.csv("OR2026DataSelectedBlounts.csv")

                                        # Demographics (all)
####################################################################################


                                        # SLG vs SLF preop BMI
par(mfrow = c(2, 2))
boxplot(data$preopBMI[grepl("SLF", data$ID)], data$preopBMI[grepl("SLG", data$ID)], names = c("SLF", "SLG"), main = "SLG vs SLF Preop BMI")
hist(data$preopBMI[grepl("SLF", data$ID)], xlim = c(10,35), ylim = c(0,25), breaks = 10, xlab = "Preop BMI", main = "Histogram of SLF Preop BMI")
hist(data$preopBMI[grepl("SLG", data$ID)], xlim = c(10,35), ylim = c(0,25), breaks = 10, xlab = "Preop BMI", main = "Histogram of SLG Preop BMI")
t.test(data$preopBMI[grepl("SLF", data$ID)], data$preopBMI[grepl("SLG", data$ID)])



                                        # SLG vs SLF age
par(mfrow = c(2, 2))
boxplot(data$age[grepl("SLF", data$ID)], data$age[grepl("SLG", data$ID)], names = c("SLF", "SLG"), main = "SLG vs SLF Age")
hist(data$age[grepl("SLF", data$ID)], xlim = c(4,16), ylim = c(0,20), breaks = 10, xlab = "Age", main = "Histogram of SLF Age")
hist(data$age[grepl("SLG", data$ID)], xlim = c(4,16), ylim = c(0,20), breaks = 10, xlab = "Age", main = "Histogram of SLG Age")
t.test(data$age[grepl("SLF", data$ID)], data$age[grepl("SLG", data$ID)])



                                        # SLG vs SLF sex
par(mfrow = c(1, 2))
barplot(data$sex[grepl("SLF", data$ID)])
barplot(data$sex[grepl("SLG", data$ID)])
barplot(table(data$sex[grepl("SLF", data$ID)]))
table(data$sex[grepl("SLG", data$ID)])
table(data$sex[grepl("SLF", data$ID)])



                                        # Which patients appear in SLG and SLF?
                                        # After running the code below, it seems like SLG08628 is also SLF08357
                                        # I checked the spreadsheet, and it looks like they have the same DOB.
SLG_patients <- data.frame(ID = data$ID[grepl("SLG", data$ID)], name = paste(data$firstName[grepl("SLG", data$ID)], data$lastName[grepl("SLG", data$ID)]), age = data$age[grepl("SLG", data$ID)])
SLF_patients <- data.frame(ID = data$ID[grepl("SLF", data$ID)], name = paste(data$firstName[grepl("SLF", data$ID)], data$lastName[grepl("SLF", data$ID)]), age = data$age[grepl("SLF", data$ID)])
SLG_patients[tolower(SLG_patients$name) %in% tolower(SLF_patients$name), ]
SLF_patients[grepl("mariama", tolower(SLF_patients$name)), ]
SLG_patients$name[tolower(SLG_patients$name) %in% tolower(SLF_patients$name)]

                                        # Comparing SLG and SLF - END
####################################################################################

