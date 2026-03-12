setwd("c:/MercyShips/Coding/Projects/OrthoAnalysis")

                                        # Reading in data
data <- read.csv("OR2026DataSelected.csv")

                                        # Comparing SLG and 
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


                                        # alignment linear model
modelAlign <- lm(oneyearAlign ~ age + preopBMI + sex + preopAlign, data = data)
summary(modelAlign)

plot(data$age, data$preopAlign - data$oneyearAlign)

as.numeric(data$sex[grepl("SLG", data$ID)])
