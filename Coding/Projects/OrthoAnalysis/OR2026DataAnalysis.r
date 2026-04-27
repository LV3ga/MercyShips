

library(scales)
library(ggplot2)
library(tidyverse)

setwd("c:/MercyShips/Coding/Projects/OrthoAnalysis")

                                        # Reading in data
data <- read.csv("OR2026DataSelected.csv")




####################################################################################
                                        # Demographics - START


                                        # SLG vs SLF Diagnosis
table(data$diagnosis[grepl("SLG", data$ID)])
table(data$diagnosis[grepl("SLF", data$ID)])



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
SLG_patients <- data.frame(ID = d
                                        # Demographics - END
####################################################################################






####################################################################################
                                        # Quality of Life - START


                           
                                        # EuroQol and Pain Data
                                        # Converting columns to numeric
euroQol_Data <- as.data.frame(apply(data[, which(grepl("EQ", colnames(data)))],2, as.numeric))
pain_Data <- as.data.frame(apply(data[, which(grepl("Pain", colnames(data)))],2, as.numeric))
euroQol_Data <- cbind(ID = data$ID, euroQol_Data)
pain_Data <- cbind(ID = data$ID, pain_Data)
qol_Data <- cbind(euroQol_Data, subset(pain_Data, select = -ID))




                                        # Selecting for unique patientIDs since there are multiple entries
                                        # (duplicates) since some patients had surgery on two legs
qol_Data <- qol_Data[!duplicated(qol_Data[c('ID')]), ]
write.csv(qol_Data, "OR2026QOL.csv")



                                        # Getting SLG EuroQol Data (no pain data because nothing to compare it to)
                                        # Getting SLF EuroQol and Pain Data
SLG_euroQol_Data <- euroQol_Data[which(grepl("SLG", euroQol_Data$ID)), ]
SLG_Pain_Data <- pain_Data[which(grepl("SLG", euroQol_Data$ID)), ]
SLF_euroQol_Data <- euroQol_Data[which(grepl("SLF", euroQol_Data$ID)), ]




                                        # Removing SLF EuroQol rows without one year or two year data
                                        # Removing duplicate ID entries
SLF_euroQol_Data <- SLF_euroQol_Data[!(is.na(SLF_euroQol_Data$oneyearEQ.final) | is.na(SLF_euroQol_Data$twoyearEQ.final)), ]
SLF_euroQol_Data <- SLF_euroQol_Data[!duplicated(SLF_euroQol_Data[c('ID')]), ]




                                        # Removing SLG Pain data rows without one year data
                                        # Removing SLG EuroQol rows without one year data
                                        # Removing duplicate ID entries from both data frames
SLG_Pain_Data <- SLG_Pain_Data[!(is.na(SLG_Pain_Data$oneyearBestPain) | is.na(SLG_Pain_Data$oneyearWorstPain)), ]
SLG_euroQol_Data <- SLG_euroQol_Data[!(is.na(SLG_euroQol_Data$oneyearEQ.final)), ]
SLG_euroQol_Data <- SLG_euroQol_Data[!duplicated(SLG_euroQol_Data[c('ID')]), ]
SLG_Pain_Data <- SLG_Pain_Data[!duplicated(SLG_Pain_Data[c('ID')]), ]




                                        # Jitter Plots
ggplot(SLG_Pain_Data[, which(grepl("Worst", colnames(SLG_Pain_Data)))])


                                        # Writing SLG Pain Data, SLG EuroQol Data and SLF EuroQol Data to csv
write.csv(SLF_euroQol_Data, "OR2026SLFEuroQol.csv")
write.csv(SLG_euroQol_Data, "OR2026SLGEuroQol.csv")
write.csv(SLG_Pain_Data, "OR2026SLGPain.csv")
                                        # Quality of Life - END
####################################################################################

StageOfCare <- data.frame(Stage.Of.Care = c(rep("Pre-Op", nrow(SLG_Pain_Data)), rep("Discharge", nrow(SLG_Pain_Data)),  rep("One Year", nrow(SLG_Pain_Data))))

worstPain <- rbind(data.frame(Worst.Pain = SLG_Pain_Data$preopWorstPain),
        rbind( data.frame(Worst.Pain = SLG_Pain_Data$dischargeWorstPain),
        data.frame(Worst.Pain = SLG_Pain_Data$oneyearWorstPain)))

SLG_Pain_Plot_Data <- cbind(StageOfCare, worstPain)

mean <- SLG_Pain_Plot_Data %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Worst.Pain, na.rm = TRUE))


dat_text <- data.frame(
  label = c("Mean = 3.89", "Mean = 1.88", "Mean = 0.88"),
  Stage.Of.Care = c("Pre-Op", "Discharge", "One Year"),
  x = c(0,0,0),
  y = c(3.1, 1.6, 0.6)
)


# create ggplot scatter plot
# add horizontal line overlay at mean using geom_hline()
# divide plot in facet using function facet_grid()
ggplot(data = SLG_Pain_Plot_Data, aes(x = c(0), y=Worst.Pain)) +
scale_y_continuous(breaks = pretty_breaks()) +
geom_point(aes(colour = Stage.Of.Care), position = position_jitter(w = 0.1, h = 0)) + 
geom_hline(data= mean, aes(yintercept = mean_val,col=Stage.Of.Care))+
geom_text(
  data    = dat_text,
  mapping = aes(x = x, y = y, label = label),
  family = "serif",
  size = 3
) +
facet_grid(~factor(Stage.Of.Care, levels = c("Pre-Op", "Discharge", "One Year"))) +
theme(panel.background = element_rect(fill = "aliceblue", colour = "grey"),
      strip.background  = element_rect(fill = "white", colour = "grey"),
      text = element_text(family = "serif", size = 11),
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "Worst Pain Score (0-10)") +
scale_x_continuous(breaks = NULL) +
scale_color_manual(values = c("red", "darkgreen", "blue"), labels = c("Patients at Pre-Op", "Patients at Discharge", "Patients at One Year")) +
labs(fill = "Stages of Care")



