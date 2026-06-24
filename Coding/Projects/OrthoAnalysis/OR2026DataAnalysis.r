
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



                           
                                        # Research - START
####################################################################################




                                        # Research - END
####################################################################################



                                        # Clinical Report Analysis - START
####################################################################################

summary(data$preopAlign, na.rm=TRUE)
summary(data$dischargeAlign, na.rm=TRUE)
summary(data$oneyearAlign, na.rm=TRUE)
summary(data$twoyearAlign, na.rm=TRUE)

summary(data$preopMechAxisDegrees, na.rm=TRUE)
summary(data$dischargeMechAxisDegrees, na.rm=TRUE)
summary(data$oneyearMechAxisDegrees, na.rm=TRUE)
summary(data$twoyearMechAxisDegrees, na.rm=TRUE)

summary(data$preopMAD, na.rm=TRUE)
summary(data$dischargeMAD, na.rm=TRUE)
summary(data$oneyearMAD, na.rm=TRUE)
summary(data$twoyearMAD, na.rm=TRUE)



                                        # Clinical Report Analysis - END
####################################################################################

                           
                           


####################################################################################
                                        # Quality of Life - START


                           
                                        # EuroQol and Pain Data
                                        # Converting columns to numeric
euroQol_Data <- as.data.frame(apply(data[, which(grepl("EQ", colnames(data)))],2, as.numeric))
euroQol_Data$preop.EQ.score <- (1- (rowSums(euroQol_Data[, 1:5])/20)) * 100
euroQol_Data$discharge.EQ.score <- (1- (rowSums(euroQol_Data[, 7:11])/20)) * 100
euroQol_Data$oneyear.EQ.score <- (1- (rowSums(euroQol_Data[, 13:17])/20)) * 100
euroQol_Data$twoyear.EQ.score <- (1- (rowSums(euroQol_Data[, 19:23])/20)) * 100
pain_Data <- as.data.frame(apply(data[, which(grepl("Pain", colnames(data)))],2, as.numeric))
euroQol_Data <- cbind(ID = data$ID, euroQol_Data)
pain_Data <- cbind(ID = data$ID, pain_Data)
qol_Data <- cbind(euroQol_Data, subset(pain_Data, select = -ID))




                                        # Selecting for unique patientIDs since there are multiple entries
                                        # (duplicates) since some patients had surgery on two legs
qol_Data <- qol_Data[!duplicated(qol_Data[c('ID')]), ]
write.csv(qol_Data, "OR2026QOL.csv")



                                        # Getting SLF EuroQol Data (no pain data because nothing to compare it to)
                                        # Getting SLG EuroQol and Pain Data
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





                                        # Writing SLG Pain Data, SLG EuroQol Data and SLF EuroQol Data to csv
write.csv(SLF_euroQol_Data, "OR2026SLFEuroQol.csv")
write.csv(SLG_euroQol_Data, "OR2026SLGEuroQol.csv")
write.csv(SLG_Pain_Data, "OR2026SLGPain.csv")
                                        # Quality of Life - END
####################################################################################
                                        # Plotting SLG Pain Data - START



StageOfCare <- data.frame(Stage.Of.Care = c(rep("Pre-Op", nrow(SLG_Pain_Data)), rep("Discharge", nrow(SLG_Pain_Data)),  rep("One Year", nrow(SLG_Pain_Data))))

worstPain <- rbind(data.frame(Worst.Pain = SLG_Pain_Data$preopWorstPain),
        rbind( data.frame(Worst.Pain = SLG_Pain_Data$dischargeWorstPain),
        data.frame(Worst.Pain = SLG_Pain_Data$oneyearWorstPain)))

SLG_Pain_Plot_Data <- cbind(StageOfCare, worstPain)

mean <- SLG_Pain_Plot_Data %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Worst.Pain, na.rm = TRUE))


dat_text <- data.frame(
  label = c("Average = 3.89", "Average = 1.88", "Average = 0.88"),
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
      legend.position = "none",
      legend.title = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "Worst Pain Score (0-10)") +
scale_x_continuous(breaks = NULL) 

                                        # Plotting SLG Pain Data - END
####################################################################################





####################################################################################
                                        # Plotting SLG EuroQOL Data - START


                                        # SLG EuroQol Score Data
StageOfCare.EQ <- data.frame(Stage.Of.Care = c(rep("Pre-Op", nrow(SLG_euroQol_Data)), rep("Discharge", nrow(SLG_euroQol_Data)), rep("One Year", nrow(SLG_euroQol_Data))))

EuroQol.Score  <- rbind(data.frame(Score = SLG_euroQol_Data$preop.EQ.score),
        rbind( data.frame(Score = SLG_euroQol_Data$discharge.EQ.score),
        data.frame(Score = SLG_euroQol_Data$oneyear.EQ.score)))

SLG_euroQol_Plot_Data <- cbind(StageOfCare.EQ, EuroQol.Score)

mean_euroQol <- SLG_euroQol_Plot_Data %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Score, na.rm = TRUE))


dat_text_euroQol <- data.frame(
  label = c("Average = 50.5", "Average = 89.4", "Average = 94.6"),
  Stage.Of.Care = c("Pre-Op", "Discharge", "One Year"),
  x = c(0,0,0),
  y = c(48.3, 88, 93.3)
)


# create ggplot scatter plot
# add horizontal line overlay at mean using geom_hline()
# divide plot in facet using function facet_grid()
ggplot(data = SLG_euroQol_Plot_Data, aes(x = c(0), y=Score)) +
scale_y_continuous(breaks = pretty_breaks()) +
geom_point(aes(colour = Stage.Of.Care), position = position_jitter(w = 0.1, h = 0)) + 
geom_hline(data= mean_euroQol, aes(yintercept = mean_val,col=Stage.Of.Care))+
geom_text(
  data    = dat_text_euroQol,
  mapping = aes(x = x, y = y, label = label),
  family = "serif",
  size = 3
) +
facet_grid(~factor(Stage.Of.Care, levels = c("Pre-Op", "Discharge", "One Year"))) +
theme(panel.background = element_rect(fill = "aliceblue", colour = "grey"),
      strip.background  = element_rect(fill = "white", colour = "grey"),
      text = element_text(family = "serif", size = 11),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "EuroQol Score (0-100)") +
scale_x_continuous(breaks = NULL) +
scale_fill_discrete(breaks=c("Pre-Op", "Discharge", "One Year")) +
labs(fill = "Stages of Care")





                                        # SLG EuroQol Health Ranking Data

EuroQol.Final  <- rbind(data.frame(Score = SLG_euroQol_Data$preopEQ.final),
        rbind( data.frame(Score = SLG_euroQol_Data$dischargeEQ.final),
        data.frame(Score = SLG_euroQol_Data$oneyearEQ.final)))

SLG_euroQol2_Plot_Data <- cbind(StageOfCare.EQ, EuroQol.Final)

mean_euroQol2 <- SLG_euroQol2_Plot_Data %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Score, na.rm = TRUE))


dat_text_euroQol2 <- data.frame(
  label = c("Average = 40.5", "Average = 94.8", "Average = 97.5"),
  Stage.Of.Care = c("Pre-Op", "Discharge", "One Year"),
  x = c(0,0,0),
  y = c(39, 93, 96)
)


# create ggplot scatter plot
# add horizontal line overlay at mean using geom_hline()
# divide plot in facet using function facet_grid()
ggplot(data = SLG_euroQol2_Plot_Data, aes(x = c(0), y=Score)) +
scale_y_continuous(breaks = pretty_breaks()) +
geom_point(aes(colour = Stage.Of.Care), position = position_jitter(w = 0.1, h = 0)) + 
geom_hline(data= mean_euroQol2, aes(yintercept = mean_val,col=Stage.Of.Care))+
geom_text(
  data    = dat_text_euroQol2,
  mapping = aes(x = x, y = y, label = label),
  family = "serif",
  size = 3
) +
facet_grid(~factor(Stage.Of.Care, levels = c("Pre-Op", "Discharge", "One Year"))) +
theme(panel.background = element_rect(fill = "aliceblue", colour = "grey"),
      strip.background  = element_rect(fill = "white", colour = "grey"),
      text = element_text(family = "serif", size = 11),
      legend.position = "none",
      legend.title = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "Patient Reported Health (0-100)") +
scale_x_continuous(breaks = NULL) +
scale_fill_discrete(breaks=c("Pre-Op", "Discharge", "One Year")) +
labs(fill = "Stages of Care")


                                        # Plotting SLG EuroQol Data - END
####################################################################################





####################################################################################
                                        # Plotting SLF EuroQOL Data - START


                                        # SLF EuroQol Score Data
StageOfCare.EQ <- data.frame(Stage.Of.Care = c(rep("One Year", nrow(SLF_euroQol_Data)), rep("Two Year", nrow(SLF_euroQol_Data))))

EuroQol.Score  <- rbind(data.frame(Score = SLF_euroQol_Data$oneyear.EQ.score), data.frame(Score = SLF_euroQol_Data$twoyear.EQ.score))

SLF_euroQol_Plot_Data <- cbind(StageOfCare.EQ, EuroQol.Score)

mean_euroQol <- SLF_euroQol_Plot_Data %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Score, na.rm = TRUE))


dat_text_euroQol <- data.frame(
  label = c("Average = 93", "Average = 92.8"),
  Stage.Of.Care = c("One Year", "Two Year"),
  x = c(0,0),
  y = c(90, 89.8)
)


# create ggplot scatter plot
# add horizontal line overlay at mean using geom_hline()
# divide plot in facet using function facet_grid()
ggplot(data = SLF_euroQol_Plot_Data, aes(x = c(0), y=Score)) +
scale_y_continuous(breaks = pretty_breaks()) +
geom_point(aes(colour = Stage.Of.Care), position = position_jitter(w = 0.1, h = 0)) + 
geom_hline(data= mean_euroQol, aes(yintercept = mean_val,col=Stage.Of.Care))+
geom_text(
  data    = dat_text_euroQol,
  mapping = aes(x = x, y = y, label = label),
  family = "serif",
  size = 3
) +
facet_grid(~factor(Stage.Of.Care, levels = c("One Year", "Two Year"))) +
theme(panel.background = element_rect(fill = "aliceblue", colour = "grey"),
      strip.background  = element_rect(fill = "white", colour = "grey"),
      text = element_text(family = "serif", size = 11),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "EuroQol Score (0-100)") +
scale_x_continuous(breaks = NULL) +
ylim(0,100) +
scale_fill_discrete(breaks=c("One Year", "Two Year")) +
labs(fill = "Stages of Care")





                                        # SLF EuroQol Health Ranking Data

EuroQol.Final  <- rbind(data.frame(Score = SLF_euroQol_Data$preopEQ.final),
        rbind( data.frame(Score = SLF_euroQol_Data$dischargeEQ.final),
        data.frame(Score = SLF_euroQol_Data$oneyearEQ.final)))

SLF_euroQol2_Plot_Data <- cbind(StageOfCare.EQ, EuroQol.Final)

mean_euroQol2 <- SLF_euroQol2_Plot_Data %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Score, na.rm = TRUE))


dat_text_euroQol2 <- data.frame(
  label = c("Average = 40.5", "Average = 94.8", "Average = 97.5"),
  Stage.Of.Care = c("Pre-Op", "Discharge", "One Year"),
  x = c(0,0,0),
  y = c(39, 93, 96)
)


# create ggplot scatter plot
# add horizontal line overlay at mean using geom_hline()
# divide plot in facet using function facet_grid()
ggplot(data = SLF_euroQol2_Plot_Data, aes(x = c(0), y=Score)) +
scale_y_continuous(breaks = pretty_breaks()) +
geom_point(aes(colour = Stage.Of.Care), position = position_jitter(w = 0.1, h = 0)) + 
geom_hline(data= mean_euroQol2, aes(yintercept = mean_val,col=Stage.Of.Care))+
geom_text(
  data    = dat_text_euroQol2,
  mapping = aes(x = x, y = y, label = label),
  family = "serif",
  size = 3
) +
facet_grid(~factor(Stage.Of.Care, levels = c("Pre-Op", "Discharge", "One Year"))) +
theme(panel.background = element_rect(fill = "aliceblue", colour = "grey"),
      strip.background  = element_rect(fill = "white", colour = "grey"),
      text = element_text(family = "serif", size = 11),
      legend.position = "none",
      legend.title = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "Patient Reported Health (0-100)") +
scale_x_continuous(breaks = NULL) +
scale_fill_discrete(breaks=c("Pre-Op", "Discharge", "One Year")) +
labs(fill = "Stages of Care")


                                        # Plotting SLF EuroQol Data - END
####################################################################################




####################################################################################
                                        # Plotting SLG Functional Score Data - START

functionalData <- read.csv("functionalData.csv")
functionalData <- functionalData[,which(grepl("AVERAGE", colnames(functionalData)))]
names(functionalData) <- c("Functional.Preop.Score", "Functional.Discharge.Score", "Functional.OneYear.Score")



                                        # SLG Functional Score Stage of Care Data
StageOfCare.FUN <- data.frame(Stage.Of.Care = c(rep("Pre-Op", nrow(functionalData)), rep("Discharge", nrow(functionalData)),  rep("One Year", nrow(functionalData))))

Functional.Score  <- rbind(data.frame(Score = functionalData$Functional.Preop.Score),
        rbind( data.frame(Score = functionalData$Functional.Discharge.Score),
        data.frame(Score = functionalData$Functional.OneYear.Score)))

functionalScore_Plot <- cbind(StageOfCare.FUN, Functional.Score)

mean_FUN <- functionalScore_Plot %>% group_by(Stage.Of.Care) %>% summarise(mean_val = mean(Score, na.rm = TRUE))


dat_text_FUN <- data.frame(
  label = c("Average = 3.41", "Average = 8.20", "Average = 9.05"),
  Stage.Of.Care = c("Pre-Op", "Discharge", "One Year"),
  x = c(0,0,0),
  y = c(3.2, 7.8, 8.85)
)


# create ggplot scatter plot
# add horizontal line overlay at mean using geom_hline()
# divide plot in facet using function facet_grid()
ggplot(data = functionalScore_Plot, aes(x = c(0), y=Score)) +
scale_y_continuous(breaks = pretty_breaks()) +
geom_point(aes(colour = Stage.Of.Care), position = position_jitter(w = 0.1, h = 0)) + 
geom_hline(data= mean_FUN, aes(yintercept = mean_val,col=Stage.Of.Care))+
geom_text(
  data    = dat_text_FUN,
  mapping = aes(x = x, y = y, label = label),
  family = "serif",
  size = 3
) +
facet_grid(~factor(Stage.Of.Care, levels = c("Pre-Op", "Discharge", "One Year"))) +
theme(panel.background = element_rect(fill = "aliceblue", colour = "grey"),
      strip.background  = element_rect(fill = "white", colour = "grey"),
      text = element_text(family = "serif", size = 11),
      legend.position = "none",
      legend.title = element_blank(),
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
labs(y = "Average Functional Score (0-10)") +
scale_x_continuous(breaks = NULL) +
scale_fill_discrete(breaks=c("Pre-Op", "Discharge", "One Year")) +
labs(fill = "Stages of Care")




                                        # Plotting SLG Functional Score Data - END
####################################################################################




