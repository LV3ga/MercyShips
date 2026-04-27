                                        # In this program, we are adding columns for each stage of care
                                        # (rather than using a variable to identify the stage of care)
                                        # This decision was made for the 2026 Orthopedic Analysis.
                                        # For future reviews, we believe that entering data in columns
                                        # instead of scrolling to the correct row will be easier for
                                        # data entry. Thus, we are transforming the current data
                                        # (even though it was collected in a row oriented way) so the
                                        # resulting R script will be closer to future R scripts.
###################################################################################################################

                                        # setting directory and loading libraries)
setwd("c://MercyShips//Coding//Projects//OrthoAnalysis")




                                        # getting original data and storing it in two variables

data <- read.csv("test.csv")
data_switch <- data

colnames(data)


                                        # Creating new columns for each stage of care
                                        # We start at 25, since columns 1-25 are data the doesn't change across
                                        # stages of care. Essentially patient demographics.
for(i in 25:length(colnames(data))){
                                        # for tracking loop progress
    print(i)
                                        # New column names
    original_colname <- colnames(data_switch)[i]
    preop_colname <-  sub(" ", ".", paste("Preop", original_colname))
    afterwedge_colname <-  sub(" ", ".", paste("AfterWedge", original_colname))
    outofcast_colname <-  sub(" ", ".", paste("OutOfCast", original_colname))
    discharge_colname <-  sub(" ", ".", paste("Discharge", original_colname))
    oneyear_colname <-  sub(" ", ".", paste("Oneyear", original_colname))
    twoyear_colname <-  sub(" ", ".", paste("Twoyear", original_colname))
    
                                        # preop column 
    data_switch[,preop_colname] <- data_switch[, original_colname]
                                        # after wedge
    data_switch[,afterwedge_colname] <- data_switch[, original_colname]
                                        # out of cast
    data_switch[,outofcast_colname] <- data_switch[, original_colname]
                                        # discharge
    data_switch[,discharge_colname] <- data_switch[, original_colname]
                                        # one year
    data_switch[,oneyear_colname] <- data_switch[, original_colname]
                                        # two year
    data_switch[,twoyear_colname] <- data_switch[, original_colname]


}

colnames(data_switch)
                                        # Removing columns without stage of care labels
data_switch <- data_switch[,c(c(1:25), c(219:ncol(data_switch)))]
colnames(data_switch)


                                        # Ensure stage of care between columns and rows matches
preop_data  <- data_switch[data_switch$Stage.of.Care == "Assess_Preop", ]
preop_data  <-  preop_data[, grepl("Preop", colnames(data_switch))]

afterwedge_data  <- data_switch[data_switch$Stage.of.Care == "XAssess_After_Wedge", ]
afterwedge_data  <- afterwedge_data[, grepl("AfterWedge", colnames(data_switch))]

outcast_data  <- data_switch[data_switch$Stage.of.Care == "XAssess_Immediately_Out_of_Cast", ]
outcast_data  <- outcast_data[, grepl("OutOfCast", colnames(data_switch))]

discharge_data  <- data_switch[data_switch$Stage.of.Care == "Assess_Initial_Discharge", ]
discharge_data  <- discharge_data[, grepl("Discharge", colnames(data_switch))]

oneyear_data  <- data_switch[data_switch$Stage.of.Care == "Assess_12_month_postop", ]
oneyear_data  <- oneyear_data[, grepl("Oneyear", colnames(data_switch))]

twoyear_data  <- data_switch[data_switch$Stage.of.Care == "Assess_24_month_postop", ]
twoyear_data  <- twoyear_data[, grepl("Twoyear", colnames(data_switch))]




                                        # SLG has not had their two year appointments (as of 2026)
                                        # Thus, twoyear_data will have less rows than the other data
                                        # To make sure we have a full dataframe, we will add empty rows
                                        # to twoyear data that correspond to SLG patients
twoyear_data[nrow(twoyear_data) + nrow(general_data[grepl("SLG", general_data$PatientID),]),] <-NA

                                        # Get data that is same for all stages of care
                                        # Also only selecting one stage of care to match row count of above data
                                        # Also removing Stage.of.Care column as it is now meaningless
general_data <- data_switch[,c(1:25)]
general_data <- general_data[general_data$Stage.of.Care == "Assess_Preop",]
general_data$Stage.of.Care <- NULL



                                        # Recombining all data to make column oriented structure
                                        # Also re-indexing rows
final_data_switch <- cbind(general_data, preop_data, afterwedge_data, outcast_data, discharge_data, oneyear_data,
                           twoyear_data)
rownames(final_data_switch) <- 1:nrow(final_data_switch)



                                        # We need to only select legs that were operated on
                                        # However, if you run the code below, you can see some
                                        # legs are missing this data. We need to fill this data in.
final_data_switch[,c(2, 14, 20)]
final_data_switch$Operated.on. <- ifelse(final_data_switch$Location == "bilateral", "Y", ifelse(final_data_switch$Location == final_data_switch$Right.or.Left.Leg., "Y", "N"))



                                        # Only selecting legs that were operated on.
                                        # Also re-labeling row indicies
final_data_switch <- final_data_switch[final_data_switch$Operated.on. == "Y",]
rownames(final_data_switch) <- 1:nrow(final_data_switch)



                                        # Exporting reorganized data
write.csv(final_data_switch, "OR2026DataTransformed.csv", row.names=FALSE)



