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

data <- read.csv("OR2026DataUntransformed.csv")
data_switch <- data

colnames(data)


                                        # Creating new columns for each stage of care
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

}

colnames(data_switch)
                                        # Removing columns without stage of care labels
data_switch <- data_switch[,c(c(1:25),c(65:324))]



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


                                        # Get data that is same for all stages of care
                                        # Also only selecting one stage of care to match row count of above data
                                        # Also removing Stage.of.Care column as it is now meaningless
general_data <- data_switch[,c(1:25)]
general_data <- general_data[general_data$Stage.of.Care == "Assess_Preop",]
general_data$Stage.of.Care <- NULL



                                        # Recombining all data to make column oriented structure
                                        # Also only selecting legs that were operated on.
final_data_switch <- cbind(general_data, preop_data, afterwedge_data, outcast_data, discharge_data, oneyear_data)
final_data_switch <- final_data_switch[final_data_switch$Operated.on. == "Y", ]


                                        # Exporting reorganized data
write.csv(final_data_switch, "OR2026DataTransformed.csv", row.names=FALSE)

colnames(final_data_switch)
