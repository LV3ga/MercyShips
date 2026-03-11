
                                        # setting directory and loading libraries)

setwd("c://MercyShips//Coding//Projects//OrthoAnalysis")




                                        # getting original data and storing it in two variables

data <- read.csv("Cleaned_Assess.csv")
data_switch <- data




                                        # Creating new columns for each stage of care
for(i in 19:length(colnames(data))){
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

                                        # Removing columns without stage of care labels
data_switch <- data_switch[,c(c(1:22),c(50:204))]



                                        #Sorting by SLF/SLG Number and Stage of
data_switch <- data_switch[order(data_switch$SLF..,data_switch$Right.or.Left.Leg.,  data_switch$Stage.of.Care), ]


                                        #This is where the new data will be stored. We don't need extra rows for stages of care. That's what the extra columns are for.
test <- data_switch[data_switch$Stage.of.Care == "Assess_Preop", ]


                                        #Fills stage of care columns with correct data
colnames(test)

preop_data_switch_r <- data_switch[data_switch$Stage.of.Care == "Assess_Preop" & data_switch$Right.or.Left.Leg. == "right", ]
