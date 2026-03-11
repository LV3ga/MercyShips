# ORDER OF TASKS
# 1. Handle genuine duplicates from Participants and create mapping for Attendance
# 2. Addressing duplicates/issues from Projects and create mapping
# 3. Change Participant profession column to be the same as powerapps dropdown
# 4. Replace Attendance IDs with Powerapps IDs, using the mapping for duplicates


library(tidyverse)
library(readxl)
library(DescTools)

#######################################################################################
# TASK 1: Handle genuine duplicates from Participants and create mapping for Attendance
#######################################################################################


# SET CURRENT WORKING DIRECTORY
setwd("C:\\MercyShips\\Coding\\Projects\\v4Cleaning")


# IMPORTING PARTICIPANT DATA (MCB PARTICIPANT INFO)
participants <- read_excel('v4Workbook.xlsx', sheet = "MCB Participant Info")


# REPLACING NA WITH EMPTY STRINGS (HELPS WITH CLEANING)
participants <- replace(participants, is.na(participants), "")


# REPLACING EMPTY STRINGS IN FIRST/LAST NAME WITH "MISSING"
participants$`Participant First Name` <- replace(participants$`Participant First Name`, participants$`Participant First Name` == "", "MISSING")
participants$`Participant Last Name` <- replace(participants$`Participant Last Name`, participants$`Participant Last Name` == "", "MISSING")


# MAKING NEW NAME COLUMN
participants$Name <- paste(participants$`Participant First Name`, participants$`Participant Last Name`)


# CLEANING NAMES
participants$Name <- participants$Name %>% iconv(from = 'ISO-8859-1', to = 'utf8') %>% toupper() %>%  
  trimws(which = c("both", "left", "right"), whitespace = "[ \t\r\n]") 



# SORTING DATA BY NAME COLUMN (SHOULD MAKE THE BELOW LOOP WORK BECAUSE WE'RE LOOKING FOR SAME/SIMILAR NAME AND SAME EMAIL/PHONE)
participants <- participants[order(participants$Name, participants$`Participant Email`, participants$`Participant Contact Mobile Phone`),]


# REPLACING NON-EMAILS IN THE EMAIL COLUMN WITH "MISSING", "MISSING1", "MISSING2", TO ENSURE THAT EVERY NAME + EMAIL
# COMBINATION IS UNIQUE
counter <- 1
for(i in 2:nrow(participants)){ 
  
  print(i)
  prev_name <- participants$Name[i-1]
  
  # checks if email at row i is valid
  email_vector <- strsplit(participants$`Participant Email`[i], "")
  validEmail <- length(intersect(c("@"), email_vector[[1]])) > 0
  
  if(participants$`Participant Email`[i] == "" || !validEmail){
    
    if(!(participants$Name[i] == prev_name)){
      participants$`Participant Email`[i] <- "missing"
      counter <- 1
    }
    
    else{
      participants$`Participant Email`[i] <- paste("missing", counter, sep = "")
      counter <- counter + 1
    }
  }
}


# FIRST ENTRY HAS A MISSING EMAIL
participants$`Participant Email`[1] <- "missing"


# MAKING A NEW COLUMN CALLED DUPLICATE WHICH WILL BE USED TO IDENTIFY DUPLICATES (SAME EMAIL OR PHONE NUMBER)
participants$duplicate <- c(0)


# IDENTIFY ROWS WITH THE SAME EMAIL OR PHONE NUMBER (EXCLUDING INVALID EMAIL ADDRESSES/PHONE NUMBERS)
i = 1
j = 2
ints <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9") # USED TO CHECK IF PHONE NUMBER IS VALID
while(j <= nrow(participants)){
  print(j)
  
  # checks if email at row i is valid
  email_vector <- strsplit(participants$`Participant Email`[i], "")
  validEmail <- length(intersect(c("@"), email_vector[[1]])) > 0

  # checks if phone number at row i is valid
  phone_vector <- strsplit(participants$`Participant Contact Mobile Phone`[i], "")
  validPhone <- length(intersect(ints, phone_vector[[1]])) > 0
  
  
  if(participants$`Participant Email`[i] == participants$`Participant Email`[j] && validEmail)
  {
    participants$duplicate[i] <- 1
    participants$duplicate[j] <- 1
    j = j + 1
  }
  
  else if(participants$`Participant Contact Mobile Phone`[i] == participants$`Participant Contact Mobile Phone`[j] && validPhone)
  {
    participants$duplicate[i] <- 1
    participants$duplicate[j] <- 1
    j = j + 1
  }
  else{
    i = j
    j = i + 1
  }
}


# GETTING TABLE OF DUPLICATES AND TABLE OF NON-DUPLICATES
part_duplicates <- participants[participants$duplicate == 1,]
part_nonduplicates <- participants[participants$duplicate == 0,]



# MAKING ID DICTIONARY FOR MAPPING. 
#
# WHY ARE WE DOING THIS? >>
# WE EVENTUALLY WILL NEED TO FILL THE ATTENDANCE TABLE WITH NEW IDS THAT MATCH THE 
# PARTICIPANT IDS IN POWERAPPS SO THE LOOKUP CAN WORK. TO FIGURE OUT WHAT IDS GO WITH WHAT ATTENDANCE RECORDS, WE NEED
# TO TAKE INTO ACCOUNT PARTICIPANTS WITH MULTIPLE ID NUMBERS (DUE TO DUPLICATES). FOR INSTANCE, IF JOHN RED (JRED@GMAIL.COM)
# IS THE PARTICIPANT ID IN POWERAPPS, AND IN THE EXCEL SHEET, JOHN RED HAS IDS 2232, 2334, 9924 ALL OF THESE NEED TO BE REPLACED WITH
# JOHN RED (JRED@GMAIL.COM). HERE, FOR EACH PARTICIPANT, WE WILL GET ALL THE IDS THAT ARE ASSOCIATED WITH THEM (ID DICTIONARY)

part_duplicates <- part_duplicates[order(part_duplicates$Name),]
IDNumberKeys <- list()
IDValues <- list()
i = 1
j = i + 1


while(i <= nrow(part_duplicates))
{
  print(j)
  
  # creating participant ID that will be created in powerapps
  partEmail <- part_duplicates$`Participant Email`[i]
  partNumber <- part_duplicates$`Participant Contact Mobile Phone`[i]
  partFirstName <- part_duplicates$`Participant First Name`[i]
  partLastName <- part_duplicates$`Participant Last Name`[i]
  partID <- paste(partFirstName, " ", partLastName, " ", "(", partEmail, ")", sep = "")
  
  
  # maps the ID number to a powerapps ID
  IDNumberKeys <- c(IDNumberKeys, part_duplicates$Participant_ID_Number[i])
  IDValues <- c(IDValues, partID)
  
  while(partEmail == part_duplicates$`Participant Email`[j] || partNumber == part_duplicates$`Participant Contact Mobile Phone`[j])
  {
    IDNumberKeys <- c(IDNumberKeys, part_duplicates$Participant_ID_Number[j])
    IDValues <- c(IDValues, partID)
    j = j + 1
  }
  i = j
  j = i + 1
}


# CREATING DICTIONARY
IDLookup <- IDValues
names(IDLookup) <- IDNumberKeys
IDValues



# REMOVING DUPLICATES IN PART_DUPLICATE BASED ON WHICH DUPLICATE HAS THE LEAST AMOUNT OF INFORMATION
# IDENTIFY ROWS WITH THE SAME EMAIL OR PHONE NUMBER (EXCLUDING INVALID EMAIL ADDRESSES/PHONE NUMBERS)
i = 1
j = 2
ints <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9") # USED TO CHECK IF PHONE NUMBER IS VALID
while(j <= nrow(part_duplicates)){
  print(j)
  
  
  if(part_duplicates$`Participant Email`[i] == part_duplicates$`Participant Email`[j] || part_duplicates$`Participant Contact Mobile Phone`[i] == part_duplicates$`Participant Contact Mobile Phone`[j])
  {
    iscore = 0
    jscore = 0
    for(k in 1:ncol(part_duplicates)){
      ifelse(part_duplicates[i,k] == "", iscore <- iscore + 1, iscore)
      ifelse(part_duplicates[j,k] == "", jscore <- jscore + 1, jscore)
      
    }
    
    if(iscore > jscore)
    {
      part_duplicates$duplicate[j] <- 0
      j = j + 1
    }
    
    else{
      part_duplicates$duplicate[i] <- 0
      i = j
      j = i + 1
    }
    
  }
 
  else{
    i = j
    j = i + 1
  }
}


part_nonduplicates <- rbind(part_nonduplicates, part_duplicates[part_duplicates$duplicate == 1,])
part_true_duplicates <- part_duplicates[part_duplicates$duplicate == 0,]


# CHECKING IF THERE ARE ANY DUPLICATE POWERAPP IDS (FIRST NAME + LAST NAME + (EMAIL))
# IN PART_NONDUPLICATES

IDframe <- data.frame(ID = rep(c(0), nrow(part_nonduplicates)))

for(i in 1:nrow(part_nonduplicates)){ # THE FIRST FOUR ENTRIES ARE EMPTY
  print(i)
  partEmail <- part_nonduplicates$`Participant Email`[i]
  partName <- part_nonduplicates$Name[i]
  partID <- paste(partName, " ", "(", partEmail, ")", sep = "")
  
  IDframe[i,1] <- partID
}



# LOOKS LIKE THERE ARE DUPLICATES STILL, HMMMMM...
IDframeUnique <- unique(IDframe)
nrow(IDframe)
nrow(IDframeUnique)


# WHAT ARE THESE DUPLICATES?
i = 1
j = 1
duplicate_indicies <- list()
while(i <= nrow(IDframe) && j <= nrow(IDframeUnique)){

    if(IDframe[i,1] != IDframeUnique[j,1]){
    print(i)
    print(j)
    duplicate_indicies <- c(duplicate_indicies, i)
    i <- i + 1
  }
  
  else{
    i <- i + 1
    j <- j + 1
  }
}

# THEY ARE ONLY "DID NOT OBTAIN CHRISTIAN MISSION HEALTHCARE WORKERS (Male/Female)
# SO IT'S OKAY
IDframe[unlist(duplicate_indicies),]

# FINALLY, WE'RE GOING TO ADD TO THE LOOKUP BY ADDING NONDUPLICATE ID MAPPINGS
Keys <- list()
Values <- list()
for(i in 1:nrow(part_nonduplicates)){
  print(i)
  if(!(part_nonduplicates$Participant_ID_Number[i] %in% IDNumberKeys)){
    
    # creating participant ID that will be created in powerapps
    partEmail <- part_nonduplicates$`Participant Email`[i]
    partNumber <- part_nonduplicates$`Participant Contact Mobile Phone`[i]
    partFirstName <- part_nonduplicates$`Participant First Name`[i]
    partLastName <- part_nonduplicates$`Participant Last Name`[i]
    partID <- paste(partFirstName, " ", partLastName, " ", "(", partEmail, ")", sep = "")
    
    # Adding to lists that will be used in mapping
    Keys <- c(Keys, part_nonduplicates$Participant_ID_Number[i])
    Values <- c(Values, partID)
  }
  
}

# CREATING FINAL MAPPING
IDLookup2 <- Values
names(IDLookup2) <- Keys
FinalLookup <- c(IDLookup, IDLookup2)


# FINAL PARTICIPANT LIST
part_nonduplicates_final <- part_nonduplicates[-unlist(duplicate_indicies),]
part_nonduplicates_final <- part_nonduplicates



# FIXING SPELLING MISTAKES WITH COUNTRIES
part_nonduplicates_final$`Participant Country` <- replace(part_nonduplicates_final$`Participant Country`, part_nonduplicates_final$`Participant Country` == "Morroco", "Morocco")
part_nonduplicates_final$`Participant Country` <- replace(part_nonduplicates_final$`Participant Country`, part_nonduplicates_final$`Participant Country` == "SA", "South Africa")
part_nonduplicates_final$`Participant Country` <- replace(part_nonduplicates_final$`Participant Country`, part_nonduplicates_final$`Participant Country` == "Libieria", "Liberia")
part_nonduplicates_final$`Participant Country` <- replace(part_nonduplicates_final$`Participant Country`, part_nonduplicates_final$`Participant Country` == "Libera", "Liberia")
part_nonduplicates_final$`Participant Country` <- replace(part_nonduplicates_final$`Participant Country`, part_nonduplicates_final$`Participant Country` == "Congo", "Republic of Congo")
part_nonduplicates_final$`Participant Country` <- replace(part_nonduplicates_final$`Participant Country`, part_nonduplicates_final$`Participant Country` == "UK", "United Kingdom")


# EXPORTING PARTICIPANTS TO CSV
write.csv(part_nonduplicates_final, "Participants.csv")


####################################################################################
# TASK 2: Addressing duplicates/issues from Projects and create mapping
####################################################################################
# NOW WE WILL REPLACE THE PROJECT IDS IN ATTENDANCE WITH POWERAPP IDS
projects <- read_excel('v4Workbook.xlsx', sheet = "Project Info")


# NEW COLUMN TO STORE POWERAPPS IDS
projects$powerappID <- rep(c(0), nrow(projects))


# CREATING MAPPING BETWEEN EXCEL PROJECT ID NUMBERS AND POWERAPP PROJECT IDs
Project_ID_numbers <- list()
Project_powerapp_IDs <- list()
for(i in 1:nrow(projects)){
  print(i)
  
  # VARIABLES USED IN POWERAPPS ID
  project_name <- projects$`Project Title`[i]
  project_country <- projects$Country[i]
  project_start_date <- format(projects$`Project Start Date`[i], "%d-%b-%Y")
  project_end_date <- format(projects$`Project End Date`[i], "%d-%b-%Y")
  powerapps_id <- paste(project_name, " | ", project_country, " | ", project_start_date, " - ", project_end_date, sep="")
  
  
  # FILLING LISTS
  projects$powerappID[i] <- powerapps_id
  Project_ID_numbers <- c(Project_ID_numbers, projects$Project_ID_Number[i])
  Project_powerapp_IDs <- c(Project_powerapp_IDs, powerapps_id)
  
}


# REPLACE NAs WITH EMPTY STRINGS (HELPS WITH HANDLING DUPLICATES)
projects$`Project Participant Type` <- replace(projects$`Project Participant Type`, is.na(projects$`Project Participant Type`), "")
projects$`Project Venue` <- replace(projects$`Project Venue`, is.na(projects$`Project Venue`), "")
projects <- projects[order(projects$powerappID),]

write.csv(projects, "PROJ.csv")


# DEALING WITH DUPLICATES
i <- 1
j <- 2
dupIDs <- list()

while(j <= nrow(projects)){
  print(j)
  
  dupIDs <- c(dupIDs, projects$powerappID[i])
  counter <- 1
  
  while (projects$powerappID[j] %in% dupIDs){
    part <- projects$`Project Participant Type`[j]
    venue <- projects$`Project Venue`[j]
    
    # VALUES FOR CREATING POWERAPP ID
    project_country <- projects$Country[j]
    project_start_date <- format(projects$`Project Start Date`[j], "%d-%b-%Y")
    project_end_date <- format(projects$`Project End Date`[j], "%d-%b-%Y")
    
    # CREATING POWERAPP ID WITH PARTICIPANT TYPE AND VALUE
    project_name_part <- paste(projects$`Project Title`[j], "-", part, sep = "")
    project_name_venue <- paste(projects$`Project Title`[j], "-", venue, sep = "")
    project_name_counter <- paste(projects$`Project Title`[j], "-", counter, sep = "")
    powerapps_id_part <- paste(project_name_part, " | ", project_country, " | ", project_start_date, " - ", project_end_date, sep="")
    powerapps_id_venue <-  paste(project_name_venue, " | ", project_country, " | ", project_start_date, " - ", project_end_date, sep="")
    powerapps_id_counter <- paste(project_name_counter, " | ", project_country, " | ", project_start_date, " - ", project_end_date, sep="")
    
    
    
    if(part != "" && !(powerapps_id_part %in% dupIDs)){
      
      # CHANGING TITLE
      projects$`Project Title`[j] <- paste(projects$`Project Title`[j], "-", part, sep = "")
    
      # ADDING POWERAPPS ID
      powerapps_id <- powerapps_id_part
      projects$powerappID[j] <- powerapps_id_part
      
    }
    
    else if(venue != "" && !(powerapps_id_venue %in% dupIDs)){
      # CHANGING TITLE
      projects$`Project Title`[j] <- paste(projects$`Project Title`[j], "-", venue, sep = "")
      
      # ADDING POWERAPPS ID
      powerapps_id <- powerapps_id_venue
      projects$powerappID[j] <- powerapps_id_venue
    }
    
    else{
      # CHANGING TITLE
      projects$`Project Title`[j] <- paste(projects$`Project Title`[j], "-", counter, sep = "")
      
      # ADDING POWERAPPS ID
      powerapps_id <- powerapps_id_counter
      projects$powerappID[j] <- powerapps_id_counter
      counter <- counter + 1
    }
    
    
    # ADDING POWERAPP ID TO LIST AND UPDATING VARIABLES
    dupIDs <- c(dupIDs, powerapps_id)
    j <- j + 1
  }
  
  dupIDs <- list()
  i <- j
  j <- i + 1
}


# DONE!
length(projects$powerappID)
length(unique(projects$powerappID))


# LETS UPDATE THE MAPPING
# CREATING MAPPING BETWEEN EXCEL PROJECT ID NUMBERS AND POWERAPP PROJECT IDs
Project_ID_numbers <- list()
Project_powerapp_IDs <- list()
for(i in 1:nrow(projects)){
  print(i)
  
  # CREATING LISTS
  Project_ID_numbers <- c(Project_ID_numbers, projects$Project_ID_Number[i])
  Project_powerapp_IDs <- c(Project_powerapp_IDs, projects$powerappID[i])
  
}


# CREATING PROJECT MAPPING
project_mapping <- Project_powerapp_IDs
names(project_mapping) <- Project_ID_numbers
project_mapping


# WRITING UPDATED PROJECTS TO FILE
write.csv(projects, "Projects.csv")


####################################################################################
# TASK 3: Change Participant profession column to be the same as powerapps dropdown
####################################################################################

# GETTING UNIQUE PROFESSIONS
professions <- unique(part_nonduplicates_final$Participant.Profession.Title..English.)
professions

#############################################################################################
# TASK 3: Replace Attendance IDs with Powerapps IDS and ensuring no NA values in Output Hours
#############################################################################################


# IMPORTING ATTENDANCE RECORDS (ACTUAL MCB OUTPUT)
attendances <- read_excel('v4Workbook.xlsx', sheet = "Actual MCB Output")


# ADDING NEW COLUMN WHICH WILL HOLD THE POWERAPPS ID FOR PARTICIPANTS
attendances$PowerappsID <- rep(c(0), nrow(attendances))


# FILLING THIS COLUMN WITH OUR MAPPING/DICTIONARY
for(i in 1:nrow(attendances)){
  print(i)
  attendances$PowerappsID[i] <- FinalLookup[[paste(attendances$Participant_ID_Number[i])]]
  
}


# NOW WE WILL REPLACE THE PROJECT IDS IN ATTENDANCE WITH POWERAPP IDS
projects <- read.csv('Projects.csv')



# ADDING NEW COLUMN WHICH WILL HOLD POWERAPPS IDS FOR PROJECTS
attendances$projectPowerappsID <- rep(c(0),nrow(attendances))



# FILLING THIS COLUMN WITH OUR MAPPING/DICTIONARY
for(i in 1:nrow(attendances)){
  print(i)
  attendances$projectPowerappsID[i] <- project_mapping[[paste(attendances$Project_ID_Number[i])]]
}


# TAKING NAs IN THE OUTPUT COLUMN AND MAKING THEM 0s
attendances$`Output Amount for Reporting Period` <- replace(attendances$`Output Amount for Reporting Period`, is.na(attendances$`Output Amount for Reporting Period`), 0)


# EXPORTING TO CSV
write.csv(attendances, "Attendance.csv")


# EXPORTING SMALLER ATTENDANCE RECORDS FOR TESTING
attendances_small <- attendances[seq(1, nrow(attendances), 10), ]
write.csv(attendances_small, "Attendace_small.csv")


