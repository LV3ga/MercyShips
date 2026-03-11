# ORDER OF TASKS
# 1. Remove genuine duplicates from Participants
# 2. Make sure every participant in Attendances appears in Participants
# 3. Remove genuine duplicates from Attendances









#####################################################
# TASK 1: Remove genuine duplicates
#####################################################

# SET CURRENT WORKING DIRECTORY
setwd("C:\\MercyShips\\Coding\\Projects\\v4Cleaning")


# IMPORTING PARTICIPANT DATA (MCB PARTICIPANT INFO)
participants <- read.csv('participants.csv')


# CHANGE FORMAT TYPE (EASIER FOR CLEANING)
participants$Name <- iconv(participants$Name, from = 'ISO-8859-1', to = 'utf8') 


# MAKING NAMES ALL UPPER-CASE
participants$Name <- toupper(participants$Name) 


# REMOVING LEADING AND TRAILING WHITESPACE
participants$Name <- trimws(participants$Name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]") 
  

# SORTING DATA BY NAME COLUMN (SHOULD MAKE THE BELOW LOOP WORK BECAUSE WE'RE LOOKING FOR SAME/SIMILAR NAME AND SAME EMAIL/PHONE)
participants <- participants[order(participants$Name),]


# REPLACING NA WITH EMPTY STRINGS (HELPS WITH CLEANING)
participants <- replace(participants, is.na(participants), "")


# MAKING A NEW COLUMN CALLED DUPLICATE WHICH WILL BE USED TO IDENTIFY DUPLICATES (SAME EMAIL OR PHONE NUMBER)
participants$duplicate <- c(0)


# IDENTIFY ROWS WITH THE SAME EMAIL OR PHONE NUMBER (DUPLICATE)
i = 1
j = 2
while(j <= nrow(participants)){
 print(j)
 if(participants$Participant.Email[i] == participants$Participant.Email[j] || participants$Participant.Contact.Mobile.Phone[i] == participants$Participant.Contact.Mobile.Phone[j])
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


# SORTING BY EMAIL COLUMN THIS TIME!
participants_email_sort <- participants[order(participants$Participant.Email), ]


# IDENTIFY ROWS WITH SAME EMAIL (BY ID)
i = 1
j = 2
duplicate_indicies_email <- list()
while(j <= nrow(participants)){
  print(j)
  if(participants_email_sort$Participant.Email[i] == participants_email_sort$Participant.Email[j])
  {
    duplicate_indicies_email <- rbind(duplicate_indicies_email, c(participants_email_sort$Participant_ID_Number[i], participants_email_sort$Participant_ID_Number[j]))
    j = j + 1
  }
  else{
    i = j
    j = i + 1
  }
}


# SORTING BY PHONE COLUMN THIS TIME!
participants_phone_sort <- participants[order(participants$Participant.Contact.Mobile.Phone), ]


# IDENTIFY ROWS WITH SAME PHONE NUMBER (BY ID)
i = 1
j = 2
duplicate_indicies_phone <- list()
while(j <= nrow(participants)){
  print(j)
  if(participants_phone_sort$Participant.Email[i] == participants_phone_sort$Participant.Email[j])
  {
    duplicate_indicies_email <- rbind(duplicate_indicies_email, c(participants$Participant_ID_Number[i], participants$Participant_ID_Number[j]))
    j = j + 1
  }
  else{
    i = j
    j = i + 1
  }
}



# MARKING INCIDICES WITH THE SAME PHONE NUMBER OR SAME EMAIL
true_duplicate_indicies <- list()
for(i in 1:nrow(duplicate_indicies)){
  print(i)
  
  # participant emails
  email1 <- participants$Participant.Email[duplicate_indicies[[i,1]]]
  email2 <- participants$Participant.Email[duplicate_indicies[[i,2]]]
  
  # participant phone numbers
  number1 <- participants$Participant.Contact.Mobile.Phone[duplicate_indicies[[i,1]]]
  number2 <- participants$Participant.Contact.Mobile.Phone[duplicate_indicies[[i,2]]]
  
  # ensures both emails are not empty strings
  if(!(email1 == "" && email2 == "")){
    
    # checks is email addresses are the same. If so, we consider the rows duplicates
    if(email1 == email2){
      true_duplicate_indicies <- rbind(true_duplicate_indicies, c(duplicate_indicies[[i,1]],duplicate_indicies[[i,2]]))
    }
  }
  
  # ensures both phone numbers are not empty strings
  if(!(number1 == "" && number2 == "")){
    
    # checks is phone numbers are the same. If so, we consider the rows duplicates
    if(number1 == number2){
      true_duplicate_indicies <- rbind(true_duplicate_indicies, c(duplicate_indicies[[i,1]],duplicate_indicies[[i,2]]))
    }
  }
}


# REMOVE ENTIRES THAT ARE THE SAME (POSSIBLE WITH THE WAY ABOVE LOOP WAS WRITTEN)
true_duplicate_indicies <- unique(true_duplicate_indicies)
nrow(true_duplicate_indicies)


# GET ID NUMBERS OF TRUE DUPLICATES (IMPORTANT FOR CLEANING ATTENDANCES)
# MOST RECENT IS IN THE FIRST SPOT
true_duplicate_IDs <- data.frame("ID1" = c(rep(0, nrow(true_duplicate_indicies))), "ID2" = c(rep(0, nrow(true_duplicate_indicies)))) 
for(i in 1:nrow(true_duplicate_indicies)){
  
  # participant IDs
  ID1 <- participants$Participant_ID_Number[duplicate_indicies[[i,1]]]
  ID2 <- participants$Participant_ID_Number[duplicate_indicies[[i,2]]]
  
  
  
  if(ID1 < ID2){
    true_duplicate_IDs[i,1] = ID2
    true_duplicate_IDs[i,2] = ID1
  }
  
  else{
    true_duplicate_IDs[i,2] = ID1
    true_duplicate_IDs[i,1] = ID2  }
  
}


true_duplicate_IDs

# "GROUPS" DUPLICATE IDS (SEPEARATES THEM WITH THE WORD "NEXT")
true_duplicate_IDs <- true_duplicate_IDs[order(true_duplicate_IDs$ID1),]

i = 1
true_duplicate_IDs_groups <- list()
while (i <= nrow(true_duplicate_IDs))
{
  print(i)
  ID <- true_duplicate_IDs[i,2]
  templist <- list()
  templist <- rbind(templist, ID)
  
  while(  ID == true_duplicate_IDs[i,2] )
  {
    templist <- rbind(templist, true_duplicate_IDs[i,1])
    i = i + 1
  }
  
  true_duplicate_IDs_groups <- rbind(true_duplicate_IDs_groups, templist)
  true_duplicate_IDs_groups <- rbind(true_duplicate_IDs_groups, "NEXT")
}

true_duplicate_IDs
true_duplicate_IDs_groups

# LOOP COMBINES FIELDS FROM DUPLICATE PARTICIPANTS AND KEEPS THE MOST RECENT IN A CONFLICT
# THIS COULD GET COMPLICATED. FOR NOW, LET'S JUST SIMPLIFY IT AND TAKE THE MOST RECENT ID 
# AS THE NON DUPLICATE, AND MARK ALL OTHERS AS DUPLICATES. WE'LL MOVE FORWARD WITH MIGRATION 
# TESTING. WHEN WE ACTUALLY DO THE MIGRATION, I'LL MEET WITH DENNY TO DISCUSS HOW WE WANT TO 
# MERGE DATA WITH DUPLICATE PARTICIPANTS.

i = 1
k = 1
while(i <= nrow(true_duplicate_IDs_groups))
{

  # making list of duplicates of a given ID
  tempList <- list()
  temp <- true_duplicate_IDs_groups[i,1]
  while( temp != "NEXT")
  {  
    tempList <- rbind(tempList, temp)
    i = i + 1
    temp <- true_duplicate_IDs_groups[i,1]
  }
  
  i = i + 1
  
  # sorting the list of duplicates of a given ID
  tempList <- sort(unlist(tempList))
  most_recent_ID <- max(unlist(tempList))
  # mapping duplicate IDs to most recent (highest) ID in the Attendance records
  
  for(j in 1:nrow(participants))
  {
    if(participants$Participant_ID_Number[j] %in% tempList && participants$Participant_ID_Number[j] != most_recent_ID)
    {
      print(k)
      participants$Name[j] <- "DUPLICATE"
      k = k + 1
    }
  }
  
}


# REMOVES ROWS WHERE NAME IS "DUPLICATE" OR "UNKNOWN UNKNOWN"
participants_no_duplicates <- participants[participants$Name != "DUPLICATE", ]
participants_no_duplicates <- participants_no_duplicates[participants_no_duplicates$Name != "UNKNOWN UNKNOWN", ]
nrow(participants_no_duplicates)


participants_no_duplicates = participants_no_duplicates[order(participants_no_duplicates$Name),]
participants_no_duplicates$Name


# SEEING HOW MANY PARTICIPANTS WITH THE SAME NAME THERE ARE AFTER REMOVING DUPLICATES
i = 1
j = 2
duplicate_indicies_new <- list()
while(j <= nrow(participants_no_duplicates)){
  print(j)
  if(participants_no_duplicates$Name[i] == participants_no_duplicates$Name[j])
  {
    duplicate_indicies_new <- rbind(duplicate_indicies_new, c(i, j))
    j = j + 1
  }
  else{
    i = j
    j = i + 1
  }
}



# LOOKS LIKE THERES 422 PAIRS OF PEOPLE WITH SAME NAME BUT WITH DIFFERENT EMAILS OR PHONE NUMBERS
duplicate_indicies_new


# GET IDS OF THESE PAIRS OF PEOPLE
ID_same_name <- list()
for(i in 1:nrow(duplicate_indicies_new)){
  print(i)
  ID1 <- participants_no_duplicates$Participant_ID_Number[duplicate_indicies_new[[i,1]]]
  ID2 <- participants_no_duplicates$Participant_ID_Number[duplicate_indicies_new[[i,2]]]
  
  if(!(ID1 %in% ID_same_name)){
    ID_same_name <- rbind(ID_same_name, ID1)
  }
  
  if(!(ID2 %in% ID_same_name)){
    ID_same_name <- rbind(ID_same_name, ID2)
  }
}


# MAKE DATAFRAME WITHOUT THESE PAIRS OF PEOPLE
participants_diff_names <- participants_no_duplicates[!(participants_no_duplicates$Participant_ID_Number %in% ID_same_name), ]


# MAKE DATAFRAME WITH ONLY THESE PAIRS OF PEOPLE
participants_same_names <- participants_no_duplicates[participants_no_duplicates$Participant_ID_Number %in% ID_same_name, ]


############################################################################
# TASK 2: Make sure every participant in Attendances appears in Participants
############################################################################
library(readxl)


# IMPORTING ATTENDANCE RECORDS (ACTUAL MCB OUTPUT)
attendances <- read_excel('v4Workbook.xlsx', sheet = "Actual MCB Output")


# MAPPING DUPLICATE PARTICIPANT IDS TO THE SAME ID IN THE ATTENDANCE TABLE
i = 1
while(i <= nrow(true_duplicate_IDs_groups))
{
  print(i)
  
  # making list of duplicates of a given ID
  tempList <- list()
  temp <- true_duplicate_IDs_groups[i,1]
  while( temp != "NEXT")
  {  
    tempList <- rbind(tempList, true_duplicate_IDs_groups[i,1])
    i = i + 1
    temp <- true_duplicate_IDs_groups[i,1]
  }
  
  i = i + 1
  
  # sorting the list of duplicates of a given ID
  tempList <- sort(unlist(tempList))
  most_recent_ID <- max(unlist(tempList))
  # mapping duplicate IDs to most recent (highest) ID in the Attendance records
  
  for(j in 1:nrow(attendances))
  {
    if(attendances$Project_ID_Number[j] %in% tempList)
    {
      attendances$Project_ID_Number[j] <- most_recent_ID
    }
  }
  
}


# PARTITIONING ATTENDANCE RECORDS INTO ONE WITH UNIQUE NAMES AND ANOTHER WITH REPEATS (SEE ABOVE TASK 2 BAR)
attendances_diff_names <- attendances[!(attendances$Participant_ID_Number %in% ID_same_name), ]
attendances_same_names <- attendances[attendances$Participant_ID_Number %in% ID_same_name, ]
  


# GETTING UNIQUE PARTICIPANT ID NUMBERS IN ATTENDANCE RECORDS
attendance_IDs <- as.data.frame(unique(attendances$Participant_ID_Number))
attendance_IDs


# CHECKING IF ALL ATTENDANCE RECORDS APPEAR IN OUR NO DUPLICATES DATA FRAME
IDs_no_duplicates <- participants_no_duplicates$Participant_ID_Number
IDs_attendance_only <- list()
for(i in 1:nrow(attendance_IDs)){
  if(!(attendance_IDs[i,1] %in% IDs_no_duplicates)){
    IDs_attendance_only <- rbind(IDs_attendance_only, attendance_IDs[i,1])
  }
}

nrow(IDs_attendance_only)

#what are these attendance IDs? Are they in our participant table?
IDs_attendance_only



# DO THESE IDS APPEAR IN OUR DUPLICATE LIST?
IDS_duplicate <- participants[participants$Name == "DUPLICATE", ]
IDS_duplicate <- IDS_duplicate$Participant_ID_Number

IDS_weird <- list()
for(i in 1:nrow(IDs_attendance_only)){
  if(!(IDs_attendance_only[i,1] %in% IDS_duplicate)){
    IDS_weird <- rbind(IDS_weird, IDs_attendance_only[i,1])
  }
}


# SOME DO NOT, WHAT ARE THESE?
IDS_weird


# THEY ARE ALL UNKNOWN!!! GOOD.
attendances_weird <- attendances[attendances$Participant_ID_Number %in% IDS_weird,]
