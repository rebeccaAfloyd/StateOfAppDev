###############################################################
######### Devex Survey Data Clean Up Script 2024 ##############
###############################################################
############# 3 Jan 2025 #####################################
###############################################################


###### Goals ############
# A - To set a up a series of lists to apply to the data and create 2 columns:
## 1. Exclude_From_Analysis
## 2. Exclude_From_PrizeDraw

# B - To create 3 csv file exports:
## 1. Cleaned up data for importing into Qualtrics
## 2. Cleaned up data to send to Mac for prize draw
## 3. Select names and emails for the first 200 completed, cleaned up responses for the sock prize
####### Assumption - sock prize winners aren't excluded from the prize draw #####################


# Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/Docker State of Application Development Surveys and Analysis/Devex Survey 2024 Analysis")

# Install necessary packages
library("dplyr")
library("stringr")

# Pull in data
RawData<-read.csv("DevEx Final Data Nov 22 2024.csv")


# Look at response time profile and delete 'too fast' responses
# As discussed at Quant Qollab 6 Dec 24 after looking at Qualtrics data, threshold is <600s
#Make duration numeric so we can set a limit and exclude values below - 
#NAs will be introduced but then resolved in next step
RawData$Duration..in.seconds.<-as.numeric(RawData$Duration..in.seconds.)

#Set limit for duration
limit <- 600

# Exclude rows where duration is below the limit
RawData <- RawData %>%
  filter(Duration..in.seconds. >= limit)


# Ensure respondents <18 years are deleted

#Set criteria
#NOTE HERE!!!! At least a few age categories have a space after the last letter
##This means they cannot be called unless the space is added (see below)
exclude_age <- "Under 18 years "

#Renaming to nicer col name
RawData <- RawData %>% rename(Age = X9.2)

#Make age a factor so we can exclude categorically
RawData$Age<-as.factor(RawData$Age)

# Exclude rows in which Under 18 was selected
RawData <- RawData %>%
  filter(Age != exclude_age)

# Look at completes and partials
###  check for partials started and completed by same individual (IP or email)
# ******* As discussed at Quant Qollab 6 Dec 24 - all duplicate email addresses and weed out duplicates 
# ******* of IP addressess with a count of more than 5 - then check <5 manually

##########################################################################
############ This section of code used to explore the duplicate IP addresses and help
############ decide how to proceed. Don't need to run for actual data clean up 
#################################################################################

# #DupIPAdd<-sum(duplicated(IPAdds))
# 
# DupIPAddList<-IPAdds %>%
#   filter(duplicated(IPAddress)) %>%
#   select(Status, IPAddress) %>%
#   group_by(Status) %>%
#   count(Status)
# 
# UniqueIPAddsList<-IPAdds %>%
#   filter(!duplicated(IPAddress)) %>%
#   select(Status, IPAddress) %>%
#   group_by(Status) %>%
#   count(Status)
# 
# 
# # Create a table of duplicate IP addresses
# DupIpCount <- as.data.frame(table(RawData$IPAddress))
# 
# DupIpCount <-DupIpCount$Freq >1


# Check for duplicate IP addresses
# IPAdds<- RawData %>% 
#     filter(!(IPAddress=="")) %>%
#     select(Status, IPAddress) 
# 
# IPAddsTable<-as.data.frame(table(IPAdds))
# 
# IPAddsMoreThan1 <- IPAddsTable %>%
#   filter(Freq>1) %>%
#   select(IPAddress, Freq)
# 
# IPAddsMoreThan1Unfinished <- IPAddsTable %>%
#   filter(Freq>1 ) %>%
#   select(IPAddress, Freq)
# 
# IPAddsByStatus <- RawData %>%
#   group_by(IPAddress) %>%
#   filter(n_distinct(Finished)>1) %>%
#   select(ResponseId, Progress, Finished, IPAddress)

# New rule! Delete where both IP addresses have incomplete status, delete unfinished where 2 duplicates
# exist, delete and >2
# New rule 2! Once IP duplicate removal complete, delete any with 2 or more duplicate emails
# Some are double incompletes - these can be deleted...
# Looks like the double completes are also dodgy, so delete double 'true' completes

######################

# DodgyIPAdd1 <- RawData %>%
#   filter(IPAddress == "120.241.44.45")%>%
#  select(IPAddress, ResponseId)
# 
# ResponseToExclude<- as.list(DodgyIPAdd1$ResponseId)
# 
# 
# RawData$ToExclude <- ifelse(
#   RawData$ResponseId %in% ResponseToExclude, 
#   "Exclude", 
#   "Include"
# )
# 

# OR, the simpler way...
# First, exclude all responses from these IP Addresses because we had >2 
#ResponseToExclude <- c("120.241.44.45", "180.211.110.146", "45.133.172.94")

####################################################################################
###################################################################################
###################################################################################


######JULIA EDITED/ADDED STUFF HERE JAN 2 2025#############

#Exclude IPs that appear more than 2x
RawData <- RawData %>%
  group_by(IPAddress) %>%
  filter(n() <= 2) %>%
  ungroup()

#Check that worked ok - Pull out the data where there are multiple IP addresses
DupeIPs <- RawData %>% 
  group_by(IPAddress) %>% 
  filter(n() >= 2) %>% ungroup()%>%
  select(ResponseId, IPAddress) %>%
  group_by(IPAddress)

DupeIPsTable <- as.data.frame(table(DupeIPs$IPAddress))
#Yep!

#NOW we are left with only 2x dupes. We want to exclude by completion status now.
#Pull out the data where there are 2 of the same IP addresses and look at whether they 
#finished or didn't
TwoDupes_CompletionStatus <- RawData %>% 
  group_by(IPAddress) %>% 
  filter(n() == 2) %>% ungroup()%>%
  select(ResponseId, IPAddress, Finished) %>%
    group_by(IPAddress)

#Exclude responses that are both dupe IPs AND contain only one finished status (true OR false) 
RawData <- RawData %>%
  group_by(IPAddress) %>%
  filter(n_distinct(Finished) > 1 | n() == 1) %>%
  ungroup()

#Check that that worked by redoing step above 
TwoDupes_CompletionStatus <- RawData %>% 
  group_by(IPAddress) %>% 
  filter(n() == 2) %>% ungroup()%>%
  select(ResponseId, IPAddress, Finished) %>%
  group_by(IPAddress)
#Yep!

#Now we want to delete only the incomplete responses from within the dupe IP address data
RawData <- RawData %>%
  mutate(is_duplicate = duplicated(IPAddress) | duplicated(IPAddress, fromLast = TRUE)) %>% 
  filter(!(is_duplicate & Finished == "False")) %>% 
  select(-is_duplicate) 

#One last QA check by looking at Dupes/Statuses left
DupeIPs <- RawData %>% 
  group_by(IPAddress) %>% 
  filter(n() >= 2) %>% ungroup()%>%
  select(ResponseId, IPAddress) %>%
  group_by(IPAddress)
#No Dupes left, so that's good. 

#Let's just make sure the COMPLETE copy of an original T/F dupe is still in the data for sanity...

#IP address examples to check:
# >2 Duplicate addresses - 120.241.44.45
# 2 finished duplicate addresses - 180.211.110.146
# 2 unfinished duplicates - don't have any of those in this data set
# Duplicate with one unfinished, but should still have 1 completed version in RawData - 117.245.50.95

ip_to_check <- "120.241.44.45"
ip_exists <- ip_to_check %in% RawData$IPAddress
print(ip_exists)
#All is well!

######################JULIA STOPPED WORKING HERE JAN 2 2025##########
############ Julia done an awesome job on Jan 2 2025! That all works beautifully :-D ####
#########################################################################################



####################################################################
####### Duplicate emails ###########################################
###################################################################

# Check for duplicate email addresses in 'Recipient Email' ie those invited from Potatobase

EmailAdds<- RawData %>%
  filter(!(RecipientEmail=="")) %>%
  select(ResponseId, Status, RecipientEmail)

DupEmailAdd<-sum(duplicated(EmailAdds))

DupEmailAddList<-EmailAdds %>%
  filter(duplicated(RecipientEmail)) %>%
  select(ResponseId, Status, RecipientEmail) %>%
  arrange(RecipientEmail)
 # group_by(Status) %>%
  #count(Status)

UniqueEmailAddList<-EmailAdds %>%
  filter(!duplicated(RecipientEmail)) %>%
  select(Status, RecipientEmail) %>%
  group_by(Status) %>%
  count(Status)

### Used this to determine Response IDs to delete...
duplicates_EmailAdd <- RawData %>%
  filter(RecipientEmail!="") %>%
  group_by(RecipientEmail) %>%
  filter(n() > 1) %>%
  select(ResponseId, Status, RecipientEmail) %>%
  arrange(ResponseId) %>%
  ungroup()

## Conclusion - no duplicates left for these email addresses after IP address clean up

###############
# Same again for those entering the prize draw...

# Remove empty email fields
PDEmailAdds<- RawData %>%
  filter(!(X10.1_2 =="")) %>%
  select(ResponseId, Status, X10.1_2)


PDDupEmailAdd<-sum(duplicated(PDEmailAdds$X10.1_2))
PDDupEmailCount <-as.data.frame( table(PDEmailAdds$X10.1_2))

PDDupEmailAddList<-PDEmailAdds %>%
  filter(duplicated(X10.1_2)) %>%
  select(ResponseId, Status, X10.1_2) %>%
  arrange(X10.1_2)


DupePDEmail <- RawData %>% 
  group_by(X10.1_2) %>% 
  filter(n() >= 2) %>%
 filter(!(X10.1_2 =="")) %>% 
  ungroup()%>%
  select(ResponseId, X10.1_2, Finished) %>%
  group_by(X10.1_2)

# removing all non-empty duplicated responses (14 responses, 7 individuals). NB All of them "Finished"
# the survey.

RawData <- RawData %>%
  mutate(is_duplicate = duplicated(X10.1_2) | duplicated(X10.1_2, fromLast = TRUE)) %>% 
  filter(!(is_duplicate & X10.1_2 != "")) %>% 
  select(-is_duplicate) 


###########################

### Remove response with Progress <4%


RawData <- RawData %>%
    filter(Progress != "0")%>%
    filter(Progress != "1")%>%
    filter(Progress != "2")

############################################################################################
#############################################################################################
# Check for bot style responses ##################
### check for duplicate free text responses ######

# X2.2a        	How are the serverless technologies used?
# X2.3_4_TEXT  	What OS do you use for development? (Select all that apply) - My development process is OS-agnostic (please explain): - Text
# X2.5a	        Which internal developer portal do you use?
# Q98	          Please briefly describe your organisation's bring your own cloud/private cloud requirements:
# Q101	        Are there any other languages, tools, stores, services or frameworks that you use not included in previous questions that you would like to tell us about? If so, please list below:
# X2a.4b	      Please briefly explain why the provider(s) that you use were selected:
# Q149	        What is the primary value that the Docker suite of products brings to you?
# X6.11	        If you could add just one feature or improvement to the Docker suite of products, what would it be?
# X5.2	        Do you have any other work-related obstacles? Please describe what they are and the impacts on your
# X7.5a	        Please explain why you gave the rating above:
# X7.7d	        Is there anything that would make you feel more confident developing AI/ML capabilities as part of an application? If so, please describe:


### For each, pull out any non-empty ones

Field1<-RawData %>%
  filter(X2.2a!="") %>%
  select(ResponseId, X2.2a) %>%
  arrange(ResponseId)

Field1Dups <- Field1 %>%
  filter(duplicated(X2.2a)) %>%
  select(ResponseId, X2.2a) %>%
  group_by(X2.2a)

### Duplicate responses make sense
#####
  
Field2<-RawData %>%
  filter(X2.3_4_TEXT!="") %>%
  select(ResponseId, X2.3_4_TEXT) %>%
  arrange(ResponseId)

Field2Dups <- Field2 %>%
  filter(duplicated(X2.3_4_TEXT)) %>%
  select(ResponseId, X2.3_4_TEXT) %>%
  group_by(X2.3_4_TEXT)

# 0 duplicate responses

######

Field3<-RawData %>%
  filter(X2.5a!="") %>%
  select(ResponseId, X2.5a ) %>%
  arrange(ResponseId)

Field3Dups <- Field3 %>%
  filter(duplicated(X2.5a)) %>%
  select(ResponseId, X2.5a) %>%
  group_by(X2.5a)


### Duplicate responses make sense
######


Field4<-RawData %>%
  filter(Q98!="") %>%
  select(ResponseId, Q98) %>%
  arrange(ResponseId)

Field4Dups <- Field4 %>%
  filter(duplicated(Q98)) %>%
  select(ResponseId, Q98) %>%
  group_by(Q98)
# 0 duplicate responses

####

Field6<-RawData %>%
  filter(Q101!="") %>%
  select(ResponseId, Q101) %>%
  arrange(ResponseId)

Field6Dups <- Field6 %>%
  filter(duplicated(Q101)) %>%
  select(ResponseId, Q101) %>%
  group_by(Q101)

#Duplicates make sense
######

Field7<-RawData %>%
  filter(X2a.4b!="") %>%
  select(ResponseId, X2a.4b) %>%
  arrange(ResponseId)

Fiel7Dups <- Field7 %>%
  filter(duplicated(X2a.4b)) %>%
  select(ResponseId, X2a.4b) %>%
  group_by(X2a.4b)

# 2 suspicious

#######################

Field8<-RawData %>%
  filter(Q149!="") %>%
  select(ResponseId, Q149) %>%
  arrange(ResponseId)

Fiel8Dups <- Field8 %>%
  filter(duplicated(Q149)) %>%
  select(ResponseId, Q149) %>%
  group_by(Q149)

#Duplicates potentially make sense
#######################

Field9<-RawData %>%
  filter(X6.11!="") %>%
  select(ResponseId, X6.11) %>%
  arrange(ResponseId)

Fiel9Dups <- Field9 %>%
  filter(duplicated(X6.11)) %>%
  select(ResponseId, X6.11) %>%
  group_by(X6.11)

#Possibly worth further investigating - no patterns other than quite a few 'idk'
#######################

Field10<-RawData %>%
  filter(X5.2!="") %>%
  select(ResponseId, X5.2) %>%
  arrange(ResponseId)

Fiel10Dups <- Field10 %>%
  filter(duplicated(X5.2)) %>%
  select(ResponseId, X5.2) %>%
  group_by(X5.2)
#Check responses in context
#######################

Field11<-RawData %>%
  filter(X7.5a!="") %>%
  select(ResponseId, X7.5a) %>%
  arrange(ResponseId)

Fiel11Dups <- Field11 %>%
  filter(duplicated(X7.5a)) %>%
  select(ResponseId, X7.5a) %>%
  group_by(X7.5a)

# 0 duplicates
#######################

Field12<-RawData %>%
  filter(X7.7d!="") %>%
  select(ResponseId, X7.7d) %>%
  arrange(ResponseId)

Fiel12Dups <- Field12 %>%
  filter(duplicated(X7.7d)) %>%
  select(ResponseId, X7.7d) %>%
  group_by(X7.7d)
#Check responses in context
####################################################################################################
####################################################################################################


#### Check for docker employees and create a flag for them to help with analysis



#install.packages("stringr") #Uncomment this if stringr not installed

#Rename column
RawData <- RawData %>% rename(Email = X10.1_2)

#identify docker employees based on email
library("stringr")
Docker_employees <- RawData %>%
  filter(str_detect(Email, "docker"))

#After manual inspection of the 24 entries that contain "docker" in email, it appears 0 docker employees took the survey :(


####################################
# Specific for prize draw list... ###
#####################################

# Country of residence requirements:
# All entrants must be legal residents of the fifty (50) United States (excluding Puerto
# Rico) and the District of Columbia, Argentina, Australia, Austria, Belgium, Brazil,
# Bolivia, Canada (excluding Quebec), Chile, Colombia, Costa Rica, Denmark, Ecuador,
# Finland, France, Germany, Greece, Hungary, Ireland, Israel, Italy, Japan, Malaysia,
# Netherlands, New Zealand, Norway, Poland, Singapore, Slovakia, South Africa, Spain,
# Sweden, Switzerland, Turkey, United Arab Emirates, United Kingdom, and Uruguay,
# who are at least age eighteen (18) or older or are the legal age of majority in the
# jurisdiction in which they reside and capable of forming a binding contract with Sponsor
# on the date of their registration for the Promotion. 

# Canadian residents must correctly
# answer a time-limited mathematical skill-testing question, if selected as winner, in order
# to claim the prize. 

# Excluded are residents of Cuba, Iran, North Korea, Sudan and Syria

### Russia, Cuba, Iran, North Korea, Sudan and Syria
## NB A lot of rows aren't populated...

CountriesToInclude<- c("Argentina", "Australia", "Austria", "Belgium", "Brazil",
"Bolivia", "Canada", "Chile", "Colombia", "Costa Rica", "Denmark", "Ecuador",
"Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Israel", "Italy", "Japan", "Malaysia",
 "Netherlands", "New Zealand", "Norway", "Poland", "Singapore", "Slovakia", "South Africa",
 "Spain", "Sweden", "Switzerland", "Turkey", "United Arab Emirates",
"United Kingdom of Great Britain and Northern Ireland", "Uruguay", "United States of America")

CountryCount <-as.data.frame(table(RawData$X9.1))
#Russian Federation - 1
#Cuba - 0
#Iran - 1
#North Korea - 0
#Sudan - 0
#Syria - 0


# CompleteData <- RawData %>%
#   filter(Status =="Complete")
# 
# AllowedCountriesOnly <- CompleteData %>%
#   filter(Where.are.you.located.!='Russia') %>%
#   filter(Where.are.you.located.!='Iran')

# #CountriesToDeleteR
# RawData <- RawData %>%
#   filter(X9.1=='Russian Federation') %>%
#  # filter(Where.are.you.located.=='Iran') %>%
#   #select(ResponseId, X9.1)
# 
# #CountriesToDeleteC 
# RawData<- RawData %>%
#  # filter(Where.are.you.located.=='Russian Federation') %>%
#    filter(X9.1=='Iran') %>%
#   select(ResponseId, X9.1)


RawData <- RawData %>%
  filter(X9.1 != "Russian Federation")%>%
  filter(X9.1 != "Iran")
  


# # Only complete (in a meaningful way) surveys (unless strapped for numbers)
# ##NOTE: THIS EXCLUSION IS ONLY FOR PRIZE DRAW, NOT ANALYSIS
# ####DO NOT RUN THIS SECTION FOR ANALYSIS
# ######ONLY RUN THIS SECTION FOR PRIZE DRAW
# exclude_status <- "False"
# 
# #Make completion status a factor so we can exclude categorically
# RawData$Finished<-as.factor(RawData$Finished)
# 
# # Exclude rows in which Under 18 was selected
# RawData <- RawData %>%
#   filter(Finished != exclude_status)







########################################################################################
##### Exported file creation ###########################################################
########################################################################################

# 1. Cleaned Up Data

# Specify the file name
file_name <- "Cleaned_Up_Data.csv"

# Write the data frame to a CSV file
write.csv(RawData, file = file_name, row.names = FALSE)

# Confirm the file was created
cat("Data frame 'Cleaned_Up_Data' has been written to", file_name, "in your working directory.\n")


#2. Prize Draw Data

#Pull in the cleaned up data set

Prize_Draw_Data<- read.csv("Cleaned_Up_Data.csv")

# Specify the file name
file_name <- "Prize_Draw_Data.csv"

#Only included specific countries and Finished=True

Prize_Draw_Data$ToExclude <- ifelse(
  Prize_Draw_Data$X9.1 %in% CountriesToInclude, 
  "Include", 
  "Exclude"
)

Prize_Draw_Data <- Prize_Draw_Data %>%
  filter(Finished == "True") %>%
  filter(ToExclude == "Include") 


# Write the data frame to a CSV file
write.csv(Prize_Draw_Data, file = file_name, row.names = FALSE)

# Confirm the file was created
cat("Data frame 'Prize_Draw_Data' has been written to", file_name, "in your working directory.\n")




# 3. Sock winner data

#Which to use - cleaned up data or prize draw data??????
Sock_Winner_Data <- read.csv("")

# Specify the file name
file_name <- "Sock_Winner_Data.csv"

# Write the data frame to a CSV file
write.csv(Sock_Winner_Data, file = file_name, row.names = FALSE)

# Confirm the file was created
cat("Data frame 'Sock_Winner_Data' has been written to", file_name, "in your working directory.\n")





##################################
# Note area....
################################

#### Based on a list, create an indicator in a column called 'ToEXclude' based on whether
# that line item has a value in that list (in this case, Country)

# Example list of countries to include
CountriesToInclude <- c("USA", "Canada", "UK", "Germany") # Replace with required list

# Add the 'ToExclude' column to the dataframe
Cleaned_Up_Data$ToExclude <- ifelse(
  Cleaned_Up_Data$Country %in% CountriesToInclude, 
  "Include", 
  "Exclude"
)

# View the updated dataframe (optional)
head(Cleaned_Up_Data)



################
# Convert contents of a column to a list variable:
# Specify the column to convert (e.g., 'Country')
column_as_list <- as.list(df$Country)

# Print the list (optional)
print(column_as_list)


