###############################################################
######### Devex Survey Data Clean Up Script ###################
###############################################################
############# 22 Nov 2023 #####################################
###############################################################


# Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/Devex Survey 2023 Analysis")


# Install necessary packages
library("dplyr")

# Pull in data
RawData<-read.csv("Full raw data UNCLEANED Nov 27 - 20231127093525-SurveyExport.csv")


# Look at response time profile and delete 'too fast' responses
### Meeting at QuantQollab decided that had little confidence in
### the Alchemer time stamps as some responses which take very little time
### (less than 5 mins) appear to be complete and sensible responses
### CONCLUSION: Don't delete any responses based on time 

# Look at completes and partials
###  check for partials started and completed by same individual (IP or email)


# Check for duplicate IP addresses
IPAdds<- RawData %>% 
    filter(!(IP.Address=="")) %>%
    select(Status, IP.Address) 

#DupIPAdd<-sum(duplicated(IPAdds))

DupIPAddList<-IPAdds %>%
    filter(duplicated(IP.Address)) %>%
    select(Status, IP.Address) %>%
    group_by(Status) %>%
    count(Status)

UniqueIPAddsList<-IPAdds %>%
  filter(!duplicated(IP.Address)) %>%
  select(Status, IP.Address) %>%
  group_by(Status) %>%
  count(Status)
            

# Check for duplicate email addresses

# EmailAdds<- RawData %>% 
#   filter(!(Email.address=="")) %>%
#   select(Response.ID, Status,Email.address) 

#DupEmailAdd<-sum(duplicated(EmailAdds))

# DupEmailAddList<-EmailAdds %>%
#   filter(duplicated(Email.address)) %>%
#   select(Response.ID, Status, Email.address) %>%
#   arrange(Email.address)
#  # group_by(Status) %>%
#   #count(Status)
# 
# UniqueEmailAddList<-EmailAdds %>%
#   filter(!duplicated(Email.address)) %>%
#   select(Status, Email.address) %>%
#   group_by(Status) %>%
#   count(Status)

### Used this to determine Respons IDs to delete...
duplicates_EmailAdd <- RawData %>%
  filter(Email.address!="") %>%
  group_by(Email.address) %>%
  filter(n() > 1) %>%
  select(Response.ID, Status, Email, Email.address) %>%
  arrange(Response.ID) %>%
  ungroup()



##
# Check for bot style responses
### check for duplicate free text responses

#### List the free text response fields
##### - 1. Is there a particular reason that you use the selected containerization technologies? Please describe.
##### - 2. Do you have any other work-related obstacles? Please describe what they are and the impacts on your work. (Optional)
##### - 3. Please explain why you gave the rating above. (Optional)
##### - 4. If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.
##### - 5. If you'd like to be entered into the draw to win one of the amazing prizes we have on offer, please complete the following information...

### For each, pull out any non-empty ones

Field1<-RawData %>%
  filter(Is.there.a.particular.reason.that.you.use.the.selected.containerization.technologies..Please.describe...Optional.!="") %>%
  select(Response.ID, Is.there.a.particular.reason.that.you.use.the.selected.containerization.technologies..Please.describe...Optional.) %>%
  arrange(Response.ID)

Field1Dups <- Field1 %>%
  filter(duplicated(Is.there.a.particular.reason.that.you.use.the.selected.containerization.technologies..Please.describe...Optional.)) %>%
  select(Response.ID, Is.there.a.particular.reason.that.you.use.the.selected.containerization.technologies..Please.describe...Optional.) %>%
  group_by(Is.there.a.particular.reason.that.you.use.the.selected.containerization.technologies..Please.describe...Optional.)

#####
  
Field2<-RawData %>%
  filter(Do.you.have.any.other.work.related.obstacles..Please.describe.what.they.are.and.the.impacts.on.your.work...Optional.!="") %>%
  select(Response.ID, Do.you.have.any.other.work.related.obstacles..Please.describe.what.they.are.and.the.impacts.on.your.work...Optional.) %>%
  arrange(Response.ID)

Field2Dups <- Field2 %>%
  filter(duplicated(Do.you.have.any.other.work.related.obstacles..Please.describe.what.they.are.and.the.impacts.on.your.work...Optional.)) %>%
  select(Response.ID, Do.you.have.any.other.work.related.obstacles..Please.describe.what.they.are.and.the.impacts.on.your.work...Optional.) %>%
  group_by(Do.you.have.any.other.work.related.obstacles..Please.describe.what.they.are.and.the.impacts.on.your.work...Optional.)

#!!!!Suspicious Response ID 396 to 404 - all responses identical 'no'
# Investigated and they all look questionable, so decided to select
# one at random to keep (404) and disregard the rest

######

Field3<-RawData %>%
  filter(Please.explain.why.you.gave.the.rating.above...Optional.!="") %>%
  select(Response.ID, Please.explain.why.you.gave.the.rating.above...Optional. ) %>%
  arrange(Response.ID)

Field3Dups <- Field3 %>%
  filter(duplicated(Please.explain.why.you.gave.the.rating.above...Optional.)) %>%
  select(Response.ID, Please.explain.why.you.gave.the.rating.above...Optional.) %>%
  group_by(Please.explain.why.you.gave.the.rating.above...Optional.)

######
# Not an open ended question!
# Field4<-RawData %>%
#   filter(If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.!="") %>%
#   select(Response.ID, If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.) %>%
#   arrange(Response.ID)
# 
# Field4Dups <- Field4 %>%
#   filter(duplicated(If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.)) %>%
#   select(Response.ID, If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.) %>%
#   group_by(If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.)

####

Field5<-RawData %>%
  
  select(Response.ID, Status, Email, Email.address, If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name) %>%
  arrange(Response.ID)

Field5Dups <- Field5 %>%
  filter(duplicated(If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name)) %>%
  select(Response.ID, Status, Email, Email.address, If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name) %>%
  group_by(If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name)

# Duplicate Names...
# Filter rows with duplicate entries in the 'If.you.d.like' column
duplicates_df <- RawData %>%
  filter(If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name!="") %>%
  group_by(If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name) %>%
  filter(n() > 1) %>%
  select(Response.ID, Status, Email, Email.address, If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name) %>%
  arrange(If.you.d.like.to.be.entered.into.the.draw.to.win.one.of.the.amazing.prizes.we.have.on.offer..please.complete.the.following.information....Full.Name)%>%
  ungroup()

##!!! 7 duplicate names, 5 using same email address twice, 2 with different email addresses 
###########
#Decision - 1106 & 1262 Answers very similar, but not similar enough to be sure they're the same person...
# 1148 & 359 Location the same, but answers quite different so not able to determine if they're the same person

####################################
# Specific for prize draw list... ###
#####################################

# Exclude banned countries
# List of countries to exclude from survey:
### Russia, Cuba, Iran, North Korea, Sudan and Syria
## NB A lot of rows aren't populated...

CountryCount <-table(RawData$Where.are.you.located.)
#Russia - 3
#Cuba - 2
#Iran - 0
#North Korea - 0
#Sudan - 0
#Syria - 0

# CompleteData <- RawData %>%
#   filter(Status =="Complete")
# 
# AllowedCountriesOnly <- CompleteData %>%
#   filter(Where.are.you.located.!='Russia') %>%
#   filter(Where.are.you.located.!='Cuba')

CountriesToDeleteR <- RawData %>%
  filter(Where.are.you.located.=='Russia') %>%
 # filter(Where.are.you.located.=='Cuba') %>%
  select(Response.ID, Where.are.you.located.)

CountriesToDeleteC <- RawData %>%
 # filter(Where.are.you.located.=='Russia') %>%
   filter(Where.are.you.located.=='Cuba') %>%
  select(Response.ID, Where.are.you.located.)


# Only complete surveys (unless strapped for numbers)



# Make 'do you use AI' = No and impact of AI  to blank