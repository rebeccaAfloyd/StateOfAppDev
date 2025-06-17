#################################################################
#### Script to analyse debugging and troubleshooting questions ##
#################################################################
############## 14 Dec 2023 ######################################
#################################################################


########### Set up ##################################

# Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/Devex Survey 2023 Analysis")

#Install packages
#install.packages("googlesheets4")
# install.packages("tidyverse")
#install.packages("table1")
# install.packages("ggplot2")
#install.packages("stringr")

# Start necessary packages
library("dplyr")
library(googlesheets4)
library(tidyverse)
library(table1)
library(ggplot2)
library(tidyr)
library(stringr)

# Pull in data
RawData<-read.csv("For Analysis- CleanedDevexData 7 Dec 23.csv")


# Code to create filters to re-run the analyses below for complete responses only
# and docker users vs non-docker
### Instructions: Find code for analysis wanted, copy, paste and comment
### and do search and replace with dataframe names below, as required...

#All Complete responses
CompleteResponses <- RawData %>%
  filter(Status == "Complete")

# Container users only
ContainerUsersOnly <-RawData %>%
  filter(Do.you.currently.use.containers.in.development. == "Yes")

# Non-container users only
NonContainerUsersOnly <-RawData %>%
  filter(Do.you.currently.use.containers.in.development. == "No")

# Complete responses, container users
CompContUsersOnly <- RawData %>%
  filter(Status == "Complete" & Do.you.currently.use.containers.in.development. == "Yes")

# Complete responses, non-container users
CompNonContUsersOnly <- RawData %>%
  filter(Status == "Complete" & Do.you.currently.use.containers.in.development. == "No")


# To help with analysis, use the column header and question name here:
# https://docs.google.com/spreadsheets/d/1QzoMgEeb-T_WrvMOlBMBeroOCqLUoueD/edit?usp=drive_link&ouid=102058165909952470147&rtpof=true&sd=true
# Sheet: 'Column Titles and refs'


####################################################################
##### Code for creating clean freq table sorted descending by freq #
####################################################################
# Run this function first, then for each time you want a nice frequency table, 
# run the following, putting in the column names of interest:
# selected_columns <- c("columnName1", 
#                       "columnName2")
# 
# FrequencyTable <- combined_frequency_tables(RawData, selected_columns)
# 
# ### Tidy up unnecessary columns and add percentages
# FrequencyTable<- FrequencyTable %>%
#   mutate(Percent = (Freq/dim(RawData)[1])*100)%>%
#   select(-Var1) %>%
#   rename("Option" = "Var2") 


### Credit to ChatGPT below for the function (after 3 attempts)
# Function to create individual frequency tables for selected columns and combine them
combined_frequency_tables <- function(data, selected_columns) {
  # Initialize an empty list to store individual frequency tables
  individual_tables <- list()
  
  # Loop through selected columns
  for (col in selected_columns) {
    # Remove empty strings and create a frequency table for each column, excluding null values
    non_empty_values <- data[[col]] != ""
    frequency_table <- table(data[[col]][non_empty_values], useNA = "ifany")
    
    # Store the frequency table in the list
    individual_tables[[col]] <- as.data.frame(t(frequency_table))
  }
  
  # Combine all individual frequency tables into one
  combined_table <- do.call(rbind, individual_tables)
  
  return(combined_table)
}


##### Relevant questions and their options #######

# Monitoring and logging tools
### !!!!!!!! NB The 'Other' responses sometimes repeat the options. As of 18 Dec, decided
### to not include these in the first round of analysis, but readdress before 
### signing off report!!!!!!!!!!!!!!!!!!!!
## Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)*
#### (457) Amazon Cloudwatch:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Datadog:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Elastic:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Fluentd:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Fluent bit:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Grafana:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Loki:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Prometheus:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Splunk:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Sysdig:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Dynatrace:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### Kibana:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)
#### (469)Sumo Logic:Please rate the monitoring/logging tool(s) that you use in your work: (Select “I don’t use this” if not applicable)

FrequencyTableTools <-apply((RawData[,457:469]), 2, table)

## This uses base R with 'wide' data above
#barplot(height = FrequencyTableTools, beside = TRUE)

InterimV<- as.data.frame(row.names(FrequencyTableTools))
FrequencyTableTools <- cbind(InterimV, FrequencyTableTools)

# Convert to 'long' format for ggplot
FreqTableToolsLong<-gather(FrequencyTableTools, condition, measurement, 2:14, factor_key = TRUE)
# Tidy up condition names, get rid of frequency of blanks and 'I don't use this'
FreqTableToolsLong$condition<-gsub('.Please.rate.the.monitoring.logging.tool.s..that.you.use.in.your.work...Select..I.don.t.use.this..if.not.applicable.', '', FreqTableToolsLong$condition)
FreqTableToolsLong$`row.names(FrequencyTableTools)`<-gsub("['‘’”“]", "", FreqTableToolsLong$`row.names(FrequencyTableTools)`)
FreqTableToolsLong<-FreqTableToolsLong %>%
  filter(`row.names(FrequencyTableTools)`!='')%>%
  filter(`row.names(FrequencyTableTools)`!='I dont use this')

#Create percentages...
  FreqTableToolsLongPercent<-  FreqTableToolsLong %>%
    group_by(condition) %>%
    mutate(
      count = n(),                        # Count of observations in each group
      Percentage = measurement / sum(measurement) * 100  # Percentage within each group
    ) %>%
    ungroup()  # Ungroup the data
  
  
# ADJUSTABLE:: add titles 
title <- "Ratings of Monitoring/Logging Tools by users who use the tools"
# axis titles
x<-" "
y <-" "

# ADJUSTABLE:: data to use 
data<-FreqTableToolsLongPercent
Category <-FreqTableToolsLongPercent$condition
Value <- FreqTableToolsLongPercent$Percentage
Subgroup <- FreqTableToolsLong$`row.names(FrequencyTableTools)`
ScaleToUse <- c("Very poor", "Poor", "Acceptable", "Good", "Very Good")

# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = Category, y = Value, fill=factor(Subgroup, ScaleToUse))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

# Display the bar chart
print(bar_chart)




# Dev Processes
## How would you rate your company/organization’s development processes?*
### Debugging in development:How would you rate your company/organization’s development processes?
### Debugging in production:How would you rate your company/organization’s development processes?
###  Troubleshooting system issues in development:How would you rate your company/organization’s development processes?
### Troubleshooting system issues in production:How would you rate your company/organization’s development processes?
#### Scale = c("Not Applicable", "Very poor", "Poor", "Acceptable", "Good", "Very Good")


selected_columns <- c("Debugging.in.development.How.would.you.rate.your.company.organization.s.development.processes.", 
                        "Debugging.in.production.How.would.you.rate.your.company.organization.s.development.processes.",
                        "Troubleshooting.system.issues.in.development.How.would.you.rate.your.company.organization.s.development.processes.",
                        "Troubleshooting.system.issues.in.production.How.would.you.rate.your.company.organization.s.development.processes.")

FrequencyTable <- combined_frequency_tables(RawData, selected_columns)
#Comparing container users and non-container users
FrequencyTable1 <- combined_frequency_tables(ContainerUsersOnly, selected_columns[4])
FrequencyTable2 <- combined_frequency_tables(NonContainerUsersOnly, selected_columns[4])


### Tidy up unnecessary columns and add percentages based on the number of responses
# to the question in question
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 

FrequencyTable1<- FrequencyTable1 %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2")

FrequencyTable2<- FrequencyTable2 %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2")


# ADJUSTABLE:: add titles 
title <- "Container Users - Troubleshooting.in.production.How.would.you.rate.your.company.organization.s.development.processes."
# axis titles
x<-" "
y <-"Percent"

# ADJUSTABLE:: data to use 
data<-FrequencyTable1
Category <-FrequencyTable1$Option
Value <- FrequencyTable1$Percent
ScaleToUseQual <- c("Not Applicable", "Very poor", "Poor", "Acceptable", "Good", "Very Good")
ScaleToUseEase <-  c("Not Applicable", "Very difficult", "Difficult", "Neutral", "Easy", "Very Easy")


# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = factor(Category, ScaleToUseQual), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

# Display the bar chart
print(bar_chart)




# Organisation Tasks
## Rate how you find the following tasks at your company/organization:*
### Debugging/troubleshooting:Rate how you find the following tasks at your company/organization:
### Collaboration while debugging:Rate how you find the following tasks at your company/organization:
### Scale  = c("Not Applicable", "Very difficult", "Difficult", "Neutral", "Easy", "Very Easy")

#RawData$Collaboration.while.debugging.Rate.how.you.find.the.following.tasks.at.your.company.organization.

selected_columns <- c("Debugging.troubleshooting.Rate.how.you.find.the.following.tasks.at.your.company.organization.", 
                      "Collaboration.while.debugging.Rate.how.you.find.the.following.tasks.at.your.company.organization.")


FrequencyTable <- combined_frequency_tables(RawData, selected_columns[2])

### Tidy up unnecessary columns and add percentages based on the number of responses
# to the question in question
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 

# AI use
## How do you typically use AI? (Select all that apply)*
### Design
### Troubleshooting/debugging
### Templates
### Understanding documentation
### CLI commands
### Writing documentation
### Optimizing performance
### Writing tests
### Running tests
### Research
### Writing code
### Setting up and/or managing environments
### Generating or help with configuration files
### Other (please explain): 

#RawData$

selected_columns <- c("Design.How.do.you.typically.use.AI...Select.all.that.apply.", 
                      "Troubleshooting.debugging.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Templates.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Understanding.documentation.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "CLI.commands.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Writing.documentation.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Optimizing.performance.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Writing.tests.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Running.tests.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Research.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Writing.code.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Setting.up.and.or.managing.environments.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Generating.or.help.with.configuration.files.How.do.you.typically.use.AI...Select.all.that.apply.",
                      "Other..please.explain..How.do.you.typically.use.AI...Select.all.that.apply.")


FrequencyTable <- combined_frequency_tables(RawData, selected_columns)

### Tidy up unnecessary columns and add percentages based on the number of responses
# to the question in question
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 


# ADJUSTABLE:: add titles 
title <- "How Do You Typicall Use AI?"
# axis titles
x<-" "
y <-" "


data<-FrequencyTable
Category <-FrequencyTable$Option
Value <- FrequencyTable$Freq

# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = reorder(Category, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

# Display the bar chart
print(bar_chart)


# Docker Use
## Were any of the following challenging for you when starting to use Docker? (Select all that apply)*
### Finding trusted and current information
### How to containerize my app
### How to debug or troubleshoot my application when in containers
### Learning how to deploy containers
### Understanding how Docker works
### Other (please explain):
### None of the above

# RawData$

selected_columns <- c("Finding.trusted.and.current.information.Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.",
                      "How.to.containerize.my.app.Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.",
                      "How.to.debug.or.troubleshoot.my.application.when.in.containers.Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.",
                      "Learning.how.to.deploy.containers.Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.",
                      "Understanding.how.Docker.works.Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.",
                      "Other..please.explain..Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.",
                      "None.of.the.above.Were.any.of.the.following.challenging.for.you.when.starting.to.use.Docker...Select.all.that.apply.")

FrequencyTable <- combined_frequency_tables(RawData, selected_columns)

### Tidy up unnecessary columns and add percentages based on the number of responses
# to the question in question
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 


# ADJUSTABLE:: add titles 
title <- "Were any of the following challenging starting to user Docker?"
# axis titles
x<-" "
y <-" "
axis_label_size <- 10
axis_label_angle <- 0  # Change the angle as needed

data<-FrequencyTable
Category <-FrequencyTable$Option
Value <- FrequencyTable$Freq

# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = reorder(Category, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
    theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

bar_chart<-bar_chart +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  #Code to wrap text of x axis labels

# Display the bar chart
print(bar_chart)


# Additional Docker features
## If you could add one feature or improvement to Docker Desktop, what would it be?*
### Troubleshooting production containers
### Monitoring and alert set up

FrequencyTable <- as.data.frame(table(RawData$If.you.could.add.one.feature.or.improvement.to.Docker.Desktop..what.would.it.be.))

### Add percentages based on the number of responses
# to the question in question
#And
#Remove blank responses

FrequencyTable<- FrequencyTable %>%
  filter(Var1!='')%>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)

# ADJUSTABLE:: add titles 
title <- "If you could add one feature or improvement to Docker Desktop..."
# axis titles
x<-" "
y <-"% of Responses"
axis_label_size <- 10
axis_label_angle <- 0  # Change the angle as needed

data<-FrequencyTable
Category <-FrequencyTable$Var1
Value <- FrequencyTable$Percent

# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = reorder(Category, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

bar_chart<-bar_chart +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))  #Code to wrap text of x axis labels

# Display the bar chart
print(bar_chart)


# Getting stuck
## Where do you or your team get stuck in the development process most often? (Select top 3)
### Estimation:Where do you or your team get stuck in the development process most often? (Select top 3)
### Planning:Where do you or your team get stuck in the development process most often? (Select top 3)
### Designing:Where do you or your team get stuck in the development process most often? (Select top 3)
### Writing code:Where do you or your team get stuck in the development process most often? (Select top 3)
### Building/Compiling:Where do you or your team get stuck in the development process most often? (Select top 3)
### Testing:Where do you or your team get stuck in the development process most often? (Select top 3)
### Debugging/troubleshooting:Where do you or your team get stuck in the development process most often? (Select top 3)
### Collaboration:Where do you or your team get stuck in the development process most often? (Select top 3)
### PR review:Where do you or your team get stuck in the development process most often? (Select top 3)
### Continuous Integration (CI):Where do you or your team get stuck in the development process most often? (Select top 3)
### Continuous Delivery/Deployment (CD):Where do you or your team get stuck in the development process most often? (Select top 3)
### Provisioning:Where do you or your team get stuck in the development process most often? (Select top 3)
### Monitoring/Logging/Maintenance:Where do you or your team get stuck in the development process most often? (Select top 3)
### Security/vulnerability remediation:Where do you or your team get stuck in the development process most often? (Select top 3)
### None of the above:Where do you or your team get stuck in the development process most often? (Select top 3)
### Other (please explain):Where do you or your team get stuck in the development process most often? (Select top 3)

# RawData$

selected_columns <- c("Estimation.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Planning.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Designing.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Writing.code.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Building.Compiling.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Testing.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Debugging.troubleshooting.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Collaboration.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "PR.review.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Continuous.Integration..CI..Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Continuous.Delivery.Deployment..CD..Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Provisioning.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Monitoring.Logging.Maintenance.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Security.vulnerability.remediation.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "None.of.the.above.Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                      "Other..please.explain..Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.")

FrequencyTable <- combined_frequency_tables(RawData, selected_columns)

### Tidy up unnecessary columns and add percentages based on the number of responses
# to the question in question
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 



# ADJUSTABLE:: add titles 
title <- "Where do you get stuck in the development process?"
# axis titles
x<-" "
y <-" "
axis_label_size <- 10
axis_label_angle <- 90  # Change the angle as needed

data<-FrequencyTable
Category <-FrequencyTable$Option
Value <- FrequencyTable$Freq

# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = reorder(Category, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

bar_chart<-bar_chart +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  #Code to wrap text of x axis labels

# Display the bar chart
print(bar_chart)


# Better tools wish
## Do you wish you had better tools in any of the following aspects of the development process? (Select all that apply)
### Debugging/troubleshooting
### Monitoring/Logging/Maintenance

# RawData$Estimation.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.

selected_columns <- c("Estimation.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Planning.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Designing.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Writing.code.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Building.Compiling.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Testing.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Debugging.troubleshooting.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Collaboration.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "PR.review.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Continuous.Integration..CI..Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Continuous.Delivery.Deployment..CD..Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Provisioning.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Monitoring.Logging.Maintenance.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Security.vulnerability.remediation.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "None.of.the.above.Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                      "Other..please.explain..Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.")

FrequencyTable <- combined_frequency_tables(RawData, selected_columns)

### Tidy up unnecessary columns and add percentages based on the number of responses
# to the question in question
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/sum(as.numeric(Freq)))*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 


# ADJUSTABLE:: add titles 
title <- "Do you wish you had better tools in any of the following aspects of the development process?"
# axis titles
x<-" "
y <-" "
axis_label_size <- 10
axis_label_angle <- 90  # Change the angle as needed

data<-FrequencyTable
Category <-FrequencyTable$Option
Value <- FrequencyTable$Freq

# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = reorder(Category, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

bar_chart<-bar_chart +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  #Code to wrap text of x axis labels

# Display the bar chart
print(bar_chart)









### Creating crosstabs ###

# df %>%
#   group_by(team, position) %>%
#   tally() %>%
#   spread(team, n)
# 


###############################################################
################## Graph Plotting Code ########################
###############################################################

# ADJUSTABLE:: Specify the size and position of axis label text
axis_label_size <- 12
axis_label_position <- "top"  # Change to "bottom", "left", or "right" as needed
axis_label_angle <- 45  # Change the angle as needed

# ADJUSTABLE:: add titles 
title <- selected_columns[2]
# axis titles
x<-" "
y <-" "

# ADJUSTABLE:: data to use 
data<-FrequencyTable
Category <-FrequencyTable$Option
Value <- FrequencyTable$Percent
ScaleToUseQual <- c("Not Applicable", "Very poor", "Poor", "Acceptable", "Good", "Very Good")
ScaleToUseEase <-  c("Not Applicable", "Very difficult", "Difficult", "Neutral", "Easy", "Very Easy")


# Create a ggplot bar chart
bar_chart <- ggplot(data, aes(x = factor(Category, ScaleToUseEase), y = Value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size, angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size, vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

# Display the bar chart
print(bar_chart)
