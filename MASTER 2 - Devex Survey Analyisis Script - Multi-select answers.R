
#####################################################
############ Data Clean Up Part 2 ###################
#####################################################
######### CONTENTS #################################
##  Set  Up
##  Multi-select questions and their answers
##  Code for grouping columns
##  Code for creating clean freq table sorted descending by freq
##  Graph Plotting Code
#######################################################

#####################################################
########### Set up ##################################
#####################################################

# Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/Devex Survey 2023 Analysis")

#Install packages
#install.packages("googlesheets4")
# install.packages("tidyverse")
#install.packages("table1")
# install.packages("ggplot2")

# Start necessary packages
library("dplyr")
library(googlesheets4)
library(tidyverse)
library(table1)
library(ggplot2)

# Pull in data
RawData<-read.csv("For Analysis- CleanedDevexData 7 Dec 23.csv")

###########################################################
##### Multi select questions and their answers ###########
#########################################################

# What do you think are the most important trends in the industry right now? (Select top 3)
##RawData$GroupedTrends 
###AI assistants for software engineering:What do you think are the most important trends in the industry right now? (Select top 3)	
###Generative AI:What do you think are the most important trends in the industry right now? (Select top 3)	
###Cloud-native solutions:What do you think are the most important trends in the industry right now? (Select top 3)	
###Cloud development environments:What do you think are the most important trends in the industry right now? (Select top 3)	
###Internal Developer Platforms (IDPs):What do you think are the most important trends in the industry right now? (Select top 3)	
###GitOps:What do you think are the most important trends in the industry right now? (Select top 3)
###Shifting security to the left:What do you think are the most important trends in the industry right now? (Select top 3)	
###Infrastructure as Code:What do you think are the most important trends in the industry right now? (Select top 3)	
###Low code/no code solutions:What do you think are the most important trends in the industry right now? (Select top 3)	
###WebAssembly (WASM):What do you think are the most important trends in the industry right now? (Select top 3)	
###Other (please explain):What do you think are the most important trends in the industry right now? (Select top 3)
###I don’t see any current trends as important:What do you think are the most important trends in the industry right now? (Select top 3)





##################################

# How did you learn to code? (Select all that apply)
## CodeLearning
### Books / Physical media
### Coding Bootcamp
### Colleagues
### Friends or family members
### Hackathons (virtual or in-person)
### On the job training
### Online Courses or certification
### Other online resources (videos, blogs, forum, etc)
### School (University, College, etc)
### Other (please explain): 
### I do not know how to code

RawData<-RawData %>% 
  unite(CodeLearning, 
        Books...Physical.media.How.did.you.learn.to.code...Select.all.that.apply.,
        Coding.Bootcamp.How.did.you.learn.to.code...Select.all.that.apply.,
        Colleagues.How.did.you.learn.to.code...Select.all.that.apply.,
        Friends.or.family.members.How.did.you.learn.to.code...Select.all.that.apply.,
        Hackathons..virtual.or.in.person..How.did.you.learn.to.code...Select.all.that.apply.,
        On.the.job.training.How.did.you.learn.to.code...Select.all.that.apply.,
        Online.Courses.or.certification.How.did.you.learn.to.code...Select.all.that.apply.,
        School..University..College..etc..How.did.you.learn.to.code...Select.all.that.apply.,
        Other..please.explain..How.did.you.learn.to.code...Select.all.that.apply.,
        I.do.not.know.how.to.code.How.did.you.learn.to.code...Select.all.that.apply.,
        sep = "_", remove = FALSE)


#########################
#Which of these resources have you used for online courses or certifications? (Select all that apply)
#
### Codecademy
### Coursera
### edX
### freeCodeCamp
### Pluralsight
### Skillsoft
### Udacity
### Udemy
### Other (please explain)






#####################
# Which online resources did you use to learn to code? (Select all that apply)*
### Technical documentation
### Blogs
### Online books
### Coding sessions (live or recorded)
### How-to videos
### Auditory material (for example, podcasts)
### Games that teach programming
### Online challenges (for example, daily or weekly coding challenges)
### Interactive tutorial
### Written tutorials
### Online forum
### Other (please explain):







###########################
#How do you like to learn?
### Videos
### Streams (such as Twitch or YouTube Live)
### Podcasts
### Live conference talks
### Developer communities
### Reviewing source code
### Blogs
### Experimentation/building a side project
### Online training courses
### In-person training courses
### Books
### Reading documentation
### Other (please explain):




#######################
#How do you find out about new developer tools?
### Videos
### Streams (such as Twitch or YouTube Live)
### Podcasts
### Live conference talks
### Social media
### Searching for them myself (online)
### When my company adopts new tools
### Marketing correspondence from companies
### Friends/Colleagues
### Developer communities
### Blogs
### Industry news
### Books
### Reading documentation
### GitHub Q&A
### Meetups
### Conferences
### Other (please explain): 







################################################
# What is the structure of the application(s) you work on? (Select all that apply)*
  
### Microservices
  ### Monolith
  ### Hybrid monolith/microservices
  ### Transitioning from monolith to microservices
  ### Transitioning from microservices to monolith
  ### I don’t know
  ### Other (please explain): 
  


################################################
#What OS do you use for development? (Select all that apply)*
  ### Mac
  ### Windows
  ### Linux
  ### My development process is OS agnostic (please explain): 
  ### Other (please explain): 
  


############################################
#Which languages and frameworks do you use? (Select all that apply)*
### Angular
### ASP.NET
### C
### C#
### C++
### Go
### django
### Express.js
### Flask
### Flutter
### Java
### JavaScript
### Kotlin
### Laravel
### Node.js
### Objective-C
### PHP
### Python
### React
### React Native
### Ruby
### Ruby on Rails
### Rust
### Spring/Spring Boot
### Swift
### Symfony
### Vue
### Xamarin
### Other (please explain)
### Not applicable



###############
#Which cloud service(s) do you use? (Select all that apply)*
  ### Amazon Web Service (AWS)
  ### Microsoft Azure
  ### Google Cloud Platform
  ### IBM Cloud Services
  ### Adobe Creative Cloud
  ### VMware
  ### RackSpace
  ### Red Hat
  ### ServerSpace
  ### Digital Ocean
  ### Linode
### OVH
### Fly.io
### Heroku
### Alibaba Cloud
### Oracle Cloud
### Other (please explain):*
### Not applicable




#############################################
#Which data store(s) do you use? (Select all that apply)
##
### MySQL/MariaDB
### Apache Cassandra
### DynamoDB
### MLDB
### PostgresSQL
### Redis
### Memcached
### Elasticsearch
### MongoDB
### Amazon RDS
### Other AWS databases
### Google Cloud databases
### Microsoft Azure databases
### SQL Server
### Kafka
### InfluxDB
### Apache Spark
### Oracle
### Other (please explain):
### Not applicable




######################################################
#Which security tool(s) do you use? (Select all that apply)
  ### Snyk
  ### Docker Scout
  ### ChainGuard
  ### SonarQube
  ### VeraCode
  ### Aqua Security
  ### JFrog
  ### Anchore
  ### Tenable
  ### Slim/AI
  ### Palo Alto Prisma Cloud
### Wiz
### Kubescape
### Synopsis
### Whitesource
### AWS SecurityHub
### Checkmarx SAST
### Burp Suite Professional
### GitLab DevSecOps Platform
### Qualys
### Contrast Code Security Platform
### WhiteHat Dynamic
### InsightAppSec
### HCL AppScan
### Other (please explain): 
### Not applicable



##################
#Where do you primarily use containers in your work? (Select all that apply)*
### Development
### Testing
### Integration
### Deployment
### Staging
### Production
### Monitoring
### Troubleshooting
### Other (please explain):




######################
#Which of the following containerization tools/technologies do you use, if any? (Select all that apply)*
### Docker Engine
### Podman
### Rancher
### Apache Mesos
### CodeReady Container
### Docker Compose
### Docker Swarm
### Docker Desktop Kubernetes
### kind (Kubernetes in Docker)
### Kubernetes
### Mirantis Container Cloud
### Portainer
### k0s
### K3s + K3d
### MicroK8S
### Minikube
### Minishift
### OpenShift
### Hashicorp Nomad
### Other (please explain): 
### I don't use any




##########################################
#Do you use any of these other tools in your work? (Select all that apply)*
### Diagrams.net (draw.io)
### Figma
### Lucidchart
### Miro
### Omnigraffle
### Visio
### Airtable
### Confluence
### Excalidraw
### Dropbox
### Google Drive
### Microsoft Teams
### Monday.com
### Notion
### Skype for Business
### Slack
### Zoom
### Asana
### GitHub roadmaps/projects (for planning)
### Jira
### Trello
### Other (please explain): 
### None of the above



#######################################
# Where do you or your team get stuck in the development process most often? (Select top 3)*
### Estimation
### Planning
### Designing
### Writing code
### Building/Compiling
### Testing
### Debugging/troubleshooting
### Collaboration
### PR review
### Continuous Integration (CI)
### Continuous Delivery/Deployment (CD)
### Provisioning
### Monitoring/Logging/Maintenance
### Security/vulnerability remediation
### None of the above
### Other (please explain): 



###################
#What AI tool(s) do you use? (Select all that apply)*
### GitHub Copilot
### Scribe
### Bard
### ChatGPT
### Amazon Codewhisperer
### Bing AI
### Tabnine
### Mintlify
### Sourcegraph Code
### Adrenaline AI
### Grit.io
### Replit Ghostwriter
### warp.dev
### Other (please explain): 




#####################
#How do you typically use AI? (Select all that apply)*
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



##############################
#Are you working on Machine Learning (ML) in any capacity? (Select all that apply)*
### Yes, I leverage pre-trained ML models in one or more projects
### Yes, I train and deploy ML models in one or more projects
### Yes, I work on ML infrastructure in one or more projects
### I don't know
### No
### Other (please explain): 



############################
#Do you deal with any security related tasks? (Select all that apply)*
### Create/update security policy
### Deal with security scan results
### Fix vulnerabilities
### Log data analysis
### Monitor security incidents
### Run security scans
### Support other teams with security related tasks
### Other (please explain):
### None of the above



###############
# What are the roles/teams that focus on software security at your company/organization? (Select all that apply)*
  ### Developers
  ### Security Engineers
  ### DevOps team
  ### Platform Engineers
  ### DevSecOps
  ### Security is outsourced
  ### Security is not a concern at my company/organization
  ### Other (please explain): 
  ### I don't know



###########
#Were any of the following challenging for you when starting to use Docker? (Select all that apply)*
### Finding trusted and current information
### How to containerize my app
### How to debug or troubleshoot my application when in containers
### Learning how to deploy containers
### Understanding how Docker works
### Other (please explain): 
### None of the above



############
#What types of activities do you perform with Docker most often? (Select top 3)*
### Running containers locally
### Running containers remotely
### Running Compose apps locally
### Running Compose apps remotely
### Setting up environments
### Creating/editing Dockerfiles
### Creating/editing Compose files
### Building images
### Pushing images to registry
### Reviewing image layers
### Scanning images for vulnerabilities
### Other (please explain): 





################
#When do you refer to support/documentation for Docker most often? (Select top 3)*
### Running a container
### Building an image
### Creating a Dockerfile
### Using Scout
### Creating a Compose file
### Running a Compose file
### Configuring a new image I never used
### Other (please explain): 




###############
#Where in the Docker ecosystem would you want to discover new Docker tools? (Select all that apply)*
### Docker.com
### Docker Docs
### Docker Desktop
### Command Line Interface (CLI)
### Docker Hub
### Docker Social Media
### Other (please explain): 





###################
#Which developer communities do you participate in? (Select all that apply)*
### GitHub
### Stack Overflow
### Hacker News
### HackerNoon
### Hashnode
### freeCodeCamp
### Women Who Code
### dev.to
### CodeProject
### Docker
### Reddit
### Other (please describe): 

###########################################################
##### Code for grouping columns ##########################
######################################################

# Use this code to combine columns to give most frequent combinations
RawData<-RawData %>% 
  unite(GroupedTrends, 
        AI.assistants.for.software.engineering.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3., 
        Generative.AI.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3., 
        Cloud.native.solutions.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3., 
        Cloud.development.environments.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        Internal.Developer.Platforms..IDPs..What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        GitOps.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        Shifting.security.to.the.left.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        Infrastructure.as.Code.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        Low.code.no.code.solutions.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        WebAssembly..WASM..What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        Other..please.explain..What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.,
        sep = ", ", remove = FALSE)

GroupedTrendsFreqs <- RawData %>%
  count(GroupedTrends)%>%
  arrange(desc(n))

####################################################################
##### Code for creating clean freq table sorted descending by freq #
####################################################################

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

selected_columns <- c("AI.assistants.for.software.engineering.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.", 
                      "Generative.AI.What.do.you.think.are.the.most.important.trends.in.the.industry.right.now...Select.top.3.")

FrequencyTable <- combined_frequency_tables(RawData, selected_columns)

### Tidy up unnecessary columns and add percentages
FrequencyTable<- FrequencyTable %>%
  mutate(Percent = (Freq/dim(RawData)[1])*100)%>%
  select(-Var1) %>%
  rename("Option" = "Var2") 



#########################################################
################## Graph Plotting Code ##################
#########################################################

# Specify the size and position of axis label text
axis_label_size <- 12
axis_label_position <- "top"  # Change to "bottom", "left", or "right" as needed
axis_label_angle <- 45  # Change the angle as needed

#add title
title <- " "
# axis titles
x<-" "
y <-" "

#data to use
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

  