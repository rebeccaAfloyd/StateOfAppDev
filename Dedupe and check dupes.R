setwd("/Users/juliawilson/Downloads")
library(tidyverse)

#De-dupe Martine list
data <-read.csv("FullSignUP042523 - Martine.csv")

data<-data %>% distinct(Email.Address.Contact.information, .keep_all = TRUE)

write.csv(data, "/Users/juliawilson/Downloads\\Martines.csv", row.names=FALSE)

#Check to see if any bounced/unsubed last time
data1<-read.csv("FullSignUP042523 - ForRdupecheck.csv")

n_occur <- data.frame(table(data1$Email))

n_occur[n_occur$Freq > 1,]

data1[data1$Email %in% n_occur$Var1[n_occur$Freq > 1],]
