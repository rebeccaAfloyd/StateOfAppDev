###################################################################
# Code to generate and easily tweak presentation ready graphs #####
###################################################################
######################### 15 Jan 2024 #############################
###################################################################

# How to use this script...
# 1. Make sure you've 'loaded' your environment with the data you want to use

# 2. Run 'Basic Set Up' below, if you haven't already

# 3. Determine which kind of barchart you want from these options:
##### # 1. Simple barchart with an x-axis in the order specified above
############## Complete: Category, Value and ScaleToUse
##### # 2. Simple barchart with an x-axis descending order
############## Complete: Category, Value 
##### # 3. "Nested" barchart with subgroup elements in order specified in 'ScaleToUse'
############## Complete: Category, SubCategory, Value and ScaleToUse

# 4. Enter the values specific to your requirements in the 'Set Up Graph Parameters'
# 5. Tweak values as required to produce the best looking charts! 
# The section called 'Useful bits of code to enhance graphs' has some code snippets that can help.


# Basic Set Up

## Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/Devex Survey 2023 Analysis")


## Install packages
# install.packages("ggplot2")
# install.packages("RColorBrewer") 

## Start required packages
library(ggplot2)
library(RColorBrewer)

###############################################################

# Set Up Graph Parameters

# Axis Details
# ADJUSTABLE:: Specify the size and position of axis label text
axis_label_size <- 12
axis_label_position <- "top"  # Change to "bottom", "left", or "right" as needed
axis_label_angle <- 0  # Change the angle as needed depending on length of text

# ADJUSTABLE:: add titles you require
## Graph title
title <- ""
## axis titles
x<-" "
y <-" "

# ADJUSTABLE:: data to use 
data<- df  #Replace 'df' with the dataframe you want to use  
Category <- df$a  #replace 'df$a' with the column for the x axis
Value <- df$b     #replace 'df$b' with the column containing the values (y axis)

#If you're generating a chart with 'subgroups' then specify that here
SubCategory <- df$c #replace 'df$c' with the column which has the sub group in it

#If you want to force the Category or SubCategory into a specific order (eg for likert scales)
ScaleToUse <- c("Pos1", "Pos2","Pos3") # Replace with your own categories

# But for ease here are the 2 we use in the Devex survey...
# ScaleToUse <- c("Not Applicable", "Very poor", "Poor", "Acceptable", "Good", "Very Good")
# ScaleToUse <-  c("Not Applicable", "Very difficult", "Difficult", "Neutral", "Easy", "Very Easy")

##############


# Pick the type of graph you need...


# 1. Simple barchart with an x-axis in the order specified above
## Needs completed in the above: Category, Value and ScaleToUse

bar_chart <- ggplot(data, aes(x = factor(Category, ScaleToUse), y = Value)) +
  geom_bar(stat = "identity", color = "#E5F2FC") 
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")+
    theme(axis.text.x = element_text(size = axis_label_size,color = "#474747",  angle = axis_label_angle, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = axis_label_size, color = "#474747"),
          axis.title.x = element_text(size = axis_label_size, color = "#474747",  vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
          axis.title.y = element_text(size = axis_label_size,color = "#474747", vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
  #Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

# Display the bar chart
print(bar_chart)


# 2. Simple barchart with an x-axis descending order
bar_chart <- ggplot(data, aes(x = reorder(Category, -Value), y = Value)) +
  geom_bar(stat = "identity", fill = "#1D63ED") +
  labs(title = title,
       x = x,
       y = y) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size,color = "#474747",  angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size, color = "#474747"),
        axis.title.x = element_text(size = axis_label_size, color = "#474747",  vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size,color = "#474747", vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

bar_chart<-bar_chart +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  #Code to wrap text of x axis labels

# Display the bar chart
print(bar_chart)



# 3. "Nested" barchart with subgroup elements in order specified in 'ScaleToUse'

bar_chart <- ggplot(data, aes(x = Category, y = Value, fill=factor(SubCategory, ScaleToUse))) +
  geom_bar(stat = "identity", position = "dodge", color = "#CCE4EB") +
  labs(title = title,
       x = x,
       y = y) +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = axis_label_size,color = "#474747",  angle = axis_label_angle, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = axis_label_size, color = "#474747"),
        axis.title.x = element_text(size = axis_label_size, color = "#474747",  vjust = ifelse(axis_label_position %in% c("top", "bottom"), 0.5, 1)),
        axis.title.y = element_text(size = axis_label_size,color = "#474747", vjust = ifelse(axis_label_position %in% c("left", "right"), 0.5, 1)))
#Adjust the hjust & vjust parameters if you need to fine-tune the horizontal and vertical justification of the labels.

bar_chart<-bar_chart +
  labs(fill = "Key")+
  theme(legend.text = element_text(size = 12)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  #Code to wrap text of x axis labels


# Display the bar chart
print(bar_chart)


##### Useful bits of code to enhance graphs...

# Add any of the following to your chart before
# the 'print(bar_chart)' command.

 # If you need to wrap the text on your x axis. Adjust width as required...
 bar_chart<-bar_chart +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))  #Code to wrap text of x axis labels

 # If you want to specify the range of the x and y axes
 # Put in/delete your required values...
 bar_chart<-bar_chart +
 xlim(-100, 100)+
 ylim(0, 1)

# If you want to reverse the order of a legend add this...
bar_chart<-bar_chart + 
  guides(fill = guide_legend(reverse = TRUE)) 

# To specify the title of the 'legend' and the size of the legend text, add this...
bar_chart<-bar_chart +
  labs(fill = "Key")+
  theme(legend.text = element_text(size = 12))

# To make your chart vertical instead of horizontal...

bar_chart<-bar_chart +
  coord_flip()

# To create a stacked chart (position = "stack")
bar_chart <-geom_bar(stat = "identity", position = "stack")
#NB a good colour palette to use is as follows:
colors <- brewer.pal(5, "RdYlGn")  # This palette provides a good range from red (negative) to green (positive)
bar_chart <-scale_fill_manual(values = colors)

# To create a pie chart you can use the following... 
# (For guidance on how to adjust its look and feel 
# see also https://r-graph-gallery.com/piechart-ggplot2.html)

ggplot(data, aes(x="", y=Value, fill=Category)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + # remove background, grid, numeric labels
  scale_fill_brewer(palette = "Blues")
  
########################################################################

##### Code for horizontal stacked barcharts showing data labels ########

########################################################################

library(dplyr)
library(ggplot2)
library(RColorBrewer)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!! Replace with data to be used.... !!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Create sample data
set.seed(42)  # Ensures reproducibility
df <- data.frame(
  Question = rep(c("Q1: Service Quality", "Q2: Product Satisfaction", "Q3: Overall Experience"), each = 200),
  Response = factor(sample(c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), 600, replace = TRUE),
                    levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
)

# Calculate percentages and prepare for plotting
df_summary <- df %>%
  group_by(Question, Response) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Total = sum(Count), .by = "Question") %>%
  mutate(Percentage = (Count / Total) * 100) %>%
  arrange(Question, desc(Response)) %>%
  group_by(Question) %>%
  mutate(CumSum = cumsum(Percentage)) %>%
  mutate(LabelPos = CumSum - 0.5 * Percentage)


# Plot with corrected label positioning
## Specify Colours
colors <- brewer.pal(5, "RdYlGn")  # This palette provides a good range from red (negative) to green (positive)


ggplot(df_summary, aes(x = Question, y = Percentage, fill = Response)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack bars to show actual percentages
  geom_text(aes(y = LabelPos, label = sprintf("%.1f%%", Percentage)),  # Position text correctly within each segment
            color = "black", size = 3, vjust = 0.5) +  # Ensure text is visually centered
  #scale_y_continuous(labels = scales::percent) +
  labs(title = "Survey Responses by Question",
       x = "Question",
       y = "Percentage of Responses",
       fill = "Response Category") +
  ylim(0,100)+
  theme_minimal() +
  scale_fill_manual(values = colors)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()

