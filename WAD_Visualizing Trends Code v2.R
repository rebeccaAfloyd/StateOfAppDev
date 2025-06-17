
##################################################################
####################################################################
#########WAD Talk Data Exploration/Trend Visualization#############

#Uncomment this if you don't have the packages already
#install.packages(c("tidyverse", "lme4", "reshape2", "ggplot2"))

setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/WeAreDevelopers Talk")


library(tidyverse)
library(lme4)
library(reshape2)
library(ggplot2)
library(Polychrome)
library(colorspace)

#Load data 
df <- read.csv("Cleaned 2022-24 Data.csv") 

### Let's do some data clean up for multi-select options

##---- How Developers Like to Learn ----
# Define the target column name
target_col <- "How_Like_To_Learn"
group_cols <- c("Survey_Year")  # Can add more grouping variables here

# Step 1: Get total responses per group
group_totals <- df %>%
  distinct(Response_ID, .data[[group_cols]]) %>%
  count(.data[[group_cols]], name = "unique_respondents")

# Step 2: Count option selections per group
option_counts <- df %>%
  filter(!is.na(.data[[target_col]])) %>%
  separate_rows(all_of(target_col), sep = ";\\s*") %>%
  filter(.data[[target_col]] != "") %>%
  group_by(across(all_of(group_cols)), option = .data[[target_col]]) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Join totals and calculate percentage
option_counts <- option_counts %>%
  left_join(group_totals, by = group_cols) %>%
  mutate(percent = round(100 * count / unique_respondents, 1)) %>%
  arrange(across(all_of(group_cols)), desc(count))


#----Generate graph...----

# Set the metric you want to plot: either "count" or "percent"
y_metric <- "percent"    #"count"   or "percent"

# Plot
ggplot(option_counts, aes(x = option, y = .data[[y_metric]], fill = factor(Survey_Year))) +
  geom_col(position = "dodge") +
  labs(
    x = "Option",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Survey Year",
    title = paste("Multi-select Responses by Option and Year (", y_metric, ")", sep = "")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with Survey_year on the x-axis and options grouped within each year
ggplot(option_counts, aes(x = factor(Survey_Year), y = .data[[y_metric]], fill = option)) +
  geom_col(position = "dodge") +
  labs(
    x = "Survey Year",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Option",
    title = paste("Responses by Survey Year and Option (", y_metric, ")", sep = "")
  ) +
  theme_minimal()



##---- Languages Used ----
# Define the target column name
target_col <- "Languages_Used"
group_cols <- c("Survey_Year")  # Can add more grouping variables here

### NB Need to tidy up the delimiters first
# Replace commas with "; " in that column
df <- df %>%
  mutate(
    !!target_col := str_replace_all(.data[[target_col]], ",", "; ")
  )


# Step 1: Get total responses per group
group_totals <- df %>%
  distinct(Response_ID, .data[[group_cols]]) %>%
  count(.data[[group_cols]], name = "unique_respondents")

# Step 2: Count option selections per group
option_counts <- df %>%
  filter(!is.na(.data[[target_col]])) %>%
  separate_rows(all_of(target_col), sep = ";\\s*") %>%
  filter(.data[[target_col]] != "") %>%
  group_by(across(all_of(group_cols)), option = .data[[target_col]]) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Join totals and calculate percentage
option_counts <- option_counts %>%
  left_join(group_totals, by = group_cols) %>%
  mutate(percent = round(100 * count / unique_respondents, 1)) %>%
  arrange(across(all_of(group_cols)), desc(count))


#----Generate graph...----

# Set the metric you want to plot: either "count" or "percent"
y_metric <- "percent"    #"count"   or "percent"

# Plot
ggplot(option_counts, aes(x = option, y = .data[[y_metric]], fill = factor(Survey_Year))) +
  geom_col(position = "dodge") +
  labs(
    x = "Option",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Survey Year",
    title = paste("Multi-select Responses by Option and Year Languages Used (", y_metric, ")", sep = "")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with Survey_year on the x-axis and options grouped within each year
ggplot(option_counts, aes(x = factor(Survey_Year), y = .data[[y_metric]], fill = option)) +
  geom_col(position = "dodge") +
  labs(
    x = "Survey Year",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Option",
    title = paste("Responses by Survey Year and Option Languages Used (", y_metric, ")", sep = "")
  ) +
  theme_minimal()





##---- Frameworks Used ----
# Define the target column name
target_col <- "Frameworks_Used"
group_cols <- c("Survey_Year")  # Can add more grouping variables here

### NB Need to tidy up the delimiters first
# Replace commas with "; " in that column
df <- df %>%
  mutate(
    !!target_col := str_replace_all(.data[[target_col]], ",", "; ")
  )


# Step 1: Get total responses per group
group_totals <- df %>%
  distinct(Response_ID, .data[[group_cols]]) %>%
  count(.data[[group_cols]], name = "unique_respondents")

# Step 2: Count option selections per group
option_counts <- df %>%
  filter(!is.na(.data[[target_col]])) %>%
  separate_rows(all_of(target_col), sep = ";\\s*") %>%
  filter(.data[[target_col]] != "") %>%
  group_by(across(all_of(group_cols)), option = .data[[target_col]]) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Join totals and calculate percentage
option_counts <- option_counts %>%
  left_join(group_totals, by = group_cols) %>%
  mutate(percent = round(100 * count / unique_respondents, 1)) %>%
  arrange(across(all_of(group_cols)), desc(count))


#----Generate graph...----

# Set the metric you want to plot: either "count" or "percent"
y_metric <- "percent"    #"count"   or "percent"

# Plot
ggplot(option_counts, aes(x = option, y = .data[[y_metric]], fill = factor(Survey_Year))) +
  geom_col(position = "dodge") +
  labs(
    x = "Option",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Survey Year",
    title = paste("Multi-select Responses by Option and Year Frameworks Used (", y_metric, ")", sep = "")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with Survey_year on the x-axis and options grouped within each year
ggplot(option_counts, aes(x = factor(Survey_Year), y = .data[[y_metric]], fill = option)) +
  geom_col(position = "dodge") +
  labs(
    x = "Survey Year",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Option",
    title = paste("Responses by Survey Year and Option Frameworks Used (", y_metric, ")", sep = "")
  ) +
  theme_minimal()






##---- Where Devs get stuck in dev process ----
# Define the target column name
target_col <- "Stuck_in_Dev_Proc"
group_cols <- c("Survey_Year")  # Can add more grouping variables here

# Step 1: Get total responses per group
group_totals <- df %>%
  distinct(Response_ID, .data[[group_cols]]) %>%
  count(.data[[group_cols]], name = "unique_respondents")

# Step 2: Count option selections per group
option_counts <- df %>%
  filter(!is.na(.data[[target_col]])) %>%
  separate_rows(all_of(target_col), sep = ";\\s*") %>%
  filter(.data[[target_col]] != "") %>%
  group_by(across(all_of(group_cols)), option = .data[[target_col]]) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Join totals and calculate percentage
option_counts <- option_counts %>%
  left_join(group_totals, by = group_cols) %>%
  mutate(percent = round(100 * count / unique_respondents, 1)) %>%
  arrange(across(all_of(group_cols)), desc(count))


#----Generate graph...----
# Setting up colour palette to optimise readability of lots of options in a graph
n <- length(unique(target_col))
n<-45

# Polychrome’s algorithm: maximises pairwise colour distance in CIE L*u*v*
set.seed(123)   # reproducible, optional
pal <- createPalette(N = 45, seedcolors = palette36.colors(6)) names(pal) <- NULL # or pal <- unname(pal) ggplot(df, aes(category, value, fill = category)) + geom_col() + scale_fill_manual(values = pal)


# Set the metric you want to plot: either "count" or "percent"
y_metric <- "percent"    #"count"   or "percent"

# Plot
ggplot(option_counts, aes(x = option, y = .data[[y_metric]], fill = factor(Survey_Year))) +
  geom_col(position = "dodge") +
  labs(
    x = "Option",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Survey Year",
    title = paste("Multi-select Responses Stuck in Dev Process(", y_metric, ")", sep = "")
  ) +
  theme_minimal() +
  scale_fill_manual(values = pal, guide = "none") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot with Survey_year on the x-axis and options grouped within each year
ggplot(option_counts, aes(x = factor(Survey_Year), y = .data[[y_metric]], fill = option)) +
  geom_col(position = "dodge") +
  labs(
    x = "Survey Year",
    y = ifelse(y_metric == "count", "Number of Respondents", "Percentage of Respondents"),
    fill = "Option",
    title = paste("Responses by Survey Year and Option (", y_metric, ")", sep = "")
  ) +
  theme_minimal()+
  scale_fill_manual(values = pal, guide = "none")















#ID single select variables and multi-select - nice to have them in a list and for pre-processing multi-select below
outcome_variables <- names(df)[!(names(df) %in% c("Survey_Year", "Response_ID"))]
multiselect_vars <- outcome_variables[sapply(df[outcome_variables], function(x) any(grepl(";", x, fixed = TRUE)))]
singleselect_vars <- setdiff(outcome_variables, multiselect_vars)


#Pre-process multiselect variables - make them binary 0/1 columns
#Doing it one at a time because looping became a failure and nightmare and I am not strong enough to continue

#Define the variable from the "multiselect_vars" list you want to pre-process
var <- "Primary_Container_Use"

#Make the variable above into multiple binary columns, joined on Response ID and Year
binary_df <- df %>%
  select(Response_ID, Survey_Year, all_of(var)) %>%
  filter(!is.na(.data[[var]])) %>%
  separate_rows(all_of(var), sep = ";") %>%
  mutate(choice = trimws(.data[[var]])) %>%
  mutate(colname = paste0(var, "_", make.names(choice)),
         selected = 1) %>%
  select(Response_ID, Survey_Year, colname, selected) %>%
  pivot_wider(
    id_cols = c(Response_ID, Survey_Year),
    names_from = colname,
    values_from = selected,
    values_fill = list(selected = 0)
  )

# Merge the new binary columns back into the original dataframe
df <- left_join(df, binary_df, by = c("Response_ID", "Survey_Year"))

# Optional: remove the original multiselect column (easiest for graphing below)
df <- df %>% select(-all_of(var))

#There are columns with "_X" at the end denoting blank answer choices. Just remove em
df <- df %>% select(-matches("_X$"))



#Graph distribution of single select variables segmented by year
df %>%
  filter(!is.na(Primary_Role)) %>% ##REPLACE "Primary_Role" with the variable of interest from single select vars above
  group_by(Survey_Year, Primary_Role) %>% ##Do same here!
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = as.factor(Survey_Year), y = count, fill = Primary_Role)) + ##Do same here!
  geom_col(position = "dodge") +
  labs(title = "Trends in Primary Role by Survey Year", ##Change title of graph here
       x = "Survey Year", y = "Count of Respondents",
       fill = "Primary Role") + ##Change variable name here
  theme_minimal()


#Graph distribution of multi select variables

#first need to get proportions summarized here
df_forgraph <- df %>%
  select(Survey_Year, starts_with("Primary_Container_Use_")) %>% ##REPLACE "Primary_Container_Use_" with the variable of interest name - they all START with the same phrase, so this partial matches
  pivot_longer(cols = starts_with("Primary_Container_Use_"), names_to = "ContainerUse", values_to = "Selected") %>% ##REPLACE same as above here too, and choose an easy name for names_to
  group_by(Survey_Year, ContainerUse, Selected) %>% ##REPLACE "ContainerUse" with names_to variable here too
  summarise(count = n(), .groups = "drop")


#Now Plot multi-select variables - segmented by year
ggplot(df_forgraph, aes(x = ContainerUse, y = count, fill = as.factor(Selected))) + #REPLACE x value with names_to variable above
  geom_col(position = "stack") +
  facet_wrap(~ Survey_Year) +
  labs(title = "Container Use Selection (0 = Not Selected, 1 = Selected)",
       x = "Container Option", y = "Number of Respondents", fill = "Selected") + ##Change title and X value as desired
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
