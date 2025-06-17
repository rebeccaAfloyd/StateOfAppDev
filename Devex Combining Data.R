############################################################################################
############## Composite Devex Data from 2022 to 2024 ######################################
#################### 15 Apr 2025 ###########################################################
############################################################################################

#install.packages("googlesheets4")
library(googlesheets4)
library(dplyr)
library(purrr)

# Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/WeAreDevelopers Talk")


# Pull in data
df2022 <-data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1-F59XQBYGXg3WiKpWcri2VOvJtoyzSCkYfHuN2ocvZ8/edit?usp=sharing"))
df2023<-data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1fWhFNkDYU_YGUufaiYAjRM5mFZ8LYMpx69ozMwORT7o/edit?usp=sharing"))
df2024<-data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1R7KVi6InNg6Nx49vgXdr5IbNRMNBcT0EpygNcRQhulQ/edit?usp=sharing"))

#Add emails back to 2022 data

# Create list of data frames
dfs <- list(df2022 = df2022, df2023 = df2023, df2024 = df2024)

# Define mapping: standard name = all possible variants across data frames
column_mapping <- list(
  A = c("A", "a", "Ay"),
  B = c("B", "b", "Bee"),
  C = c("C", "c"),
  D = c("D"),
  F = c("F", "f", "Eff"),
  K = c("K", "Kay"),
  Z = c("Z", "Zed")
)

# Function to rename and select columns according to mapping
standardize_df <- function(df, mapping) {
  # Reverse the mapping to get: old_name -> standard_name
  reverse_map <- map_dfr(mapping, ~ tibble(old = ., new = names(mapping)[map_lgl(mapping, function(x) . %in% x)])) %>%
    distinct(old, new)
  
  # Keep only columns that are in the mapping
  intersecting <- intersect(names(df), reverse_map$old)
  if (length(intersecting) == 0) return(NULL)
  
  df %>%
    select(all_of(intersecting)) %>%
    rename_with(~ reverse_map$new[match(., reverse_map$old)])
}

# Apply standardization to all data frames
standardized_dfs <- map(dfs, ~ standardize_df(.x, column_mapping)) %>%
  compact()  # remove NULLs for data frames with no common columns

# Combine into one merged data frame
merged_df <- bind_rows(standardized_dfs)

# View the result
View(merged_df)



###############################################
#NOTES FOR DATA WRANGLING#######################
#### - Watch out for " in column names
#### - DATA NEEDS TO BE CONVERTED TO LIST IN A SINGLE COLUMN
######## -- INDUSTRY 
######## -- How did you learn to code 2022 & 2023. 2024 has additional info on the AI selection option
######## -- Which of these resources have you used for online courses or certifications? Select all that apply." 2022 & 2023
######## -- Which.online.resources.did.you.use.to.learn.to.code 2022 & 2023
######## -- How do you like to learn? 2022 & 2023
######## -- What.OS.do.you.use.for.development...Select.all.that.apply for 2022 & 2023. 2024 has OS agnostic option to account for
######## -- Which.of.the.following.containerization.tools.technologies.do.you.use..if.any...Select.all.that.apply.
######## -- ...Basically, any multi-select questions from 2022 & 2023
######## -- NB 'Please rate tools for X' in 2024 survey will need some work...
######## -- 
######## -- 
######## -- 