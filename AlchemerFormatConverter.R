#########################################################################
########### Converting Alchemer Data to Single Columns per Question #####
#########################################################################

# This covers extracting 2022 and 2023 data
# It uses ';' to delimit multiselect values
# Be sure to swap out the year data etc where appropriate
#########
# The second half of the script involves taking a look up table of column names and taking
# the 3 years of spreadsheets/dfs and mapping them into a single data frame

# Start up libraries
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(googlesheets4)
library(readr)
# Set working directory in Shared Google Drive
setwd("G:/.shortcut-targets-by-id/1BhKV86qu8b_iuReHqqIDDzOt7lqML0zr/UX Research/WeAreDevelopers Talk")

#Import data
# 2022 Data
data_url<-"https://docs.google.com/spreadsheets/d/1-F59XQBYGXg3WiKpWcri2VOvJtoyzSCkYfHuN2ocvZ8/edit?usp=sharing"

#2023 Data
data_url<- "https://docs.google.com/spreadsheets/d/1fWhFNkDYU_YGUufaiYAjRM5mFZ8LYMpx69ozMwORT7o/edit?usp=drive_link"


##########
# Import data for relevant year (as per data_url)

survey_df <-data.frame(read_sheet(data_url, skip = 1, col_names = TRUE))

#For 2023 data ONLY - delete the "User.Agent" column which is where ';' is used and may confuse future analysis
survey_df <- survey_df %>% select(-"User.Agent")
survey_df <- survey_df %>% rename("Survey.year" = "X2023")

#Run for both years...

names(survey_df) <- gsub("^X\\.", "", names(survey_df))
###################################

#### 2022 Data Wrangling
known_questions <- c("What.industry.do.you.do.your.development.work.in...Check.all.that.apply..",
"How.did.you.learn.to.code..Select.all.that.apply..",
"Which.of.these.resources.have.you.used.for.online.courses.or.certifications..Select.all.that.apply..",
"Which.online.resources.did.you.use.to.learn.to.code...Please.select.all.that.apply..",
"How.do.you.like.to.learn..",
"How.do.you.find.out.about.new.tech..",
"What.OS.do.you.use.for.development..",
"Which.container.orchestration.system.s..do.you.use..",
"What.languages.and.frameworks.do.you.use...choose.all.that.apply..",
"Where.are.you.most.likely.to.get.stuck.in.the.development.process....Pick.up.to.3..",
"Where.do.you.wish.you.had.better.tools.in.the.development.process..",
"Which.developer.communities.do.you.participate.in..Check.all.that.apply..",
"Which.cloud.services.do.you.use..",
"What.data.stores.do.you.use..",
"Where.do.you.primarily.use.containers.in.your.work..")

# 
# #### 2023 Data wrangling
known_questions <-c("What.industry.does.your.company.organization.belong.to...Select.the.single.option.that.best.fits.",
                    "How.did.you.learn.to.code...Select.all.that.apply.",
                    "Which.of.these.resources.have.you.used.for.online.courses.or.certifications...Select.all.that.apply.",
                    "Which.online.resources.did.you.use.to.learn.to.code...Select.all.that.apply.",
                    "How.do.you.like.to.learn.",
                    "How.do.you.find.out.about.new.developer.tools.",
                    "What.OS.do.you.use.for.development...Select.all.that.apply.",
                    "Which.of.the.following.containerization.tools.technologies.do.you.use..if.any...Select.all.that.apply.",
                    "Which.languages.and.frameworks.do.you.use...Select.all.that.apply.",
                    "Where.do.you.or.your.team.get.stuck.in.the.development.process.most.often...Select.top.3.",
                    "Do.you.wish.you.had.better.tools.in.any.of.the.following.aspects.of.the.development.process...Select.all.that.apply.",
                    "Which.developer.communities.do.you.participate.in...Select.all.that.apply.",
                    "Which.cloud.service.s..do.you.use...Select.all.that.apply.",
                    "Which.data.store.s..do.you.use...Select.all.that.apply.",
                    "Where.do.you.primarily.use.containers.in.your.work...Select.all.that.apply.")

### Some data tidying... 

### IGNORE!!! But keeping for reference #####
### Originally tried taking the raw questions and manipulating them to match, but it transforms it lots of weird ways, so just copied and pasted the questions of interest from the imported data
# ## Tidy up questions to make them match df below...
# ### Swap spaces for '.'
# known_questions <- gsub(" ", ".", known_questions)
# ### Swap 'spaces'?' for '.'
# known_questions <- gsub("\\?", ".", known_questions)
### Also to swap for '.' are '(', ')', ''', '/'

### Here's some code to export the df headings so you can copy and paste the correct titles into 'known_questions' above.
# # Convert column names to a data frame (one name per row)
# colnames_df <- data.frame(Column_Names = names(survey_df))
# 
# # Export to CSV
# write.csv(colnames_df, "column_names.csv", row.names = FALSE)
# 
# ## Tidy up df to remove leading 'X.'
# # Assume your dataframe is called `survey_df`
# names(survey_df) <- gsub("^X\\.", "", names(survey_df))



# --- Step 1: Identify columns related to known_questions ---
matching_info <- map_dfr(known_questions, function(q) {
  matching_cols <- names(survey_df)[str_ends(names(survey_df), fixed(q))]
  tibble(question = q, col_name = matching_cols)
})

# Safety check
# Count how many questions were expected vs. matched
expected_n <- length(known_questions)
matched_n <- length(unique(matching_info$question))

cat("Expected questions to process: ", expected_n, "\n")
cat("Found matching questions in data: ", matched_n, "\n")

if (expected_n != matched_n) {
  stop("❌ Mismatch between expected and found questions. Check 'known_questions' and column naming.")
} else {
  message("✅ All expected questions matched successfully.")
}


missing_questions <- setdiff(known_questions, unique(matching_info$question))
if (length(missing_questions) > 0) {
  cat("Missing questions:\n")
  print(missing_questions)
}

# --- Step 2: Separate out non-multiselect columns ---
cols_to_remove <- matching_info$col_name
non_multiselect_df <- survey_df[, !names(survey_df) %in% cols_to_remove, drop = FALSE]

# --- Step 3: Collapse multiselect columns ---
# NB WE ARE USING '; ' INSTEAD OF , TO DELIMIT VALUES (TAB RESULTED IN VALUES BEING SPREAD ACROSS COLUMS IN GOOGLESHEETS)

collapsed_list <- matching_info %>%
  group_by(question) %>%
  summarise(
    collapsed = list({
      cols <- col_name
      df_subset <- survey_df[, cols, drop = FALSE]
      
      # Clean and collapse per rowi865
      apply(df_subset, 1, function(row) {
        values <- trimws(row)
        values <- values[values != "" & !is.na(values)]
        paste(values, collapse = "; ")
      })
    }),
    .groups = "drop"
  )

# --- Step 4: Convert collapsed results to dataframe ---
collapsed_df <- collapsed_list$collapsed %>%
  setNames(collapsed_list$question) %>%
  as_tibble()

# --- Step 5: Combine everything back together ---
final_df <- bind_cols(non_multiselect_df, collapsed_df)

# --- Done! ---
#print(final_df)

# # Export to CSV if required...
# Write your data frame to a CSV file
# >>>>>> Match file name to year as appropriate
write_csv(final_df, "2022 Collapsed Data.csv")
# 
write_csv(final_df, "2023 Collapsed Data.csv")

##################################################################
##################################################################
##################################################################
############## Combine 3 years of data into 1 dataframe ##########
##################################################################
##################################################################

### >>> ASSUMPTION!!!! ONLY PULLING IN COLUMNS OF INTEREST ACROSS THE 3 YEARS WITH THIS CODE <<<###
column_map_url<-"https://docs.google.com/spreadsheets/d/1wJ5GCJjPSyPeWzXAdgwlqyi7WiGjCXtfTCLRBuZ5tVs/edit?usp=drive_link"
column_map_df <- data.frame(read_sheet(column_map_url, col_names = TRUE))


# Import 2024 Data
data_url_2024<-"https://docs.google.com/spreadsheets/d/1R7KVi6InNg6Nx49vgXdr5IbNRMNBcT0EpygNcRQhulQ/edit?usp=drive_link"
data_2024 <-data.frame(read_sheet(data_url_2024, skip = 1, col_names = TRUE))





# --- FUNCTION: Select and Rename Mapped Columns ---
### Version with logging to figure out where mismatch in column names is occuring

extract_mapped_columns <- function(df, year, map_df, output_dir = ".") {
  year_col <- paste0("colname_", year)
  year_map <- map_df[[year_col]]
  std_names <- map_df$standard_name
  
  # Build mapping vector
  mapping_table <- tibble(
    standard_name = std_names,
    original_col  = year_map,
    in_data       = year_map %in% colnames(df)
  )
  
  # Save mapping result to CSV
  summary_file <- file.path(output_dir, paste0("mapped_summary_", year, ".csv"))
  write.csv(mapping_table, summary_file, row.names = FALSE)
  message("✔️  Mapping summary saved to: ", summary_file)
  
  # Filter valid columns
  valid_map <- mapping_table %>%
    filter(in_data, !is.na(original_col))
  
  if (nrow(valid_map) == 0) {
    warning(paste("No valid mapped columns found for year", year))
    return(data.frame(Year = year))  # avoid crashing on bind_rows
  }
  
  df_selected <- df %>%
    select(all_of(valid_map$original_col)) %>%
    rename_at(vars(all_of(valid_map$original_col)), ~ valid_map$standard_name) %>%
    mutate(Year = year)
  
  return(df_selected)
}


# --- LOAD AND PROCESS EACH YEAR ---
data_2022 <- read.csv("2022 Collapsed Data.csv") 
#colnames(data_2022) <- trimws(colnames(data_2022))


data_2022 <- data_2022%>%
  extract_mapped_columns(2022, column_map_df)

data_2023 <- read.csv("2023 Collapsed Data.csv") 
#rename 'Year' column appropriately
#data_2023 <- data_2023 %>% rename("Survey.year" = "Survey.Year")

data_2023 <- data_2023  %>%
  extract_mapped_columns(2023, column_map_df)

data_2024 <- data_2024 %>%
extract_mapped_columns(2024, column_map_df)

#Code here to convert ', ' to ';' in the relevant columns...
# Define which columns in 2024 contain comma-separated selections
multiselect_columns_2024 <- as.character(column_map_df$standard_name[9:nrow(column_map_df)])

# Convert commas to "; " in those columns
data_2024 <- data_2024 %>%
  mutate(across(all_of(multiselect_columns_2024), ~ gsub(",\\s*", "; ", .)))

#Have to coerce data types to be consistent before binding

data_2022 <- data_2022 %>% mutate(Response_ID = as.character(Response_ID))
data_2023 <- data_2023 %>% mutate(Response_ID = as.character(Response_ID))
data_2024 <- data_2024 %>% mutate(Response_ID = as.character(Response_ID))

# --- COMBINE ALL YEARS (ONLY MAPPED COLUMNS) ---
combined_mapped_data <- bind_rows(data_2022, data_2023, data_2024)



# --- SAVE OR VIEW ---
write.csv(combined_mapped_data, "mapped_survey_data_2022_2024.csv", row.names = FALSE)




##########################################################################################
##########################################################################################

### >>> ASSUMPTION!!!! PULLING IN COLUMNS THAT OVERLAP AND COMBINING ALL ACROSS THE 3 YEARS WITH THIS CODE <<<###

########## >>>>> NB Code not yet tested <<<<< #############################################


# # --- LOAD LIBRARIES ---
# library(readxl)
# library(readr)
# library(dplyr)
# library(tidyr)
# library(stringdist)
# library(purrr)
# 
# # --- LOAD DATASETS ---
# data_2021 <- read_excel("survey_2021.xlsx") %>% mutate(Year = 2021)
# data_2022 <- read_excel("survey_2022.xlsx") %>% mutate(Year = 2022)
# data_2023 <- read_excel("survey_2023.xlsx") %>% mutate(Year = 2023)
# 
# # --- LOAD COLUMN MAPPING ---
# column_map_df <- read_csv("column_mapping.csv")
# 
# # --- FUNCTION TO APPLY COLUMN MAPPING ---
# map_columns <- function(df, year, map_df) {
#   year_col <- paste0("colname_", year)
#   name_lookup <- map_df[[year_col]]
#   names(name_lookup) <- map_df$standard_name
#   name_lookup <- name_lookup[!is.na(name_lookup)]
#   
#   col_renamed <- setNames(names(name_lookup), name_lookup)
#   df %>%
#     rename(any_of(col_renamed))
# }
# 
# # --- APPLY MAPPING TO EACH YEAR'S DATA ---
# data_2021 <- map_columns(data_2021, 2021, column_map_df)
# data_2022 <- map_columns(data_2022, 2022, column_map_df)
# data_2023 <- map_columns(data_2023, 2023, column_map_df)
# 
# # --- OPTIONAL: Fuzzy Matching Suggestions for Unmapped Columns ---
# 
# get_fuzzy_matches <- function(set1, set2, threshold = 0.3) {
#   expand.grid(set1, set2, stringsAsFactors = FALSE) %>%
#     rename(col1 = Var1, col2 = Var2) %>%
#     mutate(dist = stringdist::stringdist(col1, col2, method = "jw")) %>%
#     filter(dist <= threshold) %>%
#     arrange(dist)
# }
# 
# # Identify unmatched columns in each dataset
# mapped_2021 <- na.omit(column_map_df$colname_2021)
# mapped_2022 <- na.omit(column_map_df$colname_2022)
# mapped_2023 <- na.omit(column_map_df$colname_2023)
# 
# unmapped_2021 <- setdiff(colnames(data_2021), c(mapped_2021, "Year"))
# unmapped_2022 <- setdiff(colnames(data_2022), c(mapped_2022, "Year"))
# unmapped_2023 <- setdiff(colnames(data_2023), c(mapped_2023, "Year"))
# 
# # Suggest fuzzy matches
# suggest_21_22 <- get_fuzzy_matches(unmapped_2021, unmapped_2022)
# suggest_22_23 <- get_fuzzy_matches(unmapped_2022, unmapped_2023)
# suggest_21_23 <- get_fuzzy_matches(unmapped_2021, unmapped_2023)
# 
# # View suggestions
# print("Suggested matches 2021 vs 2022:")
# print(suggest_21_22)
# 
# print("Suggested matches 2022 vs 2023:")
# print(suggest_22_23)
# 
# print("Suggested matches 2021 vs 2023:")
# print(suggest_21_23)
# 
# # --- COMBINE ALL YEARS ---
# combined_data <- bind_rows(data_2021, data_2022, data_2023)
# 
# # --- OPTIONAL: Save to file ---
# write.csv(combined_data, "combined_survey_data.csv", row.names = FALSE)

