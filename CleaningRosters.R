library(dplyr)
#install.packages("stringdist")
library(stringdist)
library(tidyr)
library(stringr)


rosters <- read.csv("~/Downloads/front_rosters23.csv", stringsAsFactors = FALSE)
frontier_hitters <- read.csv("~/Downloads/frontier_all_hitters23 (1).csv", stringsAsFactors = FALSE)
frontier_pitchers <- read.csv("~/Downloads/frontier_all_pitchers23.csv", stringsAsFactors = FALSE)
clean_yakker <- read.csv("~/Downloads/clean_yakker23 (1).csv", stringsAsFactors = FALSE)


# Function to clean and standardize names
clean_name <- function(name) {
  name <- tolower(name)  # Convert to lowercase
  name <- str_remove(name, "[[:punct:]]")  # Remove punctuation
  name <- str_squish(name)  # Remove extra whitespaces
  name <- str_replace_all(name, "(?i)iii$", "")  # Remove 'III' suffix (case-insensitive)
  name <- str_replace_all(name, "(?i)jr$", "")  # Remove 'Jr' suffix (case-insensitive)
  return(name)
}

# Get unique player names from the clean_yakker data frame
unique_batters <- unique(clean_yakker$Batter)
unique_pitchers <- unique(clean_yakker$Pitcher)
unique_catchers <- unique(clean_yakker$Catcher)

# Clean and standardize unique player names
cleaned_batters <- clean_name(unique_batters)
cleaned_pitchers <- clean_name(unique_pitchers)
cleaned_catchers <- clean_name(unique_catchers)

# Create a data frame to store the changes for each category separately
changed_batters <- data.frame(
  OriginalName = unique_batters,
  CleanedName = cleaned_batters
)

changed_pitchers <- data.frame(
  OriginalName = unique_pitchers,
  CleanedName = cleaned_pitchers
)

changed_catchers <- data.frame(
  OriginalName = unique_catchers,
  CleanedName = cleaned_catchers
)

# Identify the changed names for each category
changed_batter_names <- changed_batters[changed_batters$OriginalName != changed_batters$CleanedName, "OriginalName"]
changed_pitcher_names <- changed_pitchers[changed_pitchers$OriginalName != changed_pitchers$CleanedName, "OriginalName"]
changed_catcher_names <- changed_catchers[changed_catchers$OriginalName != changed_catchers$CleanedName, "OriginalName"]

# Combine all changed names into one list
all_changed_names <- c(changed_batter_names, changed_pitcher_names, changed_catcher_names)

# Display the list of changed names
print(all_changed_names)























# clean_yakker %>%  mutate( across(c(Pitcher, Batter, Catcher), ~str_replace(., "Ray Zuberer Iii, Ray Zuberer")))
# 
# unique_batters <- unique(clean_yakker$Batter)
# unique_pitchers <- unique(clean_yakker$Pitcher)
# unique_catchers <- unique(clean_yakker$Catcher)
# 
# # Mismatches
# mismatched_batters <- setdiff(clean_yakker$Batter, unique_batters)
# mismatched_pitchers <- setdiff(clean_yakker$Pitcher, unique_pitchers)
# mismatched_catchers <- setdiff(clean_yakker$Catcher, unique_catchers)
# 
# clean_yakker_corrected <- clean_yakker %>%
#   mutate(
#     Batter = ifelse(Batter %in% mismatched_batters, gsub(paste(mismatched_batters, collapse = "|"), paste(unique_batters, collapse = "|"), Batter, ignore.case = TRUE), Batter),
#     Pitcher = ifelse(Pitcher %in% mismatched_pitchers, gsub(paste(mismatched_pitchers, collapse = "|"), paste(unique_pitchers, collapse = "|"), Pitcher, ignore.case = TRUE), Pitcher),
#     Catcher = ifelse(Catcher %in% mismatched_catchers, gsub(paste(mismatched_catchers, collapse = "|"), paste(unique_catchers, collapse = "|"), Catcher, ignore.case = TRUE), Catcher)
#   )
# 
# 
# library(dplyr)
# library(stringdist)
# library(RecordLinkage)
# 
# # Standardize the name "Iii" to "III" in the "Batter" column
# clean_yakker_corrected$Batter <- gsub("Iii", "III", clean_yakker_corrected$Batter, ignore.case = TRUE)
# 
# # Combine similar names in the "Batter" column (case-insensitive)
# clean_yakker_corrected <- clean_yakker_corrected %>%
#   group_by(Batter) %>%
#   summarize(Catcher = first(Catcher), Pitcher = first(Pitcher))
# 
# # Calculate the Jaccard similarity between names in the "Batter" column
# name_similarities <- stringdist::stringdistmatrix(clean_yakker_corrected$Batter, clean_yakker_corrected$Batter, method = "jaccard")
# 
# # Convert the Jaccard similarity matrix to a data frame
# similarity_df <- as.data.frame(as.table(1 - name_similarities))
# 
# # Use a threshold to consider names as similar and group them together
# similar_name_threshold <- 0.85  # Adjust this threshold as needed
# 
# # Get the indices of similar names and group them
# similar_names <- which(similarity_df$value > similar_name_threshold & similarity_df$Var1 != similarity_df$Var2, arr.ind = TRUE)
# 
# # Combine similar names
# for (i in 1:nrow(similar_names)) {
#   row_idx <- similar_names[i, "Var1"]
#   col_idx <- similar_names[i, "Var2"]
#   
#   # Check if we have similar names to combine
#   if (length(row_idx) > 0 && length(col_idx) > 0) {
#     # Combine similar names (case-insensitive)
#     clean_yakker_corrected$Batter[row_idx] <- clean_yakker_corrected$Batter[col_idx]
#   }
# }
# 
# # Remove duplicate rows
# clean_yakker_corrected <- clean_yakker_corrected %>%
#   distinct()
# 
# # Count occurrences of each unique name in each column
# name_counts <- clean_yakker_corrected %>%
#   gather(Column, Name, Batter, Catcher, Pitcher) %>%
#   group_by(Column, Name) %>%
#   count() %>%
#   ungroup()
# 
# # View the result
# print(name_counts)
# 
# 
# 
# 
# 
# 
# # Function
# # fuzzy_match <- function(df1, df2, name_column) {
# #   matched_names <- vector("list", nrow(df1))
# #   
# #   for (i in 1:nrow(df1)) {
# #     name1 <- df1[[name_column]][i]
# #     matches <- stringdist::stringdistmatrix(name1, df2[[name_column]])
# #     closest_match <- df2[which.min(matches), ][[name_column]]
# #     matched_names[[i]] <- c(name1, closest_match)
# #   }
# #   
# #   return(as.data.frame(do.call(rbind, matched_names)))
# # }
# # 
# # # Desired column names from clean yakker
# # clean_yakker_matches_hitter <- clean_yakker %>%
# #   select(CleanYakker = Pitcher, FrontierHittersMatch = Batter)
# # 
# # #Fuzzy match with hitters
# # clean_yakker_matches_hitter <- fuzzy_match(clean_yakker_matches_hitter, frontier_hitters, "FrontierHittersMatch")
# # 
# # # Rename the columns to match
# # colnames(clean_yakker_matches_hitter) <- c("CleanYakker", "FrontierHittersMatch")
# # 
# # # Repeat for pitchers
# # clean_yakker_matches_pitcher <- clean_yakker %>%
# #   select(CleanYakker = Pitcher, FrontierPitchersMatch = Batter)
# # 
# # clean_yakker_matches_pitcher <- fuzzy_match(clean_yakker_matches_pitcher, frontier_pitchers, "FrontierPitchersMatch")
# # 
# # colnames(clean_yakker_matches_pitcher) <- c("CleanYakker", "FrontierPitchersMatch")
# # 
# # # Join hitters and pitchers
# # clean_yakker_matches <- full_join(clean_yakker_matches_hitter, clean_yakker_matches_pitcher, by = "CleanYakker")
# # 
# # # Fuzzy match the roster data and pitchers and hitters
# # clean_yakker_matches <- fuzzy_match(clean_yakker_matches, rosters, "FrontierPitchersMatch")
# # 
# # # Repeat
# # colnames(clean_yakker_matches) <- c("CleanYakker", "FrontierHittersMatch", "FrontierPitchersMatch", "RostersMatch")
# # 
# # # Check for inconsistencies where the matches are not consistent across data frames
# # #inconsistent_matches <- filter(clean_yakker_matches, CleanYakker != RostersMatch)
# # 
# # # View the inconsistencies
# # #print(inconsistent_matches)
# # 
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# # 
# # 
# # 
# # 
# # 
# # # Perform fuzzy matching with Clean Yakker and the other data frames
# # clean_yakker_matches <- fuzzy_match(clean_yakker, frontier_hitters, "NAME")
# # colnames(clean_yakker_matches) <- c("CleanYakker", "FrontierHittersMatch")
# # 
# # clean_yakker_matches <- fuzzy_match(clean_yakker_matches, frontier_pitchers, "FrontierHittersMatch")
# # colnames(clean_yakker_matches) <- c("CleanYakker", "FrontierHittersMatch", "FrontierPitchersMatch")
# # 
# # clean_yakker_matches <- fuzzy_match(clean_yakker_matches, rosters, "FrontierPitchersMatch")
# # colnames(clean_yakker_matches) <- c("CleanYakker", "FrontierHittersMatch", "FrontierPitchersMatch", "RostersMatch")
# # 
# # # Check for inconsistencies where the matches are not consistent across data frames
# # inconsistent_matches <- filter(clean_yakker_matches, CleanYakker != RostersMatch)
# # 
# # # View the inconsistencies
# # print(inconsistent_matches)
