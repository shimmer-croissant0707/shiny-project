library(dplyr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyr)
library(googleVis)
library(DT)
library(dashboardthemes)
library(GGally)

anime1 <- read.csv("anime_info.csv", header = TRUE, na.strings = c("","NA"), stringsAsFactors = FALSE)
#users <- read.csv("user_info.csv", na.strings = c("","NA"),stringsAsFactors = FALSE, header = TRUE)

# Data Cleaning for anime dataframe
# selecting needed columns and remove NA values
anime1 <- anime1 %>% select(title, title_japanese, type, source, episodes, aired_string, duration,
                          rating, score, scored_by, members, favorites, studio, genre,
                          opening_theme, ending_theme)

#modify type
anime1$type <- as.factor(anime1$type)
levels(anime1$type)[3] = "OVA"
levels(anime1$type)[4] = "OVA"
levels(anime1$type)[5] = NA
anime1$type <- as.character(anime1$type)

#modify source
anime1$source <- as.factor(anime1$source)
levels(anime1$source)[c(1,4,12,16)] = "Manga"
levels(anime1$source)[c(2,5,7,12)] = "Light Novel"
levels(anime1$source)[3] = "Game"
levels(anime1$source)[8] = "Other"
levels(anime1$source)[7] = NA
anime1$source <- as.character(anime1$source)

# modifying aired_string (changed column name to year)
anime1$aired_string <- gsub(' to.*' ,'', anime1$aired_string)
anime1$aired_string <- as.numeric(gsub('.*, ', '', anime1$aired_string))
anime1 <- rename(anime1, year = `aired_string`)
anime1 <- rename(anime1, raters = `scored_by`)


# modifying duration
anime1$duration <- gsub(" per.*", "",anime1$duration)
anime1$duration <- gsub(" hr. ","\\1", anime1$duration)
# for convenience, we treat those animes less than a minute to one minute
anime1$duration <- gsub(".*sec.", "1 min.", anime1$duration)
anime1$duration <- as.numeric(gsub(" min.", "", anime1$duration))
anime1 <- anime1[complete.cases(anime1), ]


# modifying rating
anime1$rating <- gsub(" -.*","",anime1$rating)

# modifying studio (use tidyr::seperate_rows to split one record into several ones to get closer to better
# data presentation principles), we perform this action to genre/OP/ED as well
anime_studio <- separate_rows(anime1, studio, sep = ', ')

# modifying genre
anime_genre <- separate_rows(anime1, genre, sep = ', ')

anime_ranking <- separate_rows(anime_studio, genre, sep = ', ')

# modifying opening_theme/ending_theme
anime1 <- rename(anime1, OP = `opening_theme`)
anime1 <- rename(anime1, ED = `ending_theme`)
anime1$OP <- gsub('\\[|\\]','',anime1$OP)
anime1$ED <- gsub('\\[|\\]','',anime1$ED)


#write a function to organize OP and ED
organizeOPED = function(string){
  return(paste(gsub(".*by | \\(.*|'$", "", unlist(strsplit(string, split = ','))), collapse = ', '))
}
anime1$OP <- sapply(anime1$OP, organizeOPED)
anime1$ED <- sapply(anime1$ED, organizeOPED)
anime1$OP <- gsub("&#039;", "'", anime1$OP, fixed = TRUE)
anime1$ED <- gsub("&#039;", "'", anime1$ED, fixed = TRUE)
anime1$OP <- gsub(" featuring | [Ff]eat\\. | and | \\& ", ",", anime1$OP)
anime1$ED <- gsub(" featuring | [Ff]eat\\. | and | \\&", ",", anime1$ED)
anime_op <- separate_rows(anime1, OP, sep = ',')
anime_ed <- separate_rows(anime1, ED, sep = ',')
anime_op$OP <- gsub(".*\\d.*", NA, anime_op$OP)
anime_ed$ED <- gsub(".*\\d.*", NA, anime_ed$ED)
anime_op$OP <- gsub("^\\s+", "", anime_op$OP)
anime_ed$ED <- gsub("^\\s+", "", anime_ed$ED)

# removing all NA's and empty spaces
anime_op <- anime_op[anime_op$OP != "" & anime_op$ED != "", ]
anime_op <- anime_op[complete.cases(anime_op), ]
anime_ed <- anime_ed[anime_ed$OP != "" & anime_ed$ED != "", ]
anime_ed <- anime_ed[complete.cases(anime_ed), ]

choice <- c("score", "raters", "members", "favorites")
unique_source <- unique(anime1$source)
unique_rating <- unique(anime1$rating)
displaychoice <- c("count","average score", "average raters", "average watching", "average favorites")
displaychoice1 <- c("average per year","average score", "average raters", 
                    "average watching", "average favorites") 
 

# a function that filters outliers
is_outlier <- function(vector_) {
  return(vector_ < quantile(vector_, 0.25) - 1.5 * IQR(vector_) 
         | vector_ > quantile(vector_, 0.75) + 1.5 * IQR(vector_))
}

rows_anime1 <- nrow(anime1)
anime_top_10_genre <- anime_genre %>% group_by(genre) %>% summarise(count = n()) %>% 
  arrange(desc(count)) %>% head(10)
rows_genre <- sum(anime_top_10_genre$count)

corr_choice = c("episodes", "year", "duration", "score", "raters", "members", "favorites")

