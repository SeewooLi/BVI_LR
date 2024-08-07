library(tidyverse)
library(xlsx)
library(jsonlite)

source("automated_literature_review.R")

# import data
data <- xlsx::read.xlsx("savedrecs.xls", sheetIndex = 1)

# filter columns
data_reduced <- data %>%
  select(Authors, Author.Full.Names, Article.Title, Source.Title, Document.Type, Conference.Title, Conference.Date, Conference.Location, Author.Keywords, Abstract, Affiliations, Publisher, Publication.Date, Publication.Year, Volume, Issue, Start.Page, End.Page) %>%
  filter(Abstract != "")

# questions
r_questions <- "
1. [Yes or No] Is the paper about blindness and visually impaired students (BVI)?
2. [Accommodation or not] Does this paper describe accommodation for students with BVI?
3. [Description of the Tech] What is the accommodation/technology about?
4. [learning vs assessment and classroom vs large scale] Where is this accommodation used?
5. Which grades or levels of education (e.g., elementary, middle school, high school, college, professional, post-graduate)?
6. What’s the sample size of the participants in the accomodation?
7. What country and/or state is the assessment used in?
8. In which subject or area is this accommodation applied to?
9. Is the accommodation actually being used? By how many people?
"

# implementation
df_answers <- NULL
for(n in 1:100){
  bib_data <- data_reduced[n,] %>%
    select(Article.Title, Abstract) %>%
    toJSON(pretty = TRUE)

  temp <- labelling(r_questions, bib_data)
  temp <- fromJSON(temp)
  temp <- unlist(temp, use.names = FALSE)

  df_answers <- rbind(df_answers,unlist(temp, use.names = FALSE))
}

df_out <- cbind(data_reduced[1:100,] %>% select(Author.Full.Names, Article.Title, Publication.Year, Abstract), df_answers)

colnames(df_out)[5:13] <- paste("Answer", 1:9, sep = ".")

df_out %>%
  write.csv("BVI_literature_review_table.csv", row.names = FALSE)

################################################################################
data2 <- read.csv("BVI_literature_review_table.csv")

data2$Answer.3
