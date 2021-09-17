library(DBI)
library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(ggplot2)


dbDisconnect(likert_survey)
likert_survey <- dbConnect(RSQLite::SQLite(), "likert_survey.db")

dbListTables(likert_survey)

professors <- DBI::dbGetQuery(likert_survey,"SELECT * FROM Professors")
students <- DBI::dbGetQuery(likert_survey,"SELECT * FROM Students")
students <- as_tibble(students)
professors <- as_tibble(professors)

numberOfQuestionsStudents <- nrow(students)
numberOfQuestionProfessors <- nrow(professors)

numberOfQuestionsStudents
numberOfQuestionProfessors

professors_long <- professors %>%
  pivot_longer(cols = c(TotallyDisagree,Disagree,Agree,TotallyAgree),
               names_to = "Question_Answer",
               values_to = "Question_Value")
professors_long$Question_Value <- as.integer(professors_long$Question_Value)
professors_long %>%
  group_by(QuestionID) %>%
  summarise(total_answers = sum(Question_Value))

students_long <- students %>%
  pivot_longer(cols = c(TotallyDisagree,Disagree,Agree,TotallyAgree),
               names_to = "Question_Answer",
               values_to = "Question_Value")
students_long$Question_Value <- as.integer(students_long$Question_Value)
students_long %>%
  group_by(QuestionID) %>%
  summarise(total_answers = sum(Question_Value))

professors_tbl <- professors_long %>% mutate(respondant = "professor")
professors_tbl

students_tbl <- students_long %>% mutate(respondant = "student")
students_tbl

combine_tbl <- bind_rows(professors_tbl,students_tbl)
combine_tbl %>% ggplot(aes(x=ç, y=Question_Value, fill=respondant)) +
  geom_col(position="dodge") + facet_wrap(vars(QuestionID))

combine_tbl %>% select(QuestionID,count_of_response=Question_Value,type_of_response=Question_Answer,respondant) %>%
  ggplot(aes(x=type_of_response, y=count_of_response, fill=respondant)) +
  geom_col(position="dodge") + facet_wrap(vars(QuestionID))

combine_tbl %>%
    group_by(QuestionID,respondant) %>%
    summarise(max_answer = max(Question_Value)) %>%
    ungroup

combine_tbl %>%
  arrange(desc(Question_Value)) %>%
  group_by(QuestionID,respondant) %>%
  filter(row_number()==2) %>%
  arrange(desc(QuestionID))


head(combine_tbl,20) %>% arrange(desc(QuestionID))
