library(tidyverse)
library(readxl)

school_grade <- read_excel("school-grade.xlsx", skip=6) %>%
  arrange(SCHOOL_NAME)
public_school <- read.csv("Public_Schools.csv")

school <- public_school %>%
  left_join(school_grade, by=c("SCH_NAME"="SCHOOL_NAME"))
