library(readxl)
library(tidyverse)

d <- read_xlsx("data/MinmallySpeaking_SHW.xlsx") |>
  select(interview_age, nProduced, subjectkey)



