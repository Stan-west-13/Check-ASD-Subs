
## ISSUE WITH VSOAS WAS THERE WERE DUPLICATED WORDS AND GESTURES ENTRIES THAT WERE NOT FILTERED OUT
## THERE ARE INDIVIDUAL IDS ASSOCIATED WITH DUPLICATED DATA, SO LOOK OUT FOR THAT IN THE FUTURE. 


old <- readRDS("data/asd_na-osg-2023_11_20.rds")
new <- readRDS("data/asd_na-osg-2025-04-24.rds")
library(dplyr)
library(tidyr)
nrow(new)


new2 <- unique(new)

nrow(new) - nrow(new2)


write_rds(new2,"data/asd_na-osg-2025-05-07.rds")

all_nonfilt <- rbind(old %>% mutate(data_orig = "old"), new %>% mutate(data_orig = "new"))

all_nonfilt %>%
  select(subjectkey, interview_age, group,data_orig, nproduced) %>%
  unique() %>%
  group_by(data_orig,group) %>%
  summarize(med = median(nproduced)) %>%
  as.data.frame()





new <- new %>%
  filter(subjectkey %in% old$subjectkey) %>%
  group_by(subjectkey) 

true_produced_new <- new %>% 
  group_by(subjectkey,interview_age) %>%
  filter(produced == TRUE, group == "ASD") %>%
  mutate(data_orig = "new") %>%
  arrange(subjectkey, nproduced)

duped <- true_produced_new %>%
  group_by(subjectkey,interview_age) %>%
  filter(duplicated(num_item_id)) 


true_produced_old <- old %>% 
  group_by(subjectkey,interview_age) %>%
  filter(produced == TRUE, group == "ASD") %>%
  mutate(data_orig= "old") %>%
  arrange(subjectkey)

all <- rbind(true_produced_new,true_produced_old) 

all %>%
  group_by(subjectkey,interview_age)
  



new2 %>%
  select(subjectkey,interview_age,num_item_id,group,nproduced,produced) %>%
  filter(produced == TRUE, group == "ASD") %>%
  View()




x <- true_produced_new %>%
  select(subjectkey,interview_age,nproduced) %>%
  unique() %>%
  group_by(subjectkey) %>%
  summarize(n = length(interview_age)) %>%
  arrange(subjectkey) %>%
  mutate(data_orig = "new")


y <- true_produced_old %>%
  select(subjectkey,interview_age,nproduced) %>%
  unique() %>%
  group_by(subjectkey) %>%
  summarize(n = length(interview_age)) %>%
  arrange(subjectkey)%>%
  mutate(data_orig = "old")

z <- rbind(x,y) %>%
  pivot_wider(names_from = data_orig,
              values_from = n) %>%
  mutate(diff = abs(new - old))




