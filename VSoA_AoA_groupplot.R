load("data/ASD_all_GM.Rdata")
load("data/NA_all_long.Rdata")
library(dplyr)
library(ggplot2)
d_ASD <- ASD_all %>%
  select(subjectkey, num_item_id, interview_age, nProduced,Produces) %>%
  unique() %>%
  mutate(group = "autistic")
d_NA <- mci_all %>%
  select(subjectkey, num_item_id, interview_age, nProduced,Produces = produced ) %>%
  unique() %>%
  mutate(group ="non-autistic")

all <- rbind(d_ASD,d_NA)


all %>%
  group_by(group,interview_age,num_item_id) %>%
  mutate(p_overlap = sum(Produces)/length(unique(subjectkey))) %>%
  select(group,interview_age,p_overlap) %>%
  unique() %>%
  ggplot(.,aes(x = interview_age, y = p_overlap))+
    geom_point()+
    facet_wrap(~group,scale="free")+
  geom_smooth(method = "lm")

