d <- readRDS("data/asd_na-osg-2025-05-07.rds")
load("data/ASD_longitudinal_all.Rdata")
load("data/NA_all_long.Rdata")

d %>% 
  select(subjectkey,interview_age,nproduced,group) %>% 
  unique() %>% 
  group_by(group) %>% 
  summarize(mean = mean(nproduced),
            median = median(nproduced), 
            sd = sd(nproduced)) %>% 
  as.data.frame()

all_demo <- rbind(ASD_total  %>% select(subjectkey, sex,interview_age), mci_all %>% mutate(subjectkey = as.character(subjectkey)) %>% select(subjectkey,sex,interview_age)) %>% unique()


d %>% 
  select(subjectkey,interview_age,group) %>% 
  unique() %>% 
  ungroup() %>%
  left_join(all_demo %>% ungroup() %>% select(subjectkey,sex) %>% unique(), by = "subjectkey") %>%
  unique() %>%
  group_by(group,sex) %>% 
  summarize(n = n()) %>% 
  as.data.frame()


x <- d %>% 
  select(subjectkey,interview_age,group) %>% 
  mutate(subjectkey = as.factor(subjectkey)) %>%
  filter(group == "ASD") %>%
  unique() %>% 
  ungroup() %>%
  group_by(subjectkey) %>% 
  summarize(n = n()) %>% 
  as.data.frame() %>%
  arrange(subjectkey)

mean(x$n)
