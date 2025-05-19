load("data/ASD_longitudinal_all.Rdata")
load("data/NA_all_long.Rdata")
load("data/cdi-metadata.Rdata")

library(dplyr)
library(MatchIt)
library(tidyverse)
ASD_total$group <- "ASD"

## Take care of multiple forms at the same age
ASD_total <- ASD_total %>%
  group_by(subjectkey,interview_age) %>%
  slice_max(nProduced)
## Setup data for matchit
gender_match_ASD <- ASD_total %>%
  ungroup() %>%
  select(subjectkey,sex,nProduced,group)


gender_match_NA <- mci_all %>%
  ungroup() %>%
  select(subjectkey,sex,nProduced,group)

df_all <- rbind(gender_match_ASD,gender_match_NA) %>%
  mutate(group = as.factor(group),
         group = relevel(group, ref = "NA"),
         subjeckey = as.factor(subjectkey),
         sex = as.factor(sex))

## In, Inside, and Inside/In is being counted multiple times for NA group.
## Here I am changing all cases of in and inside to the all-encompassing inside/in
## If a NA kid is reported to produce that word, just keep the TRUE produced row for that kid.
## slice_max will only keep the TRUE for produced for each word when grouped by word and subjectkey.

mci_all <- mci_all %>%
  mutate(word = ifelse(word == "inside" | word == "in", "inside/in",word)) %>%
  unique() %>%
  group_by(subjectkey,num_item_id) %>%
  slice_max(produced) %>%
  ungroup()
## Check num_item_ids ######
ids_asd <- ASD_total %>%
  ungroup() %>%
  select(num_item_id,CDI_Metadata_compatible) %>% 
  rename(word = CDI_Metadata_compatible) %>%
  unique() %>%
  arrange(num_item_id)

ids_na <- mci_all %>%
  ungroup() %>% 
  select(num_item_id,word) %>%
  unique() %>%
  arrange(num_item_id)

ids_na[duplicated(ids_na$num_item_id),]


all.equal(ids_asd,ids_na)

###########################






##############################################
## Matching males
m.out_male <- matchit(group ~ nProduced, data = df_all %>%
                        filter(sex == "M") %>%
                        select(subjectkey,nProduced,sex, group) %>%
                        unique(),
                      method = "optimal", distance = "glm", ratio = 3)
summary(m.out_male)

plot(m.out_male, type = "density", interactive = TRUE,
     which.xs = ~nProduced )

subs_M <- match.data(m.out_male)[,c("subjectkey","nProduced","sex","group" )]
####################################
## Matching females
m.out_female <- matchit(group ~ nProduced, data = df_all %>%
                          filter(sex == "F") %>%
                          select(subjectkey,nProduced,sex, group) %>%
                          unique(),
                        method = "optimal", distance = "glm", ratio = 3)
summary(m.out_female)

plot(m.out_female, type = "density", interactive = TRUE,
     which.xs = ~nProduced )

subs_F <- match.data(m.out_female)[,c("subjectkey","nProduced","sex","group" )]
#####################################
## Combining data
all_prep_matched <- rbind(as.data.frame(subs_M),as.data.frame(subs_F)) %>%
  rename("nproduced" = "nProduced")


all_prep_matched %>%
  group_by(group) %>%
  summarize(m_nprod = mean(nproduced),
            n_m = length(sex == "M"),
            n_f = length(sex == "F"))
########################################

## Check matching
ttest_d <- all_prep_matched %>%
  select(subjectkey, group, sex, nproduced) %>%
  unique() 



ttest_d %>%
  group_by(group) %>%
  summarize(mean_nprod = mean(nproduced),
            sd_prod = sd(nproduced)) %>%
  as.data.frame()
t.test(nproduced~group, data = ttest_d)


ttest_d %>%
  group_by(group,sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(prop = .[.$sex == "M",]$count/.[.$sex == "F",]$count)
chisq.test(ttest_d$group,ttest_d$sex)

#############################################

## VSOA Setup
NA_setup <- mci_all %>%
  select(group,subjectkey,interview_age,num_item_id,produced,nProduced) %>%
  mutate(group = as.factor(group),
         subjectkey = as.factor(subjectkey)) %>%
  rename("nproduced" = "nProduced")

ASD_setup <- ASD_total %>%
  select(group,subjectkey,interview_age,num_item_id,Produces,nProduced) %>%
  mutate(group = as.factor(group),
         subjectkey = as.factor(subjectkey)) %>%
  rename("produced" = "Produces",
         "nproduced" = "nProduced")

all <- rbind(NA_setup,ASD_setup) %>%
  filter(subjectkey %in% all_prep_matched$subjectkey & nproduced %in% all_prep_matched$nproduced)

VSOA <- all_prep_matched %>%
  left_join(all, by = c("subjectkey","nproduced","group"))



write_rds(VSOA, file = paste0("data/asd_na-osg-",Sys.Date(),".rds"))


