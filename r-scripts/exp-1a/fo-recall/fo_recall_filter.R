# Run the following 3 lines to execute this script independently
# setwd('../../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
#-------------------------------------------------------------------------------

fo <- data_1a %>%
  filter(
    !(subject %in% exclude$subject), # remove catch exclusions
    trial_type == "fo_judge"
  ) %>%
  select(
    -(chosen_stim:ITI),
    -(Resp0.RESP:Resp80.RT)
  )

# Clean Participant Responses
table(fo$fo_resp)

fo$fo_resp <- fo$fo_resp %>% 
  str_replace_all("[[:punct:]ENTER]", "") %>% 
  as.numeric()

# Find and remove non-real responses
  #View(fo %>% select(subject, fo_resp))

nr <- fo %>%
  group_by(subject) %>%
  summarise(
    tot = sum(fo_resp)
  ) %>%
  filter(tot < 20 | tot > 300)

fo <- fo %>% 
  filter(!(subject %in% nr$subject))

# Create value column
fo <- fo %>%
  mutate(fo_value = case_when(
    q_door_val %in% c("risky_low", "fixed_low") ~ "Low",
    q_door_val %in% c("risky_high", "fixed_high") ~ "High"
  ))

# Exclude fixed outcomes
fo <- fo %>% 
  filter(!(q_door_val %in% c("fixed_low", "fixed_high")))

# Create category column for results
fo <- fo %>%
  mutate(fo_cat = case_when(
    fo_value == "Low" & fo_resp == 0 ~ "0",
    fo_value == "Low" & fo_resp == 40 ~ "+40",
    fo_value == "High" & fo_resp == 40 ~ "+40",
    fo_value == "High" & fo_resp == 80 ~ "+80",
    fo_value == "Low" & fo_resp != c(0, 40) ~ "Other",
    fo_value == "High" & fo_resp != c(40, 80) ~ "Other"
  ))

# Proportion results
props <- fo %>%
  group_by(fo_value, group) %>%
  count(fo_cat) %>% 
  mutate(prop = n / sum(n))
