# setwd('../..') # assumes working dir is ./r-scripts/exp-1b
# source("r-scripts/prelim_code.R")
#-------------------------------------------------------------------------------


# Load data
data_1b <- read_csv("data/exp_1b_choice_complete.csv")

# Collect catch trials
ctch <- data_1b %>%
  filter(trial_type == "catch")
# names(ctch)

# Correct Choice
ctch <- ctch %>%
  mutate(corr_resp = case_when(
    door_select == "risky_high" | door_select == "fixed_high" ~ 1,
    TRUE ~ 0
  ))

# Catch Res
ctch_res <- ctch %>%
  group_by(group, subject, block) %>%
  summarise(cp = mean(corr_resp))

# Catch Exclusions
exclude <- filter(ctch_res, block == 6 & cp < 0.6)
ctch_res <- filter(ctch_res, !(subject %in% exclude$subject))
ctch_res

# Get relevant info
demo_info <- data_1b %>% 
  filter(trial == 1) %>%
  select(subject, age, gender, group)

# Stats pre catch exclusion
pre_catch <- demo_info %>%
  group_by(group, gender) %>%
  summarise(
    n = length(subject),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE)
  )

N <- sum(pre_catch$n)

# Stats post catch exclusion
demo_info_f <- filter(demo_info, !(subject %in% exclude$subject))

post_catch <- demo_info_f %>%
  group_by(group, gender) %>%
  summarise(
    n = length(subject),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE)
  )

catch_n <- nrow(exclude)

# Condition totals
cond_tot <- demo_info_f %>%
  group_by(group) %>%
  summarise(
    n = length(subject),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    age_median = median(age, na.rm = TRUE),
    age_IQR = IQR(age, na.rm = TRUE)
  )


