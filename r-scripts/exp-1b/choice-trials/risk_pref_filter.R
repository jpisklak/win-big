# Run the following 3 lines to execute this script independently
# setwd('../../..') # assumes working dir is ./r-scripts/exp-1b
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1b/subj_stats.R")
#-------------------------------------------------------------------------------

risky <- data_1b %>% 
  filter(!(subject %in% exclude$subject), # remove catch exclusions
         trial_type == 'decision') %>% 
  select(-(q_door_num:Resp80.RT))

risky$risky_resp <- ifelse(risky$door_select == "risky_high" |
  risky$door_select == "risky_low", 1, 0)

risky$choice_value <- ifelse(risky$door_select == "risky_high" |
  risky$door_select == "fixed_high", "High", "Low")

# Subject Data
risky_res <- risky %>%
  group_by(subject, group, block, choice_value) %>%
  summarise(
    cp = mean(risky_resp)
  )

# Factor and rename conditions
risky_res$choice_value <- factor(risky_res$choice_value,
  levels = c("Low", "High")
)

# Diff Scores
diffs <- risky_res %>%
  group_by(subject, group, block) %>%
  reframe(
    diff = cp[choice_value == "High"] - cp[choice_value == "Low"]
  )

risky_sum_stats <- risky_res %>% 
  group_by(group, block, choice_value) %>% 
  summarise(
    n = length(cp),
    mean = mean(cp),
    sd = sd(cp),
    median = median(cp),
    IQR = IQR(cp)
  ) %>% 
  filter(block > 4)
