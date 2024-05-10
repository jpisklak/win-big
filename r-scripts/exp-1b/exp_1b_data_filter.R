#setwd('../..') #Running this code in isolation requires being in the main dir.
source("r-scripts/prelim_code.R")

# Experiment 1b
#-------------------------------------------------------------------------------

# Low Value Options
# Fixed: 20 (Door 3)
# Risky: 0 or 40 (Door 4)

# High Value Options
# Fixed: 60 (Door 1)
# Risky: 40 or 80 (Door 2)

# Six blocks of 80 trials.
# Decision Trials: (fixed low vs risky low) & (fixed high vs risky high) - 24 per block (12 low, 12 high)
# Single door trials: one door to click - 40 per block
# Catch trials: high vs low - 16 per block

# Group 1: 
# Block 1: 80% chance of best outcome on risky options.
# Block 2: 50% of best outcome on risky options.
# Block 3: 20% chance of best outcome on risky options.
# Block 4-6: 50% of best outcome on risky options.

# Group 2: 
# Block 1: 20% chance of best outcome on risky options.
# Block 2: 50% of best outcome on risky options.
# Block 3: 80% chance of best outcome on risky options.
# Block 4-6: 50% of best outcome on risky options.

# Group 3 (control): 
# All blocks: 50% of best outcome on risky options.

# Load raw data
exp_1b <- read_csv('data/raw-data/ex2-data.csv') 
names(exp_1b)

# Filter choice data
exp_1b <- exp_1b %>%
  select(
    SessionDate,
    SessionTime,
    Age,
    Gender,
    Subject,
    Group,
    Block, # ePrime's blocking (e.g., treats riddle as a new block)
    `Procedure[Block]`,
    Trial, # within block
    TrialType,
    L,
    R,
    Location,
    RiskyOutcome,
    Click.Location,
    Click.RT,
    Click.Stim,
    ITI,
    RewardAmount,
    CumulativeReward
  )

# Remove riddle and questionaire
exp_1b <- exp_1b %>% filter(`Procedure[Block]` == "ExpRun")

# Order data
exp_1b <- exp_1b %>%
  arrange(Subject, Block, Trial)

# Rename subjects
exp_1b$Subject <- factor(exp_1b$Subject)
levels(exp_1b$Subject) <- 1:length(levels(exp_1b$Subject))

# Create complete trial column for each subject
trials <- vector()

for (i in 1 : length(unique(exp_1b$Subject)) ) {
  sub_trials <- 1 : nrow(filter(exp_1b, Subject == i))
  trials <- c(trials, sub_trials)
}
exp_1b$trial <- trials

# Create a Block Column
exp_1b$Block <- factor(exp_1b$Block)
levels(exp_1b$Block) <- 1:6

# Create good trial type column
exp_1b <- exp_1b %>%
  mutate(trial_type = case_when(
    TrialType == "CTProbe" ~ "catch",
    TrialType == "CProbe" ~ "decision",
    TRUE ~ 'single'
  ))

# Create door select column
exp_1b <- exp_1b %>%
  mutate(door_select = case_when(
    Click.Stim == "Door1" ~ "fixed_high",
    Click.Stim == "Door2" ~ "risky_high",
    Click.Stim == "Door3" ~ "fixed_low",
    Click.Stim == "Door4" ~ "risky_low",
  ))

# Create extreme-outcome column
exp_1b <- exp_1b %>%
  mutate(extreme_outcome = case_when(
    door_select == "risky_high" & L == 'Win80' | R == 'Win80' ~ 1,
    door_select == "risky_low" & L == 'Win0' | R == 'Win0' ~ 1,
    TRUE ~ 0
  ))

# Pull relevant columns
exp_1b <- exp_1b %>%
  select(
    Subject,
    Age,
    Gender,
    Group,
    Block,
    trial,
    Trial,
    trial_type,
    door_select,
    extreme_outcome,
    Click.Location,
    RewardAmount,
    CumulativeReward,
    ITI
  ) %>%
  rename(
    subject = Subject,
    age = Age,
    gender = Gender,
    group = Group,
    block = Block,
    block_trial = Trial,
    choice_loc = Click.Location,
    reward_amount = RewardAmount,
    cumulative_reward = CumulativeReward
  )

# Filter incomplete sessions
incomplete <- data.frame(table(exp_1b$subject))
incomplete <- incomplete %>% filter(Freq < 480)
#exp_1b_incom <- filter(exp_1b, subject == incomplete$Var1)

#exp_1b <- exp_1b %>% 
#  filter(!(subject %in% incomplete$Var1))
#exp_1b$subject <- droplevels(exp_1b$subject)

# Write data
write_csv(exp_1b, 'data/exp_1b_choice_complete.csv')
#write_csv(exp_1b_incom, 'data/exp_1b_choice_incomplete.csv')

