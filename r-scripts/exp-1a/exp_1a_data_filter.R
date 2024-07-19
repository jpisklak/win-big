# setwd('../..') # Running this code in isolation requires being in the main dir.
source("r-scripts/prelim_code.R")

# Experiment 1a
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
# Block 1: 80% extreme values (0 low, 80 high)
# Block 2: 50% extreme values.
# Block 3: 20% extreme values (0 low, 80 high)
# Block 4-6: Block 2: 50% extreme values.

# Group 2: 
# Block 1: 20% extreme values (0 low, 80 high)
# Block 2: 50% extreme values.
# Block 3: 80% extreme values (0 low, 80 high)
# Block 4-6: Block 2: 50% extreme values.

# Group 3 (control): 
# All blocks: 50% extreme values.


# Load raw data
exp_1a <- read_csv('data/raw-data/ex1-data.csv') 
names(exp_1a)

# Filter choice data
exp_1a <- exp_1a %>%
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
    Click.Stim,
    Click.Location,
    Click.RT,
    ITI,
    RewardAmount,
    CumulativeReward,
      # Questionaire
    Door,
    AskFirstSlide.RESP,
    AskFirstSlide.RT,
    (Resp0.RESP:Resp80.RT)
  ) %>%
  rename(
    session_date = SessionDate,
    session_time = SessionTime,
    subject = Subject,
    age = Age,
    gender = Gender,
    group = Group,
    block = Block,
    procedure = `Procedure[Block]`,
    block_trial = Trial,
    chosen_stim = Click.Stim,
    click_RT = Click.RT,
    choice_loc = Click.Location,
    reward_amount = RewardAmount,
    cumulative_reward = CumulativeReward,
    q_door_num = Door,
    fo_resp = AskFirstSlide.RESP,
    fo_rt = AskFirstSlide.RT
  )

# Better date and time column
date <- paste(exp_1a$session_date, exp_1a$session_time, sep = " ")

exp_1a$session_date <- as.POSIXct(date,
  format = "%m-%d-%Y %H:%M:%OS",
  tz = "America/Edmonton"
)

# Remove riddle
exp_1a <- exp_1a %>% filter(procedure %in%
  c("ExpRun", "QFirstOutcome", "QPercentage"))

# Order data
exp_1a <- exp_1a %>%
  arrange(session_date, subject, block, block_trial)

# Rename subjects
exp_1a$subject <- factor(exp_1a$subject)
levels(exp_1a$subject) <- 1:length(levels(exp_1a$subject))

# Create complete trial column for each subject
trials <- vector()

for (i in 1 : length(unique(exp_1a$subject)) ) {
  sub_trials <- 1 : nrow(filter(exp_1a, subject == i))
  trials <- c(trials, sub_trials)
}
exp_1a$trial <- trials

# Create a Block Column
exp_1a$block <- factor(exp_1a$block)
levels(exp_1a$block) <- 1:8

# Create good trial type column
exp_1a <- exp_1a %>%
  mutate(trial_type = case_when(
    procedure == "ExpRun" & TrialType == "CTProbe" ~ "catch",
    procedure == "ExpRun" & TrialType == "CProbe" ~ "decision",
    procedure == "ExpRun" & is.na(TrialType) ~ "single",
    procedure == "QFirstOutcome" ~ "fo_judge",
    procedure == "QPercentage" ~ "freq_judge"
  ))

# Create door select column
exp_1a <- exp_1a %>%
  mutate(door_select = case_when(
    chosen_stim == "Door1" ~ "fixed_high",
    chosen_stim == "Door2" ~ "risky_high",
    chosen_stim == "Door3" ~ "fixed_low",
    chosen_stim == "Door4" ~ "risky_low",
  ))

# Create extreme-outcome column
exp_1a <- exp_1a %>%
  mutate(extreme_outcome = case_when(
    door_select == "risky_high" & L == 'Win80' | R == 'Win80' ~ 1,
    door_select == "risky_low" & L == 'Win0' | R == 'Win0' ~ 1,
    TRUE ~ 0
  ))

# Filter incomplete sessions
incom <- exp_1a %>% 
  group_by(subject) %>% 
  count(procedure)

ct_incom <- incom %>% filter(procedure == "ExpRun" & n < 480)
fo_incom <- incom %>% filter(procedure == "QFirstOutcome" & n < 4)
fj_incom <- incom %>% filter(procedure == "QPercentage" & n < 4)

exp_1a <- exp_1a %>% filter(!(subject %in% ct_incom$subject))

# Rename group levels
exp_1a$group <- factor(exp_1a$group, levels = c(3, 1, 2))
levels(exp_1a$group) <- c("EX 50", "EX 80-20", "EX 20-80")

# Questionaire door names
exp_1a <- exp_1a %>%
  mutate(q_door_val = case_when(
    q_door_num == 1 ~ "fixed_high", # 60
    q_door_num == 2 ~ "risky_high", # 40 or 80
    q_door_num == 3 ~ "fixed_low",  # 20
    q_door_num == 4 ~ "risky_low",  # 0 or 40
  ))

# Pull relevant columns
exp_1a <- exp_1a %>%
  select(
    session_date,
    age,
    gender,
    subject,
    group,
    trial,
    block,
    block_trial,
    trial_type,
    chosen_stim,
    choice_loc,
    click_RT,
    door_select,
    extreme_outcome,
    reward_amount,
    cumulative_reward,
    ITI,
    q_door_num,
    q_door_val,
    fo_resp,
    fo_rt,
    (Resp0.RESP:Resp80.RT)
  )

# Write data
write_csv(exp_1a, 'data/exp_1a_data.csv')


