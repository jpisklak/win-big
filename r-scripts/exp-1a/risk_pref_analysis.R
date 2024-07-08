# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/risk_pref_filter.R")
#-------------------------------------------------------------------------------


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

#-------------------------------------------------------------------------------

# Subject Data
risky_5_6 <- risky %>%
  filter(block >= 5) %>% 
  group_by(subject, group, choice_value) %>%
  summarise(
    cp = mean(risky_resp)
  )

# Factor Data
risky_5_6$subject <- factor(risky_5_6$subject)

risky_5_6$choice_value <- factor(
  risky_5_6$choice_value,
  levels = c("High", "Low")
)

# Group Comparisons
` 8020_vs_50 ` <- c(0, 1, 0)
` 2080_vs_50 ` <- c(0, 0, 1)
contrasts(risky_5_6$group) <- cbind(` 8020_vs_50 `, ` 2080_vs_50 `)

# Choice Value Comparisons
` high_vs_low ` <- c(1, -1)
contrasts(risky_5_6$choice_value) <- ` high_vs_low `

# Multilevel model
base_mod <- lme(cp ~ 1,
  random = ~ 1 | subject / choice_value,
  method = "ML", data = risky_5_6
)

  #Main effects
grp_mod <- update(base_mod, .~. + group)
value_mod <- update(grp_mod, .~. + choice_value)

  # Interactions
grp_val <- update(value_mod, .~. + group:choice_value)

# Main Effects
risky_main <- anova(base_mod, grp_mod, value_mod, grp_val)
risky_main

# Planned Comparisons
summary(grp_val)

