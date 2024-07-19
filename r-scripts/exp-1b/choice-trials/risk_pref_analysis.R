# Run the following 4 lines to execute this script independently
# setwd('../../..') # assumes wd is main dir.
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1b/subj_stats.R")
# source("r-scripts/exp-1b/choice-trials/risk_pref_filter.R")
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

