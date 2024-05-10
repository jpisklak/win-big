# setwd('../..') #Running this R script alone requires being in the main dir.
# source("r-scripts/exp-1a/subj_stats.R")

risky <- data_1a %>% 
  filter(!(subject %in% exclude$subject), # remove catch exclusions
         trial_type == 'decision')

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
risky_res$group <- factor(risky_res$group)
levels(risky_res$group) <- c('Group 1', 'Group 2', 'Group 3')
risky_res$choice_value <- factor(risky_res$choice_value,
  levels = c("Low", "High")
)

# Diff Scores
diffs <- risky_res %>%
  group_by(subject, group, block) %>%
  summarise(
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
  filter(block <= 4)


# Conventional EO Plot by Block
#-------------------------------------------------------------------------------
dodge <- 0.25
plt_risky_std_blks <- ggplot(risky_res, aes(
  x = block, y = cp,
  group = choice_value,
  shape = choice_value,
  fill = choice_value
)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    aes(colour = choice_value),
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    # fun.data = "mean_cl_boot",
    fun.args = list(conf.int = .95),
    # fun.args = list(conf.int = .95, B = 5000),
    width = 0.2,
    # aes(group = condition),
    colour = "black",
    alpha = 0.25,
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_point(
    stat = "summary", fun = mean,
    stroke = 2,
    size = 5,
    position = position_dodge(width = dodge)
  ) +
  facet_wrap(~ group) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_colour_manual(values = c("#ff6961", "#77dd77")) +
  scale_fill_manual(values = c("#ff6961", "#77dd77")) +
  scale_shape_manual(values = 21:23) +
  xlab("Block") +
  ylab("p(Risky)") +
  labs(shape = "Choice Value:",
       fill = "Choice Value:",
       colour = "Choice Value:") +
  theme_custom() +
  theme(axis.text.x = element_text(size = 20),
        legend.position = 'bottom') +
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, linewidth = 1) 

# Save Plot
ggsave("plots/exp-1a/plt_risky_std_blks.png",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 7,
  dpi = 500
)

ggsave("plots/exp-1a/plt_risky_std_blks.svg",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 7
)

