# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/risk_pref_filter.R")
#-------------------------------------------------------------------------------

# Conventional EO Plot by Block
#-------------------------------------------------------------------------------
dodge <- 0.25
plt_risky_blk <- ggplot(risky_res, aes(
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
    # aes(group = group),
    colour = "black",
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  geom_point(
    stat = "summary", fun = mean,
    stroke = 1.5,
    size = 5,
    position = position_dodge(width = dodge)
  ) +
  facet_wrap(~ group) +
  annotate(
    "segment",
           x = -Inf, xend = -Inf, y = -Inf, yend = Inf, 
    linewidth = 1
    ) +
  scale_colour_manual(values = c("#ff6961", "#77dd77")) +
  scale_fill_manual(values = c("#ff6961", "#77dd77")) +
  scale_shape_manual(values = 21:23) +
  scale_x_continuous(breaks = 1:6) +
  coord_cartesian(ylim = c(0, 0.8)) +
  xlab("Block") +
  ylab("p(Risky)") +
  labs(
    shape = "Choice Value:",
    fill = "Choice Value:",
    colour = "Choice Value:"
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 28),
    strip.text = element_text(size = 30),
    legend.position = "bottom"
  )


# Save Plot
ggsave("plots/exp-1a/plt_risky_blk.png",
  plot = plt_risky_blk,
  units = "in", width = 11, height = 7,
  dpi = 500
)

ggsave("plots/exp-1a/plt_risky_blk.svg",
  plot = plt_risky_blk,
  units = "in", width = 11, height = 7
)

# Barplot Showing ANOVA Results
#-------------------------------------------------------------------------------

aov_res <- risky %>%
  filter(block >= 5) %>% 
  group_by(subject, group, choice_value) %>%
  summarise(
    cp = mean(risky_resp)
  )

# Diff Scores
diffs <- aov_res %>%
  group_by(subject, group) %>%
  summarise(
    cp = cp[choice_value == "High"] - cp[choice_value == "Low"]
  )

diffs$choice_value <- rep("High-Low", nrow(diffs))
diffs <- diffs[, c(1, 2, 4, 3)]
aov_res <- rbind(aov_res, diffs)
aov_res$choice_value <- factor(aov_res$choice_value,
                               levels = c("Low", "High", "High-Low")
)

dodge <- 0.9

aov_bar <- ggplot(aov_res, aes(x = group, y = cp)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_bar(
    stat = "summary", fun = mean,
    #position='dodge',
    aes(fill = choice_value),
    colour = "black",
    linewidth = 1,
    position = position_dodge()
  ) +
  geom_errorbar(
    stat = "summary",
    aes(group = choice_value),
    #position = 'dodge',
    fun.data = "mean_cl_normal",
    fun.args = list(conf.int = .95),
    width = 0.2,
    colour = "black",
    linewidth = 1,
    position = position_dodge(width = dodge)
  ) +
  scale_fill_manual(values = c("#ff6961", "#77dd77", "#bca36d")) +
  coord_cartesian(ylim = c(0, 0.8)) +
  xlab("Group") +
  ylab("p(Risky)") +
  labs(fill = "Choice Value:") +
  #guides(fill = guide_legend(byrow = TRUE)) +
  guides(
    fill   = guide_legend(position = "bottom", byrow = TRUE)
  ) +
  theme_custom() +
  theme(
    legend.key.spacing.x = unit(0.75, 'cm')
  )

# Interaction Plot
#-------------------------------------------------------------------------------

int_res <- risky %>%
  filter(block >= 5) %>% 
  group_by(subject, group, choice_value) %>%
  summarise(
    cp = mean(risky_resp)
  )

int_res$choice_value <- factor(int_res$choice_value, levels = c("Low", "High"))

int_plot <- ggplot(int_res, aes(
  x = group, y = cp,
  group = choice_value,
  shape = choice_value,
  fill = choice_value
)) +
  geom_hline(yintercept = 0.5, linetype = 3) +
  geom_line(
    stat = "summary", fun = mean,
    aes(colour = choice_value),
    linewidth = 1
  ) +
  geom_errorbar(
    stat = "summary",
    fun.data = "mean_cl_normal",
    # fun.data = "mean_cl_boot",
    fun.args = list(conf.int = .95),
    # fun.args = list(conf.int = .95, B = 5000),
    width = 0.2,
    # aes(group = group),
    colour = "black",
    linewidth = 1) +
  geom_point(
    stat = "summary", fun = mean,
    stroke = 1.5,
    size = 5
  ) +
  scale_colour_manual(values = c("#ff6961", "#77dd77")) +
  scale_fill_manual(values = c("#ff6961", "#77dd77")) +
  scale_shape_manual(values = 21:23) +
  coord_cartesian(ylim = c(0, 0.8)) +
  xlab("Group") +
  ylab("p(Risky)") +
  labs(
    shape = "Choice Value:",
    fill = "Choice Value:",
    colour = "Choice Value:"
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(size = 22),
    axis.text.y = element_text(size = 28),
    strip.text = element_text(size = 30),
    legend.position = "bottom"
  )

# Save Plot
ggsave("plots/exp-1a/plt_risky_int.png",
       plot = int_plot,
       units = "in", width = 9, height = 7,
       dpi = 500
)

ggsave("plots/exp-1a/plt_risky_int.svg",
       plot = int_plot,
       units = "in", width = 9, height = 7
)


