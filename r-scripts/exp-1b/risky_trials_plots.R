# setwd('../..') #Running this R script alone requires being in the main dir.
# source("r-scripts/exp-1b/subj_stats.R")



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
ggsave("plots/exp-1b/plt_risky_std_blks.png",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 9,
  dpi = 500
)

ggsave("plots/exp-1b/plt_risky_std_blks.svg",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 9
)

