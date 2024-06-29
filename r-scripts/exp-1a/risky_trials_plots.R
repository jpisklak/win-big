# Run the following 4 lines to execute this script independently
# setwd('../..') # assumes working dir is ./r-scripts/exp-1a
# source("r-scripts/prelim_code.R")
# source("r-scripts/exp-1a/subj_stats.R")
# source("r-scripts/exp-1a/risky_trials_filter.R")
#-------------------------------------------------------------------------------


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
  units = "in", width = 16, height = 9,
  dpi = 500
)

ggsave("plots/exp-1a/plt_risky_std_blks.svg",
  plot = plt_risky_std_blks,
  units = "in", width = 16, height = 9
)


# Histogram of Responses
#-------------------------------------------------------------------------------

high_res <- risky_res %>%
  filter(`choice_value` == 'High')

sort(unique(high_res$cp))

low_res <- risky_res %>%
  filter(`choice_value` == 'Low')

sort(unique(low_res$cp))

high_hist <- ggplot(high_res, aes(x = cp)) +
  geom_histogram(
    colour = 'black',
    fill = "#77dd77",
    bins = 13
    #binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))
  ) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  facet_grid(group ~ block) + 
  xlab("Choice Prop.") +
  ylab("Count") +
  ggtitle("High Value Choices") +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black', size = '12'),
    axis.title = element_text(colour = 'black', size = '12'),
    strip.text = element_text(colour = 'black', size = '12'),
    plot.title = element_text(colour = 'black', size = '16'),
    legend.position = 'none'
  )

low_hist <- ggplot(low_res, aes(x = cp)) +
  geom_histogram(
    fill = "#ff6961",
    colour = 'black',
    #binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3))
    bins = 13
  ) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  facet_grid(group ~ block) + 
  xlab("Choice Prop.") +
  ylab("Count") +
  ggtitle("Low Value Choices") +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black', size = '12'),
    axis.title = element_text(colour = 'black', size = '12'),
    strip.text = element_text(colour = 'black', size = '12'),
    plot.title = element_text(colour = 'black', size = '16'),
    legend.position = 'none'
  )