library(tidyverse)
load("data/Irish_adj.RData")
load("data/Irish_adj_train.RData")
ggplot2::theme_set(theme_bw())
cbpal <-
  RColorBrewer::brewer.pal(8, "Set2")[c(3, 4, 1, 2, 5, 6, 7, 8)]

extra <- Irish_adj$extra

# Relevel days of week
extra$dow <- extra$dow %>%
  fct_relevel("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# Add weekend indicator
extra$wknd <- fct_collapse(
  extra$dow,
  Weekend = c('Sun', 'Sat'),
  Weekday = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri')
)

# Add time for labelling
extra$Time <- format(extra$dateTime, format = "%H:%M")

# Calculate the weekly profile of each household (average demand at each tod at each dow)
avg_weekly_profile <-
  cbind(
    Irish_adj$indCons,
    Time = as.factor(extra$Time),
    dow = extra$dow,
    wknd = extra$wknd,
    tod = extra$tod
  ) %>%
  pivot_longer(cols = starts_with("I"),
               names_to = "ID",
               values_to = "demand") %>%
  group_by(tod, dow, wknd, Time) %>%
  summarise(avg_demand = mean(demand))

# Define labels for x-axis ticks
my_labels <- levels(avg_weekly_profile$Time)

### Plot average weekly profile for all households
ggplot(avg_weekly_profile) +
  aes(x = tod,
      y = avg_demand,
      colour = dow,
      lty = wknd) +
  geom_line(lwd = 0.5) +
  # geom_smooth(method="gam", se=F, lwd=0.5) +
  scale_color_manual(values = cbpal) +
  labs(x = "Time of Day",
       y = "Average Demand (kWh)",
       color = "Day of Week",
       lty = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal") +
  scale_x_continuous(labels = my_labels[seq(1, 48, 2)],
                     breaks = seq(0, 47, 2)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
