#Load necessary packages
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(ez)

#Read in the full data on election results
data_election_results = list.files(path = "data/elections", full.names = T) %>%
  map(read.table, header = T, sep = "\t") %>%
  reduce(rbind)

# Read in extra data about specific elections
data_elections = read.table("data/rcourse_lesson5_data_elections.txt", header=T, sep="\t")

# Read in extra data about specific states
data_states = read.table("data/rcourse_lesson5_data_states.txt", header=T, sep="\t")

# See how many states in union versus confederacy
xtabs(~civil_war, data_states)


## CLEAN DATA ##

# Make data set balanced for Union and Confederacy states
data_states_clean = data_states %>%
  filter(!is.na(civil_war))

#Only include the first 11 states that joined the Union
data_states_clean = data_states %>%
  filter(!is.na(civil_war)) %>%
  group_by(civil_war) %>%
  arrange(order_enter) %>%
  filter(row_number() <= 11) %>%
  ungroup()

# Double check balanced for 'civil_war' variable
xtabs(~civil_war, data_states_clean)

# Combine three data frames
data_clean = data_election_results %>%
  inner_join(data_elections) %>%
  inner_join(data_states_clean) %>%
  mutate(state = factor(state))

# Double check all of numbers are balanced
xtabs(~incumbent_party+civil_war, data_clean)


## ORGANIZE DATA ##
data_figs = data_clean %>%
  mutate(civil_war = factor(civil_war,
                            levels = c("union", "confederacy"),
                            labels = c("Union", "Confederacy"))) %>%
  mutate(incumbent_party = factor(incumbent_party,
                                  levels = c("democrat", "republican"),
                                  labels = c("Democrat", "Republican")))

# Average data over years but not states
data_figs_state_sum = data_figs %>%
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean =
              mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Data averaged over year and states for barplot
data_figs_sum = data_figs_state_sum %>%
  group_by(incumbent_party, civil_war) %>%
  summarise(mean = mean(perc_incumbent_mean, na.rm = T),
            sd = sd(perc_incumbent_mean, na.rm = T),
            n = n()) %>%
  ungroup()

data_figs_sum = data_figs_state_sum %>%
  group_by(incumbent_party, civil_war) %>%
  summarise(mean = mean(perc_incumbent_mean, na.rm = T),
            sd = sd(perc_incumbent_mean, na.rm = T),
            n = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_high = mean + se) %>%
  mutate(se_low = mean - se)


## MAKE FIGURES ##

# Histogram of full data set
incumbent_histogram_full.plot = ggplot(data_figs, aes(x = perc_votes_incumbent,
                                                      fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  facet_grid(incumbent_party ~ civil_war) +
  scale_fill_manual(values = c("blue", "red"))

incumbent_histogram_full.plot

# Histogram of data averaged over years
incumbent_histogram_sum.plot = ggplot(data_figs_state_sum, aes(x = perc_incumbent_mean,
                                                               fill = incumbent_party)) +
  geom_histogram(bins = 10) +
  facet_grid(incumbent_party ~ civil_war) +
  scale_fill_manual(values = c("blue", "red"))

incumbent_histogram_sum.plot

# Boxplot
incumbent_boxplot.plot = 
  ggplot(data_figs_state_sum, aes(x = civil_war, y = perc_incumbent_mean, fill = incumbent_party)) +
  geom_boxplot() +
  ylim(0, 100) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values = c("blue", "red"))

incumbent_boxplot.plot

# Barplot
incumbent_barplot.plot = ggplot(data_figs_sum, aes(x = civil_war,
                                                   y = mean,
                                                   fill = incumbent_party)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = se_low, ymax = se_high),
                width = 0.2,
                position = position_dodge(0.9)) +
  ylim(0, 100) +
  geom_hline(yintercept = 50) +
  scale_fill_manual(values = c("blue", "red"))

incumbent_barplot.plot

## ORAGANIZE DATA ##
# Make data for statistics
data_stats = data_clean %>%
  mutate(civil_war = factor(civil_war, levels = c("union", "confederacy"))) %>%
  group_by(state, incumbent_party, civil_war) %>%
  summarise(perc_incumbent_mean = mean(perc_votes_incumbent, na.rm = T)) %>%
  ungroup()

# Check if incumbent party is within-state
xtabs(~state+incumbent_party, data_stats)

# Check if civil war is within-state
xtabs(~state+civil_war, data_stats)

## BUILD MODELS ##
# ANOVA (base R)
incumbent.aov = aov(perc_incumbent_mean ~ incumbent_party * civil_war +
                      Error(state/incumbent_party), data = data_stats)

incumbent.aov_sum = summary(incumbent.aov)
incumbent.aov_sum

# ezANOVA
incumbent.ezanova = ezANOVA(data.frame(data_stats),
                            dv = perc_incumbent_mean,
                            wid = state,
                            within = incumbent_party,
                            between = civil_war,
                            type = 3)

incumbent.ezanova

# Prepare data for t-test
data_union_stats = data_stats %>%
  filter(civil_war == "union") %>%
  spread(incumbent_party, perc_incumbent_mean)

data_confederacy_stats = data_stats %>%
  filter(civil_war == "confederacy") %>%
  spread(incumbent_party, perc_incumbent_mean)

data_democrat_stats = data_stats %>%
  filter(incumbent_party == "democrat")

data_republican_stats = data_stats %>%
  filter(incumbent_party == "republican")

## FOLLOW-UP T-TESTS ##
# Effect of incumbent party, separated by civil war
incumbent_union.ttest = t.test(data_union_stats$democrat,
                               data_union_stats$republican,
                               paired = T)
incumbent_union.ttest

incumbent_confederacy.ttest = t.test(data_confederacy_stats$democrat,
                                     data_confederacy_stats$republican,
                                     paired = T)
incumbent_confederacy.ttest

# Effect of incumbent party, separated by civil war
incumbent_democrat.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                  paired = F,
                                  data = data_democrat_stats)
incumbent_democrat.ttest

incumbent_republican.ttest = t.test(perc_incumbent_mean ~ civil_war,
                                    paired = F,
                                    data = data_republican_stats)
incumbent_republican.ttest






