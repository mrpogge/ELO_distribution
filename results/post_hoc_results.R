################################################################################
# Dependencies
################################################################################
library(tidyverse)
library(patchwork)
source("elo_functions.R")
source("outcome_measures.R")
source("plotting.R")

################################################################################
# Loading sim results (elo true)
################################################################################
load("output/post_hoc_output/PlotEloTrue.RData")

################################################################################
# traceplot elo true
################################################################################
dat1 = plot1 %>% 
        select(-iteration) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        mutate("iteration" = rep(plot1$iteration, each = ncol(plot1)-1)) %>%
        mutate(type = case_when(Variable %in% c("V2","V3", "V4", "V5", "V6") ~ "est",
                                Variable %in% c("V7", "V8", "V9", "V10", "V11") ~ "true")) %>%
        mutate(Variable = recode(Variable, 
                                 "V7" = "V2", 
                                 "V8" = "V3",
                                 "V9" = "V4", 
                                 "V10" = "V5", 
                                 "V11" = "V6"))

lab1 = parse(text = paste("\u03B8", " = ", round(plot1[1,c("V7", "V8", "V9", "V10", "V11")], 2)))
dat1$Variable = factor(dat1$Variable, levels = c("V2", "V3", "V4", "V5", "V6"), labels = lab1)
dat1$type = factor(dat1$type, levels = c("est", "true"), labels = c("Elo Rating", "True Ability"))


true_trace_plot = ggplot(dat1, aes(x = iteration, y = Value, color = Variable, linetype = type)) +
  geom_line() +
  labs(x = "Items Answered", y = expression(theta), color = "Student") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


################################################################################
# inflation plot true elo
################################################################################

true_inflation_plot = ggplot(plot2, aes(x = iteration, y = sd)) +
  geom_line() +
  labs(x = "Items Answered", y = "Standard Deviation of average Elo ratings") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

true_patch = true_trace_plot + true_inflation_plot 

ggsave("figures/figure_7_true.png", true_patch, units = "mm", dpi = 300, height = 100, width = 200)

################################################################################
# read inflation data
################################################################################
load("output/post_hoc_output/PlotVarianceInflation.RData")

dat1 = plot1 %>% 
        select(-Timepoint) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        mutate("Timepoint" = rep(plot1$Timepoint, each = ncol(plot1)-1))
        
lab1 = parse(text = paste("\u03B8", " = ", c("-1.15", "-0.15", "0.85", "1.85", "2.84")))
dat1$Variable = factor(dat1$Variable, levels = c("-1.15", "-0.15", "0.85", "1.85", "2.84"), labels = lab1)

inflation_plot1 = ggplot(dat1, aes(x = Timepoint, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Items Answered", y = "Expected error in item difficulty") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

dat2 = plot2 %>% 
        select(-Timepoint) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        mutate("Timepoint" = rep(plot2$Timepoint, each = ncol(plot2)-1))
lab2 = parse(text = paste("\u03B4", " = ", c("-2", "-1", "0", "1", "2")))
dat2$Variable = factor(dat2$Variable, levels = c("-2", "-1", "0", "1", "2"), labels = lab2)

inflation_plot2 = ggplot(dat2, aes(x = Timepoint, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Items Answered", y = "Average error in person ability") +
  jtools::theme_apa(legend.font.size = 10)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

inflation_patch = inflation_plot1 + inflation_plot2

ggsave("figures/figure_8_inflation.png", inflation_patch, units = "mm", dpi = 300, height = 100, width = 200)

################################################################################
# read autocorrelation data
################################################################################
load("output/post_hoc_output/PlotAutocorrelation.RData")
colnames(results) = c("results", "K = 0.1", "K = 0.2", "K = 0.3", "K = 0.4", "K = 0.5")

dat1 = results %>% 
        select(-results) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        mutate(lag = rep(1:500, each = ncol(results)-1))

autocorrelation_plot = ggplot(dat1, aes(x = lag, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Lag", y = "Autocorrelation") +
  jtools::theme_apa(legend.font.size = 10)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
ggsave("figures/figure_9_autocorrelation.png", autocorrelation_plot, units = "mm", dpi = 300, height = 100, width = 200)

################################################################################
# read lagged results
################################################################################
load("output/post_hoc_output/PlotLag.RData")

colnames(plot1) = c("Timepoint", 1,2,5,10,30,50,100)
dat1 = plot1 %>%
        select(-Timepoint) %>%
        pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
        mutate("Timepoint" = rep(plot1$Timepoint, each = ncol(plot1)-1),
               "Variable_factor" = factor(Variable, levels = c(1,2,5,10,30,50,100), labels = c("l = 1",
                                                                                               "l = 2",
                                                                                               "l = 5", 
                                                                                               "l = 10", 
                                                                                               "l = 30", 
                                                                                               "l = 50", 
                                                                                               "l = 100")))

#order the legend based on the lag numbers not alphabetically
lag_plot = ggplot(dat1, aes(x = Timepoint, y = Value, color = Variable_factor)) +
  geom_line() +
  labs(x = "Items Answered", y = "Standard deviation of average Elo ratings") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
ggsave("figures/figure_10_lag.png", lag_plot, units = "mm", dpi = 300, height = 100, width = 200)

################################################################################
# updated adaptive scenario
################################################################################
load("output/ResultsUpdatedAdaptive_K.RData")
load("output/ResultsUpdatedAdaptive_mu.RData")

Vars_K=as.data.frame(sapply(res_updated_items_adaptive_K,FUN=function(X){apply(X$mean,2,sd)}))
Vars_mu=as.data.frame(sapply(res_updated_items_adaptive_mu,FUN=function(X){apply(X$mean,2,sd)}))

colnames(Vars_K) = c("K = 0.1", "K = 0.2", "K = 0.3", "K = 0.4", "K = 0.5")
colnames(Vars_mu) = parse(text = paste("P", " = ", c("0.5", "0.6", "0.7", "0.8", "0.9")))

Timepoints = 1:nrow(Vars_K)

dat1 = as.data.frame(Vars_K) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate("Timepoint" = rep(Timepoints, each = ncol(Vars_K)))

dat2 = as.data.frame(Vars_mu) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate("Timepoint" = rep(Timepoints, each = ncol(Vars_mu)))

ua_plot_k = ggplot(dat1, aes(x = Timepoint, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Items Answered", y = "Standard deviation of average Elo ratings") +
  jtools::theme_apa(legend.font.size = 10)+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


ua_plot_mu = ggplot(dat2, aes(x = Timepoint, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Items Answered", y = "Standard deviation of average Elo ratings") +
  jtools::theme_apa(legend.font.size = 10) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

ua_patch_inflation = ua_plot_k + ua_plot_mu

ggsave("figures/figure_6_ua_inf.png", ua_patch_inflation, units = "mm", dpi = 300, height = 100, width = 200)
