library(tidyverse)
library(ggplot2)

# read in all simulation results

rda_files <- list.files(here::here("data"), pattern = "\\.RDA$", full.names = TRUE)

all_results = NA
for (i in 1:length(rda_files)) {
  load(rda_files[i])
  tmp = bind_rows(results)
  all_results = rbind(all_results, tmp)
}

all_results = na.omit(all_results)


# bias of beta hat

dat_bias = all_results %>% group_by(true_beta, epsilon_dist, n) %>%
  summarise(beta_hat = mean(beta_hat)) %>%
  mutate(beta_bias = beta_hat - true_beta)

p = ggplot(dat_bias, aes(x = factor(n), y = beta_bias)) + geom_col() +
  facet_grid(rows = vars(epsilon_dist), cols = vars(true_beta),
             labeller = labeller(true_beta = function(x) paste0("True beta = ", x))) + 
  labs(x = "Sample Size (n)", y = "Bias of estimated beta") + 
  theme(text = element_text(size = 22))
  
png(here::here("results","figure_for_bias.png"), width = 720, height = 540)
p
dev.off()


# coverage of beta hat

dat_coverage_w = all_results %>% group_by(true_beta, epsilon_dist, n) %>%
  summarise(coverage = mean(coverage_wald)) %>% mutate(method = "Wald confidence intervals")

dat_coverage_p = all_results %>% group_by(true_beta, epsilon_dist, n) %>%
  summarise(coverage = mean(coverage_p)) %>% mutate(method = "bootstrap percentile intervals")

dat_coverage_t = all_results %>% group_by(true_beta, epsilon_dist, n) %>%
  summarise(coverage = mean(coverage_t)) %>% mutate(method = "bootstrap t intervals")

dat_coverage = rbind(dat_coverage_w, dat_coverage_p, dat_coverage_t)
dat_coverage$method = factor(dat_coverage$method, 
                             levels = c("Wald confidence intervals",
                                        "bootstrap percentile intervals",
                                        "bootstrap t intervals"))
       

p = ggplot(dat_coverage, aes(x = factor(n), y = coverage, fill = method)) + geom_col(position = "dodge", alpha = 0.8) +
  geom_hline(yintercept = 0.95, color = 1, lty = "dashed", linewidth = 1) + 
  facet_grid(rows = vars(epsilon_dist), cols = vars(true_beta),
             labeller = labeller(true_beta = function(x) paste0("True beta = ", x))) + 
  labs(x = "Sample Size (n)", y = "Coverage") + 
  theme(text = element_text(size = 20), legend.position = "bottom")

png(here::here("results","figure_for_coverage.png"), width = 720, height = 540)
p
dev.off()


# computation time
dat_time_w = all_results %>% group_by(n) %>%
  summarise(time = mean(time_wald)) %>% mutate(method = "Wald confidence intervals")

dat_time_p = all_results %>% group_by(n) %>%
  summarise(time = mean(time_p)) %>% mutate(method = "bootstrap percentile intervals")

dat_time_t = all_results %>% group_by(n) %>%
  summarise(time = mean(time_t)) %>% mutate(method = "bootstrap t intervals")

dat_time = rbind(dat_time_w, dat_time_p, dat_time_t)

write.csv(dat_time, here::here("results","cimputation_time.csv"), row.names = F)

# distribution of se
p = ggplot(all_results, aes(x = std.error, color = factor(n))) + 
  geom_density(linewidth = 1) + 
  facet_grid(rows = vars(epsilon_dist), cols = vars(true_beta),
             labeller = labeller(true_beta = function(x) paste0("True beta = ", x))) +
  labs(x = "Standard Error", y = "Density", color = "Sample Size (n)") + 
  theme(text = element_text(size = 20), legend.position = "bottom")

png(here::here("results","figure_for_se.png"), width = 720, height = 540)
p
dev.off()
