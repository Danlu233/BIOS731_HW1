library(broom)

get_estimates = function(model_fit, true_beta){
  
  start_time = Sys.time()
  
  tidy(model_fit, conf.int = TRUE) %>%
    filter(term == "x") %>%
    mutate(coverage_wald = ifelse(true_beta >= estimate - 1.96*std.error & true_beta <= estimate + 1.96*std.error, 1, 0),
           time_wald = as.numeric(difftime(Sys.time(), start_time, units = "secs"))) %>%
    rename(beta_hat = estimate) %>%
    select(term, beta_hat, std.error, coverage_wald, time_wald)
  
} 