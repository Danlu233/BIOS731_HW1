simulate_data <- function(n, true_beta, dist_epsilon) {
   
   beta0 = 1
   
   x = rbinom(n, 1, prob = 0.5)
   
   if (dist_epsilon == "normal") {
     
     epsilon = rnorm(n, 0, sqrt(2))
     
   } else if (dist_epsilon == "lognormal") {
     
     epsilon = rlnorm(n, 0, log(2))
     
   } else {
     
     error("wrong distribution of epsilon")
     
   }
   
   y = beta0 + true_beta * x + epsilon
   
   tibble(
     x = x,
     y = y
   )

}