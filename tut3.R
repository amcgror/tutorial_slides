library(tidyverse)



n_obs <- 50    # number of observations 
n_sim <- 1500  # number of simulations
props <- rep(NA, n_sim)   # an empty vector with 1500 entries



for (i in 1:n_sim){    # 1 - 1500
  
  # equally likely to draw heads or tails with replacement
  samp <- sample(c("Heads", "Tails"), size = n_obs, replace = T, prob = c(0.5, 0.5))
  
  # the proportion for this simulation 
  current_prop <- sum(samp == "Heads") / n_obs
  
  # append it to our proportions vector
  props[i] <- current_prop
}

tibb <- tibble(prop_heads = props)

ggplot(tibb, aes(x = prop_heads)) + 
  geom_histogram(binwidth = 0.02, color = "black", fill = "white") +
  xlab("The proportion of Heads") + 
  theme_minimal()


# given a test statistic of 40 / 50, how unlikely is this? 

t_stat <- 40 / 50

ggplot(tibb, aes(x = prop_heads)) + 
  geom_histogram(binwidth = 0.02, color = "black", fill = "white") +
  xlab("The proportion of Heads") +
  
  # draw a vertical line at the specified x-intercept 
  geom_vline(xintercept = 0.5 + abs(t_stat - 0.5), color = "red") +
  geom_vline(xintercept = 0.5 - abs(t_stat - 0.5), color = "red") +
  theme_minimal()


tibb %>% 
  filter(prop_heads >= 0.8 | prop_heads <= 0.2) %>% 
  summarize(p_value = n() / n_sim)
