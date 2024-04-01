############################################################################################
##                                                                                        ##
##  An experimental evaluation of Deep Reinforcement Learning algorithms for HVAC control ##
##                                                                                        ##
##  Authors: A. Manjavacas, A. Campoy, J. Jimenez, M. Molina, J. Gomez                    ##
##                                                                                        ##
##  Contact: manjavacas@ugr.es                                                            ##
##                                                                                        ##
############################################################################################

library(tidyverse)

load_data <- function(path) {
  read_csv(
    path,
    col_types = cols(
      episode_num = col_integer(),
      `length(timesteps)` = col_integer(),
      `time_elapsed(seconds)` = col_integer()
    )
  )
}

# SAC Cool

SAC_cool_cool <-
  load_data('data/data_eval_robustness/SAC_cool_cool/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Cool',
         Test = 'Cool') %>% slice_head(n = 20)
SAC_cool_mixed <-
  load_data('data/data_eval_robustness/SAC_cool_mixed/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Cool',
         Test = 'Mixed') %>% slice_head(n = 20)
SAC_cool_hot <-
  load_data('data/data_eval_robustness/SAC_cool_hot/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Cool',
         Test = 'Hot') %>% slice_head(n = 20)

# SAC Mixed

SAC_mixed_cool <-
  load_data('data/data_eval_robustness/SAC_mixed_cool/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Mixed',
         Test = 'Cool') %>% slice_head(n = 20)
SAC_mixed_mixed <-
  load_data('data/data_eval_robustness/SAC_mixed_mixed/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Mixed',
         Test = 'Mixed') %>% slice_head(n = 20)
SAC_mixed_hot <-
  load_data('data/data_eval_robustness/SAC_mixed_hot/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Mixed',
         Test = 'Hot') %>% slice_head(n = 20)
# SAC Hot

SAC_hot_cool <-
  load_data('data/data_eval_robustness/SAC_hot_cool/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Hot',
         Test = 'Cool') %>% slice_head(n = 20)
SAC_hot_mixed <-
  load_data('data/data_eval_robustness/SAC_hot_mixed/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Hot',
         Test = 'Mixed') %>% slice_head(n = 20)
SAC_hot_hot <-
  load_data('data/data_eval_robustness/SAC_hot_hot/progress.csv') %>%
  mutate(Agent = 'SAC',
         Train = 'Hot',
         Test = 'Hot') %>% slice_head(n = 20)

# Tests

wilcox.test(SAC_cool_cool$mean_reward, SAC_mixed_cool$mean_reward)$p.value
wilcox.test(SAC_cool_cool$mean_reward, SAC_hot_cool$mean_reward)$p.value

wilcox.test(SAC_mixed_mixed$mean_reward, SAC_cool_mixed$mean_reward)$p.value
wilcox.test(SAC_mixed_mixed$mean_reward, SAC_hot_mixed$mean_reward)$p.value

wilcox.test(SAC_hot_hot$mean_reward, SAC_cool_hot$mean_reward)$p.value
wilcox.test(SAC_hot_hot$mean_reward, SAC_mixed_hot$mean_reward)$p.value
