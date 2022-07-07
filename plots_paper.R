library(readr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(ggpubr)

Sys.setlocale('LC_TIME', 'C')

load_data <- function(path) {
  read_csv(
    paste('data/', path, sep = ''),
    col_types = cols(
      episode_num = col_integer(),
      `length(timesteps)` = col_integer(),
      `time_elapsed(seconds)` = col_integer()
    )
  )
}
plot_train_reward <- function(df) {
  df %>%
    select('Agent',
           `Mean episode reward` = 'mean_reward',
           'Climate',
           `Episode` = 'episode_num') %>%
    ggplot(aes(
      y = `Mean episode reward`,
      x = Episode,
      colour = Agent,
      linetype = Agent
    )) +
    geom_line() +
    facet_wrap( ~ Climate) +
    ylab('Mean reward') +
    xlab('Episode') +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'grey98')) +
    scale_colour_brewer('', palette = 'Set1') +
    scale_linetype_manual('', values = c('dashed', 'dotdash', 'longdash'))
  # labs(fill = '',
  #      title = 'Reward evolution during training',
  #      subtitle = '20 episodes')
}
plot_metrics <- function(df) {
  # Mean reward
  p1 <- df %>%
    select('Agent', `Mean episode reward` = 'mean_reward', 'Climate') %>%
    ggplot(aes(y = `Mean episode reward`, x = Agent, col = Agent)) +
    geom_boxplot() +
    facet_wrap(~ Climate, scale = 'free') +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      strip.background = element_rect(fill = 'grey98'),
      axis.title = element_text(size = 9)
    ) +
    labs(title = 'Mean episode rewards') +
    scale_colour_brewer('', palette = 'Set1')
  # labs(fill = '',
  #      title = 'Mean episode rewards',
  #      subtitle = '20 episodes evaluation') +
  
  # Mean power consumption
  p2 <- df %>%
    select('Agent', `Power consumption (kWh)` = 'mean_power_consumption', 'Climate') %>%
    ggplot(aes(y = `Power consumption (kWh)`, x = Agent, col = Agent)) +
    geom_boxplot() +
    facet_wrap(~ Climate, scale = 'free') +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      strip.background = element_rect(fill = 'grey98'),
      axis.title = element_text(size = 9)
    ) +
    labs(title = 'Mean power consumption per episode') +
    scale_colour_brewer('', palette = 'Set1')
  # labs(fill = '',
  #      title = 'Mean power consumption per episode',
  #      subtitle = '20 episodes evaluation') +
  
  # Comfort violation time
  p3 <- df %>%
    select('Agent', `Comfort violation time (%)` = 'comfort_violation (%)', 'Climate') %>%
    ggplot(aes(y = `Comfort violation time (%)`, x = Agent, col = Agent)) +
    geom_boxplot() +
    facet_wrap(~ Climate, scale = 'free') +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      strip.background = element_rect(fill = 'grey98'),
      axis.title = element_text(size = 9)
    ) +
    labs(title = '% of time under comfort violation during episodes') +
    scale_colour_brewer('', palette = 'Set1')
  # labs(fill = '',
  #      title = '% of time under comfort violation during episodes',
  #      subtitle = '20 episodes evaluation') +
  
  ggarrange(
    plotlist = list(p1, p2, p3),
    nrow = 3,
    ncol = 1,
    common.legend = TRUE
  )
  
}

################################## TRAIN  ##################################

## Load data

# 5Zone-mixed
PPO_5Zone_mixed_train <-
  load_data('data_train_5Zone/PPO_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_5Zone_mixed_train <-
  load_data('data_train_5Zone/SAC_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_5Zone_mixed_train <-
  load_data('data_train_5Zone/TD3_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed') %>% slice_head(n = 20)

# 5Zone-hot
PPO_5Zone_hot_train <-
  load_data('data_train_5Zone/PPO_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_train <-
  load_data('data_train_5Zone/SAC_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot') %>% slice_head(n = 20)
TD3_5Zone_hot_train <-
  load_data('data_train_5Zone/TD3_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot') %>% slice_head(n = 20)

# 5Zone-cool
PPO_5Zone_cool_train <-
  load_data('data_train_5Zone/PPO_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool') %>% slice_head(n = 20)
SAC_5Zone_cool_train <-
  load_data('data_train_5Zone/SAC_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool') %>% slice_head(n = 20)
TD3_5Zone_cool_train <-
  load_data('data_train_5Zone/TD3_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Cool') %>% slice_head(n = 20)

data_5Zone_train <-
  rbind(
    PPO_5Zone_mixed_train,
    SAC_5Zone_mixed_train,
    TD3_5Zone_mixed_train,
    PPO_5Zone_hot_train,
    SAC_5Zone_hot_train,
    TD3_5Zone_hot_train,
    PPO_5Zone_cool_train,
    SAC_5Zone_cool_train,
    TD3_5Zone_cool_train
  )

# 5Zone-mixed
PPO_datacenter_mixed_train <-
  load_data('data_train_datacenter/PPO_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_datacenter_mixed_train <-
  load_data('data_train_datacenter/SAC_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_datacenter_mixed_train <-
  load_data('data_train_datacenter/TD3_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed') %>% slice_head(n = 20)

# 5Zone-hot
PPO_datacenter_hot_train <-
  load_data('data_train_datacenter/PPO_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot') %>% slice_head(n = 20)
SAC_datacenter_hot_train <-
  load_data('data_train_datacenter/SAC_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot') %>% slice_head(n = 20)
TD3_datacenter_hot_train <-
  load_data('data_train_datacenter/TD3_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot') %>% slice_head(n = 20)

# 5Zone-cool
PPO_datacenter_cool_train <-
  load_data('data_train_datacenter/PPO_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool') %>% slice_head(n = 20)
SAC_datacenter_cool_train <-
  load_data('data_train_datacenter/SAC_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool') %>% slice_head(n = 20)
TD3_datacenter_cool_train <-
  load_data('data_train_datacenter/TD3_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Cool') %>% slice_head(n = 20)

data_datacenter_train <-
  rbind(
    PPO_datacenter_mixed_train,
    SAC_datacenter_mixed_train,
    TD3_datacenter_mixed_train,
    PPO_datacenter_hot_train,
    SAC_datacenter_hot_train,
    TD3_datacenter_hot_train,
    PPO_datacenter_cool_train,
    SAC_datacenter_cool_train,
    TD3_datacenter_cool_train
  )

data_5Zone_train$Climate <-
  factor(data_5Zone_train$Climate, levels = c('Cool', 'Mixed', 'Hot'))
data_datacenter_train$Climate <-
  factor(data_datacenter_train$Climate,
         levels = c('Cool', 'Mixed', 'Hot'))

## Plot reward evolution during training

plot_train_reward(data_5Zone_train)
ggsave(
  'img/train_rewards_5Zone.png',
  units = 'px',
  width = 2500,
  height = 1150
)

plot_train_reward(data_datacenter_train)
ggsave(
  'img/train_rewards_datacenter.png',
  units = 'px',
  width = 2500,
  height = 1150
)

################################## EVALUATION ##################################

## Load data

# 5Zone-mixed
PPO_5Zone_mixed_eval <-
  load_data('data_eval_5Zone/PPO_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed')
SAC_5Zone_mixed_eval <-
  load_data('data_eval_5Zone/SAC_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed')
TD3_5Zone_mixed_eval <-
  load_data('data_eval_5Zone/TD3_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed')
RBC_5Zone_mixed_eval <-
  load_data('data_eval_5Zone/RBC_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Mixed')
RAND_5Zone_mixed_eval <-
  load_data('data_eval_5Zone/RAND_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Mixed')

# 5Zone-hot
PPO_5Zone_hot_eval <-
  load_data('data_eval_5Zone/PPO_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot')
SAC_5Zone_hot_eval <-
  load_data('data_eval_5Zone/SAC_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot')
TD3_5Zone_hot_eval <-
  load_data('data_eval_5Zone/TD3_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot')
RBC_5Zone_hot_eval <-
  load_data('data_eval_5Zone/RBC_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Hot')
RAND_5Zone_hot_eval <-
  load_data('data_eval_5Zone/RAND_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Hot')

# 5Zone-cool
PPO_5Zone_cool_eval <-
  load_data('data_eval_5Zone/PPO_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool')
SAC_5Zone_cool_eval <-
  load_data('data_eval_5Zone/SAC_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool')
TD3_5Zone_cool_eval <-
  load_data('data_eval_5Zone/TD3_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Cool')
RBC_5Zone_cool_eval <-
  load_data('data_eval_5Zone/RBC_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Cool')
RAND_5Zone_cool_eval <-
  load_data('data_eval_5Zone/RAND_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Cool')

data_5Zone_eval <-
  rbind(
    PPO_5Zone_mixed_eval,
    SAC_5Zone_mixed_eval,
    TD3_5Zone_mixed_eval,
    PPO_5Zone_hot_eval,
    SAC_5Zone_hot_eval,
    TD3_5Zone_hot_eval,
    PPO_5Zone_cool_eval,
    SAC_5Zone_cool_eval,
    TD3_5Zone_cool_eval,
    RBC_5Zone_mixed_eval,
    RBC_5Zone_hot_eval,
    RBC_5Zone_cool_eval,
    RAND_5Zone_mixed_eval,
    RAND_5Zone_hot_eval,
    RAND_5Zone_cool_eval
  )

# datacenter-mixed
PPO_datacenter_mixed_eval <-
  load_data('data_eval_datacenter/PPO_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed')
SAC_datacenter_mixed_eval <-
  load_data('data_eval_datacenter/SAC_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed')
TD3_datacenter_mixed_eval <-
  load_data('data_eval_datacenter/TD3_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed')
RBC_datacenter_mixed_eval <-
  load_data('data_eval_datacenter/RBC_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Mixed')
RAND_datacenter_mixed_eval <-
  load_data('data_eval_datacenter/RAND_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Mixed')

# datacenter-hot
PPO_datacenter_hot_eval <-
  load_data('data_eval_datacenter/PPO_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot')
SAC_datacenter_hot_eval <-
  load_data('data_eval_datacenter/SAC_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot')
TD3_datacenter_hot_eval <-
  load_data('data_eval_datacenter/TD3_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot')
RBC_datacenter_hot_eval <-
  load_data('data_eval_datacenter/RBC_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Hot')
RAND_datacenter_hot_eval <-
  load_data('data_eval_datacenter/RAND_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Hot')

# datacenter-cool
PPO_datacenter_cool_eval <-
  load_data('data_eval_datacenter/PPO_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool')
SAC_datacenter_cool_eval <-
  load_data('data_eval_datacenter/SAC_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool')
TD3_datacenter_cool_eval <-
  load_data('data_eval_datacenter/TD3_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Cool')
RBC_datacenter_cool_eval <-
  load_data('data_eval_datacenter/RBC_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Cool')
RAND_datacenter_cool_eval <-
  load_data('data_eval_datacenter/RAND_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Cool')

data_datacenter_eval <-
  rbind(
    PPO_datacenter_mixed_eval,
    SAC_datacenter_mixed_eval,
    TD3_datacenter_mixed_eval,
    PPO_datacenter_hot_eval,
    SAC_datacenter_hot_eval,
    TD3_datacenter_hot_eval,
    PPO_datacenter_cool_eval,
    SAC_datacenter_cool_eval,
    TD3_datacenter_cool_eval,
    RBC_datacenter_mixed_eval,
    RBC_datacenter_hot_eval,
    RBC_datacenter_cool_eval,
    RAND_datacenter_mixed_eval,
    RAND_datacenter_hot_eval,
    RAND_datacenter_cool_eval
  )

data_5Zone_eval$Climate <-
  factor(data_5Zone_eval$Climate, levels = c('Cool', 'Mixed', 'Hot'))
data_datacenter_eval$Climate <-
  factor(data_datacenter_eval$Climate, levels = c('Cool', 'Mixed', 'Hot'))

data_5Zone_eval$Agent <-
  factor(data_5Zone_eval$Agent, levels = c('PPO', 'SAC', 'TD3', 'RBC', 'RAND'))
data_datacenter_eval$Agent <-
  factor(data_datacenter_eval$Agent, levels = c('PPO', 'SAC', 'TD3', 'RBC', 'RAND'))

## Evaluation metrics

plot_metrics(data_5Zone_eval)
ggsave('img/eval_metrics_5Zone.png', units='px', width=2875, height=1930)

plot_metrics(data_datacenter_eval)
ggsave('img/eval_metrics_datacenter.png', units='px', width=2875, height=1930)

############################## ROBUSTNESS TESTS ##############################


################################# CV LEARNING #################################


######################### COMFORT-CONSUMPTION TRADE-OFF #########################
