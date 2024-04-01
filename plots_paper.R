############################################################################################
##                                                                                        ##
##  An experimental evaluation of Deep Reinforcement Learning algorithms for HVAC control ##
##                                                                                        ##
##  Authors: A. Manjavacas, A. Campoy, J. Jimenez, M. Molina, J. Gomez                    ##
##                                                                                        ##
##  Contact: manjavacas@ugr.es                                                            ##
##                                                                                        ##
############################################################################################

library(readr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(reshape2)

Sys.setlocale('LC_TIME', 'C')

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
plot_metrics <- function(df, label_size = 9) {
  # Mean reward
  p1 <- df %>%
    select('Agent', `Mean episode reward` = 'mean_reward', 'Climate') %>%
    ggplot(aes(y = `Mean episode reward`, x = Agent, col = Agent)) +
    geom_boxplot() +
    facet_wrap(~ Climate, scale = 'free') +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text = element_text(size = label_size),
      legend.title = element_blank(),
      strip.background = element_rect(fill = 'grey98'),
      axis.title = element_text(size = 9)
    ) +
    labs(title = 'Mean episode rewards') +
    scale_colour_brewer('', palette = 'Set1')
  # labs(fill = '',
  #      title = 'Mean episode rewards',
  #      subtitle = '20 episodes evaluation') +
  
  # Mean power demand
  p2 <- df %>%
    select('Agent', `Power demand (W)` = 'mean_power_consumption', 'Climate') %>%
    ggplot(aes(y = `Power demand (W)`, x = Agent, col = Agent)) +
    geom_boxplot() +
    facet_wrap(~ Climate, scale = 'free') +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text = element_text(size = label_size),
      legend.title = element_blank(),
      strip.background = element_rect(fill = 'grey98'),
      axis.title = element_text(size = 9)
    ) +
    labs(title = 'Mean power demand per episode') +
    scale_colour_brewer('', palette = 'Set1')
  # labs(fill = '',
  #      title = 'Mean power demand per episode',
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
      axis.text = element_text(size = label_size),
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
plot_temperatures_datacenter <- function(df) {
  df <- df %>% mutate(
    minute = case_when(
      timestep %% 4 == 1 ~ 15,
      timestep %% 4 == 2 ~ 30,
      timestep %% 4 == 3 ~ 45,
      timestep %% 4 == 0 ~ 0
    )
  )
  
  df$year <- 2022
  df$Date <-
    ISOdate(df$year, df$month, df$day, df$hour, df$minute) %>% as.Date()
  
  df <-
    df %>% select(
      Date,
      `Zone Air Temperature (West Zone)` = 'Zone Air Temperature(West Zone)',
      `Zone Air Temperature (East Zone)` = 'Zone Air Temperature(East Zone)',
      `Outdoor Air Drybulb Temperature` = 'Site Outdoor Air Drybulb Temperature(Environment)'
    )
  
  custom_breaks = c(18, 21, 24, 27, pretty(range(df$`Outdoor Air Drybulb Temperature`),
                                           n = 8))
  custom_breaks <-
    custom_breaks[custom_breaks != 20 & custom_breaks != 25]
  
  plot1 <- df %>% ggplot(aes(x = Date)) +
    geom_line(
      aes(y = `Outdoor Air Drybulb Temperature`, colour = 'Outdoor Air Drybulb Temperature'),
      alpha = .4
    ) +
    geom_line(
      aes(y = `Zone Air Temperature (West Zone)`, colour = 'Zone Air Temperature (West Zone)'),
      alpha = .8
    ) +
    geom_segment(
      x = 0,
      xend = df$Date[35039],
      y = 27,
      yend = 27,
      colour = 'red',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = 0,
      xend = df$Date[35039],
      y = 18,
      yend = 18,
      colour = 'blue',
      linetype = 'dashed'
    ) +
    ylab('Temperature (ºC)') +
    xlab('') +
    theme_bw() +
    theme(
      legend.position = 'top',
      legend.justification = 'left',
      legend.text = element_text(size = 12)
    ) +
    scale_x_date(date_labels = '%b', date_breaks = '1 month') +
    scale_y_continuous(breaks = custom_breaks) +
    scale_colour_manual(
      '',
      values = c(
        `Outdoor Air Drybulb Temperature` = 'black',
        `Zone Air Temperature (West Zone)` =
          'forestgreen'
      )
    ) +
    labs(fill = '',
         title = '2ZoneDataCenterHVAC West Zone')
  
  plot2 <- df %>% ggplot(aes(x = Date)) +
    geom_line(
      aes(y = `Outdoor Air Drybulb Temperature`, colour = 'Outdoor Air Drybulb Temperature'),
      alpha = .4
    ) +
    geom_line(
      aes(y = `Zone Air Temperature (East Zone)`, colour = 'Zone Air Temperature (East Zone)'),
      alpha = .8
    ) +
    geom_segment(
      x = 0,
      xend = df$Date[35039],
      y = 27,
      yend = 27,
      colour = 'red',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = 0,
      xend = df$Date[35039],
      y = 18,
      yend = 18,
      colour = 'blue',
      linetype = 'dashed'
    ) +
    ylab('Temperature (ºC)') +
    xlab('') +
    theme_bw() +
    theme(
      legend.position = 'top',
      legend.justification = 'left',
      legend.text = element_text(size = 12)
    ) +
    scale_x_date(date_labels = '%b', date_breaks = '1 month') +
    scale_y_continuous(breaks = custom_breaks) +
    scale_colour_manual(
      '',
      values = c(
        `Outdoor Air Drybulb Temperature` = 'black',
        `Zone Air Temperature (East Zone)` =
          'sandybrown'
      )
    ) +
    labs(fill = '',
         title = '2ZoneDataCenterHVAC East Zone')
  
  ggarrange(plotlist = list(plot1, plot2),
            nrow = 2,
            ncol = 1)
}
plot_temperatures_5Zone <- function(df) {
  df <- df %>% mutate(
    minute = case_when(
      timestep %% 4 == 1 ~ 15,
      timestep %% 4 == 2 ~ 30,
      timestep %% 4 == 3 ~ 45,
      timestep %% 4 == 0 ~ 0
    )
  )
  df$year <- 2022
  df$Date <-
    ISOdate(df$year, df$month, df$day, df$hour, df$minute) %>% as.Date()
  
  df <-
    df %>% select(
      Date,
      `Zone Air Temperature` = 'Zone Air Temperature(SPACE1-1)',
      `Outdoor Air Drybulb Temperature` = 'Site Outdoor Air Drybulb Temperature(Environment)'
    )
  
  custom_breaks = c(20, 23.5, 26, pretty(range(df$`Outdoor Air Drybulb Temperature`),
                                         n = 8))
  custom_breaks <- custom_breaks[custom_breaks != 25]
  
  df %>% ggplot(aes(x = Date)) +
    geom_line(
      aes(y = `Outdoor Air Drybulb Temperature`, colour = 'Outdoor Air Drybulb Temperature'),
      alpha = .4
    ) +
    geom_line(aes(y = `Zone Air Temperature`, colour = 'Zone Air Temperature')) +
    geom_segment(
      x = 0,
      xend = df$Date[14500],
      y = 23.5,
      yend = 23.5,
      colour = 'red',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = 0,
      xend = df$Date[14500],
      y = 20,
      yend = 20,
      colour = 'blue',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = df$Date[14500],
      xend = df$Date[26200],
      y = 26,
      yend = 26,
      colour = 'red',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = df$Date[14500],
      xend = df$Date[26200],
      y = 23,
      yend = 23,
      colour = 'blue',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = df$Date[26201],
      xend = df$Date[35039],
      y = 23.5,
      yend = 23.5,
      colour = 'red',
      linetype = 'dashed'
    ) +
    geom_segment(
      x = df$Date[26201],
      xend = df$Date[35039],
      y = 20,
      yend = 20,
      colour = 'blue',
      linetype = 'dashed'
    ) +
    ylab('Temperature (ºC)') +
    xlab('') +
    theme_bw() +
    theme(
      legend.position = 'top',
      legend.justification = 'left',
      legend.text = element_text(size = 12)
    ) +
    scale_x_date(date_labels = '%b', date_breaks = '1 month') +
    scale_y_continuous(breaks = custom_breaks) +
    scale_colour_manual(
      '',
      values = c(
        `Zone Air Temperature` = 'forestgreen',
        `Outdoor Air Drybulb Temperature` = 'black'
      )
    )
}
get_comf_viol_5Zone <- function(df) {
  df %>%
    mutate(comfort_violation = ifelse(
      month %in% c(6:9),
      ifelse(
        `Zone Air Temperature(SPACE1-1)` > 26,
        `Zone Air Temperature(SPACE1-1)` - 26,
        ifelse(
          `Zone Air Temperature(SPACE1-1)` < 23,
          `Zone Air Temperature(SPACE1-1)` - 23,
          0
        )
      ),
      ifelse(
        month %in% c(1:5, 10:12),
        ifelse(
          `Zone Air Temperature(SPACE1-1)` > 23.5,
          `Zone Air Temperature(SPACE1-1)` - 23.5,
          ifelse(
            `Zone Air Temperature(SPACE1-1)` < 20,
            `Zone Air Temperature(SPACE1-1)` - 20,
            0
          )
        ),
        0
      )
    ))
}
get_comf_viol_datacenter <- function(df) {
  df %>%
    mutate(
      `West Zone comfort violation` =
        ifelse(
          `Zone Air Temperature(West Zone)` > 27,
          `Zone Air Temperature(West Zone)` - 27,
          ifelse(
            `Zone Air Temperature(West Zone)` < 18,
            `Zone Air Temperature(West Zone)` - 18,
            0
          )
        )
    ) %>%
    mutate(
      `East Zone comfort violation` =
        ifelse(
          `Zone Air Temperature(East Zone)` > 27,
          `Zone Air Temperature(East Zone)` - 27,
          ifelse(
            `Zone Air Temperature(East Zone)` < 18,
            `Zone Air Temperature(East Zone)` - 18,
            0
          )
        )
    )
}
plot_comf_viol_5Zone <- function(df) {
  custom_breaks = c(pretty(range(df$comfort_violation), n = 8))
  
  df %>% select(comfort_violation) %>% melt() %>% rename('Comfort violation (ºC)' = value) %>%
    ggplot(aes(y = `Comfort violation (ºC)`, x = variable)) + geom_violin(trim =
                                                                            F, fill = 'orange') +
    scale_y_continuous(breaks = custom_breaks) + theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    geom_segment(
      x = 0,
      xend = 2,
      y = 0,
      yend = 0,
      colour = 'darkgrey',
      linetype = 'dashed',
      alpha = .5
    )
  # + labs(fill = '', title = 'Comfort temperature violations', subtitle = '5ZoneAutoDXVAV - 20 episodes')
}
plot_comf_viol_datacenter <- function(df) {
  custom_breaks = c(pretty(range(df$`East Zone comfort violation`), n = 7))
  
  df %>%
    select('East Zone' = `East Zone comfort violation`, 'West Zone' = `West Zone comfort violation`) %>%
    melt() %>% rename('Comfort violation (ºC)' = value, 'Zone' = variable) %>%
    ggplot(aes(y = `Comfort violation (ºC)`, x = `Zone`)) + geom_violin(trim =
                                                                          F, aes(fill = `Zone`)) +
    scale_y_continuous(breaks = custom_breaks) + theme_bw() + theme(legend.position =
                                                                      'none') +
    theme(axis.title.x = element_blank()) +
    geom_segment(
      x = 0,
      xend = 4,
      y = 0,
      yend = 0,
      colour = 'darkgrey',
      linetype = 'dashed',
      alpha = .5
    )
  # + labs(fill = '', title = 'Comfort temperature violations', subtitle = '2ZoneDataCenterHVAC - 20 episodes')
}
plot_mean_comf_viol <- function(df) {
  df %>%
    select('Agent', `Mean comfort violation (ºC)` = 'mean_comfort_violation', 'Climate') %>%
    ggplot(aes(y = `Mean comfort violation (ºC)`, x = Agent, col = Agent)) +
    geom_boxplot() +
    facet_wrap(~ Climate, scale = 'free') +
    theme_bw() +
    scale_colour_brewer('', palette = 'Set1') +
    theme(
      legend.position = 'none',
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      strip.background = element_rect(fill = 'grey95'),
      axis.title = element_text(size = 9)
    )
  # labs(title = 'Mean comfort violation during episodes')
}

########################################## TRAIN  ##########################################

## Load data

# 5Zone-mixed
PPO_5Zone_mixed_train <-
  load_data('data/data_train_5Zone/PPO_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_5Zone_mixed_train <-
  load_data('data/data_train_5Zone/SAC_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_5Zone_mixed_train <-
  load_data('data/data_train_5Zone/TD3_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed') %>% slice_head(n = 20)

# 5Zone-hot
PPO_5Zone_hot_train <-
  load_data('data/data_train_5Zone/PPO_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_train <-
  load_data('data/data_train_5Zone/SAC_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot') %>% slice_head(n = 20)
TD3_5Zone_hot_train <-
  load_data('data/data_train_5Zone/TD3_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot') %>% slice_head(n = 20)

# 5Zone-cool
PPO_5Zone_cool_train <-
  load_data('data/data_train_5Zone/PPO_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool') %>% slice_head(n = 20)
SAC_5Zone_cool_train <-
  load_data('data/data_train_5Zone/SAC_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool') %>% slice_head(n = 20)
TD3_5Zone_cool_train <-
  load_data('data/data_train_5Zone/TD3_5Zone_cool/progress.csv') %>%
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
  load_data('data/data_train_datacenter/PPO_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_datacenter_mixed_train <-
  load_data('data/data_train_datacenter/SAC_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_datacenter_mixed_train <-
  load_data('data/data_train_datacenter/TD3_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed') %>% slice_head(n = 20)

# 5Zone-hot
PPO_datacenter_hot_train <-
  load_data('data/data_train_datacenter/PPO_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot') %>% slice_head(n = 20)
SAC_datacenter_hot_train <-
  load_data('data/data_train_datacenter/SAC_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot') %>% slice_head(n = 20)
TD3_datacenter_hot_train <-
  load_data('data/data_train_datacenter/TD3_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot') %>% slice_head(n = 20)

# 5Zone-cool
PPO_datacenter_cool_train <-
  load_data('data/data_train_datacenter/PPO_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool') %>% slice_head(n = 20)
SAC_datacenter_cool_train <-
  load_data('data/data_train_datacenter/SAC_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool') %>% slice_head(n = 20)
TD3_datacenter_cool_train <-
  load_data('data/data_train_datacenter/TD3_datacenter_cool/progress.csv') %>%
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

######################################## EVALUATION ########################################

## Load data

# 5Zone-mixed
PPO_5Zone_mixed_eval <-
  load_data('data/data_eval_5Zone/PPO_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed')
SAC_5Zone_mixed_eval <-
  load_data('data/data_eval_5Zone/SAC_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed')
TD3_5Zone_mixed_eval <-
  load_data('data/data_eval_5Zone/TD3_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed')
RBC_5Zone_mixed_eval <-
  load_data('data/data_eval_5Zone/RBC_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Mixed')
RAND_5Zone_mixed_eval <-
  load_data('data/data_eval_5Zone/RAND_5Zone_mixed/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Mixed')

# 5Zone-hot
PPO_5Zone_hot_eval <-
  load_data('data/data_eval_5Zone/PPO_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot')
SAC_5Zone_hot_eval <-
  load_data('data/data_eval_5Zone/SAC_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot')
TD3_5Zone_hot_eval <-
  load_data('data/data_eval_5Zone/TD3_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot')
RBC_5Zone_hot_eval <-
  load_data('data/data_eval_5Zone/RBC_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Hot')
RAND_5Zone_hot_eval <-
  load_data('data/data_eval_5Zone/RAND_5Zone_hot/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Hot')

# 5Zone-cool
PPO_5Zone_cool_eval <-
  load_data('data/data_eval_5Zone/PPO_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool')
SAC_5Zone_cool_eval <-
  load_data('data/data_eval_5Zone/SAC_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool')
TD3_5Zone_cool_eval <-
  load_data('data/data_eval_5Zone/TD3_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Cool')
RBC_5Zone_cool_eval <-
  load_data('data/data_eval_5Zone/RBC_5Zone_cool/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Cool')
RAND_5Zone_cool_eval <-
  load_data('data/data_eval_5Zone/RAND_5Zone_cool/progress.csv') %>%
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
  load_data('data/data_eval_datacenter/PPO_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Mixed')
SAC_datacenter_mixed_eval <-
  load_data('data/data_eval_datacenter/SAC_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Mixed')
TD3_datacenter_mixed_eval <-
  load_data('data/data_eval_datacenter/TD3_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Mixed')
RBC_datacenter_mixed_eval <-
  load_data('data/data_eval_datacenter/RBC_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Mixed')
RAND_datacenter_mixed_eval <-
  load_data('data/data_eval_datacenter/RAND_datacenter_mixed/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Mixed')

# datacenter-hot
PPO_datacenter_hot_eval <-
  load_data('data/data_eval_datacenter/PPO_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Hot')
SAC_datacenter_hot_eval <-
  load_data('data/data_eval_datacenter/SAC_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Hot')
TD3_datacenter_hot_eval <-
  load_data('data/data_eval_datacenter/TD3_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Hot')
RBC_datacenter_hot_eval <-
  load_data('data/data_eval_datacenter/RBC_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Hot')
RAND_datacenter_hot_eval <-
  load_data('data/data_eval_datacenter/RAND_datacenter_hot/progress.csv') %>%
  mutate(Agent = 'RAND', Climate = 'Hot')

# datacenter-cool
PPO_datacenter_cool_eval <-
  load_data('data/data_eval_datacenter/PPO_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'PPO', Climate = 'Cool')
SAC_datacenter_cool_eval <-
  load_data('data/data_eval_datacenter/SAC_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'SAC', Climate = 'Cool')
TD3_datacenter_cool_eval <-
  load_data('data/data_eval_datacenter/TD3_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'TD3', Climate = 'Cool')
RBC_datacenter_cool_eval <-
  load_data('data/data_eval_datacenter/RBC_datacenter_cool/progress.csv') %>%
  mutate(Agent = 'RBC', Climate = 'Cool')
RAND_datacenter_cool_eval <-
  load_data('data/data_eval_datacenter/RAND_datacenter_cool/progress.csv') %>%
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
  factor(data_5Zone_eval$Agent,
         levels = c('PPO', 'SAC', 'TD3', 'RBC', 'RAND'))
data_datacenter_eval$Agent <-
  factor(data_datacenter_eval$Agent,
         levels = c('PPO', 'SAC', 'TD3', 'RBC', 'RAND'))

## Evaluation metrics

plot_metrics(data_5Zone_eval)
ggsave(
  'img/eval_metrics_5Zone.png',
  units = 'px',
  width = 2875,
  height = 1930
)

plot_metrics(data_datacenter_eval)
ggsave(
  'img/eval_metrics_datacenter.png',
  units = 'px',
  width = 2875,
  height = 1930
)

## Evolution of temperatures over one year

df_temperatures_TD3_datacenter <-
  read_csv('data/data_eval_datacenter/TD3_datacenter_hot/Eplus-env-sub_run19/monitor.csv') %>% slice(-c(1))
df_temperatures_RBC_datacenter <-
  read_csv('data/data_eval_datacenter/RBC_datacenter_hot/Eplus-env-sub_run19/monitor.csv') %>% slice(-c(1))

plot_temperatures_datacenter(df_temperatures_TD3_datacenter)
ggsave(
  'img/temperatures_TD3_datacenter_hot.png',
  units = 'px',
  width = 3100,
  height = 2120
)

plot_temperatures_datacenter(df_temperatures_RBC_datacenter)
ggsave(
  'img/temperatures_RBC_datacenter_hot.png',
  units = 'px',
  width = 3100,
  height = 2120
)

df_temperatures_SAC_5Zone <-
  read_csv('data/data_eval_5Zone/SAC_5Zone_mixed/Eplus-env-sub_run19/monitor.csv') %>% slice(-c(1))

plot_temperatures_5Zone(df_temperatures_SAC_5Zone)
ggsave(
  'img/temperatures_SAC_5Zone_mixed.png',
  units = 'px',
  width = 2835,
  height = 1710
)

##################################### ROBUSTNESS TESTS #####################################

## Load data

# 5Zone-cool
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

df_robustness_cool <-
  rbind(SAC_cool_cool, SAC_cool_hot, SAC_cool_mixed) %>%
  select(`Episode` = 'episode_num', `Mean reward` = 'mean_reward', Train, Test)

# 5Zone-mixed
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

df_robustness_mixed <-
  rbind(SAC_mixed_cool, SAC_mixed_hot, SAC_mixed_mixed) %>%
  select(`Episode` = 'episode_num', `Mean reward` = 'mean_reward', Train, Test)

# 5Zone-hot
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

df_robustness_hot <-
  rbind(SAC_hot_cool, SAC_hot_hot, SAC_hot_mixed) %>%
  select(`Episode` = 'episode_num', `Mean reward` = 'mean_reward', Train, Test)

## Plot rewards

df_robustness_all <-
  rbind(df_robustness_cool, df_robustness_mixed, df_robustness_hot)
df_robustness_all$Train <-
  factor(df_robustness_all$Train, levels = c('Cool', 'Mixed', 'Hot'))
df_robustness_all$Test <-
  factor(df_robustness_all$Test, levels = c('Cool', 'Mixed', 'Hot'))

RBC_mean_reward_cool <-
  RBC_5Zone_cool_eval %>% summarise(mean(mean_reward)) %>% as.double
RBC_mean_reward_mixed <-
  RBC_5Zone_mixed_eval %>% summarise(mean(mean_reward)) %>% as.double
RBC_mean_reward_hot <-
  RBC_5Zone_hot_eval %>% summarise(mean(mean_reward)) %>% as.double


data_RBC <- data.frame(
  Test = c('Cool', 'Mixed', 'Hot'),
  hline = c(
    RBC_mean_reward_cool,
    RBC_mean_reward_mixed,
    RBC_mean_reward_hot
  )
)

df_robustness_all %>% ggplot() + geom_boxplot(aes(y = `Mean reward`, col = Train)) +
  facet_grid(Test ~ Train, switch = 'y', scales = 'fixed') + theme_bw() + theme(legend.position = 'None',
                                                                                strip.background = element_rect(fill = 'grey98')) +
  scale_colour_manual('',
                      values = c(
                        'Mixed' = 'forestgreen',
                        'Cool' = 'steelblue',
                        'Hot' = 'red'
                      )) +
  scale_y_continuous(
    position = 'right',
    breaks = seq(0, -0.5, -0.05),
    sec.axis = sec_axis( ~ ., breaks = NULL, name = 'Test')
  ) +
  scale_x_continuous(breaks = NULL,
                     sec.axis = sec_axis( ~ ., breaks = NULL, name = 'Train')) +
  geom_hline(data = data_RBC, aes(yintercept = hline), linetype = 'dotted')
ggsave(
  'img/robustness_test_SAC_5Zone.png',
  units = 'px',
  width = 1800,
  height = 1150
)

## Stats

df_robustness_all %>% group_by(Train, Test) %>% summarise(mean = mean(`Mean reward`), sd =
                                                            sd(`Mean reward`))

## Barplots

df_robustness_all_2 <- df_robustness_all %>%
  group_by(Train, Test) %>%
  summarise(`SAC` = mean(`Mean reward`)) %>%
  mutate(`RBC` = case_when(
    Test == "Cool" ~ data_RBC$hline[1],
    Test == "Mixed" ~ data_RBC$hline[2],
    Test == "Hot" ~ data_RBC$hline[3],
  ), Diff = `RBC` - `SAC`)

df_long_rb <- df_robustness_all_2 %>% select(-Diff) %>% 
  pivot_longer(cols = c(`SAC`, `RBC`), names_to = "Agent", values_to = "Mean ep. reward")

df_long_rb %>% ggplot(aes(x=Agent, y=`Mean ep. reward`)) + 
  geom_col(aes(fill=Agent)) + 
   geom_text(aes(label = paste0(round(`Mean ep. reward`, 2))), 
            position = position_stack(vjust = 0.1), color = "black", size = 2.5) + 
  facet_grid(Test ~ Train, switch = 'both', scales = 'fixed') + 
  theme_bw() + 
  theme(legend.position = 'top', legend.title = element_blank(), strip.background = element_rect(fill = 'grey98'), 
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.text.y = element_text(size = 8)) + 
  scale_y_continuous(
    position = 'right',
    breaks = c(-0.1, -0.25, -0.45),
    sec.axis = sec_axis( ~ ., breaks = NULL, name = 'Test')
  ) + 
  xlab('Train')

ggsave(
  'img/robustness_test_SAC_5Zone_bars.png',
  units = 'px',
  width = 1800,
  height = 1150
)

####################################### SEQ LEARNING ########################################

## Load data

SAC_CV_cool <-
  load_data('data/data_eval_cv_learning/SAC_CV_cool/progress.csv') %>%
  mutate(Agent = 'SAC (cool, mixed, hot)', Climate = 'Cool') %>% slice_head(n = 20)

SAC_CV_mixed <-
  load_data('data/data_eval_cv_learning/SAC_CV_mixed/progress.csv') %>%
  mutate(Agent = 'SAC (cool, mixed, hot)', Climate = 'Mixed') %>% slice_head(n = 20)

SAC_CV_hot <-
  load_data('data/data_eval_cv_learning/SAC_CV_hot/progress.csv') %>%
  mutate(Agent = 'SAC (cool, mixed, hot)', Climate = 'Hot') %>% slice_head(n = 20)

data_CV <-
  rbind(
    SAC_CV_cool,
    SAC_CV_mixed,
    SAC_CV_hot,
    SAC_5Zone_cool_eval,
    SAC_5Zone_hot_eval,
    SAC_5Zone_mixed_eval
  )

data_CV$Climate <-
  factor(data_CV$Climate, levels = c('Cool', 'Mixed', 'Hot'))

## Plot metrics

plot_metrics(data_CV)
ggsave(
  'img/eval_CV_learning_SAC_5Zone.png',
  units = 'px',
  width = 2875,
  height = 1930
)

############################## COMFORT-demand TRADE-OFF ###############################

## Load data

# SAC 5Zone cool
SAC_5Zone_cool_25_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_cool_25_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 25% comf.', Climate = 'Cool') %>% slice_head(n = 20)
SAC_5Zone_cool_50_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_cool_50_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 50% comf.', Climate = 'Cool') %>% slice_head(n = 20)
SAC_5Zone_cool_75_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_cool_75_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 75% comf.', Climate = 'Cool') %>% slice_head(n = 20)
SAC_5Zone_cool_full_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_cool_full_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 100% comf.', Climate = 'Cool') %>% slice_head(n = 20)

# SAC 5Zone mixed
SAC_5Zone_mixed_25_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_mixed_25_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 25% comf.', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_5Zone_mixed_50_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_mixed_50_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 50% comf.', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_5Zone_mixed_75_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_mixed_75_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 75% comf.', Climate = 'Mixed') %>% slice_head(n = 20)
SAC_5Zone_mixed_full_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_mixed_full_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 100% comf.', Climate = 'Mixed') %>% slice_head(n = 20)

# SAC 5Zone hot
SAC_5Zone_hot_25_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_25_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 25% comf.', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_50_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_50_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 50% comf.', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_75_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_75_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 75% comf.', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_full_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_full_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 100% comf.', Climate = 'Hot') %>% slice_head(n = 20)

# TD3 datacenter cool
TD3_datacenter_cool_25_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_cool_25_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 25% comf.', Climate = 'Cool') %>% slice_head(n = 20)
TD3_datacenter_cool_50_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_cool_50_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 50% comf.', Climate = 'Cool') %>% slice_head(n = 20)
TD3_datacenter_cool_75_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_cool_75_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 75% comf.', Climate = 'Cool') %>% slice_head(n = 20)
TD3_datacenter_cool_full_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_cool_full_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 100% comf.', Climate = 'Cool') %>% slice_head(n = 20)

# TD3 datacenter mixed
TD3_datacenter_mixed_25_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_mixed_25_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 25% comf.', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_datacenter_mixed_50_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_mixed_50_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 50% comf.', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_datacenter_mixed_75_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_mixed_75_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 75% comf.', Climate = 'Mixed') %>% slice_head(n = 20)
TD3_datacenter_mixed_full_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_mixed_full_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 100% comf.', Climate = 'Mixed') %>% slice_head(n = 20)

# TD3 datacenter hot
TD3_datacenter_hot_25_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_hot_25_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 25% comf.', Climate = 'Hot') %>% slice_head(n = 20)
TD3_datacenter_hot_50_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_hot_50_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 50% comf.', Climate = 'Hot') %>% slice_head(n = 20)
TD3_datacenter_hot_75_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_hot_75_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 75% comf.', Climate = 'Hot') %>% slice_head(n = 20)
TD3_datacenter_hot_full_comfort <-
  load_data('data/data_eval_tradeoff/TD3_datacenter_hot_full_comfort/progress.csv') %>%
  mutate(Agent = 'TD3 100% comf.', Climate = 'Hot') %>% slice_head(n = 20)

data_tradeoff_SAC <- rbind(
  SAC_5Zone_cool_25_comfort,
  SAC_5Zone_cool_50_comfort,
  SAC_5Zone_cool_75_comfort,
  SAC_5Zone_cool_full_comfort,
  SAC_5Zone_mixed_25_comfort,
  SAC_5Zone_mixed_50_comfort,
  SAC_5Zone_mixed_75_comfort,
  SAC_5Zone_mixed_full_comfort,
  SAC_5Zone_hot_25_comfort,
  SAC_5Zone_hot_50_comfort,
  SAC_5Zone_hot_75_comfort,
  SAC_5Zone_hot_full_comfort
)

data_tradeoff_TD3 <- rbind(
  TD3_datacenter_cool_25_comfort,
  TD3_datacenter_cool_50_comfort,
  TD3_datacenter_cool_75_comfort,
  TD3_datacenter_cool_full_comfort,
  TD3_datacenter_mixed_25_comfort,
  TD3_datacenter_mixed_50_comfort,
  TD3_datacenter_mixed_75_comfort,
  TD3_datacenter_mixed_full_comfort,
  TD3_datacenter_hot_25_comfort,
  TD3_datacenter_hot_50_comfort,
  TD3_datacenter_hot_75_comfort,
  TD3_datacenter_hot_full_comfort
)

data_tradeoff_SAC$Agent <- factor(
  data_tradeoff_SAC$Agent,
  levels = c('SAC 25% comf.', 'SAC 50% comf.', 'SAC 75% comf.', 'SAC 100% comf.')
)
data_tradeoff_SAC$Climate <- factor(data_tradeoff_SAC$Climate,
                                    levels = c('Cool', 'Mixed', 'Hot'))

data_tradeoff_TD3$Agent <- factor(
  data_tradeoff_TD3$Agent,
  levels = c('TD3 25% comf.', 'TD3 50% comf.', 'TD3 75% comf.', 'TD3 100% comf.')
)
data_tradeoff_TD3$Climate <- factor(data_tradeoff_TD3$Climate,
                                    levels = c('Cool', 'Mixed', 'Hot'))

plot_metrics(data_tradeoff_SAC, label_size = 8)
ggsave(
  'img/eval_results_SAC_5Zone_tradeoff.png',
  units = 'px',
  width = 3800,
  height = 1890
)

plot_metrics(data_tradeoff_TD3, label_size = 8)
ggsave(
  'img/eval_results_TD3_datacenter_tradeoff.png',
  units = 'px',
  width = 3800,
  height = 1890
)

##################################### ADDITIONAL PLOTS #####################################

###### Graphical abstract ######

plot_metrics(data_datacenter_eval)
ggsave(
  'img/graphical_abstract.png',
  units = 'px',
  width = 1980,
  height = 1712
)

###### Detailed comfort violation ######

df_temperatures_SAC_5Zone_comf_viol <-
  get_comf_viol_5Zone(df_temperatures_SAC_5Zone)
plot_comf_viol_5Zone(df_temperatures_SAC_5Zone_comf_viol)
ggsave(
  'img/comfort_violations_SAC_5Zone_mixed.png',
  units = 'px',
  width = 1585,
  height = 1220
)

df_temperatures_TD3_datacenter_comf_viol <-
  get_comf_viol_datacenter(df_temperatures_TD3_datacenter)
plot_comf_viol_datacenter(df_temperatures_TD3_datacenter_comf_viol)
ggsave(
  'img/comfort_violations_TD3_datacenter_hot.png',
  units = 'px',
  width = 1585,
  height = 1220
)

###### SEQ learning reward overview (all, cool, mixed, hot) ######

SAC_CV_cool <-
  SAC_CV_cool %>% mutate(Train = 'All', Test = 'Cool') %>%
  select(`Episode` = 'episode_num', `Mean reward` = 'mean_reward', Train, Test)
SAC_CV_mixed <-
  SAC_CV_mixed %>% mutate(Train = 'All', Test = 'Mixed') %>%
  select(`Episode` = 'episode_num', `Mean reward` = 'mean_reward', Train, Test)
SAC_CV_hot <- SAC_CV_hot %>% mutate(Train = 'All', Test = 'Hot') %>%
  select(`Episode` = 'episode_num', `Mean reward` = 'mean_reward', Train, Test)

data_CV <-
  rbind(SAC_CV_cool,
        SAC_CV_mixed,
        SAC_CV_hot,
        df_robustness_all)

data_CV$Train <-
  factor(data_CV$Train, levels = c('All', 'Cool', 'Mixed', 'Hot'))
data_CV$Test <-
  factor(data_CV$Test, levels = c('Cool', 'Mixed', 'Hot'))

RBC_mean_reward_cool <-
  RBC_5Zone_cool_eval %>% summarise(mean(mean_reward)) %>% as.double
RBC_mean_reward_mixed <-
  RBC_5Zone_mixed_eval %>% summarise(mean(mean_reward)) %>% as.double
RBC_mean_reward_hot <-
  RBC_5Zone_hot_eval %>% summarise(mean(mean_reward)) %>% as.double

data_RBC <- data.frame(
  Test = c('Cool', 'Mixed', 'Hot'),
  hline = c(
    RBC_mean_reward_cool,
    RBC_mean_reward_mixed,
    RBC_mean_reward_hot
  )
)

data_CV %>% ggplot() + geom_boxplot(aes(y = `Mean reward`, col = Train)) +
  facet_grid(Test ~ Train, switch = 'y', scales = 'fixed') + theme_bw() +
  theme(legend.position = 'None',
        strip.background = element_rect(fill = 'grey98')) +
  scale_colour_manual(
    '',
    values = c(
      'Mixed' = 'forestgreen',
      'Cool' = 'steelblue',
      'Hot' = 'red',
      'All' = 'orange'
    )
  ) +
  scale_y_continuous(
    position = 'right',
    breaks = seq(-.1,-.6,-.05),
    sec.axis = sec_axis( ~ ., breaks = NULL, name = 'Test')
  ) +
  scale_x_continuous(breaks = NULL,
                     sec.axis = sec_axis( ~ ., breaks = NULL, name = 'Train')) +
  geom_hline(data = data_RBC, aes(yintercept = hline), linetype = 'dotted')
ggsave(
  'img/climates_SAC_5Zone_comparison.png',
  units = 'px',
  width = 1900,
  height = 1500
)

## Barplots

data_CV_all <- data_CV %>%
  group_by(Train, Test) %>%
  summarise(`SAC` = mean(`Mean reward`)) %>%
  mutate(`RBC` = case_when(
    Test == "Cool" ~ data_RBC$hline[1],
    Test == "Mixed" ~ data_RBC$hline[2],
    Test == "Hot" ~ data_RBC$hline[3],
  ), Diff = `RBC` - `SAC`)

df_long_all <- data_CV_all %>% select(-Diff) %>% 
  pivot_longer(cols = c(`SAC`, `RBC`), names_to = "Agent", values_to = "Mean ep. reward")

df_long_all %>% ggplot(aes(x=Agent, y=`Mean ep. reward`)) + 
  geom_col(aes(fill=Agent)) + 
   geom_text(aes(label = paste0(round(`Mean ep. reward`, 2))), 
            position = position_stack(vjust = 0.1), color = "black", size = 2.5) +
  facet_grid(Test ~ Train, switch = 'both', scales = 'fixed') + 
  theme_bw() + 
  theme(legend.position = 'top', legend.title = element_blank(), strip.background = element_rect(fill = 'grey98'),
        axis.ticks.x=element_blank(), axis.text.x=element_blank(), axis.text.y = element_text(size = 8)) + 
  scale_y_continuous(
    position = 'right',
    breaks = c(-0.1, -0.25, -0.45),
    sec.axis = sec_axis( ~ ., breaks = NULL, name = 'Test')
  ) + 
  xlab('Train')

ggsave(
  'img/climates_SAC_5Zone_comparison_bars.png',
  units = 'px',
  width = 1800,
  height = 1150
)

###### Catastrophic forgetting check ######

SAC_CV_cool <-
  load_data('data/data_eval_cv_learning/SAC_CV_cool/progress.csv') %>%
  mutate(Agent = 'SAC (cool, mixed, hot)', Climate = 'Cool') %>% slice_head(n = 20)

SAC_CV_mixed <-
  load_data('data/data_eval_cv_learning/SAC_CV_mixed/progress.csv') %>%
  mutate(Agent = 'SAC (cool, mixed, hot)', Climate = 'Mixed') %>% slice_head(n = 20)

SAC_CV_hot <-
  load_data('data/data_eval_cv_learning/SAC_CV_hot/progress.csv') %>%
  mutate(Agent = 'SAC (cool, mixed, hot)', Climate = 'Hot') %>% slice_head(n = 20)

SAC_CV_cool_2 <-
  load_data('data/data_eval_cv_learning/SAC_CV_cool_2/progress.csv') %>%
  mutate(Agent = 'SAC (hot, mixed, cool)', Climate = 'Cool') %>% slice_head(n = 20) %>%
  select(names(SAC_CV_cool))

SAC_CV_mixed_2 <-
  load_data('data/data_eval_cv_learning/SAC_CV_mixed_2/progress.csv') %>%
  mutate(Agent = 'SAC (hot, mixed, cool)', Climate = 'Mixed') %>% slice_head(n = 20) %>%
  select(names(SAC_CV_mixed))

SAC_CV_hot_2 <-
  load_data('data/data_eval_cv_learning/SAC_CV_hot_2/progress.csv') %>%
  mutate(Agent = 'SAC (hot, mixed, cool)', Climate = 'Hot') %>% slice_head(n = 20) %>%
  select(names(SAC_CV_hot))

data_CV_2 <-
  rbind(
    SAC_CV_cool,
    SAC_CV_mixed,
    SAC_CV_hot,
    SAC_CV_cool_2,
    SAC_CV_mixed_2,
    SAC_CV_hot_2,
    SAC_5Zone_cool_eval,
    SAC_5Zone_hot_eval,
    SAC_5Zone_mixed_eval
  )

data_CV_2$Climate <-
  factor(data_CV_2$Climate, levels = c('Cool', 'Mixed', 'Hot'))

data_CV_2 %>%
  select('Agent', `Mean episode reward` = 'mean_reward', 'Climate') %>%
  ggplot(aes(y = `Mean episode reward`, x = Agent, col = Agent)) +
  geom_boxplot() +
  facet_wrap(~ Climate, scale = 'free') +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      size = 8,
      hjust = 1
    ),
    legend.title = element_blank(),
    strip.background = element_rect(fill = 'grey98')
  ) +
  labs(title = 'Mean episode rewards') +
  scale_colour_brewer('', palette = 'Set1')
ggsave(
  'img/eval_CV_learning_SAC_5Zone_2.png',
  units = 'px',
  width = 2657,
  height = 1485
)

###### Comfort-demand tradeoff exceptions ######

## Additional SAC-5Zone-hot datasets

SAC_5Zone_hot_95_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_95_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 95% comf.', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_97_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_97_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 97% comf.', Climate = 'Hot') %>% slice_head(n = 20)
SAC_5Zone_hot_99_comfort <-
  load_data('data/data_eval_tradeoff/SAC_5Zone_hot_99_comfort/progress.csv') %>%
  mutate(Agent = 'SAC 99% comf.', Climate = 'Hot') %>% slice_head(n = 20)

sac_comfort_weights <- rbind(
  SAC_5Zone_hot_25_comfort,
  SAC_5Zone_hot_50_comfort,
  SAC_5Zone_hot_75_comfort,
  SAC_5Zone_hot_95_comfort,
  SAC_5Zone_hot_97_comfort,
  SAC_5Zone_hot_99_comfort,
  SAC_5Zone_hot_full_comfort
)

sac_comfort_weights$Agent <- factor(
  sac_comfort_weights$Agent,
  levels = c(
    'SAC 25% comf.',
    'SAC 50% comf.',
    'SAC 75% comf.',
    'SAC 95% comf.',
    'SAC 97% comf.',
    'SAC 99% comf.',
    'SAC 100% comf.'
  )
)

sac_comfort_weights$Climate <- factor(sac_comfort_weights$Climate,
                                      levels = c('Cool', 'Mixed', 'Hot'))

plot_mean_comf_viol(sac_comfort_weights)
ggsave(
  'img/eval_tradeoff_SAC_weights_comfort.png',
  units = 'px',
  width = 2551,
  height = 1202
)
