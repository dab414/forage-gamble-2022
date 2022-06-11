### rvts eda
### basic idea is im gonna use this script as a space to just riff around and see what's good in the data
### up here keep a comment log of stuff i've looked into
### dont necessarily keep the code for stuff that didn't work out, try to keep this script as clean as possible


## log

## frober & dreisbach stuff
## doing a ttest for increasing vs. steady high in disconnect is a strong result
##    no effects of RTs, no one way anova including other transition conditions (p = .07)

## how about a regression comparing difference > 0 across both studies?
  ## the interaction washes out when using a mixed model
  ## main effect of experiment, prob just more switching in foraging
  ## even no main effect of difference

## looking back at b&a2018
  ## we were all into the current v other stuff
  ## essentially, when difference = 0, is there a bias to be more sensitive to changes in value of the task just performed than not just performed?

library(data.table)
library(tidyverse)
library(ez)
library(lme4)

d <- read.csv('../../data/rvts.csv')
head(d)


#### sudden reward increases ####
N_disconnect <- length(unique(d[d$experiment=='disconnect',]$subject))

pt <- d %>% 
  filter(experiment == 'disconnect', difference == 0, rightpoints == 3 | rightpoints == 1) %>% 
  mutate(point_transition = ifelse(shift(rightpoints) == 1 & rightpoints == 3, 'increasing',
                                   ifelse(shift(rightpoints) == 3 & rightpoints == 3, 'stay high',
                                   ifelse(shift(rightpoints) == 1 & rightpoints == 1, 'stay low',
                                   ifelse(shift(rightpoints) == 3 & rightpoints == 1, 'decreasing', ''))))) %>% 
  filter(totalPoints > shift(totalPoints), !is.na(point_transition)) %>% 
  group_by(subject, point_transition) %>% 
  summarize(rt_ = mean(rt), transcode_ = mean(transcode)) 
pt %>% 
  group_by(point_transition) %>% 
  summarize(rt_mean = mean(rt_), rt_se = sd(rt_) / sqrt(N_disconnect), transcode_mean = mean(transcode_), transcode_se = sd(transcode_) / sqrt(N_disconnect)) %>% 
  gather(col, outcome, rt_mean:transcode_se) %>% 
  separate(col, into = c('metric', 'measure'), sep = '_') %>% 
  spread(measure, outcome) %>% 
  mutate(metric = factor(metric)) %>% 
  mutate(metric = recode(metric, `rt` = 'Reponse Time (ms)', `transcode` = 'Proportion of Switching')) %>% 
  ggplot(aes(x = point_transition, y = mean)) + 
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .5) + 
  facet_wrap(~metric, scales = 'free') + 
  labs(
    x = 'Point Transition Condition',
    y = ''
  ) + 
  theme_bw() + 
  theme(strip.background = element_rect(color = 'black', fill = 'white'))

  

m1 <- ezANOVA(wid = subject, within = .(point_transition), dv = transcode_, detailed = TRUE, data = pt)

increasing <- pt[pt$point_transition == 'increasing',]$transcode_
stay_high <- pt[pt$point_transition == 'stay high',]$transcode_
t.test(increasing, stay_high, paired = TRUE)

## THAT'S A RESULT!!!!!

#### regression of difference > 0 ####

d_constrain <- d[d$difference > 0,]

m2 <- glm(transcode ~ difference * experiment, data = d_constrain, family = binomial(link = 'logit'))
m2_glmer <- glmer(transcode ~ difference * experiment + (1 + difference | subject), data = d_constrain, family = binomial(link = 'logit'))

## model fits
m2_glmer_onedown <- glmer(transcode ~ difference * experiment + (1 | subject) + (0 + difference | subject), data = d_constrain, family = binomial(link = 'logit'))
anova(m2_glmer, m2_glmer_onedown)


## hypothesis testing
m2_glmer_noint <- glmer(transcode ~ difference + experiment + (1 + difference | subject), data = d_constrain, family = binomial(link = 'logit'))
anova(m2_glmer, m2_glmer_noint)


#### current v other ####

## when difference is 0, values could have increased, decreased, or stayed same, across both current / other

## this is pretty interesting and pretty difficult to think about
## im interested by what might be going on with disconnect
  ## it's another advantage of this paradigm, we parametrically manipulate these factors so we can see all this
  ## difference = 0, so in theory switch rates shouldn't differ across these conditions 

d %>% 
  filter(difference == 0) %>% 
  group_by(subject, current_transition, other_transition, experiment) %>% 
  summarize(transcode_ = mean(transcode)) %>% 
  group_by(current_transition, other_transition, experiment) %>% 
  summarize(transcode = mean(transcode_), se = sd(transcode_) / sqrt(78)) %>% 
  filter(!is.na(current_transition)) %>% 
  ggplot(aes(x = current_transition, y = transcode, group = other_transition)) + 
  geom_bar(stat = 'identity', aes(fill = other_transition), position = position_dodge(width = .9)) + 
  geom_errorbar(aes(ymin = transcode - se, ymax = transcode + se), position = position_dodge(width = .9), width = .5) + 
  facet_wrap(~experiment) + 
  labs(
    x = 'Current Transition',
    y = 'Proportion of Switching', 
    fill = 'Other Transition'
  ) + 
 theme_bw() + 
  theme(strip.background = element_rect(fill = 'white', color = 'black'))



a <- d[d$difference==0 & d$experiment=='disconnect',]

a <- a %>% 
  mutate(current_transition_simple = ifelse(current_transition == 'constant', 'Constant', 'Change'),
         other_transition_simple = ifelse(other_transition == 'constant', 'Constant', 'Change')) %>% 
  group_by(subject, current_transition_simple, other_transition_simple) %>% 
  summarize(transcode = mean(transcode), count = n())

## cell counts
 a %>% 
  group_by(current_transition_simple, other_transition_simple) %>% 
  summarize(count = sum(count))  
   
 ## drop Ps with missing cells
 bad_subjects <- a %>% 
   group_by(subject) %>% 
   summarize(count = n()) %>% 
   filter(count < 9) %>% 
   select(subject)
 bad_subjects <- bad_subjects$subject
 
a <- a[!a$subject %in% bad_subjects,]

ezANOVA(wid = subject, within = .(current_transition_simple, other_transition_simple), dv = transcode, detailed = TRUE, data = a)

a %>% 
  group_by(current_transition_simple, other_transition_simple) %>% 
  summarize(transcode_ = mean(transcode), se = sd(transcode) / sqrt(N_disconnect)) %>% 
  mutate(other_transition_simple = factor(other_transition_simple, levels = c('Constant', 'Change')),
         current_transition_simple = factor(current_transition_simple, levels = c('Constant', 'Change'))) %>% 
  ggplot(aes(x = current_transition_simple, y = transcode_), group = other_transition_simple) +
  geom_bar(stat = 'identity', aes(fill = other_transition_simple), position = position_dodge(width = .9), color = 'black') +
  geom_errorbar(aes(ymin = transcode_ - se, ymax = transcode_ + se, group = other_transition_simple), position = position_dodge(width = .9), width = .5) + 
  ylim(0,.5) +
  labs(
    x = 'Current Task',
    y = 'Proportion Switching',
    fill = 'Other Task'
  ) +
  scale_fill_manual(values = c('Constant' = 'white', 'Change' = 'dark grey')) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
       panel.grid = element_blank(),
       legend.position = c(.2,.8))

















