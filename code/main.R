# main.R
# Created: 11/21
# Author:  John Stromme

# This code runs analysis on nba last 2 minute reports


# Preliminaries ----------------------------------------------------------

rm(list = ls()) # clear workspace
setwd("/Users/john/Dropbox/Documents/Wisconsin/Research/nba/nba-l2m/")

library(tidyverse)
library(tidylog, warn.conflicts = F); options("tidylog.display" = list(print))
library(ggthemes)



# Load data --------------
#   source: https://atlhawksfanatic.github.io/L2M/
df <- read_csv('./data/L2M.csv')

# some initial cleaning of the data
df2 <- df %>% 
  filter(!is.na(decision)) %>% 
  #score differential
  mutate(scorediff = abs(home_score - away_score),
         leadingside = case_when(home_score > away_score ~ 'home',
                                 home_score == away_score ~ 'tie',
                                 T ~ 'away')) %>% 
  #keep only vars we need
  select(season, date, period, decision, disadvantaged_team, disadvantaged_side, committing_team, attendance,
         type2, playoff, comments, home_team, away_team, scorediff, leadingside) %>% 
  #The way the nba labels 'disadvantaged' player is non-intuitive. First of all, we can ignore 
  #   correct calls... my interpretation of 'disadvantaged' is that relative to a baseline of
  #   100% correct officiating, only when there is an incorrect call can a 'disadvantage' be
  #   given.
  mutate(disadvantaged_team = if_else(decision %in% c('CC','CNC'), NA_character_, disadvantaged_team),
         disadvantaged_side = if_else(decision %in% c('CC','CNC'), NA_character_, disadvantaged_side)) %>% 
  #Second of all, if there is an incorrect call (IC), the 'disadvantaged' player listed
  #    is actually who *benefitted* by it being a call. Disadvantaged means they
  #    were fouled.
  #Third of all, if it is an incorrect noncall (INC), the 'disadvantaged' player
  #    follows my definition of 'disadvantaged'.
  #Therefore, fore all ICs, we need to change the 'disadvantaged' side to the other team
  mutate(disbackup = disadvantaged_team) %>% 
  mutate(nondis_team = if_else(disadvantaged_team==home_team, away_team, home_team)) %>% 
  mutate(disadvantaged_team = if_else(decision == 'IC', nondis_team, disadvantaged_team)) %>% 
  #now a flag to seperate out 'disadvantaged' type, 
  # i.e. can be disadvantaged by a NC for the opponent, OR  an IC on your team.
  mutate(disadvantaged_type = if_else(disadvantaged_team == committing_team,1,0)) %>% 
  mutate(decision_type = if_else(!is.na(disadvantaged_type),paste0(decision, '_', disadvantaged_type),NA_character_)) %>% 
  # there are a few unexpected results, reclassify them
  mutate(nondis_team = if_else(disadvantaged_team==home_team, away_team, home_team)) %>% 
  mutate(disadvantaged_team = if_else(decision_type == 'INC_1', nondis_team, disadvantaged_team)) %>% 
  mutate(disadvantaged_team = if_else(decision_type == 'IC_0', nondis_team, disadvantaged_team)) %>% 
  # rerun types
  mutate(disadvantaged_type = if_else(disadvantaged_team == committing_team,1,0)) %>% 
  mutate(decision_type = if_else(!is.na(disadvantaged_type),paste0(decision, '_', disadvantaged_type),NA_character_)) %>% 
  select(-nondis_team) %>% 
  #also fix the 'side' disadvantaged
  mutate(nondis_side = if_else(disadvantaged_side=='home','away','home')) %>% 
  mutate(disadvantaged_side = if_else(disadvantaged_team != disbackup,nondis_side, disadvantaged_side)) %>% 
  select(-nondis_side)
  
  #looks ok now so can remove decision_type
table(df2$decision_type)
df2 <- df2 %>% select(-decision_type)

# also need to classify the 'correct calls' and 'correct non-calls'... if
#    there were actually incorrect, who would they disadvantage?
# if a 'correct call', then an 'incorrect non-call' would disadvantage the non-comitting team
# if a 'correct non-call' then an 'incorrect call' would disadvantage the comitting team
df2 <- df2 %>% 
  mutate(noncom_team = if_else(committing_team == home_team, away_team, home_team),
         noncom_side = if_else(noncom_team==home_team,'home','away'),
         com_side    = if_else(noncom_team==home_team,'away','home')) %>% 
  mutate(disadvantaged_team = case_when(decision=='CC' ~ noncom_team,
                                         decision=='CNC' ~ committing_team,
                                         T ~ disadvantaged_team),
         disadvantaged_side = case_when(decision=='CC' ~ noncom_side,
                                        decision=='CNC' ~ com_side,
                                        T ~ disadvantaged_side)) %>% 
  #'correct' variable will be useful for classifying/reshaping
  mutate(correct = if_else(decision %in% c('CC','CNC'), 1, 0))



# things to look at:
# 1. Does the rate of blown calls differ by score differential? 
#    - could look at each type of blown call, i.e. non-call or wrong-call,
#    -    and also if it matters if the call is for up or down team
plotdf <- df2 %>% 
  filter(!is.na(scorediff),
         scorediff < 25) %>% 
  group_by(scorediff) %>% 
  # calculate correct call ratios
  summarise(CC  = sum(decision == 'CC'),
            CNC = sum(decision == 'CNC'),
            IC  = sum(decision == 'IC'),
            INC = sum(decision == 'INC')) %>% 
  mutate(ICratio = IC / (CNC+IC),
         INCratio = INC / (CC+INC),
         Iratio = (IC+INC)/ (CNC+IC+CC+INC)) %>% 
  filter(scorediff <= 9) #sample size issue

#only incorecct non-calls seem to have some selection by game tightness.
#   whistle-swallowing affect for very close games.
ggplot(plotdf, aes(x=scorediff, y = INCratio)) + geom_line() +
  xlab('Final Score Differential') +
  ylab('Referee Missed-Call Error Rate (Type II Error)') +
  ggtitle("Referees Swallow Their Whistles More in Super-Close Games") +
  theme_minimal()
ggsave('./plots_figures/closegames.png',device = 'png', height = 5, width = 5,
       bg = '#FFFFFF')
ggplot(plotdf, aes(x=scorediff, y = ICratio)) + geom_line()
ggplot(plotdf, aes(x=scorediff, y = Iratio)) + geom_line()

#if call ratio differs by which team is up, then we may conclude refs are impacting who wins
#  (although cannot separate this from a story where refs simply call tighter or looser for a team that is down
#    and then those exact teams are also more likely to lose... b/c down right before end of game.
#    would need to link in the play-by-play data to tell, with live score at that time....)
plotdf <- df2 %>% 
  filter(!is.na(scorediff),
         scorediff < 25) %>% 
  group_by(scorediff, leadingside) %>% 
  # calculate correct call ratios
  summarise(CC  = sum(decision == 'CC'),
            CNC = sum(decision == 'CNC'),
            IC  = sum(decision == 'IC'),
            INC = sum(decision == 'INC')) %>% 
  mutate(ICratio = IC / (CNC+IC),
         INCratio = INC / (CC+INC),
         Iratio = (IC+INC)/ (CNC+IC+CC+INC)) %>% 
  filter(scorediff <= 9) #sample size issue
#no clear evidence
ggplot(plotdf, aes(x=scorediff, y = INCratio, color = leadingside)) + geom_line()
ggplot(plotdf, aes(x=scorediff, y = ICratio, color = leadingside)) + geom_line()
ggplot(plotdf, aes(x=scorediff, y = Iratio, color = leadingside)) + geom_line()


# 2. Does it differ by attendance? (but attendance is endogenous as well...)
# 4. playoff robustness check (whether to include playoff data or not)

#regression (only scorediff seems to mattera)
regdf <- df2 %>% 
  mutate(type = if_else(decision %in% c('IC','CNC'),0,1))

summary(lm(correct ~ type + scorediff + scorediff:type + leadingside + attendance 
           + attendance:disadvantaged_side +
             playoff,
    data = regdf))



# 5. then create a 2x2 matrix of rate you get fucked and rate they fuck the other team
#    the quadrants would be, refs want you to win, refs want you to lose, refs love you, refs hate you
plotdf <- df2 %>% 
  group_by(disadvantaged_team) %>% 
  # calculate correct call ratios
  summarise(CC  = sum(decision == 'CC'),
            CNC = sum(decision == 'CNC'),
            IC  = sum(decision == 'IC'),
            INC = sum(decision == 'INC')) %>% 
  mutate(ICratio = IC / (CNC+IC),
         INCratio = INC / (CC+INC),
         Iratio = (IC+INC)/ (CNC+IC+CC+INC)) %>% 
  filter(!is.na(disadvantaged_team)) %>% 
  arrange(disadvantaged_team)

# on average, how many calls in the last 2m?  5.06 calls, 11.2 noncalls
df2 %>% group_by(date, period, home_team, away_team) %>% 
  summarise(calls = sum(decision %in% c('CC','INC')),
            noncalls = sum(decision %in% c('CNC','IC'))) %>% 
  ungroup() %>% 
  summarise(calls = sum(calls),
            noncalls = sum(noncalls),
            games = n()) %>% 
  mutate(callspergame = calls / games,
         noncallspergame = noncalls / games)
  
# then can use these to predict for each team how many disadvantageous events per l2m
plotdf <- plotdf %>% 
  mutate(missingcallspergame = 5.06 * ICratio,
         missingnoncallspergame = 11.2 * INCratio) %>% 
  # and demean
  ungroup() %>% 
  mutate(missingcallspergame = missingcallspergame - mean(missingcallspergame),
         missingnoncallspergame = missingnoncallspergame - mean(missingnoncallspergame))

## PLOT
library(ggimage)
library(slickR)
plotdf$logoteam <- nba_team_logo$team
plotdf$logo <- nba_team_logo$uri
#alphabetized lines up except swap boston and brooklyn
last <- ncol(plotdf)
plotdf[2,(last-1):last] <- nba_team_logo[3,]
plotdf[3,(last-1):last] <- nba_team_logo[2,]
ggplot(plotdf, aes(x=missingcallspergame,y=missingnoncallspergame)) + 
  #geom_point()+
  geom_image(aes(image=logo),size=.1) +
  #theme_void() +
  theme_tufte() +
  xlim(-.04,0.04) +
  ylim(-0.5,0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  ylab('Expected Number of Detrimental Non-Calls Per Last Two Minutes') +
  xlab('Expected Number of Detrimental Calls Per Last Two Minutes')
ggsave('./plots_figures/whistledisadvantage.png',device = 'png', height = 5, width = 5,
       bg = '#FFFFFF')


# Notes:
# -  missingness in 'disadvantaged team' -> some seem like we could
#    assign disadvantage and the call is affected, but for others not so sure
#    I'm going with that the NBA was purposeful about not assigning these 
#    correct or incorrect and am dropping them.
