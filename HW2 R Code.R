
##### What does it mean to be a man? #####

library(ggplot2)
library(tidyverse)
library(dplyr)

# 1.) Reproduce the figures and tables in the article, but do separate analyses 
# for Straight and Gay / Bisexual men. Don’t do the breakup by age groups: 
# there aren’t many Gay / Bisexual men in the sample, so breaking up the 
# analysis further into age groups leaves us with very few observations in 
# some categories. Do you notice any differences? Explain what you see in a 
# few paragraphs, written in non-technical terms.

masc = read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/masculinity-survey.csv")
View(masc)

# Figure 1

figure1_all = matrix( c(60, 39,
                  1, 58, 41,
                  1, 81, 19, 0), nrow=3, ncol = 3, byrow=FALSE)

barplot(figure1_all,
        main="Do you think that society puts pressure on men in a way that is unhealthy or bad for them?", 
        horiz=TRUE,
        col=c("blue","red", "green"),
        xlab = "Percentage",
        xlim=c(0,100),
        args.legend = list(x ='topright', bty='n', inset=c(-.05,-.02)),
        names.arg=c("All Adult Men", "Straight", "Gay/Bisexual"),
        cex.names=.63)

# Figure 2

masc2 = masc[103:114, ]
View(masc2)

figure2 = masc2 %>% select(X, Sexual.Orientation, X.5)
figure2$value = c(54, 51, 50, 31, 34, 31, 21, 18, 16, 11, 5, 13)
figure2$value2 = c(64, 74, 52, 58, 25, 42, 35, 24, 40, 23, 12, 9)
View(figure2)

# Straight
ggplot(data = figure2) + 
  aes(x = X, y = value, fill = Sexual.Orientation) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Which of the following do you worry about on a daily or near daily 
          basis? (Straight Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(54, 51, 50, 31, 34, 31, 21, 18, 16, 11, 5, 13)))

# Gay
ggplot(data = figure2) + 
  aes(x = X, y = value2, fill = X.5) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Which of the following do you worry about on a daily or near daily 
          basis? (Gay/Homosexual Men)") +
  ylab("Share of Respondents") +
  xlab("Answers") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(64, 74, 52, 58, 25, 42, 35, 24, 40, 23, 12, 9)))

# Figure 3

masc3 = masc[124:131, ]
View(masc3)

figure3 = masc3 %>% select(X, Sexual.Orientation, X.5)
figure3$value = c(21, 17, 15, 13, 10, 7, 7, 61)
View(figure3)

# Straight
ggplot(data = figure3) + 
  aes(x = X, y = value, fill = Sexual.Orientation) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("In which of the following ways would you say it's an advantage to 
          be a man at your work right now? (Straight Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(21, 17, 15, 13, 10, 7, 7, 61)))

# No Gay/Homosexual Data

# Figure 4

masc4 = masc[133:137, ]
View(masc4)

figure4 = masc4 %>% select(X, Sexual.Orientation, X.5)
figure4$value = c(19, 43, 37, 5, 43)
View(figure4)

# Straight
ggplot(data = figure4) + 
  aes(x = X, y = value, fill = Sexual.Orientation) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("In which of the following ways would you say it's a disadvantage to 
          be a man at your work right now? (Straight Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(19, 43, 37, 5, 43)))

# No Gay/Homosexual Data

# Figure 5

masc5 = masc[168:173, ]
View(masc5)

figure5 = masc5 %>% select(X, Sexual.Orientation, X.5)
figure5$value = c(54, 26, 12, 1, 6, 1)
figure5$value2 = c(16, 28, 43, 5, 6, 0)
View(figure5)

# Straight
ggplot(data = figure5) + 
  aes(x = X, y = value, fill = Sexual.Orientation) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("How often do you try to be the one who pays when on a date? 
          (Straight Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(54, 26, 12, 1, 6, 1)))

# Gay
ggplot(data = figure5) + 
  aes(x = X, y = value2, fill = X.5) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("How often do you try to be the one who pays when on a date? 
          (Gay/Homosexual Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(16, 28, 43, 5, 6, 0)))

# Figure 6

masc6 = masc[183:188, ]
View(masc6)

figure6 = masc6 %>% select(X, Sexual.Orientation, X.5)
figure6$value = c(60, 46, 34, 31, 31, 8)
figure6$value2 = c(63, 51, 40, 31, 27, 8)
View(figure6)

# Straight
ggplot(data = figure6) + 
  aes(x = X, y = value, fill = Sexual.Orientation) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("When you want to be physically intimate with someone, 
          how do you gauge their interest? (Straight Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(60, 46, 34, 31, 31, 8)))

# Gay
ggplot(data = figure6) + 
  aes(x = X, y = value2, fill = X.5) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("When you want to be physically intimate with someone, 
          how do you gauge their interest? (Gay/Homosexual Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(63, 51, 40, 31, 27, 8)))

# 2.) Reproduce the figures and tables in the article as in part 1, 
# but now break up the analysis into White / non White men. Again, don’t 
# do the break up by age groups. Explain what you see.

# Figure 1:
figure1_whitenonwhite = matrix( c(58, 41,
                        1, 65, 35,
                        0), nrow=3, ncol = 2, byrow=FALSE)

barplot(figure1_whitenonwhite,
        main="Do you think that society puts pressure on men in a way that is unhealthy or bad for them?", 
        horiz=TRUE,
        col=c("purple","tan", "green"),
        xlab = "Percentage",
        xlim=c(0,100),
        args.legend = list(x ='topright', bty='n', inset=c(-.05,-.02)),
        names.arg=c("White", "None-white"),
        cex.names=.63)

# Figure 2

masc2 = masc[103:114, ]
View(masc2)

figure2 = masc2 %>% select(X, Race, X.3)
figure2$value = c(53, 54, 53, 35, 31, 33, 23, 17, 15, 13, 4, 14)
figure2$value2 = c(56, 51, 43, 31, 35, 30, 25, 23, 26, 12, 10, 10)
View(figure2)

# White
ggplot(data = figure2) + 
  aes(x = X, y = value, fill = Race) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Which of the following do you worry about on a daily or near daily 
          basis? (White Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(53, 54, 53, 35, 31, 33, 23, 17, 15, 13, 4, 14))) +
  scale_fill_discrete(name = "White Men")

# Non White
ggplot(data = figure2) + 
  aes(x = X, y = value2, fill = X.3) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Which of the following do you worry about on a daily or near daily 
          basis? (Non White Men)") +
  ylab("Share of Respondents") +
  xlab("Answers") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(56, 51, 43, 31, 35, 30, 25, 23, 26, 12, 10, 10))) +
  scale_fill_discrete(name = "Non White Men")

# Figure 3

masc3 = masc[124:131, ]
View(masc3)

figure3 = masc3 %>% select(X, Race, X.3)
figure3$value = c(20, 16, 13, 16, 10, 8, 10, 61)
figure3$value2 = c(27, 22, 20, 11, 12, 7, 1, 57)
View(figure3)

# White
ggplot(data = figure3) + 
  aes(x = X, y = value, fill = Race) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("In which of the following ways would you say it's an advantage to 
          be a man at your work right now? (White Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(20, 16, 13, 16, 10, 8, 10, 61))) +
  scale_fill_discrete(name = "White Men")

# Non White
ggplot(data = figure3) + 
  aes(x = X, y = value2, fill = X.3) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("In which of the following ways would you say it's an advantage to 
          be a man at your work right now? (Non White Men)") +
  ylab("Share of Respondents") +
  xlab("Answers") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(27, 22, 20, 11, 12, 7, 1, 57))) +
  scale_fill_discrete(name = "Non White Men")

# Figure 4

masc4 = masc[133:137, ]
View(masc4)

figure4 = masc4 %>% select(X, Race, X.3)
figure4$value = c(17, 43, 40, 6, 43)
figure4$value2 = c(21, 41, 35, 3, 41)
View(figure4)

# White
ggplot(data = figure4) + 
  aes(x = X, y = value, fill = Race) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("In which of the following ways would you say it's a disadvantage to 
          be a man at your work right now? (White Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(17, 43, 40, 6, 43))) +
  scale_fill_discrete(name = "White Men")

# Non White
ggplot(data = figure4) + 
  aes(x = X, y = value2, fill = X.3) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("In which of the following ways would you say it's a disadvantage to 
          be a man at your work right now? (Non White Men)") +
  ylab("Share of Respondents") +
  xlab("Answers") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(21, 41, 35, 3, 41))) +
  scale_fill_discrete(name = "Non White Men")

# Figure 5

masc5 = masc[168:173, ]
View(masc5)

figure5 = masc5 %>% select(X, Race, X.3)
figure5$value = c(52, 25, 13, 1, 7, 2)
figure5$value2 = c(43, 25, 22, 3, 7, 1)
View(figure5)

# White
ggplot(data = figure5) + 
  aes(x = X, y = value, fill = Race) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("How often do you try to be the one who pays when on a date? 
          (White Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(52, 25, 13, 1, 7, 2))) +
  scale_fill_discrete(name = "White Men")

# Non White
ggplot(data = figure5) + 
  aes(x = X, y = value2, fill = X.3) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("How often do you try to be the one who pays when on a date?
          (Non White Men)") +
  ylab("Share of Respondents") +
  xlab("Answers") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(43, 25, 22, 3, 7, 1))) +
  scale_fill_discrete(name = "Non White Men")

# Figure 6

masc6 = masc[183:188, ]
View(masc6)

figure6 = masc6 %>% select(X, Race, X.3)
figure6$value = c(61, 46, 36, 29, 27, 8)
figure6$value2 = c(57, 45, 32, 36, 34, 5)
View(figure6)

# White
ggplot(data = figure6) + 
  aes(x = X, y = value, fill = Race) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("When you want to be physically intimate with someone, how do you gauge their interest? (White Men)") +
  xlab("Answers") +
  ylab("Share of Respondents") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(61, 46, 36, 29, 27, 8))) +
  scale_fill_discrete(name = "White Men")

# Non White
ggplot(data = figure6) + 
  aes(x = X, y = value2, fill = X.3) +
  coord_flip() +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("When you want to be physically intimate with someone, how do you gauge their interest? (Non White Men)") +
  ylab("Share of Respondents") +
  xlab("Answers") +
  ylim(0, 100) +
  theme(plot.title = element_text(lineheight = 0.9, face = "bold", hjust = .5)) +
  geom_text(aes(label = c(57, 45, 32, 36, 34, 5))) +
  scale_fill_discrete(name = "Non White Men")

# 3. The survey had questions that weren’t used in the article. 
# Explain the results in 2 questions that weren’t analyzed. 
# Compare the answers by some demographic information (education, age 
# group, ethnicity, sexuality, etc.).

# Have you changed your behavior in romantic relationships in the wake of 
# #MeToo movement?

figure_metoo = matrix( c(9, 90, 1, 20, 79, 1), nrow=3, ncol = 2, byrow=FALSE)
barplot(figure_metoo,
        main="Have you changed your behavior in romantic relationships in the wake of #MeToo movement?", 
        horiz=TRUE,
        col=c("lavender","turquoise1", "yellow2"),
        xlab = "Percentage",
        xlim=c(0,100),
        args.legend = list(x ='topright', bty='n', inset=c(-.05,-.02)),
        names.arg=c("White", "None-white"),
        cex.names=.63)

# Do you typically feel as though you're expected to make the first move 
# in romantic relationships?

figure_firstmove = matrix( c(63, 36, 2, 57, 42, 1), nrow=3, ncol = 2, byrow=FALSE)
barplot(figure_firstmove,
        main="Do you typically feel as though you're expected to make the first move in romantic relationships?", 
        horiz=TRUE,
        col=c("gray88","mediumspringgreen", "maroon"),
        xlab = "Percentage",
        xlim=c(0,100),
        args.legend = list(x ='topright', bty='n', inset=c(-.05,-.02)),
        names.arg=c("White", "None-white"),
        cex.names=.63)

##### Basketball League #####

# Question 1
rnorm_round = function(n, mean, sd) {
  round(rnorm(n, mean, sd))
}
rnorm_round(n = 5, mean = 10, sd = 3)

# Question 2
library(gtools)

teams = as.data.frame(matrix( c(7, 6, 7, 8, 9, 6, 7, 8, 7, 8, 6, 6, 6, 9, 6, 7, 
                                3, 3, 4, 3, 3, 5, 3, 4, .5, .25, .3, .25, .2, 
                                .35, .2, .25), 
                              nrow=8, ncol = 4, byrow=FALSE))
rownames(teams) = c("Jersey Lions", "Westchester Cats", "Long Island Tigers",
                    "Staten Island Dogs", "The Bronx Foxes", "Queens Bears",
                    "Manhattan Ducks", "Brooklyn Rats")
colnames(teams) = c("A", "D", "H", "R")
teams

formula = function(Home, Away) {
  Home = teams[Home,]
  Away = teams[Away,]
  A1 = Home$A
  D1 = Home$D
  A2 = Away$A
  D2 = Away$D
  H1 = Home$H
  R1 = Home$R
  R2 = Away$R
  mean = ((.6*A1) + (.4*D1)) - (.4*A2 + .6*D2) + H1
  sd = (1/R1) + (1/R2)
  return(c(mean, sd))
}

perm = permutations(n = 8, r = 2, rownames(teams), repeats.allowed = TRUE) %>%
  apply(1, function(z) formula(z[1], z[2]))

names = list(rownames(teams), rownames(teams))

mean = matrix(perm[1,], ncol = 8, dimnames = names, byrow = TRUE)
sd = matrix(perm[2,], ncol = 8, dimnames = names, byrow = TRUE)

output = function(Home, Away) {
  result = 0
  while (result  == 0) {
    result = rnorm_round(1, mean[Home, Away], sd[Home, Away])
  }
  if (result > 0){
    print (Home)
  } else {
    print (Away)
  }
  return(result)
}
# Example Simulation
output("Queens Bears", "The Bronx Foxes") #Point Spread

# Question 3
s = permutations(8, 2, rownames(teams), repeats.allowed = F) %>%
  rbind(., .) %>%
  data.frame()
colnames(s) = c("Home", "Away")

season = function() {
  return(s %>%
           apply(1, function(z) output(z["Home"], z["Away"])) %>%
           cbind(s) %>%
           mutate(winner = ifelse(. > 0, Home, Away)) %>%
           group_by(winner) %>%
           summarize(wins = n(), spread = sum(.)) %>%
           arrange(wins, spread, winner) %>%
           mutate(rank = 1:8) %>%
           arrange(winner) %>%
           .$rank)}
season()

nsim = 1e5
system.time(game3 <- replicate(nsim, season()))

# Graph A
data.frame(game3) %>%
  apply(2, function(z) z == 1) %>%
  rowMeans() %>%
  data.frame(percent = .) %>%
  mutate(t = rownames(teams)) %>%
  ggplot() + 
  aes(x = reorder(t, -percent), y = percent, fill = percent) +
  geom_col() +
  coord_flip() +
  ggtitle("Basketball Simulation Time Graph (Graph A)") +
  theme(plot.title = element_text(hjust = .5)) +
  xlab("Teams") +
  ylab("Win Percentage") +
  scale_fill_gradient(low = "purple", high = "purple") +
  theme(legend.position = "none") +
  geom_text(aes(label = paste(round(percent*100, 1))), hjust = -.5, color = "black") +
  scale_y_continuous(breaks = seq(0, 1, by = .5),
                     labels = scales::percent_format(), limits = c(0, .7))

# Graph B
avgranking = game3 %>%
  rowMeans() %>%
  data.frame(t = rownames(teams), avg = .) %>%
  arrange(avg) %>%
  ggplot() + 
  aes(x = reorder(t, -avg), y = avg, fill = avg) +
  geom_col() +
  coord_flip() +
  ggtitle("Basketball Simulation Graph (Graph B)") +
  theme(plot.title = element_text(hjust = .5)) +
  xlab("Teams") +
  ylab("Average Ranking") +
  ylim(0, 7) +
  scale_fill_gradient(low = "red", high = "red") +
  theme(legend.position = "none") +
  geom_text(aes(label = paste(round(avg, 1))), hjust = -.5, color = "black")
avgranking

# Question 4
avgranking = game3 %>%
  rowMeans() %>%
  data.frame(t = rownames(teams), avg = .) %>%
  arrange(avg)

matchup = function(team1rank, team2rank) {
  team1 = avgranking$t[team1rank]
  team2 = avgranking$t[team2rank]
  
  games = c(output(team1, team2),
            output(team1, team2),
            -output(team2, team1))
  
  winner = sapply(games, function(z) ifelse(z > 0, team1rank, team2rank)) %>%
    table() %>%
    which.max() %>%
    names()
  
  return(as.numeric(winner))
}
matchup(1,8)

bracket = c(1, 8, 2, 7, 3, 6, 4, 5)

tournament = function(match) {
  if (length(match) == 1) {
    return(match)
  }
  winner = sapply(seq(1, length(match), 2), function(i) matchup(match[i], match[i+1]))
  return(tournament(winner))
}
tournament(bracket)

nsim = 1e5
system.time(tourny <- replicate(nsim, tournament(bracket)))

# Graph
data.frame(t = avgranking$t, table(tourny)) %>%
  rename(Rank = tourny, wins = Freq) %>%
  mutate(percent = wins/sum(wins)) %>%
  ggplot() +
  aes(x = reorder(t, -percent), y = percent, fill = percent) +
  ggtitle("New Basketball Simulation Graph") +
  theme(plot.title = element_text(hjust = .5)) +
  xlab("Teams") +
  ylab("Win Percentage") +
  xlab("Teams") +
  ylab("Win Percentage") +
  scale_fill_gradient(low = "lightblue", high = "lightblue") +
  theme(legend.position = "none") +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = paste(round(percent*100, 1))), hjust = -.5, color = "black") +
  scale_y_continuous(breaks = seq(0, 1, by = .5),
                     labels = scales::percent_format(), limits = c(0, .7)) +
  theme(legend.position = "none")

##### Restaurant #####

# A restaurant is open every day. On weekdays, sales are approximately 
# normal with a mean of $2000 and a standard deviation of $500. On the 
# weekends, sales are approximately normal with a mean of $3000 and a 
# standard deviation of $700. The rent costs $2500 weekly, labor costs $4500 
# weekly, food costs $4500 weekly, and other expenses amount to $2500 weekly.

# Weekday Sales
# Mean = $2,000; sd = $500
# TOTAL: Mean = $10,000; sd = $2,500

# Weekend Sales
# Mean = $3,000; sd = $700
# TOTAL: Mean = $6,000; sd = $1,400

# Weekly Costs
# Rent = $2,500; Labor = $4,500; Food = $4,500; Other = $2,500

# 1.) In any given week, what is the probability that the restaurant is 
# making money?

costs = c(2500, 4500, 4500, 2500)
total_weekly_costs = sum(costs)
total_weekly_costs

nsim = 1e5
x = 0
for (i in 1:nsim) {
  weekday = rnorm(5, mean = 2000, sd = 500)
  weekend = rnorm(2, mean = 3000, sd = 700)
  if ((sum(weekday) + sum(weekend)) > total_weekly_costs) {
    x = x + 1
  }
}
x/nsim
# Ans = "~.91"

# 2.) What is the probability that they lose money in December 2019?

monthly_costs = (total_weekly_costs)*4 + ((3/7)*14000)

nsim = 1e5
x = 0
for (i in 1:nsim) {
  weekday = rnorm(22, mean = 2000, sd = 500)
  weekend = rnorm(9, mean = 3000, sd = 700)
  if ((sum(weekday) + sum(weekend)) > monthly_costs) {
    x = x + 1
  }
}
1 - x/nsim
# Ans = "~.002"

# 3.) What is the yearly expected profit of the business? (Assuming all costs 
# remain constant.)

nsim = 1e5
year = rnorm(nsim, mean = (52*(2*3000+5*2000)) - (total_weekly_costs*52), sd = sqrt(52*((5*500^2) + (2*700^2))))
mean(year)
# Ans = "~104,000"

# 4.) The manager is thinking about running a marketing campaign which costs 
# $10,000. The probability that the marketing campaign is successful is 80%. 
# If it is successful, average sales on weekdays and weekends will increase by 
# 15% (the standard deviation will stay the same). Would you recommend running 
# the marketing campaign? Why or why not?

marketing_cost = 10000
success = 52*(1.15*(5*2000 + 2*3000) - total_weekly_costs) - marketing_cost
failure = 52*(5*2000+2*3000 - total_weekly_costs) - marketing_cost

marketing = (.8)*(success) + (.2)*(failure)
marketing
# Expected Profit = "$193,840"

# I would recommend to do the marketing campaign as it has an 80% success rate
# and would increase the average sales on weekdays and weekends by 15%. This,
# in turn, can increase yearly profit by well over $100,000 with the risk of
# only losing $10,000 if the marketing campaign is unsuccessful. The yearly
# cost will amount to $738,000 for one year (with the one time marketing campaign expense), 
# which is definitely still affordable even if the marketing campaign were to
# be unsuccessful.
