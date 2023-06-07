season_data <- read.csv("/Users/ljon4/Downloads/archive/Seasons_Stats.csv")
draft_data <- read.csv("/Users/ljon4/Downloads/archive2/draft-data-20-years.csv")
standings_data <- read.csv("/Users/ljon4/Downloads/Historical NBA Performance.csv")
salary_data <- read.csv("https://raw.githubusercontent.com/erikgregorywebb/datasets/master/nba-salaries.csv")

library(tidyverse)
library(plotly)
library(readr)
library(data.table)
library(writexl)
library(fastDummies)

# salary data's earliest draft class is 2000 so I'm taking it out of the draft and standings data up until 2016
# standings_data <- standings_data %>%
  # dplyr::filter(Year == "2000-01" | Year == "2001-02" | Year == "2002-03" | Year == "2003-04" |
                  #Year == "2004-05" | Year == "2005-06" | Year == "2006-07" | Year == "2007-08" |
                  #Year == "2008-09" | Year == "2009-10" | Year == "2010-11" | Year == "2011-12" |
                  #Year == "2012-13" | Year == "2013-14" | Year == "2014-15" | Year == "2015-16" |
                  #Year == "2016-17" | Year == "2000-01 *" | Year == "2001-02 *" | Year == "2002-03 *" | Year == "2003-04 *" |
                  #Year == "2004-05 *" | Year == "2005-06 *" | Year == "2006-07 *" | Year == "2007-08 *" |
                  #Year == "2008-09 *" | Year == "2009-10 *" | Year == "2010-11 *" | Year == "2011-12 *" |
                  #Year == "2012-13 *" | Year == "2013-14 *" | Year == "2014-15 *" | Year == "2015-16 *" |
                  #Year == "2016-17 *")

draftAndSalary <- salary_data %>%
  left_join(draft_data, by = c("name" = "Player"))

year.column <- substr(standings_data$Year, 1, 4)
# bruh2 <- as.data.frame(as.numeric(unlist(bruh$year.column)))
bruh <- as.data.frame(year.column)
# bruh3 <- as.data.frame(as.integer(unlist(bruh2$`as.numeric(unlist(bruh$year.column))`)))
# colnames(bruh3) <- "YEAR"

bruh4 <- as.vector(as.numeric(unlist(bruh$year.column)))
standings_data$Year.New <- bruh4

# standings_data$New.Year <- bruh3

standings_data <- standings_data %>%
  mutate(Team = recode(Team, "Celtics" = "Boston Celtics", "Lakers" = "Los Angeles Lakers", "Hawks" = "Atlanta Hawks", "Blackhawks" = "Atlanta Hawks", "Nets" = "Brooklyn Nets",
                       "Hornets" = "Charlotte Hornets", "Bobcats" = "Charlotte Bobcats", "Bulls" = "Chicago Bulls", "Cavaliers" = "Cleveland Cavaliers", "Mavericks" = "Dallas Mavericks", 
                       "Nuggets" = "Denver Nuggets", "Pistons" = "Detroit Pistons", "Warriors" = "Golden State Warriors", "Rockets" = "Houston Rockets", "Pacers" = "Indiana Pacers", 
                       "Clippers" = "Los Angeles Clippers", "Braves" = "Los Angeles Clippers", "Grizzlies" = "Memphis Grizzlies", "Heat" = "Miami Heat", "Bucks" = "Milwaukee Bucks",
                       "Timberwolves" = "Minnesota Timberwolves", "Pelicans" = "New Orleans Pelicans", "Knicks" = "New York Knicks", "Thunder" = "Oklahoma City Thunder", "Supersonics" = "Seattle SuperSonics",
                       "Magic" = "Orlando Magic", "76ers" = "Philadelphia 76ers", "Nationals" = "Philadelphia 76ers", "Suns" = "Phoenix Suns", "Trail Blazers" = "Portland Trail Blazers",
                       "Kings" = "Sacramento Kings", "Royals" = "Sacramento Kings", "Spurs" = "San Antonio Spurs", "Raptors" = "Toronto Raptors", "Jazz" = "Utah Jazz", "Wizards" = "Washington Wizards", 
                       "Bullets" = "Washington Wizards", "Zephyrs" = "Washington Wizards"))

draftAndSalary <- draftAndSalary %>%
  mutate(team = recode(team, "New Jersey Nets" = "Brooklyn Nets", "Vancouver Grizzlies" = "Memphis Grizzlies"))

unique(standings_data$Team)
unique(draftAndSalary$team)

draftAndSalaryAndStandings <- draftAndSalary %>%
  left_join(standings_data, by = c("team" = "Team", "season" = "Year.New"))

draftAndSalaryAndStandings2 <- draftAndSalaryAndStandings[!(is.na(draftAndSalaryAndStandings$New.Year)), ]

draftAndSalaryAndStandings <- subset(draftAndSalaryAndStandings, select = -c(X.y, X.1, X.2, X.3, X.4, X.5, X.6, X.7, X.8, X.9, X.10, X.11, X.12, X.13, X.14, X.15))

draftAndSalaryAndStandings <- subset(draftAndSalaryAndStandings, select = -X.16)

write_xlsx(draftAndSalaryAndStandings2, "/Users/ljon4/Downloads/projectDataset.xlsx")

# recasting the 

# sports statistics class

# regression code
finalDataset1 <- draftAndSalaryAndStandings2 %>%
  dplyr::select(Winning.Percentage, Pk, DraftYr, MPG, salary, season, name, team)

# omitting rows w NA's for any value 
row.has.na <- apply(finalDataset1, 1, function(x){any(is.na(x))})
sum(row.has.na)
finalDataset1 <- finalDataset1[!row.has.na,]

# recode MPG into continuous numerical
finalDataset1 <- finalDataset1 %>%
  mutate(MPG = cut(MPG,
                    breaks = c(0, 12, 24, 36, Inf),
                    labels = c("Low", "Medium", "High", 
                               "Very High")))

# find the second contracts of the NBA players
finalDataset1 <- finalDataset1 %>%
  dplyr::filter(season == DraftYr + 3)

n_distinct(finalDataset1$name)
# this counts 498 unique observations out of 500

# running our first regression
model <- lm(salary ~ poly(Pk, 3) * MPG * Winning.Percentage, finalDataset1)
model

# local regression function
sqdist <- function(a, b) {
  (a$Pk - b$Pk)^2/60^2 + (a$MPG - b$MPG)^2/48^2 + (a$Winning.Percentage - b$Winning.Percentage)^2
}

predict.local = function(grid) {
  mu.hat.local = rep(NA, nrow(grid))
  for(ii in 1:nrow(grid)) { 
    row = grid[ii, ]
    w = exp(-(sqdist(row, finalDataset1)/2))
    model = lm(salary ~ Pk + MPG + Winning.Percentage, weights=w, data=finalDataset1)
    mu.hat.local[ii] = predict(model, newdata=row)
  }
  mu.hat.local
}

grid <- expand.grid(Pk=1:60, MPG=seq(0, 48, by = 12), Winning.Percentage=seq(0, 1, by = 0.25))

grid$mu.hat.local <- predict.local(grid)
grid$mu.hat.local

ATE <- data.frame(lapply(1:60, function(p) {
  grid2 <- expand.grid(Pk = p, Winning.Percentage = 0.75, MPG = 0:48)
  grid3 <- expand.grid(Pk = p, Winning.Percentage = 0.25, MPG = 0:48)
  predict.local(grid2) - predict.local(grid3)
}))

newFrameVector <- unlist(newFrame)
newFrameDATA <- as.data.frame(newFrame)
newFrame <- rowMeans(ATE)
newATE$ATE <- newFrameDATA$newFrame
newATE$MPG <- grid2 %>%
  select(MPG)
ggplot() + geom_point(aes(x = MPG, y = ATE), data = newATE, alpha = 0.4, size = 0.2)



ggplot() + geom_point(aes(x=Winning.Percentage, y=MPG, color=class.count), 
                      data=finalDataset1, alpha=.4, size=.2) + 
  geom_line(aes(x=class.size, y=mu.hat, color=class.count), data=grid2) +
  facet_grid(cols=vars(msa)) + ylim(0,5)

## session on 10/21/2022
view(finalDataset1)

fakeData1 <- finalDataset1
fakeData1$PCTCat <- fakeData1$Winning.Percentage
fakeData1 <- fakeData1 %>%
  mutate(PCTCat = cut(PCTCat, 
                      breaks = c(0, 0.5, 1),
                      labels = c("Below", "Above")))

# good / bad = winning.percentage || high / medium / low = mpg
goodhigh <- fakeData1$Winning.Percentage > 0.5 & fakeData1$MPG == "High"
fakeData1[goodhigh,]$salary = runif(sum(goodhigh), min = 2193901, max = 6000000)

goodmedium <- fakeData1$Winning.Percentage > 0.5 & fakeData1$MPG == "Medium"
fakeData1[goodmedium,]$salary = runif(sum(goodmedium), min = 799375, max = 2193900)

goodlow <- fakeData1$Winning.Percentage > 0.5 & fakeData1$MPG == "Low"
fakeData1[goodlow,]$salary = runif(sum(goodlow), min = 24263, max = 799375)

badhigh <- fakeData1$Winning.Percentage < 0.5 & fakeData1$MPG == "High"
fakeData1[badhigh,]$salary = runif(sum(badhigh), min = 2193901, max = 6000000)

badmedium <- fakeData1$Winning.Percentage < 0.5 & fakeData1$MPG == "Medium"
fakeData1[badmedium,]$salary = runif(sum(badmedium), min = 799375, max = 2193900)

badlow <- fakeData1$Winning.Percentage < 0.5 & fakeData1$MPG == "Low"
fakeData1[badlow,]$salary = runif(sum(badlow), min = 24263, max = 799375)



predict.local2 = function(grid) {
  mu.hat.local = rep(NA, nrow(grid))
  for(ii in 1:nrow(grid)) { 
    row = grid[ii, ]
    w = exp(-(sqdist(row, fakeData1)/2))
    model = lm(salary ~ Pk + MPG + Winning.Percentage, weights=w, data=finalDataset1)
    mu.hat.local[ii] = predict(model, newdata=row)
  }
  mu.hat.local
}

ATE <- data.frame(lapply(1:60, function(p) {
  grid2 <- expand.grid(Pk = p, Winning.Percentage = 0.75, MPG = 0:48)
  grid3 <- expand.grid(Pk = p, Winning.Percentage = 0.25, MPG = 0:48)
  predict.local(grid2) - predict.local(grid3)
}))




# assigning the coefficients based on model estimation
betahat <- c(705832.737, 19432.653, 80920.621, 414297.286, -40408.833, -1736.515)

# phi function as a function of our features
phi <- function(MPG, Winning.Percentage, Pk, Int1, Int2) {
  rbind(1, Pk, MPG, Winning.Percentage, Pk*Winning.Percentage, Pk*MPG)
}
# int, Pk, MPG, Winning.Percentage, Pk:WP, Pk:MPG

phi1 <- phi(finalDataset1$MPG, finalDataset1$Winning.Percentage, finalDataset1$Pk, finalDataset1$Pk*finalDataset1$Winning.Percentage, finalDataset1$Pk*finalDataset1$MPG)

# mu function as a function of our features
muhat = function(MPG, Winning.Percentage, Pk, Int1, Int2) {
  t(phi(MPG, Winning.Percentage, Pk, Int1, Int2)) %*% betahatFake
}
mu1 <- muhat(finalDataset1$MPG, finalDataset1$Winning.Percentage, finalDataset1$Pk, finalDataset1$Pk*finalDataset1$Winning.Percentage, finalDataset1$Pk*finalDataset1$MPG)

# this is what we're going to use to calculate the target
target <- function(m) {
  mean(m(finalDataset1$MPG, finalDataset1$Winning.Percentage, finalDataset1$Pk, finalDataset1$Pk*finalDataset1$Winning.Percentage, finalDataset1$Pk*finalDataset1$MPG) - m(newDataset$MPG, newDataset$Winning.Percentage, newDataset$Pk, newDataset$Pk*newDataset$Winning.Percentage, newDataset$Pk*newDataset$MPG))
}
target2 <- function(m) {
  mean(m(fakeData1$MPG, fakeData1$Winning.Percentage, fakeData1$Pk, fakeData1$Pk*fakeData1$Winning.Percentage, fakeData1$Pk*fakeData1$MPG) - m(newFakeData1$MPG, newFakeData1$Winning.Percentage, newFakeData1$Pk, newFakeData1$Pk*newFakeData1$Winning.Percentage, newFakeData1$Pk*newFakeData1$MPG))
}
# int, Pk, MPG, Winning.Percentage, Pk:WP, Pk:MPG


target(muhat)
# sigma = function(x) { 1/(1+abs(x)) } dont use this!

# sigma.hat
sigma.hat = array(0, dim=c(6,6))
for (i in 1:500) {
  phi1 = phi(finalDataset1$MPG, finalDataset1$Winning.Percentage, finalDataset1$Pk, finalDataset1$Pk*finalDataset1$Winning.Percentage, finalDataset1$Pk*finalDataset1$MPG)
  sigma.hat = sigma.hat + phi1 %*% t(phi1)/500
}
sigma.hat.inv = solve(sigma.hat)

# sigma.hat.w
epsilon.hat = finalDataset1$salary - predict(model, newdata=finalDataset1) 
sigma.hat.w = array(0, dim=c(6,6))
for (i in 1:500) {
  phi1 = phi(finalDataset1$MPG, finalDataset1$Winning.Percentage, finalDataset1$Pk, finalDataset1$Pk*finalDataset1$Winning.Percentage, finalDataset1$Pk*finalDataset1$MPG)
  sigma.hat.w = sigma.hat.w + epsilon.hat[i]^2 * phi1 %*% t(phi1)/500
}

# V
V = sigma.hat.inv %*% sigma.hat.w %*% sigma.hat.inv
V

# sigma.hat.u
u <- rowMeans(phi(finalDataset1$MPG, finalDataset1$Winning.Percentage, finalDataset1$Pk, finalDataset1$Pk*finalDataset1$Winning.Percentage, finalDataset1$Pk*finalDataset1$MPG) - phi(newDataset$MPG, newDataset$Winning.Percentage, newDataset$Pk, newDataset$Pk*newDataset$Winning.Percentage, newDataset$Pk*newDataset$MPG))

view(u)
sigma.hat.u = sqrt(t(u) %*% V %*% u / 500)
sigma.hat.u
