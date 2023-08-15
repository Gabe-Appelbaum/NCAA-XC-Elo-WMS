library(tidyverse)
actual <- read.csv("2018 national_regional/2018 national regional mens individual/2018 national mens individual.csv")
actual <- actual %>% select(PL, NAME, TEAM)
actual <- actual %>% relocate(PL, .after = TEAM)

elo <- read.csv("2018 post regional elo.csv")
elo <- elo %>% select(Elo.rank, NAME, TEAM)

flo <- read.csv("2018 final flo rankings.csv")
flo <- flo %>% select(RANK, ATHLETE, SCHOOL)
flo <- flo %>% rename(NAME = ATHLETE, TEAM = SCHOOL)


df <- list(actual, elo, flo)
df <- df %>% reduce(left_join, by= c('NAME', 'TEAM'))

#df$My_error <- df$PL - df$Elo.rank

AA <- df %>% slice(1:40)

write.csv(AA, file = "top 40 results 2018.csv")
