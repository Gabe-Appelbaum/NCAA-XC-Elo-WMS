#OKAY
# i have to figure out how to take the overlap from the main database and the new meet
# then i set a base elo for new runnersdds
# then i run elo updates based on results
# then i overwrite the old data base with the uodated results
library(dplyr)
library(elo)
library(tidyverse)


#Load in data
full <- read.csv("2017 men elo.csv")
full <- subset (full, select = -X)
wy <- read.csv("2018 invitationals/2018 invitationals mens individual/2018 wyoming invite mens individual 8_31.csv")

#run the joins
df <- list(wy, full)
#merge all data frames in list
df <- df %>% reduce(left_join, by= c('NAME', 'TEAM'))

#set new runners elos to 1000
df[is.na(df)] = 0
df$elo <- ifelse(df$elo == 0, 1000, df$elo)

#create a column for temporary storage of the elo update
df$update <- 0


for (i in df$PL){ # loop through the data frame
  
  y = i # create a second number to isolate into pairs of racers
  
  repeat{
    if(y >= nrow(df)) {break} # exit if the second runner number gets beyond the dataframe
    #if((y - i) > 100) {break} # exit if the second number is more than 150 places from the first number
    # beating runners by a ton doesn't matter
    
    y = y + 1 # sets the y to be the next runner
    
    # edit the update column, its the elo update from the pair, multiplied by whats essentially the percentile
    # 1 - place in the race, divided by nrow, rounded. So finishing first is 0.99, finishing last is just 0
    
    df[i,'update'] <- df[i,'update'] + (elo.update(c(1), c(df[i,'elo']), c(df[y,'elo']), k = 5) * 
                                          (1 - round(df[y, 'PL']/nrow(df), digits = 2)))
    
    df[y,'update'] <- df[y,'update'] - (elo.update(c(1), c(df[i,'elo']), c(df[y,'elo']), k = 5) * 
                                          (1 - round(df[i, 'PL']/nrow(df), digits = 2)))
    
  }
}

#this just means the update is slightly less drastic

df$elo <- df$elo + (df$update / nrow(df)) * 20

#df$elo <- df$elo + (df$update * (1 - round(df$PL/nrow(df), digits = 2)))/4

# cut the df down to just name, team, elo
df2 <- df %>% select(NAME, TEAM, elo)

#now join the update back onto the main one
new_full <- list(full, df2)
#merge all data frames in list
new_full <- new_full %>% reduce(full_join, by= c('NAME', 'TEAM'))

# now replace the old elo's with the greater ones
new_full[is.na(new_full)] = 0
new_full$elo <- ifelse(new_full$elo.y > new_full$elo.x, new_full$elo.y, new_full$elo.x)

#then cut it back down to just name, team, elo
new_full <- new_full %>% select(NAME, TEAM, elo)

write.csv(new_full,"2018 elo.csv")
