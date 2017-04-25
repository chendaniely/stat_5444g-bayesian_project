library(haven)

source('src/recode.R')

w1 <- read_sas('data/wave1data.sas7bdat')
weights <- read_sas('data/w1weights.sas7bdat')

wave1 <- merge(x = weights, y = w1, by.x = 'AID', by.y = 'AID')

save(df, file = 'output/wave1.RData')
