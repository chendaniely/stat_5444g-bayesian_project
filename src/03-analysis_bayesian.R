
library(MCMCpack)

rm(list = ls())
load('output/data/w1_clean.RData')


mod <-  MCMClogit(as.factor(
    df_analysis$drink_cat_2) ~ as.factor(df_analysis$school_bin) + as.factor(df_analysis$age_cat_3),
    burnin=1000, mcmc=21000, b0=0, B0=.04)

plot(mod)

head(mod)

prob_01 <- 1 / (exp(-1 * (mod[, 1] + mod[, 2] * 1 + mod[, 3] * 0 + mod[, 4] * 0)) + 1)

prob_02 <- 1 / (exp(-1 * (mod[, 1] + mod[, 2] * 1 + mod[, 3] * 0 + mod[, 4] * 1)) + 1)

prob_03 <- 1 / (exp(-1 * (mod[, 1] + mod[, 2] * 1 + mod[, 3] * 1 + mod[, 4] * 0)) + 1)
