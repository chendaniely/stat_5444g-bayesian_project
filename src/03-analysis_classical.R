rm(list = ls())
load('output/data/w1_clean.RData')


l <- glm(as.factor(df_analysis$drink_cat_2) ~ as.factor(df_analysis$school_bin), family = binomial(link='logit'))
summary(l)
