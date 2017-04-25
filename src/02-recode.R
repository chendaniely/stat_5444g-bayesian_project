library(testthat)
library(ggplot2)

rm(list = ls())

load(file = 'output/wave1.RData') ## proves the `df` of the w1 data
source('src/recode.R')

# school topics
school_pattern <- '^H1TS'
school_columns <- grep(pattern = school_pattern, x = names(df))
expect_equal(length(school_columns), 17)
school <- df[, school_columns]
school[school != 0 & school != 1] <- NA
school_counts <- apply(X = school, MARGIN = 1, sum, na.rm = TRUE)
expect_equal(length(school_counts), nrow(df))

df_analysis <- data.frame('school_counts' = school_counts)

                                        # ggplot(data = as.data.frame(school_counts), aes(x = school_counts)) + geom_bar()

school_bin <- sapply(X = school_counts, FUN = recode_school_bin, cutoff = 14)

df_analysis <- cbind(df_analysis, school_bin)

## drinking outcome variable

any_drink <- apply(X = df, MARGIN = 1, FUN = recode_any_drink)

risky_drink <- apply(X = df, MARGIN = 1, FUN = recode_risky_drink)

any_risky_drink_df <- data.frame('any_drink' = any_drink, 'risky_drink' = risky_drink)

any_risky_drink_df$drink_cat_3 <- apply(any_risky_drink_df, 1, recode_drink_cat_3)

# head(any_risky_drink_df)

df_analysis <- cbind(df_analysis, any_risky_drink_df)

df_analysis$drink_cat_2 <- apply(any_risky_drink_df, 1, recode_drink_cat_2)


## race
## TODO NEED TO LOOK INTO THIS CODE
race4 <- apply(X = df, MARGIN = 1, FUN = recode_race)

df_analysis <- cbind(df_analysis, race4)


## age
age_cont <- apply(X = df, MARGIN = 1, FUN = recode_age_cont)

                                        # hist(age_cont)
df_analysis <- cbind(df_analysis, age_cont)


## age cat 3
age_cat_3 <- apply(X = df_analysis, MARGIN = 1, FUN = recode_age_cat)

df_analysis <- cbind(df_analysis, age_cat_3)


## sex
df_analysis$sex <- apply(df, 1, recode_sex)

## parent's education
df_analysis$pedu <- apply(df, 1, recode_pEducation)

## urbanicity
df_analysis$urbanicity <- apply(df, 1, recode_urbanicity)

## income
df_analysis$income <- apply(df, 1, recode_income)

## relationship teacher
df_analysis$rel_teacher <- apply(df, 1, recode_rel_teach)

## relationship students
df_analysis$rel_student <- apply(df, 1, recode_rel_student)

save(df_analysis, file='output/data/w1_clean.RData')
