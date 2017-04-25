library(broom)
library(ggplot2)

rm(list = ls())
load('output/data/w1_clean.RData')

mod <- glm(drink_cat_2 ~
               race4 + age_cat_3 + sex + income +
               urbanicity + pedu + rel_teacher + rel_student,
           data = df_analysis, family = binomial(link = 'logit'))
summary(mod)

mod_tidy <- tidy(mod)
mod_tidy$ci_lower <- mod_tidy$estimate - (1.96*mod_tidy$std.error)
mod_tidy$ci_upper <- mod_tidy$estimate + (1.96*mod_tidy$std.error)

augment(mod)
glance(mod)

ggplot(data = mod_tidy, aes(x = exp(estimate), y = term, color = term)) +
    geom_point() +
    geom_errorbarh(aes(xmin = exp(ci_lower), xmax = exp(ci_upper))) +
    theme_minimal()
ggsave('output/plots/coeff_plot_classical.png', width = 11, height = 8)
