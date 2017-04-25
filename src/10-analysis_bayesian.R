library(MCMCpack)
library(ggplot2)
library(tidyr)
library(tibble)
library(reshape2)

rm(list = ls())

load('output/data/w1_clean.RData')

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

# mod_small <-  MCMClogit(drink_cat_2 ~ race4,
#                         data = df_analysis,
#                         burnin = 1000,
#                         mcmc = 21000,
#                         b0 = 0, B0 = .04)
# plot(mod_small)


mod <-  MCMClogit(drink_cat_2 ~
                      race4 + age_cat_3 + sex + income +
                      urbanicity + pedu + rel_teacher + rel_student,
                  data = df_analysis,
                  burnin = 1000,
                  mcmc = 210000,
                  b0 = 0, B0 = .04)

mod <- mod %>% as.data.frame()

plot_trace <- function(column){
    df <- as.data.frame(column)
    names(df) <- 'var'
    x <- 1:length(column)
    g <- ggplot(data = df, aes(x = x, y = var)) + geom_line()
    return(g)
}

plots <- lapply(X = mod, FUN = plot_trace)

label_plots <- function(plot_name, plot) {
    g <- plot + ggtitle(plot_name)
    return(g)
}

named_plots <- mapply(FUN = label_plots, names(plots), unlist(plots))


prob_01 <- 1 / (exp(-1 * (mod[, 1] + mod[, 2] * 1 + mod[, 3] * 0 + mod[, 4] * 0)) + 1)
prob_02 <- 1 / (exp(-1 * (mod[, 1] + mod[, 2] * 1 + mod[, 3] * 0 + mod[, 4] * 1)) + 1)
prob_03 <- 1 / (exp(-1 * (mod[, 1] + mod[, 2] * 1 + mod[, 3] * 1 + mod[, 4] * 0)) + 1)


prob_age_01 <- 1 / (exp(-1 * (mod$`(Intercept)` +
                                  mod$race41 * 1 +
                                  mod$age_cat_31 * 0 +
                                  mod$age_cat_32 * 0 +
                                  mod$sex2 * 1 +
                                  mod$income1 * 0 +
                                  mod$income2 * 0 +
                                  mod$income3 * 0 +
                                  mod$urbanicity1 * 0 +
                                  mod$urbanicity2 * 0 +
                                  mod$pedu1 * 0 +
                                  mod$pedu2 * 0 +
                                  mod$rel_teacher1 * 0 +
                                  mod$rel_teacher2 * 0 +
                                  mod$rel_teacher3 * 0 +
                                  mod$rel_teacher4 * 0 +
                                  mod$rel_student1 * 0 +
                                  mod$rel_student2 * 0 +
                                  mod$rel_student3 * 0 +
                                  mod$rel_student4 * 0
                              )) + 1)

prob_age_02 <- 1 / (exp(-1 * (mod$`(Intercept)` +
                                  mod$race41 * 1 +
                                  mod$age_cat_31 * 1 +
                                  mod$age_cat_32 * 0 +
                                  mod$sex2 * 1 +
                                  mod$income1 * 0 +
                                  mod$income2 * 0 +
                                  mod$income3 * 0 +
                                  mod$urbanicity1 * 0 +
                                  mod$urbanicity2 * 0 +
                                  mod$pedu1 * 0 +
                                  mod$pedu2 * 0 +
                                  mod$rel_teacher1 * 0 +
                                  mod$rel_teacher2 * 0 +
                                  mod$rel_teacher3 * 0 +
                                  mod$rel_teacher4 * 0 +
                                  mod$rel_student1 * 0 +
                                  mod$rel_student2 * 0 +
                                  mod$rel_student3 * 0 +
                                  mod$rel_student4 * 0
                              )) + 1)

prob_age_03 <- 1 / (exp(-1 * (mod$`(Intercept)` +
                                  mod$race41 * 1 +
                                  mod$age_cat_31 * 0 +
                                  mod$age_cat_32 * 1 +
                                  mod$sex2 * 1 +
                                  mod$income1 * 0 +
                                  mod$income2 * 0 +
                                  mod$income3 * 0 +
                                  mod$urbanicity1 * 0 +
                                  mod$urbanicity2 * 0 +
                                  mod$pedu1 * 0 +
                                  mod$pedu2 * 0 +
                                  mod$rel_teacher1 * 0 +
                                  mod$rel_teacher2 * 0 +
                                  mod$rel_teacher3 * 0 +
                                  mod$rel_teacher4 * 0 +
                                  mod$rel_student1 * 0 +
                                  mod$rel_student2 * 0 +
                                  mod$rel_student3 * 0 +
                                  mod$rel_student4 * 0
                              )) + 1)

his_age_01 <- ggplot(data = as.data.frame(prob_age_01), aes(x = prob_age_01)) + geom_histogram() + theme_minimal() + ggtitle('Age: 10-14') + xlim(0, .5)
his_age_02 <- ggplot(data = as.data.frame(prob_age_02), aes(x = prob_age_02)) + geom_histogram() + theme_minimal() + ggtitle('Age: 15-17') + xlim(0, .5)
his_age_03 <- ggplot(data = as.data.frame(prob_age_03), aes(x = prob_age_03)) + geom_histogram() + theme_minimal() + ggtitle('Age: 18+') + xlim(0, .5)

multiplot(his_age_01, his_age_02, his_age_03, cols = 1)


multiplot(plotlist = plots, cols = 4)

means <- as.data.frame(sapply(mod, mean))
sds <- as.data.frame(sapply(mod, sd))
cis <- sapply(mod, quantile, prob = c(0.025, 0.975)) %>% as.matrix() %>% t() %>% as.data.frame()

ms <- merge(x = means, y = sds, by = "row.names")
msc <- merge(x = ms, y = cis, by.x = "Row.names", by.y = "row.names")
names(msc) <- c('variable', 'mean', 'sd', 'ci_lower', 'ci_upper')
msc
