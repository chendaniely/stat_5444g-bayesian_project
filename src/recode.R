recode_school_bin <- function(x, cutoff) {
    x <- as.numeric(x)

    if (x >= cutoff) {
        return(1)
    } else if (x >= 0) {
        return(0)
    } else {
        return(NA)
    }
}

recode_any_drink <- function(x) {
    any_drink <- as.numeric(x['H1TO15'])
    drink <- as.numeric(x['H1TO12'])

    if (is.na(any_drink)){
        return(NA)
    } else if (any_drink %in% 1:6) {
        return(1)
    } else if (is.na(any_drink == 7 | drink == 0)) {
        return(NA)
    } else if (any_drink == 7 | drink == 0) {
        return(0)
    } else {
        return(NA)
    }
}

recode_risky_drink <- function(x) {
    # if anyone drinks like an risky male, they are risky
    h1to15 <- as.numeric(x['H1TO15'])
    h1to16 <- as.numeric(x['H1TO16'])
    h1to12 <- as.numeric(x['H1TO12'])
    bio_sex <- as.numeric(x['BIO_SEX'])

    if (is.na(h1to15)){
        return(NA)
    } else if (h1to15 %in% 1:6){
        if ((h1to16 %in% 5:95) | (h1to15 == 1 & h1to16 %in% 2:95)){
            return(1)
        } else {
            return(0)
        }
    } else if (is.na(h1to15 == 7 | h1to12 == 0)){
        return(NA)
    } else if (h1to15 == 7 | h1to12 == 0) {
        return(0)
    }

    ## females
    if (BIO_SEX == 2) {
        return(123123123)
        if (H1TO15 %in% 1:6){
            if (H1TO16 %in% 4:95 |
                (H1TO15 == 1 & H1TO16 %in% 1:95) |
                (H1TO15 == 2 & H1TO16 %in% 3:95)) {
                return(1)
            }
        }
    }
}

recode_drink_cat_3 <- function(row) {
    risky_drink <- row['risky_drink']
    any_drink <- row['any_drink']

    if (is.na(risky_drink == 1)) {
        return(NA)
    } else if (risky_drink == 1){
        return(2)
    } else if (is.na(any_drink == 1 & risky_drink == 0)) {
        return(NA)
    } else if (any_drink == 1 & risky_drink == 0) {
        return(1)
    } else if (is.na(any_drink == 0)) {
        return(NA)
    } else if (any_drink == 0) {
        return(0)
    } else {
        return(NA)
    }
}

recode_drink_cat_2 <- function(row) {
    drink_cat <- row['drink_cat_3']

    if (is.na(drink_cat)) {
        return(NA)
    } else if (drink_cat == 0 | drink_cat == 1) {
        return(0)
    } else {
        return(1)
    }
}

recode_race <- function(row) {

    ## Which one category best describe your racial background?
    ##  1=White, 2=Black/African American, 4=Asian/Pacific Islander
    h1gi8 <- as.numeric(row['H1GI8'])
    if (is.na(h1gi8 %in% c(1, 2, 4))) {
        h1gi8 <- NA
    } else if (h1gi8 %in% c(1, 2, 4)) {
        h1gi8 <- h1gi8
    } else {
        h1gi8 <- NA
    }
    ## hispanic
    h1gi4 <- as.numeric(row['H1GI4'])
    his <- h1gi4
    if (is.na(h1gi4 %in% c(0, 1))){
        his <- NA
        print('his na')
    } else if (h1gi4 %in% c(0, 1)){
        his <- h1gi4
    } else {
        his <- 0
    }
    ## white
    h1gi6a <- as.numeric(row['H1GI6A'])
    white <- h1gi6a
    if (is.na(h1gi6a %in% c(0, 1))) {
        white <- NA
        print('white na')
    } else if (h1gi6a %in% c(0, 1)) {
        white <- h1gi6a
    } else {
        white <- 0
    }
    ## black
    h1gi6b <- as.numeric(row['H1GI6B'])
    black <- h1gi6b
    if (is.na(h1gi6b %in% c(0, 1))) {
        black <- NA
        print('black na')
    } else if (h1gi6b %in% c(0, 1)) {
        black <- h1gi6b
    } else {
        black <- 0
    }
    ## asian
    h1gi6d <- as.numeric(row['H1GI6D'])
    asian <- h1gi6d
    if (is.na(h1gi6d %in% c(0, 1))) {
        asian <- NA
        print('asian na')
    } else if (h1gi6d %in% c(0, 1)) {
        asian <- h1gi6d
    } else {
        asian <- 0
    }

    if (his == 1) {
        return(1) # hispanic or latino
    } else if (his != 1 & white == 1) {
        return(2) # white
    } else if (his != 1 & black == 1) {
        return(3) # black
    } else if (his != 1 & asian == 1) {
        return(4) # asian
    } else {
        return(NA)
    }
}

recode_age_cont <- function(row) {
    h1gi1y <- as.numeric(row['H1GI1Y'])
    if (is.na(0 <= h1gi1y & h1gi1y < 96)) {
        birth_year <- NA
    } else if (0 <= h1gi1y & h1gi1y < 96) {
        birth_year <- h1gi1y
    } else {
        birth_year <- NA
    }

    iyear <- as.numeric(row['IYEAR'])
    if (iyear %in% c(94, 95)) {
        interview_year_w1 <- iyear
    } else {
        interview_year_w1 <- NA
    }
    age_w1 <- interview_year_w1 - birth_year
    return(as.numeric(age_w1))
}

recode_age_cat <- function(row) {
    ## analysis df not w1 df
    age <- as.numeric(row['age_cont'])

    if (is.na(age)) {
        return(NA)
    } else if (10 <= age & age <= 14) {
        return(0)
    } else if (15 <= age & age <= 17) {
        return(1)
    } else if (18 <= age & age <= 40) {
        return(2)
    } else {
        return(NA)
    }
}

recode_sex <- function(row) {
    sex <- as.numeric(row['BIO_SEX'])

    if (is.na(sex == 1)) {
        return(NA)
    } else if (sex == 1) {
        return(1)
    } else if (sex ==2) {
        return(2)
    } else {
        return(NA)
    }
}

recode_pEducation <- function(row) {
    pedu <- as.numeric(row['PA12'])

    if (is.na(pedu %in% c(1, 2, 3, 5))) {
        return(NA)
    } else if (pedu %in% c(1, 2, 3, 5)) {
        return(0)  ## less than high school
    } else if (is.na(pedu == 4)) {
        return(NA)
    } else if (pedu == 4) {
        return(1) ## high school
    } else if (is.na(pedu %in% 6:9)) {
        return(NA)
    } else if (pedu %in% 6:9) {
        return(2) ## more than HS
    } else {
        return(NA)
    }
}

recode_urbanicity <- function(row) {
    urban <- as.numeric(row['H1IR12'])

    tryCatch({
        if ((urban == 1)) {
            return(0) ## rural
        } else if (urban == 2) {
            return(1) ## suburban
        } else if (urban %in% 3:6) {
            return(2)
        } else {
            return(NA)
        }
    }, error = function(e) {
        return(NA)
    })
}

recode_income <- function(row) {
    income <- as.numeric(row['PA55'])

    tryCatch({
        if (0 <= income & income <= 20) {
            return(0)
        } else if (20 < income & income <= 35) {
            return(1)
        } else if (35 < income & income <= 70) {
            return(2)
        } else if (70 < income & income <= 9995) {
            return(3)
        } else {
            return(NA)
        }
    }, error = function(e) {
        return(NA)
    })
}

recode_rel_teach <- function(row) {
    rel <- as.numeric(row['H1ED15'])

    tryCatch({
        if (rel %in% c(0:4)){
            return(rel)
        } else {
            return(NA)
        }
    }, error = function(e) {
        return(NA)
    })
}

recode_rel_student <- function(row) {
    rel <- as.numeric(row['H1ED18'])

    tryCatch({
        if (rel %in% c(0:4)) {
            return(rel)
        } else {
            return(NA)
        }
    }, error = function(e) {
            return(NA)
    })
}
