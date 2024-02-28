## Report question:	
## Do people become happier or unhappier after getting divorced? Explore how life satisfaction changes after divorce, for men and women.

##-------------------------------------------------------------------------------------------------------------------------------------------------------
## VARIABLES

## Marital status (marstat)
##      1                   Single
##      2                   Married
##      5                   Divorced
##      3, 4, 6, 7, 8, 9    NA
##     -7, -2, -1, -8       NA


## Respondent's sex, derived and cross-wave checked (sex_dv)
##      1                   Male
##      2                   Female
##      0                   Inconsistent

## Life satisfaction (sclfsato)
##      1                   Completely dissatisfied
##      2                   Mostly dissatisfied
##      3                   Somewhat dissatisfied
##      4                   Niether satisfied nor dissatisfied
##      5                   Somewhat satisfied
##      6                   Mostly satisfied
##      7                   Completely satisfied
##     -1, -2, -7, -8, -9   NA

## Have you recently been thinking of yourself as a worthless person? (scghqk)
##      1                   Not at all
##      2                   No more than usual
##      3                   Rather more than usual
##      4                   Much more than usual
##     -1, -2, -7, -8, -9   NA
##
## Have you recently felt that you were playing a useful part in things? (scghqc)
##      1                   More so than usual
##      2                   Same as usual
##      3                   Less so than usual
##      4                   Much less than usual
##     -1, -2, -7, -8, -9   NA

## Have you recently been feeling reasonably happy, all things considered (scghql)
##      1                   More so than usual
##      2                   About the same as usual
##      3                   Less so than usual
##      4                   Much less than usual
##     -1, -2, -7, -8, -9   NA

##---------------------------------------------------------------------------------------------------------------------------------------------------------------
## R SETUP      -------------------------------------------------------------------------------------------------------------------------------------------------

setwd("C:/Users/Eoin/Documents/Important/University of Exeter/Year 3/Term 2/Data Analysis in Social Science 3/UKDA-6614-tab/tab/ukhls")
getwd()

install.packages("tidyverse")
install.packages("readr")
install.packages("forcats")
install.packages("ggplot2")
install.packages("knitr")
install.packages("purrr")
install.packages("tidyr")
install.packages("tibble")
install.packages("rmarkdown")
install.packages("vroom")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("scales")

library(readr)
library(forcats)
library(ggplot2)
library(knitr)
library(purrr)
library(tidyr)
library(tibble)
library(rmarkdown)
library(vroom)
library(dplyr)
library(gridExtra)
library(scales)

##---------------------------------------------------------------------------------------------------------------------------------------------------------
## CREATE DF        ---------------------------------------------------------------------------------------------------------------------------------------

## Load in data

files = dir(
     "C:/Users/Eoin/Documents/Important/University of Exeter/Year 3/Term 2/Data Analysis in Social Science 3/UKDA-6614-tab",
     pattern = "indresp",
     recursive = TRUE,
     full.names = TRUE)
files = files[stringr::str_detect(files, "ukhls")]

vars = c("sex_dv", "marstat", "sclfsato", "scghqk", "scghqc", "scghql")
for (i in c(6, 1:5, 7:11)) {
    varselect = paste(letters[i], vars, sep = "_")
    varselect = c("pidp", varselect)
    data = vroom(files[i], col_select = varselect)
    if (i == 6) {
        joined = data
    }
    else {
        joined = inner_join(joined, data, by = "pidp")
    }
    rm(data)
}
rm(vars, varselect, files)

## Put wave 6 in order

joined = joined[ , c(1, 8:37, 2:7, 38:67)]

## Format waves properly

joined = joined %>%
    pivot_longer(a_sex_dv:k_scghql, names_to = "variable", values_to = "value") %>%
    separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
    pivot_wider(names_from = variable, values_from = value)

## Fill sex variable

joined = joined %>%
     group_by(pidp) %>%
     fill(sex_dv, .direction = "down") %>%
     ungroup(pidp)

## Recode sex_dv to male, female, or NA

joined = joined %>%
    mutate(sex_dv = ifelse(sex_dv == 1, "male", 
                    ifelse(sex_dv == 2, "female", NA)))

## Recode marital status to single, married, or divorced

joined = joined %>%
    mutate(marstat = case_when(marstat == 1 ~ "single",
                               marstat == 2 ~ "married",
                               marstat == 5 ~ "divorced"))

## Recode life satisfaction responses

joined = joined %>%
    mutate(sclfsato = case_when((!is.na(sclfsato) & sclfsato %in% 1:7) ~ sclfsato))

## Recode wave indicator to numeric

joined$wave = match(joined$wave, letters)

## Recode worthlessness, usefulness, and happiness responses

if_multina = function(x, y) {
     x[x %in% c(y)] = NA
     x
}
joined[ ,6:8] = sapply(joined[ ,6:8], if_multina, y = -9:-1)
rm(if_multina)

## Obtaining graphable data for single and married men & women (life satisfaction)

catgs = c("singman", "singwom", "marman", "marwom", "divman", "divwom")
catgor = function(y){
    filtm = c("single", "single", "married", "married", "divorced", "divorced")
    filtg = c("male", "female", "male", "female", "male", "female")
    checklist = joined %>%
        group_by(pidp) %>%
        filter(marstat == filtm[y] & sex_dv == filtg[y]) %>%                
        count(pidp) %>%
        filter(n == 11) %>%
        select(pidp)
    checklist = pull(checklist)
    joined2 = joined %>%
        mutate(pidp = case_when(pidp %in% checklist ~ pidp)) %>%
        na.omit(pidp)
    rm(checklist)
    joined2 %>%
        filter(!is.na(sex_dv), !is.na(marstat), !is.na(sclfsato)) %>%
        select(wave, sclfsato) %>%
        group_by(wave) %>%
        summarise(meansat = mean(sclfsato, na.rm = TRUE))
}
out = vector("list", 6)
for (i in 1:6){
    out[[i]] = catgor(i)
}
div.cats = split(out, catgs)
div.cats = lapply(div.cats, as.data.frame)
for (i in 1:6){
list2env(div.cats[i],envir=.GlobalEnv)
}
rm(div.cats, out, catgor, i)

##-----------------------------------------------------------------------------------------------------------------------------------------
##      PLOT        -----------------------------------------------------------------------------------------------------------------------

## Creating 'x.string' to change xlim values e.g., from wave '2' to '2010-2011'

x.string = (as.character(c("miss",2009:2020)))
x.string = paste(x.string, collapse = " ")
x.string = strsplit(x.string, " ")
x.string = unlist(x.string)
x.string = rep(x.string, each = 2)
x.string = paste(x.string, collapse = " ")
x.string = gsub("([^ ]+ [^ ]+) ", "\\1-", x.string)
x.string = substring(x.string, 12)
x.string = strsplit(x.string, " ")
x.string = unlist((x.string))
x.string = as.factor(x.string[2:12])

## Creating vector for y values

y.string = c("Complete dissat", "Mostly dissat", "Somewhat dissat", "Neither/nor", "Somewhat sat", "Mostly sat", "Completely sat")

## Plotting mean life satisfaction of single, married & divorced men & women from wave 6

divorced = ggplot(data = divman, mapping = aes(x=wave, y=meansat))+
    ylim(c(y.string))+
    xlim(c(x.string))+
    geom_smooth(col = "blue")+
    xlab("Wave")+
    ylab("Life satisfaction")+
    ggtitle("Life satisfaction for divorced people")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_smooth(data = divwom, col = "magenta3")+
    geom_text(x=1, y=4.7, label="Men", color = "blue")+
    geom_text(x=1, y=5.5, label="Women", color = "magenta3")

married = ggplot(data = marman, mapping = aes(x=wave, y=meansat))+
    ylim(c(y.string))+
    xlim(c(x.string))+
    geom_smooth(col = "blue")+
    xlab("Wave")+
    ylab("Life satisfaction")+
    ggtitle("Life satisfaction for married people")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_smooth(data = marwom, col = "magenta3")+
    geom_text(x=1, y=5.3, label="Men", color = "blue")+
    geom_text(x=1, y=5.9, label="Women", color = "magenta3")

grid.arrange(divorced, married, ncol = 2) 

## Code for same graphs, but for happiness instead of satisfaction

catgs = c("singman2", "singwom2", "marman2", "marwom2", "divman2", "divwom2")
catgor = function(y){
    filtm = c("single", "single", "married", "married", "divorced", "divorced")
    filtg = c("male", "female", "male", "female", "male", "female")
    checklist = joined %>%
        group_by(pidp) %>%
        filter(marstat == filtm[y] & sex_dv == filtg[y]) %>%                
        count(pidp) %>%
        filter(n == 11) %>%
        select(pidp)
    checklist = pull(checklist)
    joined2 = joined %>%
        mutate(pidp = case_when(pidp %in% checklist ~ pidp)) %>%
        na.omit(pidp)
    rm(checklist)
    joined2 %>%
        filter(!is.na(sex_dv), !is.na(marstat), !is.na(scghqk)) %>%
        select(wave, scghql) %>%
        group_by(wave) %>%
        summarise(meansat = mean(scghql, na.rm = TRUE))
}
out1 = vector("list", 6)
for (i in 1:6){
    out1[[i]] = catgor(i)
}
div.cats1 = split(out1, catgs)
div.cats1 = lapply(div.cats1, as.data.frame)
for (i in 1:6){
list2env(div.cats1[i],envir=.GlobalEnv)
}
rm(div.cats1, out1, catgor, i)

y.string = c("More so than usual", "Same as usual", "Less so than usual", "Much less than usual")

divorced2 = ggplot(data = divman2, mapping = aes(x=wave, y=meansat))+
    ylim(c(y.string))+
    xlim(c(x.string))+
    geom_smooth(col = "blue")+
    xlab("Wave")+
    ylab("Recent feelings of happiness")+
    ggtitle("Happiness of divorced people")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_smooth(data = divwom2, col = "magenta3")+
    geom_text(x=1, y=4.7, label="Men", color = "blue")+
    geom_text(x=1, y=5.5, label="Women", color = "magenta3")

married2 = ggplot(data = marman2, mapping = aes(x=wave, y=meansat))+
    ylim(c(y.string))+
    xlim(c(x.string))+
    geom_smooth(col = "blue")+
    xlab("Wave")+
    ylab("Life satisfaction")+
    ggtitle("Happiness of married people")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_smooth(data = marwom2, col = "magenta3")+
    geom_text(x=1, y=5.3, label="Men", color = "blue")+
    geom_text(x=1, y=5.9, label="Women", color = "magenta3")

grid.arrange(divorced2, married2, single, ncol = 2) 

































## Appendix code

w1.6 = joined %>%
    filter(wave == 3:6 & marstat == "married") %>%
    count(pidp) %>%
    filter(n == 5) %>%
    select(pidp)

w7.11 = joined %>%
    filter(wave == 7:11 & marstat == "divorced") %>%
    count(pidp) %>%
    filter(n == 4) %>%
    select(pidp)

w1.6$TF = pull(w1.6) %in% pull(w7.11)

w1.6 = w1.6 %>%
    filter(TF == FALSE)

NROW(w1.6)
[1] 0                                                    

## Data unavailable































