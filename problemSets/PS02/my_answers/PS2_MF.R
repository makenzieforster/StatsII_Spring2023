#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)
library(tidyverse)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

mod <- glm(choice ~ countries + sanctions,
           data = climateSupport,
           family = binomial(link = "logit"))
summary(mod)

# Significance Test
null_mod <- glm(choice ~ 1,
                data = climateSupport,
                family = "binomial")
anova(null_mod, mod, test = "LRT")

# Per factor
reducedC <- glm(choice ~ countries,
                data = climateSupport,
                family = binomial(link = "logit"))
anova(reducedC, mod, test = "LRT")
# Effect of Countries is significant

reducedS <- glm(choice ~ sanctions,
                data = climateSupport,
                family = binomial(link = "logit"))
anova(reducedS, mod, test = "LRT")
# effect of Sanctions is also significant


#####################
# Problem 2
#####################

predicted_data <- with(climateSupport, expand.grid(countries = unique(countries),
                                                   sanctions = unique(sanctions)))

predicted_data <- cbind(predicted_data, predict(mod, 
                                                newdata = predicted_data,
                                                type = "response",
                                                se = TRUE))

predicted_data <- within(predicted_data,
                         {PredictedProb <- plogis(fit)
                         LL <- plogis(fit - (1.96 * se.fit))
                         UL <- plogis(fit + (1.96 * se.fit))
                         })
#### 2a ####
# For the policy in which 160 countries participate, 
# how does increasing sanctions from 5% to 15% (one level increase) 
# change the odds that an individual will support the policy?

exp(-0.181086)
#For the policy in which 160 countries participate, increasing sanctions from 
# 5% to 15% decreases the odds that an individual will support the policy. 
# Specifically, with sanctions at the 15% level, the individual is e^-0.181086
# or 0.8343636 less likely to support the policy than with sanctions at 5%.


#### 2b ####
# What is the estimated probability that an individual will support
# the policy if there are 80 countries participation with no sanctions?

predicted_data[4,]
# the estimated probability that an individual will support  the policy if there
# are 80 countries participation with no sanctions is 0.6261930

#### 2c ####
# Would answers to 2a and 2b change if we included the interaction term in the 
# model? Why? Perform a test to see if including an interaction is appropriate.
 
int_mod <- glm(choice ~ countries * sanctions,
                      data = climateSupport,
                      family = binomial(link = "logit"))
anova(mod, int_mod, test = "LRT")
# interaction is not appropriate bc there is not a significant difference from 
# the additive model


