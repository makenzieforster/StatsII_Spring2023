

library(dplyr)

# Predicted Probability of Response and Congratulations by Treatment Group
# OR: Pulling out the Models Used to See What's Up

# estimates logitistic regressions with treatment conditions and moderating 
# covariates, simulates predicted proability of response based on observed 
# covariate values, then plots the results
m1 <- glm(responded ~ as.factor(treatment) + ssm_history + type + population + 
         appointed + obamavote + cf, data=dt, family="binomial")
summary(m1)

m2 <- glm(congrats ~ as.factor(treatment) + ssm_history + type + population + 
         appointed + obamavote + cf, data=dt, family="binomial")
summary(m2)


# Predicted Probability of Quality Response by Treatment Group 
# estimates logistic regressions with treatment conditions and moderating 
# covariates, simulates predicted probability of response based on observed
# covariate values, then plots the results
m3 <- glm(q.cost ~ as.factor(treatment) + ssm_history + type + population + 
         appointed + obamavote + cf, data=dt, family="binomial")
m4 <- glm(q.valid ~ as.factor(treatment) + ssm_history + type + population +
         appointed + obamavote + cf, data=dt, family="binomial")
m5 <- glm(q.when ~ as.factor(treatment) + ssm_history + type + population + 
         appointed + obamavote + cf, data=dt, family="binomial")
m6 <- glm(felicitations ~ as.factor(treatment) + ssm_history + type + population
          + appointed + obamavote + cf, data=dt, family="binomial")

# The ol Razzle Dazzle

mx <- glm(congrats ~ as.factor(treatment) + sodomy_history + ssm_history + type + population + 
            appointed + obamavote + cf, data = dt, family = "binomial")
summary(mx)


### Significance Testing bc Its not in the paper
library(forcats)
data <- dt
data$treatment <- as.factor(data$treatment)
data$treatment <- fct_collapse(data$treatment, Gay = c("1","2"), Straight = c("3","4"))

# LGBT vs Straight

t.test(responded ~ treatment, data = data)
t.test(congrats ~ treatment, data = data)
t.test(felicitations ~ treatment, data = data)
t.test(q.valid ~ treatment, data = data)
t.test(q.when ~ treatment, data = data)
t.test(q.cost ~ treatment, data = data)


### Significance Testing for Men vs Women

dat <- dt
dat$treatment <- as.factor(dat$treatment)
levels(dat$treatment)
dat$treatment <- fct_collapse(dat$treatment, Male = c("1","3"), Female = c("2","4"))
t.test(responded ~ as.factor(treatment), data = dat)
t.test(congrats ~ as.factor(treatment), data = dat)

### difference between gay vs straight women
data_new <- dt[dt$treatment %in% c("2", "4"), ]                     

t.test(responded ~ as.factor(treatment), data = data_new) 
t.test(congrats ~ as.factor(treatment), data = data_new)


### difference between gay vs straight men
data_new2 <- dt[dt$treatment %in% c("1", "3"), ]
data_new2$treatment <- as.factor(data_new2$treatment)

t.test(responded ~ treatment, data = data_new2) 
t.test(congrats ~ treatment, data = data_new2)
t.test(felicitations ~ treatment, data = data_new2)
