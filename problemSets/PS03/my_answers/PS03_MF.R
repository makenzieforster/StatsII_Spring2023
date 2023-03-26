
library(MASS)
library(nnet)
library(stargazer)

######
##Q1##
######

# load data & wrangle
data <- read.csv("datasets/gdpChange.csv")
data$GDPWdiffleveled <- sign(data$GDPWdiff)
data$GDPWdiffleveled <- factor(data$GDPWdiffleveled,
                               levels = c("-1", "0", "1"),
                               labels = c("negative", "no change", "positive"))
data$GDPWdiffleveled <- relevel(data$GDPWdiffleveled, ref = "no change")

#unordered multinomial logit
mod1 <- multinom(GDPWdiffleveled ~ REG + OIL, data = data)
mod1.2 <- exp(coef(mod1)[,c(1:3)])

# interpret
# oil export ratio was not a significant predictor in GDP change

# holding all else equal, in a given country there is an increase in baseline 
# odds that GDP will grow by 5.9 times when the regime is a democracy

#outputs
stargazer(mod1, type = "text", title="Effect of Democracy and Oil on GDP Difference", digits=1, out="table1.txt")
stargazer(mod1.2, type = "text", title="Effect of Democracy and Oil on GDP Difference", digits=1, out="table12.txt")

# ordered multinomial logit
mod2 <- polr(GDPWdiffleveled ~ REG + OIL, data = data, Hess = TRUE)

(ci <- confint(mod2))
mod2.2 <- exp(cbind(OR = coef(mod2), ci))

#interpretation
# holding all else equal, in a given country there is an increase in baseline 
# odds that GDP will grow (positive) by 1.5 times when the regime is a democracy
# compared with non democracies

#outputs
stargazer(mod2, type = "text", title="Effect of Democracy and Oil on GDP Difference", digits=1, out="table2.txt")
stargazer(mod2.2, type = "text", title="Effect of Democracy and Oil on GDP Difference", digits=1, out="table22.txt")


######
##Q2##
######

dat <- read.csv("datasets/MexicoMuniData.csv")

# Part A
mod3 <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + 
              PAN.governor.06,
            data = dat, family = poisson)

# test statistic = -0.477
# p value = 0.6336
# competitive district is not a significant predictor

#outputs
stargazer(mod3, type = "text", title="Likelihood of Candidate Visit", digits=1, out="table3.txt")
stargazer(intp, type = "text", title="Likelihood of Candidate Visit", digits=1, out="table32.txt")


# Part B - Interpretation
intp <- exp(coef(mod3))

# holding all else equal, a one unit increase marginality increases the expected
# counts of a PAN candidate visiting a given district by a multiplicative factor of 0.1.

# holding all else equal, the presence of a PAN increases the expected counts of
# a PAN candidate visiting a given district by a multiplicative factor of 0.7.


#Part C - estimation
cfs <- coef(mod3)
exp(cfs[1] + cfs[2]*1 + cfs[3]*0 + cfs[4]*1)

# 0.01494818

