install.packages("eha")
library("eha")
install.packages("survival")
library("survival")
library("stargazer")
library("tidyverse")
library("ggfortify")

data("child")


child_surv <- with(child, Surv(enter, exit, event))
cox <- coxph(child_surv ~ sex + m.age, data = child)
summary(cox)


c <- drop1(cox, test = "Chisq")
stargazer(cox, type = "text")
stargazer(cox, type = "text", title="Child Mortality", digits=6, out="table1.txt")
stargazer(c, type = "text", title="Chi-Sq", digits=6, out="table2.txt")

