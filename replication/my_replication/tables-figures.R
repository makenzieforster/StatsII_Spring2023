# ---------------------------
# R version: 3.6.1
# Code: 9/30/2019
# Title: 'Bureaucratic Responsiveness to LGBT Americans'
# Authors: Kenneth Lowande and Andrew Proctor
# Summary: Replication code for producing all tables and figures. 
# Please report errors to: lowande@umich.edu
# ---------------------------

cat("\014")
rm(list=ls())

# install.packages(c('xtable','ggplot2','gridExtra','arm','rgdal','rgeos','maptools','gpclib','ggthemes','doBy','irr','ngram','aod','stargazer'))

# FOR WINDOWS USERS needing to install 'gpclib'
#pathRtools <- paste(c("c:\\Rtools\\bin","c:\\Rtools\\MinGW\\bin","c:\\MiKTeX\\miktex\\bin", "c:\\R\\bin\\i386","c:\\windows", "c:\\windows\\system32"), collapse=";")
#Sys.setenv(PATH=paste(pathRtools,Sys.getenv("PATH"),sep=";")) 
#install.packages("gpclib", type="source")

require(xtable)
require(ggplot2)
require(gridExtra)
require(arm)
require(rgdal)
require(rgeos)
require(maptools)
require(gpclib)
require(ggthemes)
require(doBy)
require(irr)
require(ngram)
require(aod)
require(stargazer)

setwd('~/replication files')

# ---------------------------
# PRINT ARTICLE
# ---------------------------

# FIGURE 1: Marriage Granting Institutions in the U.S.
  # plots the level of government that marriage licenses by state
load('map-data.Rda')
load('us-shape.Rda')

f1=ggplot() + geom_map(data = map_data, aes(map_id = state, fill = factor(type)),map = us.states) +
  geom_polygon(data=us.states, aes(x=long, y=lat, group = group), size = 0.25, colour='white', fill=NA) +
  expand_limits(x = us.states$long, y = us.states$lat) + 
  theme_map() + theme(legend.position = 'bottom') +
  scale_fill_manual(name = 'Licensing Official:',values=c('#181818','#E0E0E0'))
# ---------------------------

# TABLE 2: Mean Response by Treatment in All States
  # reports mean responses by treatment conditions
load('audit.Rda')
tb=data.frame()
tb[1,1]='Male Emailer, Male Spouse'
tb[2,1]='Female Emailer, Female Spouse'
tb[3,1]='Male Emailer, Female Spouse'
tb[4,1]='Female Emailer, Male Spouse'
tb[1,2]=round(mean(dt$responded[dt$treatment==1],na.rm=T),digits=3)
tb[2,2]=round(mean(dt$responded[dt$treatment==2],na.rm=T),digits=3)
tb[3,2]=round(mean(dt$responded[dt$treatment==3],na.rm=T),digits=3)
tb[4,2]=round(mean(dt$responded[dt$treatment==4],na.rm=T),digits=3)
tb[1,3]=round(mean(dt$congrats[dt$treatment==1]),digits=3)
tb[2,3]=round(mean(dt$congrats[dt$treatment==2]),digits=3)
tb[3,3]=round(mean(dt$congrats[dt$treatment==3]),digits=3)
tb[4,3]=round(mean(dt$congrats[dt$treatment==4]),digits=3)
tb[1,4]=round(mean(dt$q.cost[dt$treatment==1],na.rm=T),digits=3)
tb[2,4]=round(mean(dt$q.cost[dt$treatment==2],na.rm=T),digits=3)
tb[3,4]=round(mean(dt$q.cost[dt$treatment==3],na.rm=T),digits=3)
tb[4,4]=round(mean(dt$q.cost[dt$treatment==4],na.rm=T),digits=3)
tb[1,5]=round(mean(dt$q.valid[dt$treatment==1],na.rm=T),digits=3)
tb[2,5]=round(mean(dt$q.valid[dt$treatment==2],na.rm=T),digits=3)
tb[3,5]=round(mean(dt$q.valid[dt$treatment==3],na.rm=T),digits=3)
tb[4,5]=round(mean(dt$q.valid[dt$treatment==4],na.rm=T),digits=3)
tb[1,6]=round(mean(dt$q.when[dt$treatment==1],na.rm=T),digits=3)
tb[2,6]=round(mean(dt$q.when[dt$treatment==2],na.rm=T),digits=3)
tb[3,6]=round(mean(dt$q.when[dt$treatment==3],na.rm=T),digits=3)
tb[4,6]=round(mean(dt$q.when[dt$treatment==4],na.rm=T),digits=3)
tb[1,7]=round(mean(dt$felicitations[dt$treatment==1]),digits=3)
tb[2,7]=round(mean(dt$felicitations[dt$treatment==2]),digits=3)
tb[3,7]=round(mean(dt$felicitations[dt$treatment==3]),digits=3)
tb[4,7]=round(mean(dt$felicitations[dt$treatment==4]),digits=3)
tb[1,8]=nrow(dt[dt$treatment==1,])
tb[2,8]=nrow(dt[dt$treatment==2,])
tb[3,8]=nrow(dt[dt$treatment==3,])
tb[4,8]=nrow(dt[dt$treatment==4,])
names(tb)=c('Treatment','Response','Congrats','Cost','Valid','When','Well Wishes','N')

print.xtable(xtable(tb[1:8],caption='Mean Response by Treatments in All States',label='tab:sum',align='rl|ccccccc',digits=3), include.rownames=F,table.placement='H',caption.placement='top',size='small')
# ---------------------------

# FIGURE 3: Predicted Probability of Response and Congratulations by Treatment Group
# estimates logitistic regressions with treatment conditions and moderating covariates, simulates predicted proability of response based on observed corvariate values, then plots the results
m1=glm(responded ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
m2=glm(congrats ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
outcomes=matrix(data=NA, ncol=7,nrow=8)

model=m1
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)
outcomes[1,1]<- quantile(p_MM,.025)
outcomes[1,2]<- quantile(p_MM,.05)
outcomes[1,3]<- mean(p_MM)
outcomes[1,4]<- quantile(p_MM,.95)
outcomes[1,5]<- quantile(p_MM,.975)
outcomes[1,6]<- 'DV: Response'
outcomes[1,7]<- 'Male, Male'
outcomes[2,1]<- quantile(p_FF,.025)
outcomes[2,2]<- quantile(p_FF,.05)
outcomes[2,3]<- mean(p_FF)
outcomes[2,4]<- quantile(p_FF,.95)
outcomes[2,5]<- quantile(p_FF,.975)
outcomes[2,6]<- 'DV: Response'
outcomes[2,7]<- 'Female, Female'
outcomes[3,1]<- quantile(p_MF,.025)
outcomes[3,2]<- quantile(p_MF,.05)
outcomes[3,3]<- mean(p_MF)
outcomes[3,4]<- quantile(p_MF,.95)
outcomes[3,5]<- quantile(p_MF,.975)
outcomes[3,6]<- 'DV: Response'
outcomes[3,7]<- 'Male, Female'
outcomes[4,1]<- quantile(p_FM,.025)
outcomes[4,2]<- quantile(p_FM,.05)
outcomes[4,3]<- mean(p_FM)
outcomes[4,4]<- quantile(p_FM,.95)
outcomes[4,5]<- quantile(p_FM,.975)
outcomes[4,6]<- 'DV: Response'
outcomes[4,7]<- 'Female, Male'

model=m2
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)
outcomes[5,1]<- quantile(p_MM,.025)
outcomes[5,2]<- quantile(p_MM,.05)
outcomes[5,3]<- mean(p_MM)
outcomes[5,4]<- quantile(p_MM,.95)
outcomes[5,5]<- quantile(p_MM,.975)
outcomes[5,6]<- 'DV: Congratulations'
outcomes[5,7]<- 'Male, Male'
outcomes[6,1]<- quantile(p_FF,.025)
outcomes[6,2]<- quantile(p_FF,.05)
outcomes[6,3]<- mean(p_FF)
outcomes[6,4]<- quantile(p_FF,.95)
outcomes[6,5]<- quantile(p_FF,.975)
outcomes[6,6]<- 'DV: Congratulations'
outcomes[6,7]<- 'Female, Female'
outcomes[7,1]<- quantile(p_MF,.025)
outcomes[7,2]<- quantile(p_MF,.05)
outcomes[7,3]<- mean(p_MF)
outcomes[7,4]<- quantile(p_MF,.95)
outcomes[7,5]<- quantile(p_MF,.975)
outcomes[7,6]<- 'DV: Congratulations'
outcomes[7,7]<- 'Male, Female'
outcomes[8,1]<- quantile(p_FM,.025)
outcomes[8,2]<- quantile(p_FM,.05)
outcomes[8,3]<- mean(p_FM)
outcomes[8,4]<- quantile(p_FM,.95)
outcomes[8,5]<- quantile(p_FM,.975)
outcomes[8,6]<- 'DV: Congratulations'
outcomes[8,7]<- 'Female, Male'

sr=data.frame(outcomes)
names(sr)=c('lb.95','lb.90','est','ub.90','ub.95','dv','treat')
sr[,1]=as.numeric(as.character(sr[,1]))
sr[,2]=as.numeric(as.character(sr[,2]))
sr[,3]=as.numeric(as.character(sr[,3]))
sr[,4]=as.numeric(as.character(sr[,4]))
sr[,5]=as.numeric(as.character(sr[,5]))
  
rr=ggplot(sr[sr$dv=='DV: Response',]) +
  geom_point(aes(x = treat, y = est, size= 2)) +
  geom_linerange(aes(x = treat, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = treat, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  theme_bw() + xlab('') + ylab('Response Rate') +
  scale_y_continuous(limits=c(.6,.8)) +
  theme(axis.text.x=element_text(angle=10),legend.title=element_blank(),legend.position='none')
cr=ggplot(sr[sr$dv=='DV: Congratulations',]) +
  geom_point(aes(x = treat, y = est, size= 2)) +
  geom_linerange(aes(x = treat, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = treat, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  theme_bw() + xlab('') + ylab('Congratulations Rate') +
  scale_y_continuous(limits=c(0,.2)) +
  theme(axis.text.x=element_text(angle=10),legend.title=element_blank(),legend.position='none')
grid.arrange(rr,cr,nrow=1)
# ---------------------------

# FIGURE 4: Predicted Probability of Quality Response by Treatment Group
# estimates logitistic regressions with treatment conditions and moderating covariates, simulates predicted proability of response based on observed carvariate values, then plots the results
m3=glm(q.cost ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
m4=glm(q.valid ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
m5=glm(q.when ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
m6=glm(felicitations ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
outcomes=matrix(data=NA, ncol=7,nrow=16)
dds=matrix(data=NA,ncol=6,nrow=4)

model=m3
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)

diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
dds[1,1]<- quantile(diffdiff,.025)
dds[1,2]<- quantile(diffdiff,.05)
dds[1,3]<- mean(diffdiff)
dds[1,4]<- quantile(diffdiff,.95)
dds[1,5]<- quantile(diffdiff,.975)
dds[1,6]<- 'DV: Cost?'

outcomes[1,1]<- quantile(p_MM,.025)
outcomes[1,2]<- quantile(p_MM,.05)
outcomes[1,3]<- mean(p_MM)
outcomes[1,4]<- quantile(p_MM,.95)
outcomes[1,5]<- quantile(p_MM,.975)
outcomes[1,6]<- 'DV: Cost?'
outcomes[1,7]<- 'Male, Male'
outcomes[2,1]<- quantile(p_FF,.025)
outcomes[2,2]<- quantile(p_FF,.05)
outcomes[2,3]<- mean(p_FF)
outcomes[2,4]<- quantile(p_FF,.95)
outcomes[2,5]<- quantile(p_FF,.975)
outcomes[2,6]<- 'DV: Cost?'
outcomes[2,7]<- 'Female, Female'
outcomes[3,1]<- quantile(p_MF,.025)
outcomes[3,2]<- quantile(p_MF,.05)
outcomes[3,3]<- mean(p_MF)
outcomes[3,4]<- quantile(p_MF,.95)
outcomes[3,5]<- quantile(p_MF,.975)
outcomes[3,6]<- 'DV: Cost?'
outcomes[3,7]<- 'Male, Female'
outcomes[4,1]<- quantile(p_FM,.025)
outcomes[4,2]<- quantile(p_FM,.05)
outcomes[4,3]<- mean(p_FM)
outcomes[4,4]<- quantile(p_FM,.95)
outcomes[4,5]<- quantile(p_FM,.975)
outcomes[4,6]<- 'DV: Cost?'
outcomes[4,7]<- 'Female, Male'

model=m4
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)

diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
dds[2,1]<- quantile(diffdiff,.025)
dds[2,2]<- quantile(diffdiff,.05)
dds[2,3]<- mean(diffdiff)
dds[2,4]<- quantile(diffdiff,.95)
dds[2,5]<- quantile(diffdiff,.975)
dds[2,6]<- 'DV: Valid?'

outcomes[5,1]<- quantile(p_MM,.025)
outcomes[5,2]<- quantile(p_MM,.05)
outcomes[5,3]<- mean(p_MM)
outcomes[5,4]<- quantile(p_MM,.95)
outcomes[5,5]<- quantile(p_MM,.975)
outcomes[5,6]<- 'DV: Valid?'
outcomes[5,7]<- 'Male, Male'
outcomes[6,1]<- quantile(p_FF,.025)
outcomes[6,2]<- quantile(p_FF,.05)
outcomes[6,3]<- mean(p_FF)
outcomes[6,4]<- quantile(p_FF,.95)
outcomes[6,5]<- quantile(p_FF,.975)
outcomes[6,6]<- 'DV: Valid?'
outcomes[6,7]<- 'Female, Female'
outcomes[7,1]<- quantile(p_MF,.025)
outcomes[7,2]<- quantile(p_MF,.05)
outcomes[7,3]<- mean(p_MF)
outcomes[7,4]<- quantile(p_MF,.95)
outcomes[7,5]<- quantile(p_MF,.975)
outcomes[7,6]<- 'DV: Valid?'
outcomes[7,7]<- 'Male, Female'
outcomes[8,1]<- quantile(p_FM,.025)
outcomes[8,2]<- quantile(p_FM,.05)
outcomes[8,3]<- mean(p_FM)
outcomes[8,4]<- quantile(p_FM,.95)
outcomes[8,5]<- quantile(p_FM,.975)
outcomes[8,6]<- 'DV: Valid?'
outcomes[8,7]<- 'Female, Male'

model=m5
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)

diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
dds[3,1]<- quantile(diffdiff,.025)
dds[3,2]<- quantile(diffdiff,.05)
dds[3,3]<- mean(diffdiff)
dds[3,4]<- quantile(diffdiff,.95)
dds[3,5]<- quantile(diffdiff,.975)
dds[3,6]<- 'DV: When?'

outcomes[9,1]<- quantile(p_MM,.025)
outcomes[9,2]<- quantile(p_MM,.05)
outcomes[9,3]<- mean(p_MM)
outcomes[9,4]<- quantile(p_MM,.95)
outcomes[9,5]<- quantile(p_MM,.975)
outcomes[9,6]<- 'DV: When?'
outcomes[9,7]<- 'Male, Male'
outcomes[10,1]<- quantile(p_FF,.025)
outcomes[10,2]<- quantile(p_FF,.05)
outcomes[10,3]<- mean(p_FF)
outcomes[10,4]<- quantile(p_FF,.95)
outcomes[10,5]<- quantile(p_FF,.975)
outcomes[10,6]<- 'DV: When?'
outcomes[10,7]<- 'Female, Female'
outcomes[11,1]<- quantile(p_MF,.025)
outcomes[11,2]<- quantile(p_MF,.05)
outcomes[11,3]<- mean(p_MF)
outcomes[11,4]<- quantile(p_MF,.95)
outcomes[11,5]<- quantile(p_MF,.975)
outcomes[11,6]<- 'DV: When?'
outcomes[11,7]<- 'Male, Female'
outcomes[12,1]<- quantile(p_FM,.025)
outcomes[12,2]<- quantile(p_FM,.05)
outcomes[12,3]<- mean(p_FM)
outcomes[12,4]<- quantile(p_FM,.95)
outcomes[12,5]<- quantile(p_FM,.975)
outcomes[12,6]<- 'DV: When?'
outcomes[12,7]<- 'Female, Male'

model=m6
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)

diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
dds[4,1]<- quantile(diffdiff,.025)
dds[4,2]<- quantile(diffdiff,.05)
dds[4,3]<- mean(diffdiff)
dds[4,4]<- quantile(diffdiff,.95)
dds[4,5]<- quantile(diffdiff,.975)
dds[4,6]<- 'DV: Well Wishes'

outcomes[13,1]<- quantile(p_MM,.025)
outcomes[13,2]<- quantile(p_MM,.05)
outcomes[13,3]<- mean(p_MM)
outcomes[13,4]<- quantile(p_MM,.95)
outcomes[13,5]<- quantile(p_MM,.975)
outcomes[13,6]<- 'DV: Well Wishes'
outcomes[13,7]<- 'Male, Male'
outcomes[14,1]<- quantile(p_FF,.025)
outcomes[14,2]<- quantile(p_FF,.05)
outcomes[14,3]<- mean(p_FF)
outcomes[14,4]<- quantile(p_FF,.95)
outcomes[14,5]<- quantile(p_FF,.975)
outcomes[14,6]<- 'DV: Well Wishes'
outcomes[14,7]<- 'Female, Female'
outcomes[15,1]<- quantile(p_MF,.025)
outcomes[15,2]<- quantile(p_MF,.05)
outcomes[15,3]<- mean(p_MF)
outcomes[15,4]<- quantile(p_MF,.95)
outcomes[15,5]<- quantile(p_MF,.975)
outcomes[15,6]<- 'DV: Well Wishes'
outcomes[15,7]<- 'Male, Female'
outcomes[16,1]<- quantile(p_FM,.025)
outcomes[16,2]<- quantile(p_FM,.05)
outcomes[16,3]<- mean(p_FM)
outcomes[16,4]<- quantile(p_FM,.95)
outcomes[16,5]<- quantile(p_FM,.975)
outcomes[16,6]<- 'DV: Well Wishes'
outcomes[16,7]<- 'Female, Male'

sr=data.frame(outcomes)
names(sr)=c('lb.95','lb.90','est','ub.90','ub.95','dv','treat')
sr[,1]=as.numeric(as.character(sr[,1]))
sr[,2]=as.numeric(as.character(sr[,2]))
sr[,3]=as.numeric(as.character(sr[,3]))
sr[,4]=as.numeric(as.character(sr[,4]))
sr[,5]=as.numeric(as.character(sr[,5]))

dd=data.frame(dds)
names(dd)=c('lb.95','lb.90','est','ub.90','ub.95','dv')
dd[,1]=as.numeric(as.character(dd[,1]))
dd[,2]=as.numeric(as.character(dd[,2]))
dd[,3]=as.numeric(as.character(dd[,3]))
dd[,4]=as.numeric(as.character(dd[,4]))
dd[,5]=as.numeric(as.character(dd[,5]))

cost=ggplot(sr[sr$dv=='DV: Cost?',]) +
  geom_point(aes(x = treat, y = est, size= 2)) +
  geom_linerange(aes(x = treat, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = treat, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  theme_bw() + xlab('') + ylab('Answered Cost Question') +
  scale_y_continuous(limits=c(.4,.6)) +
  theme(axis.text.x=element_text(angle=10),legend.title=element_blank(),legend.position='none')
valid=ggplot(sr[sr$dv=='DV: Valid?',]) +
  geom_point(aes(x = treat, y = est, size= 2)) +
  geom_linerange(aes(x = treat, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = treat, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  theme_bw() + xlab('') + ylab('Answered Valid Question') +
  scale_y_continuous(limits=c(.4,.6)) +
  theme(axis.text.x=element_text(angle=10),legend.title=element_blank(),legend.position='none')
when=ggplot(sr[sr$dv=='DV: When?',]) +
  geom_point(aes(x = treat, y = est, size= 2)) +
  geom_linerange(aes(x = treat, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = treat, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  theme_bw() + xlab('') + ylab('Answered When Question') +
  scale_y_continuous(limits=c(.3,.5)) +
  theme(axis.text.x=element_text(angle=10),legend.title=element_blank(),legend.position='none')
well=ggplot(sr[sr$dv=='DV: Well Wishes',]) +
  geom_point(aes(x = treat, y = est, size= 2)) +
  geom_linerange(aes(x = treat, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = treat, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  theme_bw() + xlab('') + ylab('Provided Well Wishes') +
  scale_y_continuous(limits=c(0,.2)) +
  theme(axis.text.x=element_text(angle=10),legend.title=element_blank(),legend.position='none')
grid.arrange(cost,valid,when,well,nrow=2)
# ---------------------------

# FIGURE 5: No Evidence of Differences in the Quality of Responses
# estimates logitistic regressions with treatment conditions and moderating covariates, simulates predicted proability of response based on observed carvariate values, then plots the results
clrs <- c('#000000','#000000','#000000','#000000')
f5=ggplot(dd, aes(colour = dv)) +
  geom_point(aes(x = dv, y = est, size= 2)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = dv, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = dv, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Response Rate Difference') +
  scale_colour_manual(values=clrs) + scale_y_continuous(limits=c(-0.05,0.05)) +
  theme(axis.text.y=element_text(angle=45),legend.title=element_blank(),legend.position='none')
# ---------------------------

# FIGURE 6: State Laws Affecting Same-Sex Couples
map1=ggplot() + geom_map(data = map_data, aes(map_id = state, fill = factor(ssm_history)),map = us.states) +
  geom_polygon(data=us.states, aes(x=long, y=lat, group = group), size = 0.25, colour='white', fill=NA) +
  expand_limits(x = us.states$long, y = us.states$lat) + 
  theme_map() + theme(legend.position = 'bottom') +
  scale_fill_manual(name = 'Same-Sex Marriage Prior to 2015:',values=c('#181818','#e0e0e0'))
map2=ggplot() + geom_map(data = map_data, aes(map_id = state, fill = factor(sodomy_history)),map = us.states) +
  geom_polygon(data=us.states, aes(x=long, y=lat, group = group), size = 0.25, colour='white', fill=NA) +
  expand_limits(x = us.states$long, y = us.states$lat) + 
  theme_map() + theme(legend.position = 'bottom') +
  scale_fill_manual(name = 'Sodomy Law Prior to 2003:',values=c('#e0e0e0','#181818'))
grid.arrange(arrangeGrob(map1,map2),ncol=1)
# ---------------------------

# FIGURE 7: No Evidence of Responsiveness Differential from Simulated Differences
# estimates logitistic regressions with treatment conditions and moderating covariates, simulates predicted proability of response based on observed carvariate values, then plots the results
m7=glm(responded ~ as.factor(treatment) + sodomy_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
m8=glm(congrats ~ as.factor(treatment) + sodomy_history + type + population + appointed + obamavote + cf, data=dt, family="binomial")
outcomes=matrix(data=NA, ncol=7,nrow=10)

model=m1
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)
diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
outcomes[1,1]<- quantile(diffdiff,.025)
outcomes[1,2]<- quantile(diffdiff,.05)
outcomes[1,3]<- mean(diffdiff)
outcomes[1,4]<- quantile(diffdiff,.95)
outcomes[1,5]<- quantile(diffdiff,.975)
outcomes[1,6]<- 'DV: Response Rate'
outcomes[1,7]<- 'All States'

model=m2
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)
diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
outcomes[2,1]<- quantile(diffdiff,.025)
outcomes[2,2]<- quantile(diffdiff,.05)
outcomes[2,3]<- mean(diffdiff)
outcomes[2,4]<- quantile(diffdiff,.95)
outcomes[2,5]<- quantile(diffdiff,.975)
outcomes[2,6]<- 'DV: Congratulations Rate'
outcomes[2,7]<- 'All States'

model=m1
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM.Legal=cbind(1, 0, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Legal=cbind(1, 1, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Legal=cbind(1, 0, 1, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Legal=cbind(1, 0, 0, 1, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MM.Illegal=cbind(1, 0, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Illegal=cbind(1, 1, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Illegal=cbind(1, 0, 1, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Illegal=cbind(1, 0, 0, 1, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM.Legal=apply((1/(1+exp(-as.matrix(d.MM.Legal)%*% t(betas)))),1,as.vector) 
p_MM_Legal=apply(p.MM.Legal,1,mean)
p.FF.Legal=apply((1/(1+exp(-as.matrix(d.FF.Legal)%*% t(betas)))),1,as.vector) 
p_FF_Legal=apply(p.FF.Legal,1,mean)
p.MF.Legal=apply((1/(1+exp(-as.matrix(d.MF.Legal)%*% t(betas)))),1,as.vector) 
p_MF_Legal=apply(p.MF.Legal,1,mean)
p.FM.Legal=apply((1/(1+exp(-as.matrix(d.FM.Legal)%*% t(betas)))),1,as.vector) 
p_FM_Legal=apply(p.FM.Legal,1,mean)
p.MM.Illegal=apply((1/(1+exp(-as.matrix(d.MM.Illegal)%*% t(betas)))),1,as.vector) 
p_MM_Illegal=apply(p.MM.Illegal,1,mean)
p.FF.Illegal=apply((1/(1+exp(-as.matrix(d.FF.Illegal)%*% t(betas)))),1,as.vector) 
p_FF_Illegal=apply(p.FF.Illegal,1,mean)
p.MF.Illegal=apply((1/(1+exp(-as.matrix(d.MF.Illegal)%*% t(betas)))),1,as.vector) 
p_MF_Illegal=apply(p.MF.Illegal,1,mean)
p.FM.Illegal=apply((1/(1+exp(-as.matrix(d.FM.Illegal)%*% t(betas)))),1,as.vector) 
p_FM_Illegal=apply(p.FM.Illegal,1,mean)
diffdiff.Legal=((p_MM_Legal+p_FF_Legal)/2)-((p_MF_Legal+p_FM_Legal)/2)
diffdiff.Illegal=((p_MM_Illegal+p_FF_Illegal)/2)-((p_MF_Illegal+p_FM_Illegal)/2)
outcomes[3,1]<- quantile(diffdiff.Legal,.025)
outcomes[3,2]<- quantile(diffdiff.Legal,.05)
outcomes[3,3]<- mean(diffdiff.Legal)
outcomes[3,4]<- quantile(diffdiff.Legal,.95)
outcomes[3,5]<- quantile(diffdiff.Legal,.975)
outcomes[3,6]<- 'DV: Response Rate'
outcomes[3,7]<- 'State SSM Legal (Pre-2015)'
outcomes[4,1]<- quantile(diffdiff.Illegal,.025)
outcomes[4,2]<- quantile(diffdiff.Illegal,.05)
outcomes[4,3]<- mean(diffdiff.Illegal)
outcomes[4,4]<- quantile(diffdiff.Illegal,.95)
outcomes[4,5]<- quantile(diffdiff.Illegal,.975)
outcomes[4,6]<- 'DV: Response Rate'
outcomes[4,7]<- 'State SSM Ban (Pre-2015)'

model=m2
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM.Legal=cbind(1, 0, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Legal=cbind(1, 1, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Legal=cbind(1, 0, 1, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Legal=cbind(1, 0, 0, 1, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MM.Illegal=cbind(1, 0, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Illegal=cbind(1, 1, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Illegal=cbind(1, 0, 1, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Illegal=cbind(1, 0, 0, 1, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM.Legal=apply((1/(1+exp(-as.matrix(d.MM.Legal)%*% t(betas)))),1,as.vector) 
p_MM_Legal=apply(p.MM.Legal,1,mean)
p.FF.Legal=apply((1/(1+exp(-as.matrix(d.FF.Legal)%*% t(betas)))),1,as.vector) 
p_FF_Legal=apply(p.FF.Legal,1,mean)
p.MF.Legal=apply((1/(1+exp(-as.matrix(d.MF.Legal)%*% t(betas)))),1,as.vector) 
p_MF_Legal=apply(p.MF.Legal,1,mean)
p.FM.Legal=apply((1/(1+exp(-as.matrix(d.FM.Legal)%*% t(betas)))),1,as.vector) 
p_FM_Legal=apply(p.FM.Legal,1,mean)
p.MM.Illegal=apply((1/(1+exp(-as.matrix(d.MM.Illegal)%*% t(betas)))),1,as.vector) 
p_MM_Illegal=apply(p.MM.Illegal,1,mean)
p.FF.Illegal=apply((1/(1+exp(-as.matrix(d.FF.Illegal)%*% t(betas)))),1,as.vector) 
p_FF_Illegal=apply(p.FF.Illegal,1,mean)
p.MF.Illegal=apply((1/(1+exp(-as.matrix(d.MF.Illegal)%*% t(betas)))),1,as.vector) 
p_MF_Illegal=apply(p.MF.Illegal,1,mean)
p.FM.Illegal=apply((1/(1+exp(-as.matrix(d.FM.Illegal)%*% t(betas)))),1,as.vector) 
p_FM_Illegal=apply(p.FM.Illegal,1,mean)
diffdiff.Legal=((p_MM_Legal+p_FF_Legal)/2)-((p_MF_Legal+p_FM_Legal)/2)
diffdiff.Illegal=((p_MM_Illegal+p_FF_Illegal)/2)-((p_MF_Illegal+p_FM_Illegal)/2)
outcomes[5,1]<- quantile(diffdiff.Legal,.025)
outcomes[5,2]<- quantile(diffdiff.Legal,.05)
outcomes[5,3]<- mean(diffdiff.Legal)
outcomes[5,4]<- quantile(diffdiff.Legal,.95)
outcomes[5,5]<- quantile(diffdiff.Legal,.975)
outcomes[5,6]<- 'DV: Congratulations Rate'
outcomes[5,7]<- 'State SSM Legal (Pre-2015)'
outcomes[6,1]<- quantile(diffdiff.Illegal,.025)
outcomes[6,2]<- quantile(diffdiff.Illegal,.05)
outcomes[6,3]<- mean(diffdiff.Illegal)
outcomes[6,4]<- quantile(diffdiff.Illegal,.95)
outcomes[6,5]<- quantile(diffdiff.Illegal,.975)
outcomes[6,6]<- 'DV: Congratulations Rate'
outcomes[6,7]<- 'State SSM Ban (Pre-2015)'

model=m7
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM.Legal=cbind(1, 0, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Legal=cbind(1, 1, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Legal=cbind(1, 0, 1, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Legal=cbind(1, 0, 0, 1, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MM.Illegal=cbind(1, 0, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Illegal=cbind(1, 1, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Illegal=cbind(1, 0, 1, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Illegal=cbind(1, 0, 0, 1, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM.Legal=apply((1/(1+exp(-as.matrix(d.MM.Legal)%*% t(betas)))),1,as.vector) 
p_MM_Legal=apply(p.MM.Legal,1,mean)
p.FF.Legal=apply((1/(1+exp(-as.matrix(d.FF.Legal)%*% t(betas)))),1,as.vector) 
p_FF_Legal=apply(p.FF.Legal,1,mean)
p.MF.Legal=apply((1/(1+exp(-as.matrix(d.MF.Legal)%*% t(betas)))),1,as.vector) 
p_MF_Legal=apply(p.MF.Legal,1,mean)
p.FM.Legal=apply((1/(1+exp(-as.matrix(d.FM.Legal)%*% t(betas)))),1,as.vector) 
p_FM_Legal=apply(p.FM.Legal,1,mean)
p.MM.Illegal=apply((1/(1+exp(-as.matrix(d.MM.Illegal)%*% t(betas)))),1,as.vector) 
p_MM_Illegal=apply(p.MM.Illegal,1,mean)
p.FF.Illegal=apply((1/(1+exp(-as.matrix(d.FF.Illegal)%*% t(betas)))),1,as.vector) 
p_FF_Illegal=apply(p.FF.Illegal,1,mean)
p.MF.Illegal=apply((1/(1+exp(-as.matrix(d.MF.Illegal)%*% t(betas)))),1,as.vector) 
p_MF_Illegal=apply(p.MF.Illegal,1,mean)
p.FM.Illegal=apply((1/(1+exp(-as.matrix(d.FM.Illegal)%*% t(betas)))),1,as.vector) 
p_FM_Illegal=apply(p.FM.Illegal,1,mean)
diffdiff.Legal=((p_MM_Legal+p_FF_Legal)/2)-((p_MF_Legal+p_FM_Legal)/2)
diffdiff.Illegal=((p_MM_Illegal+p_FF_Illegal)/2)-((p_MF_Illegal+p_FM_Illegal)/2)
outcomes[7,1]<- quantile(diffdiff.Legal,.025)
outcomes[7,2]<- quantile(diffdiff.Legal,.05)
outcomes[7,3]<- mean(diffdiff.Legal)
outcomes[7,4]<- quantile(diffdiff.Legal,.95)
outcomes[7,5]<- quantile(diffdiff.Legal,.975)
outcomes[7,6]<- 'DV: Response Rate'
outcomes[7,7]<- 'No State Sodomy Law (Pre-2003)'
outcomes[8,1]<- quantile(diffdiff.Illegal,.025)
outcomes[8,2]<- quantile(diffdiff.Illegal,.05)
outcomes[8,3]<- mean(diffdiff.Illegal)
outcomes[8,4]<- quantile(diffdiff.Illegal,.95)
outcomes[8,5]<- quantile(diffdiff.Illegal,.975)
outcomes[8,6]<- 'DV: Response Rate'
outcomes[8,7]<- 'State Sodomy Law (Pre-2003)'

model=m8
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM.Legal=cbind(1, 0, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Legal=cbind(1, 1, 0, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Legal=cbind(1, 0, 1, 0, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Legal=cbind(1, 0, 0, 1, 1, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MM.Illegal=cbind(1, 0, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF.Illegal=cbind(1, 1, 0, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF.Illegal=cbind(1, 0, 1, 0, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM.Illegal=cbind(1, 0, 0, 1, 0, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM.Legal=apply((1/(1+exp(-as.matrix(d.MM.Legal)%*% t(betas)))),1,as.vector) 
p_MM_Legal=apply(p.MM.Legal,1,mean)
p.FF.Legal=apply((1/(1+exp(-as.matrix(d.FF.Legal)%*% t(betas)))),1,as.vector) 
p_FF_Legal=apply(p.FF.Legal,1,mean)
p.MF.Legal=apply((1/(1+exp(-as.matrix(d.MF.Legal)%*% t(betas)))),1,as.vector) 
p_MF_Legal=apply(p.MF.Legal,1,mean)
p.FM.Legal=apply((1/(1+exp(-as.matrix(d.FM.Legal)%*% t(betas)))),1,as.vector) 
p_FM_Legal=apply(p.FM.Legal,1,mean)
p.MM.Illegal=apply((1/(1+exp(-as.matrix(d.MM.Illegal)%*% t(betas)))),1,as.vector) 
p_MM_Illegal=apply(p.MM.Illegal,1,mean)
p.FF.Illegal=apply((1/(1+exp(-as.matrix(d.FF.Illegal)%*% t(betas)))),1,as.vector) 
p_FF_Illegal=apply(p.FF.Illegal,1,mean)
p.MF.Illegal=apply((1/(1+exp(-as.matrix(d.MF.Illegal)%*% t(betas)))),1,as.vector) 
p_MF_Illegal=apply(p.MF.Illegal,1,mean)
p.FM.Illegal=apply((1/(1+exp(-as.matrix(d.FM.Illegal)%*% t(betas)))),1,as.vector) 
p_FM_Illegal=apply(p.FM.Illegal,1,mean)
diffdiff.Legal=((p_MM_Legal+p_FF_Legal)/2)-((p_MF_Legal+p_FM_Legal)/2)
diffdiff.Illegal=((p_MM_Illegal+p_FF_Illegal)/2)-((p_MF_Illegal+p_FM_Illegal)/2)
outcomes[9,1]<- quantile(diffdiff.Legal,.025)
outcomes[9,2]<- quantile(diffdiff.Legal,.05)
outcomes[9,3]<- mean(diffdiff.Legal)
outcomes[9,4]<- quantile(diffdiff.Legal,.95)
outcomes[9,5]<- quantile(diffdiff.Legal,.975)
outcomes[9,6]<- 'DV: Congratulations Rate'
outcomes[9,7]<- 'No State Sodomy Law (Pre-2003)'
outcomes[10,1]<- quantile(diffdiff.Illegal,.025)
outcomes[10,2]<- quantile(diffdiff.Illegal,.05)
outcomes[10,3]<- mean(diffdiff.Illegal)
outcomes[10,4]<- quantile(diffdiff.Illegal,.95)
outcomes[10,5]<- quantile(diffdiff.Illegal,.975)
outcomes[10,6]<- 'DV: Congratulations Rate'
outcomes[10,7]<- 'State Sodomy Law (Pre-2003)'

sfd=data.frame(outcomes)
names(sfd)=c('lb.95','lb.90','est','ub.90','ub.95','dv','states')
sfd[,1]=as.numeric(as.character(sfd[,1]))
sfd[,2]=as.numeric(as.character(sfd[,2]))
sfd[,3]=as.numeric(as.character(sfd[,3]))
sfd[,4]=as.numeric(as.character(sfd[,4]))
sfd[,5]=as.numeric(as.character(sfd[,5]))
sfd$states=factor(sfd$states,levels = c("All States","No State Sodomy Law (Pre-2003)","State Sodomy Law (Pre-2003)","State SSM Legal (Pre-2015)","State SSM Ban (Pre-2015)"))

clrs <- c('#000000','#000000','#000000','#000000','#000000')
dp1=ggplot(sfd[sfd$dv=='DV: Response Rate',], aes(colour = states)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = states, y = est, size= 2)) +
  geom_linerange(aes(x = states, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = states, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Response Rates') +
  scale_colour_manual(values=clrs) + scale_y_continuous(limits=c(-0.05,0.05)) +
  theme(axis.text.y=element_text(angle=45),legend.title=element_blank())
dp2=ggplot(sfd[sfd$dv=='DV: Congratulations Rate',], aes(colour = states)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_point(aes(x = states, y = est, size= 2)) +
  geom_linerange(aes(x = states, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = states, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Congratulations Rates') +
  scale_colour_manual(values=clrs) + scale_y_continuous(limits=c(-0.05,0.05)) +
  theme(axis.text.y=element_text(angle=45),legend.title=element_blank())
dp3=ggplot(sfd[sfd$dv=='DV: Congratulations Rate',], aes(colour = states)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = states, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = states, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Congratulations Rates') +
  scale_colour_manual(values=clrs) + scale_y_continuous(limits=c(-0.05,0.05)) +
  theme(axis.text.y=element_text(angle=45),legend.title=element_blank())
grid.arrange(arrangeGrob(dp1 + theme(legend.position="none"),dp2 + theme(legend.position="none"),nrow=1),nrow=1,heights=9)
# ---------------------------

# FIGURE 8: No Evidence of Differences in Response Rates (Simulated) by Institution and Ideology
# estimates logitistic regressions with treatment conditions and moderating covariates, simulates predicted proability of response based on observed carvariate values, then plots the results
model=m1
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
insti=matrix(data=NA, ncol=7,nrow=4)
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM.e.l=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.65, D$cf)
d.FF.e.l=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.65, D$cf)
d.MF.e.l=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.65, D$cf)
d.FM.e.l=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.65, D$cf)

d.MM.e.c=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.35, D$cf)
d.FF.e.c=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.35, D$cf)
d.MF.e.c=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.35, D$cf)
d.FM.e.c=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, 1, 0, 0.35, D$cf)

d.MM.a.l=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.65, D$cf)
d.FF.a.l=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.65, D$cf)
d.MF.a.l=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.65, D$cf)
d.FM.a.l=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.65, D$cf)

d.MM.a.c=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.35, D$cf)
d.FF.a.c=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.35, D$cf)
d.MF.a.c=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.35, D$cf)
d.FM.a.c=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, 0, 0, 0.35, D$cf)

p.MM.e.l=apply((1/(1+exp(-as.matrix(d.MM.e.l)%*% t(betas)))),1,as.vector) 
p_MM.e.l=apply(p.MM.e.l,1,mean)
p.FF.e.l=apply((1/(1+exp(-as.matrix(d.FF.e.l)%*% t(betas)))),1,as.vector) 
p_FF.e.l=apply(p.FF.e.l,1,mean)
p.MF.e.l=apply((1/(1+exp(-as.matrix(d.MF.e.l)%*% t(betas)))),1,as.vector) 
p_MF.e.l=apply(p.MF.e.l,1,mean)
p.FM.e.l=apply((1/(1+exp(-as.matrix(d.FM.e.l)%*% t(betas)))),1,as.vector) 
p_FM.e.l=apply(p.FM.e.l,1,mean)

p.MM.e.c=apply((1/(1+exp(-as.matrix(d.MM.e.c)%*% t(betas)))),1,as.vector) 
p_MM.e.c=apply(p.MM.e.c,1,mean)
p.FF.e.c=apply((1/(1+exp(-as.matrix(d.FF.e.c)%*% t(betas)))),1,as.vector) 
p_FF.e.c=apply(p.FF.e.c,1,mean)
p.MF.e.c=apply((1/(1+exp(-as.matrix(d.MF.e.c)%*% t(betas)))),1,as.vector) 
p_MF.e.c=apply(p.MF.e.c,1,mean)
p.FM.e.c=apply((1/(1+exp(-as.matrix(d.FM.e.c)%*% t(betas)))),1,as.vector) 
p_FM.e.c=apply(p.FM.e.c,1,mean)

p.MM.a.l=apply((1/(1+exp(-as.matrix(d.MM.a.l)%*% t(betas)))),1,as.vector) 
p_MM.a.l=apply(p.MM.a.l,1,mean)
p.FF.a.l=apply((1/(1+exp(-as.matrix(d.FF.a.l)%*% t(betas)))),1,as.vector) 
p_FF.a.l=apply(p.FF.a.l,1,mean)
p.MF.a.l=apply((1/(1+exp(-as.matrix(d.MF.a.l)%*% t(betas)))),1,as.vector) 
p_MF.a.l=apply(p.MF.a.l,1,mean)
p.FM.a.l=apply((1/(1+exp(-as.matrix(d.FM.a.l)%*% t(betas)))),1,as.vector) 
p_FM.a.l=apply(p.FM.a.l,1,mean)

p.MM.a.c=apply((1/(1+exp(-as.matrix(d.MM.a.c)%*% t(betas)))),1,as.vector) 
p_MM.a.c=apply(p.MM.a.c,1,mean)
p.FF.a.c=apply((1/(1+exp(-as.matrix(d.FF.a.c)%*% t(betas)))),1,as.vector) 
p_FF.a.c=apply(p.FF.a.c,1,mean)
p.MF.a.c=apply((1/(1+exp(-as.matrix(d.MF.a.c)%*% t(betas)))),1,as.vector) 
p_MF.a.c=apply(p.MF.a.c,1,mean)
p.FM.a.c=apply((1/(1+exp(-as.matrix(d.FM.a.c)%*% t(betas)))),1,as.vector) 
p_FM.a.c=apply(p.FM.a.c,1,mean)

diffdiff.e.l=((p_MM.e.l+p_FF.e.l)/2)-((p_MF.e.l+p_FM.e.l)/2)
diffdiff.e.c=((p_MM.e.c+p_FF.e.c)/2)-((p_MF.e.c+p_FM.e.c)/2)
diffdiff.a.l=((p_MM.a.l+p_FF.a.l)/2)-((p_MF.a.l+p_FM.a.l)/2)
diffdiff.a.c=((p_MM.a.c+p_FF.a.c)/2)-((p_MF.a.c+p_FM.a.c)/2)

insti[1,1]<- quantile(diffdiff.e.l,.025)
insti[1,2]<- quantile(diffdiff.e.l,.05)
insti[1,3]<- mean(diffdiff.e.l)
insti[1,4]<- quantile(diffdiff.e.l,.95)
insti[1,5]<- quantile(diffdiff.e.l,.975)
insti[1,6]<- 'DV: Response Rate'
insti[1,7]<- 'Elected / Liberal'

insti[2,1]<- quantile(diffdiff.e.c,.025)
insti[2,2]<- quantile(diffdiff.e.c,.05)
insti[2,3]<- mean(diffdiff.e.c)
insti[2,4]<- quantile(diffdiff.e.c,.95)
insti[2,5]<- quantile(diffdiff.e.c,.975)
insti[2,6]<- 'DV: Response Rate'
insti[2,7]<- 'Elected / Conservative'

insti[3,1]<- quantile(diffdiff.a.l,.025)
insti[3,2]<- quantile(diffdiff.a.l,.05)
insti[3,3]<- mean(diffdiff.a.l)
insti[3,4]<- quantile(diffdiff.a.l,.95)
insti[3,5]<- quantile(diffdiff.a.l,.975)
insti[3,6]<- 'DV: Response Rate'
insti[3,7]<- 'Appointed / Liberal'

insti[4,1]<- quantile(diffdiff.a.c,.025)
insti[4,2]<- quantile(diffdiff.a.c,.05)
insti[4,3]<- mean(diffdiff.a.c)
insti[4,4]<- quantile(diffdiff.a.c,.95)
insti[4,5]<- quantile(diffdiff.a.c,.975)
insti[4,6]<- 'DV: Response Rate'
insti[4,7]<- 'Appointed / Conservative'

it=data.frame(insti)
names(it)=c('lb.95','lb.90','est','ub.90','ub.95','dv','sg')
it[,1]=as.numeric(as.character(it[,1]))
it[,2]=as.numeric(as.character(it[,2]))
it[,3]=as.numeric(as.character(it[,3]))
it[,4]=as.numeric(as.character(it[,4]))
it[,5]=as.numeric(as.character(it[,5]))

clrs <- c('#000000','#000000','#000000','#000000')
f8=ggplot(it, aes(colour = sg)) +
  geom_point(aes(x = sg, y = est, size= 2)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = sg, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = sg, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Response Rate Difference') +
  scale_colour_manual(values=clrs) + scale_y_continuous(limits=c(-0.05,0.05)) +
  theme(axis.text.y=element_text(angle=45),legend.title=element_blank(),legend.position='none')
# ---------------------------

# ---------------------------
# SUPPLEMENTARY INFORMATION
# ---------------------------

# FIGURE A1: Power Analysis
# Note: based on simulation code at http://egap.org/content/power-analysis-simulations-r
set.seed(6658)
N=4405 
powers=rep(NA, length(N)) 
alpha=0.05 
sims=1000 
taus=seq(from=0, to=0.15, by=.004)  
for (j in 1:length(taus)){
  tau=taus[j] 
  significant.experiments=rep(NA, sims) 
  for (i in 1:sims){
    model=rep(0.71, N)
    treat=rbinom(N, size=1, prob=.5) 
    model[treat==1]=model[treat==1]-tau
    Y.sim=rbinom(N,1,prob=model)
    fit.sim=lm(Y.sim ~ treat) 
    p.value=summary(fit.sim)$coefficients[2,4] 
    significant.experiments[i]=(p.value <= alpha) 
  }
  powers[j]=mean(significant.experiments)
}
d1=data.frame(taus=taus,powers=powers)

powers=rep(NA, length(N)) 
for (j in 1:length(taus)){
  tau=taus[j] 
  significant.experiments=rep(NA, sims)
  for (i in 1:sims){
    model=rep(0.71, N)
    history=c(rep(1, 1187),rep(0, 3218))
    treat=rbinom(N, size=1, prob=.5) 
    model[treat==1&history==1]=model[treat==1&history==1]-tau
    Y.sim=rbinom(N,1,prob=model)
    fit.sim=lm(Y.sim ~ treat*history) 
    p.value=wald.test(Sigma=vcov(fit.sim),b=coef(fit.sim),Terms=c(2,4))$result$chi2[3]
    significant.experiments[i]=(p.value <= alpha) 
  }
  powers[j]=mean(significant.experiments)  
}
d2=data.frame(taus=taus,powers=powers)

powers=rep(NA, length(N)) 
for (j in 1:length(taus)){
  tau=taus[j] 
  significant.experiments=rep(NA, sims) 
  for (i in 1:sims){
    model=rep(0.71, N)
    elected=c(rep(0, 1224),rep(1, 3181))
    rightwing=c(rep(1, 399),rep(0, 825),rep(1,1808),rep(0,1373))
    treat=rbinom(N, size=1, prob=.5) 
    model[treat==1&elected==1&rightwing==1]=model[treat==1&elected==1&rightwing==1]-tau
    Y.sim=rbinom(N,1,prob=model)
    fit.sim=lm(Y.sim ~ treat*elected*rightwing) 
    p.value=wald.test(Sigma=vcov(fit.sim),b=coef(fit.sim),Terms=c(2,8))$result$chi2[3]
    significant.experiments[i]=(p.value <= alpha) 
  }
  powers[j]=mean(significant.experiments)  
}
d3=data.frame(taus=taus,powers=powers)

a1=ggplot() + geom_line(data=d1,aes(x=taus, y=powers),size=1.4) + 
  geom_line(data=d2,aes(x=taus, y=powers,color='orange'),size=1.4) + 
  geom_line(data=d3,aes(x=taus, y=powers,color='red'),size=1.4)+
  annotate("text", x = .015, y = .95, label = "Discrimination")+
  annotate("text", x = .08, y = .5, label = "Legacy")+
  annotate("text", x = .125, y = .25, label = "Institutions")+
  geom_point()+ xlab('Effect Size') + ylab('Power')+
  theme_minimal() + theme(legend.title=element_blank(),legend.position='none')
# ---------------------------

# FIGURE A2: Missing Jurisdictions
# plots missing jurisdictions 
load('missing-map-1.Rda')
load('missing-map-2.Rda')
load('in-sample.Rda')

city.level=c('CT','ME','MA','NH','NJ','NY','RI','VT')
ct=pop[!(pop$stateabbrev%in%city.level),]
ct$CTFIPS=as.character(ct$combined.fips1)
for (i in 1:nrow(ct)) {if (nchar(ct$CTFIPS[i])==4) {ct$CTFIPS[i]=paste('0',ct$CTFIPS[i],sep='')}}
ct$in.sample[ct$in.sample==1]='Yes'
ct$in.sample[ct$in.sample==0]='No'

ci=pop[pop$stateabbrev%in%city.level,]
ci=summaryBy(in.sample~stateabbrev,data=ci,FUN=mean,keep.names=T)

mapa1=ggplot() + geom_map(data = ct, aes(map_id = CTFIPS, fill = factor(in.sample)),map = us1) +
  geom_polygon(data=us1, aes(x=long, y=lat, group = group), size = 0.25, colour='white', fill=NA) +
  expand_limits(x = us1$long, y = us1$lat) + 
  theme_map() + theme(legend.position = 'bottom') +
  scale_fill_manual(name = 'In Sample:',values=c('#00274c','#E8E8E8'))
mapa2=ggplot() + geom_map(data = ci, aes(map_id = stateabbrev, fill = in.sample),map = us2) +
  geom_polygon(data=us2, aes(x=long, y=lat, group = group), size = 0.25, colour='white', fill=NA) +
  expand_limits(x = us2$long, y = us2$lat) + 
  theme_map() + theme(legend.position = 'bottom') +
  scale_fill_gradient(name = '% Jurisdictions in Sample:',low='#00274c',high='#E8E8E8')
grid.arrange(arrangeGrob(mapa1,mapa2),ncol=1)
# ---------------------------

# FIGURE A3: Simulated Differences Robust to Inverse Probability Weighting
# reproduces main results using inverse probability weighting
m9=glm(responded ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, family="binomial", data=dt, weights=inverse.prob)
m10=glm(congrats ~ as.factor(treatment) + ssm_history + type + population + appointed + obamavote + cf, data=dt, family="binomial", weights=inverse.prob)

ddm=matrix(data=NA, ncol=6,nrow=2)

model=m9
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)

diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
ddm[1,1]<- quantile(diffdiff,.025)
ddm[1,2]<- quantile(diffdiff,.05)
ddm[1,3]<- mean(diffdiff)
ddm[1,4]<- quantile(diffdiff,.95)
ddm[1,5]<- quantile(diffdiff,.975)
ddm[1,6]<- 'DV: Responded'

model=m10
set.seed(6658)
N=1000
s=sim(model,N)
f=slot(s,'coef')
betas=f
D=dt
D=cbind(D,model.matrix(~type-1,data=D))
D=cbind(D,model.matrix(~appointed-1,data=D))
d.MM=cbind(1, 0, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FF=cbind(1, 1, 0, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.MF=cbind(1, 0, 1, 0, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
d.FM=cbind(1, 0, 0, 1, D$ssm_history, D$typeCOUNTY, D$population, D$appointedElected, D$appointedUnknown, D$obamavote, D$cf)
p.MM=apply((1/(1+exp(-as.matrix(d.MM)%*% t(betas)))),1,as.vector) 
p_MM=apply(p.MM,1,mean)
p.FF=apply((1/(1+exp(-as.matrix(d.FF)%*% t(betas)))),1,as.vector) 
p_FF=apply(p.FF,1,mean)
p.MF=apply((1/(1+exp(-as.matrix(d.MF)%*% t(betas)))),1,as.vector) 
p_MF=apply(p.MF,1,mean)
p.FM=apply((1/(1+exp(-as.matrix(d.FM)%*% t(betas)))),1,as.vector) 
p_FM=apply(p.FM,1,mean)

diffdiff=((p_MM+p_FF)/2)-((p_MF+p_FM)/2)
ddm[2,1]<- quantile(diffdiff,.025)
ddm[2,2]<- quantile(diffdiff,.05)
ddm[2,3]<- mean(diffdiff)
ddm[2,4]<- quantile(diffdiff,.95)
ddm[2,5]<- quantile(diffdiff,.975)
ddm[2,6]<- 'DV: Congratulations'

ddm=data.frame(ddm)
names(ddm)=c('lb.95','lb.90','est','ub.90','ub.95','dv')
ddm[,1]=as.numeric(as.character(ddm[,1]))
ddm[,2]=as.numeric(as.character(ddm[,2]))
ddm[,3]=as.numeric(as.character(ddm[,3]))
ddm[,4]=as.numeric(as.character(ddm[,4]))
ddm[,5]=as.numeric(as.character(ddm[,5]))

clrs <- c('#000000','#000000')
a3=ggplot(ddm, aes(colour = dv)) +
  geom_point(aes(x = dv, y = est, size= 2)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = dv, ymin = lb.90,ymax = ub.90),lwd = 1.5, position = position_dodge(width = 1/2)) +
  geom_linerange(aes(x = dv, ymin = lb.95,ymax = ub.95),lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + xlab('') + ylab('Difference-in-Difference (Weighted)') +
  scale_colour_manual(values=clrs) + scale_y_continuous(limits=c(-0.05,0.05)) +
  theme(axis.text.y=element_text(angle=45),legend.title=element_blank(),legend.position='none')
# ---------------------------

# TABLE A1: Imbalance Across Treatment Conditions
# reports balance across treatment conditions based on mean values of observables
imbal=data.frame()
imbal[1,1]='Population'
imbal[1,2]=round(mean(dt$population[dt$treatment==1]),digits=3)
imbal[1,3]=round(mean(dt$population[dt$treatment==2]),digits=3)
imbal[1,4]=round(mean(dt$population[dt$treatment==3]),digits=3)
imbal[1,5]=round(mean(dt$population[dt$treatment==4]),digits=3)
imbal[2,1]='Democratic Voteshare'
imbal[2,2]=round(mean(dt$obamavote[dt$treatment==1]),digits=3)
imbal[2,3]=round(mean(dt$obamavote[dt$treatment==2]),digits=3)
imbal[2,4]=round(mean(dt$obamavote[dt$treatment==3]),digits=3)
imbal[2,5]=round(mean(dt$obamavote[dt$treatment==4]),digits=3)
imbal[3,1]='Appointed'
imbal[3,2]=nrow(dt[dt$appointed=='Appointed'&dt$treatment==1,])
imbal[3,3]=nrow(dt[dt$appointed=='Appointed'&dt$treatment==1,])
imbal[3,4]=nrow(dt[dt$appointed=='Appointed'&dt$treatment==1,])
imbal[3,5]=nrow(dt[dt$appointed=='Appointed'&dt$treatment==1,])
imbal[4,1]='Contact Forms'
imbal[4,2]=nrow(dt[dt$cf==1&dt$treatment==1,])
imbal[4,3]=nrow(dt[dt$cf==1&dt$treatment==2,])
imbal[4,4]=nrow(dt[dt$cf==1&dt$treatment==3,])
imbal[4,5]=nrow(dt[dt$cf==1&dt$treatment==4,])
imbal[5,1]='Errors'
imbal[5,2]=81
imbal[5,3]=117
imbal[5,4]=122
imbal[5,5]=97
imbal[6,1]='Bounces'
imbal[6,2]=118
imbal[6,3]=150
imbal[6,4]=158
imbal[6,5]=143
imbal[7,1]='N'
imbal[7,2]=nrow(dt[dt$treatment==1,])
imbal[7,3]=nrow(dt[dt$treatment==2,])
imbal[7,4]=nrow(dt[dt$treatment==3,])
imbal[7,5]=nrow(dt[dt$treatment==4,])
names(imbal)=c(' ','Male, Male','Female, Female','Male, Female','Female, Male')

print.xtable(xtable(imbal,caption='Imbalance Across Treatment Conditions',label='tab:balance',align='llcccc',digits=2),include.rownames=F,table.placement='H',caption.placement='top')
# ---------------------------

# TABLE B2: Interrater Reliability for Response Quality
# TABLE B3: Interrater Reliability for Response Quality (Non-response Removed)
# reports interrate reliability statistics for response quality hand-coding
load('coder-1.Rda')
load('coder-2.Rda')

ir=data.frame()
ir[1,1]="% Disagree"
ir[1,2]=table(c1$q.cost!=c2$q.cost)[2]/table(c1$q.cost!=c2$q.cost)[1]
ir[1,3]=table(c1$q.valid!=c2$q.valid)[2]/table(c1$q.valid!=c2$q.valid)[1]
ir[1,4]=table(c1$q.when!=c2$q.when)[2]/table(c1$q.when!=c2$q.when)[1]
ir[1,5]=table(c1$felicitations!=c2$felicitations)[2]/table(c1$felicitations!=c2$felicitations)[1]

ir[2,1]="No. Disagree"
ir[2,2]=table(c1$q.cost!=c2$q.cost)[2]
ir[2,3]=table(c1$q.valid!=c2$q.valid)[2]
ir[2,4]=table(c1$q.when!=c2$q.when)[2]
ir[2,5]=table(c1$felicitations!=c2$felicitations)[2]

ir[3,1]="Cohen's Kappa"
ir[3,2]=kappa2(cbind(c1$q.cost,c2$q.cost))[5]
ir[3,3]=kappa2(cbind(c1$q.valid,c2$q.valid))[5]
ir[3,4]=kappa2(cbind(c1$q.when,c2$q.when))[5]
ir[3,5]=kappa2(cbind(c1$felicitations,c2$felicitations))[5]
names(ir)=c('','Cost?','Valid?','When?','Felicitations')

drop=c1$uid2[(c1$q.cost==0&c1$q.valid==0&c1$q.when==0&c1$felicitations==0)&
               (c2$q.cost==0&c2$q.valid==0&c2$q.when==0&c2$felicitations==0)]
c1=c1[!c1$uid2%in%drop,]
c2=c2[!c2$uid2%in%drop,]

ir2=data.frame()
ir2[1,1]="% Disagree"
ir2[1,2]=table(c1$q.cost!=c2$q.cost)[2]/table(c1$q.cost!=c2$q.cost)[1]
ir2[1,3]=table(c1$q.valid!=c2$q.valid)[2]/table(c1$q.valid!=c2$q.valid)[1]
ir2[1,4]=table(c1$q.when!=c2$q.when)[2]/table(c1$q.when!=c2$q.when)[1]
ir2[1,5]=table(c1$felicitations!=c2$felicitations)[2]/table(c1$felicitations!=c2$felicitations)[1]

ir2[2,1]="No. Disagree"
ir2[2,2]=table(c1$q.cost!=c2$q.cost)[2]
ir2[2,3]=table(c1$q.valid!=c2$q.valid)[2]
ir2[2,4]=table(c1$q.when!=c2$q.when)[2]
ir2[2,5]=table(c1$felicitations!=c2$felicitations)[2]

ir2[3,1]="Cohen's Kappa"
ir2[3,2]=kappa2(cbind(c1$q.cost,c2$q.cost))[5]
ir2[3,3]=kappa2(cbind(c1$q.valid,c2$q.valid))[5]
ir2[3,4]=kappa2(cbind(c1$q.when,c2$q.when))[5]
ir2[3,5]=kappa2(cbind(c1$felicitations,c2$felicitations))[5]
names(ir2)=c('','Cost?','Valid?','When?','Felicitations')

print.xtable(xtable(ir,caption='Interrater Reliability for Response Quality',label='tab:irr',align='llcccc',digits=2),include.rownames=F,table.placement='H',caption.placement='top')

print.xtable(xtable(ir2,caption='Interrater Reliability for Response Quality (Non-Response Removed)',label='tab:irr2',align='llcccc',digits=2),include.rownames=F,table.placement='H',caption.placement='top')
# ---------------------------

# TABLE C4: Estimation Results
# reports full regression results 
stargazer(m1,m2,m3,m4,m5,m6,m7,m8,title='Full Estimation Results for Figures 3, 4, 5, 7, and 8',column.labels=c('DV: Response','DV: Congrats','DV: Response','DV: Congrats','DV: Cost','DV: Valid','DV: When','DV: Felicit.'),dep.var.labels.include=F,covariate.labels=c('Female-Female','Male-Female','Female-Male','No SSM Ban','No Sodomy Ban','County-level','Population','Elected','Unk. Selection','Obama Vote','Contact Form'),column.sep.width='0pt',font.size='scriptsize',digits=2,star.cutoffs=NA,omit.table.layout = "n")
# ---------------------------