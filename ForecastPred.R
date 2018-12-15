#2016 Data
base.2016 = base[which(base$year==2016),]
attach(base.2016)
base.2016 = data.frame(country,count,year,log_pop=log(pop),log_gdp_per_cap = log(gdp/pop),host,comm_soviet)


#Linear Fitted
fitted.lm = -9.63058+0.40830*base.2016$log_gdp_per_cap+0.44275*base.2016$log_pop+0.86726*base.2016$host+1.03281*base.2016$comm_soviet
#olympic.lmfinal  
  
  


#Prediction intervals: Linear Model
nrow(base.2016)-5
t=qt(1-0.05/2,76)
mse.lm = 0.751
X = as.matrix(cbind(rep(1,nrow(Olympic_v2)),Olympic_v2$log_gdp_per_cap,Olympic_v2$log_pop,Olympic_v2$host,Olympic_v2$comm_soviet))

spred = c()
for (i in 1:nrow(base.2016)){
  xh = as.matrix(cbind(1,base.2016$log_gdp_per_cap[i],base.2016$log_pop[i],base.2016$host[i],base.2016$comm_soviet[i]))
  s2 = mse.lm*(1+xh%*%solve(t(X)%*%X)%*%t(xh))
  spred = c(spred,sqrt(s2))
}

lower.pred = fitted.lm - qt(1-0.05/2,76)*spred
upper.pred = fitted.lm + qt(1-0.05/2,76)*spred

forecast.lm = cbind(log(base.2016$count),lower.pred,upper.pred)
ininterval.lm = cbind(forecast.lm[,2]<=forecast.lm[,1] & forecast.lm[,3]>=forecast.lm[,1])
length(which(ininterval.lm))

#NB Method 2 (works!)
library(tidyverse)
library(ciTools)
library(MASS) 

set.seed(20181215)

base.2016 = base[which(base$year==2016),]
attach(base.2016)
base.2016 = data.frame(country,count,year,log_pop=log(pop),log_gdp_per_cap = log(gdp/pop),host,comm_soviet)

newData <- data.frame(base.2016[,c(2,4,5,6,7)])
train_data <- Olympic_v2[,3:7]

#Generate prediction intervals
olympic.nb = glm.nb(count ~ log_pop + log_gdp_per_cap + host + comm_soviet,data=train_data)

#add_pi comes from the library ciTools
olympic.nb_pint <- add_pi(tb=newData,fit=olympic.nb, names=c("lpb", "upb"), alpha=0.1, nSims=20000)


olympic.nb_pint %>%
  mutate(inside=(pred >lpb & pred<upb))-> preds.nb

summary(preds.nb)


















