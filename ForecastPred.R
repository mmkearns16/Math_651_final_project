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

#Prediction Intervals: NB
#NB
fitted.nb = exp(-10.17)*exp(0.402*base.2016$log_gdp_per_cap)*exp(0.498*base.2016$log_pop)*exp(0.693*base.2016$host)*exp(1.03*base.2016$comm_soviet) 
attach(base.total)
nb.df = data.frame(count,log_pop = log(pop),log_gdp_per_cap = log(gdp_per_cap),host,comm_soviet)
olympic.nb<- glm.nb(count ~ log_pop + log_gdp_per_cap + host + comm_soviet,data = nb.df)
mse.nb = 386.80/382

spred.nb = c()
for (i in 1:nrow(base.2016)){
  xh = as.matrix(cbind(1,nb.df$log_gdp_per_cap[i],nb.df$log_pop[i],nb.df$host[i],nb.df$comm_soviet[i]))
  s2 = mse.nb*(1+xh%*%solve(t(X)%*%X)%*%t(xh))
  spred.nb = c(spred.nb,sqrt(s2))
}

lower.pred.nb = fitted.nb - qt(1-0.05/2,76)*spred.nb
upper.pred.nb = fitted.nb + qt(1-0.05/2,76)*spred.nb

forecast.nb = cbind(base.2016$count,lower.pred.nb,upper.pred.nb)

ininterval = cbind(forecast.nb[,2]<=forecast.nb[,1] & forecast.nb[,3]>=forecast.nb[,1])
length(which(ininterval))
