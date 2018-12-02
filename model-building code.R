#Final project model building

setwd('C:/Users/mpeng/Documents/Math_651_final_project/data')

base = read.csv("base_data.csv")
head(base)

base.total = base[which(base$year!=2016),]
Olympic = base.total[,c(2,3,4,5,6,7,8)]

#Clean the data (Mary to add)
head(Olympic)
attach(Olympic)

#Make new dataframe with GDP / capita
Olympic_v2 <- data.frame(year, country, count, log_pop = log(pop), log_gdp_pcap = log(gdp/pop), host, comm_soviet)
head(Olympic_v2)

attach(Olympic_v2)

#paired scatterplot

#Linear regression

#Handle multicollinearity - check VIF

#No interaction
#olympic.lm <- lm(count ~ gdp + pop + host + comm_soviet)
#summary(olympic.lm)
#vif(olympic.lm)

#Add interaction - doesn't make sense from interpretation
#olympic.lm2 <- lm(count ~ scale(gdp) + scale(pop) + scale(gdp) * scale(pop) + host + comm_soviet)
#summary(olympic.lm2)

#Replace interaction term with GDP / capita - insignificant terms
#olympic.lm3 <- lm(count ~ scale(gdp) + scale(pop) + scale(exp(gdp) / exp(pop)) + host + comm_soviet)
#summary(olympic.lm3)

#Cp
olympic.lm2_leap <- leaps(y=count, x=Olympic_v2[,4:7])
olympic.lm2_r2 <- leaps(y=count, x=Olympic_v2[,4:7], method = 'adjr2')

head(Olympic_v2)
xList <- names(Olympic_v2)[4:7]

#Remove the last row that has all False's
vec <- olympic.lm2_leap$which

#Name the columns in the grid
names(vec) <- paste("X", 1:4, sep="")

#Build matrix of formula for every row
allModelsList <- apply(vec, 1, function(x) as.formula(
  paste(c("count ~ 1", xList[x]), collapse = "+")))

#Calculate the coefficients for all 16 models
allModelsResults <- lapply(allModelsList, 
                           function(x) lm(x, data=Olympic_v2))

#PRESS
olympic.lm = PRESS(lm(count~log_gdp+log_pop+host+comm_soviet))
olympic.lmX1 = PRESS(lm(count~log_gdp))
olympic.lmX2 = PRESS(lm(count~log_pop))
olympic.lmX3 = PRESS(lm(count~host))
olympic.lmX4 = PRESS(lm(count~comm_soviet))
olympic.lmX1X2 = PRESS(lm(count~log_gdp+log_pop))
olympic.lmX1X3 = PRESS(lm(count~log_gdp+host))
olympic.lmX1X4 = PRESS(lm(count~log_gdp+comm_soviet))
olympic.lmX2X3 = PRESS(lm(count~log_pop+host))
olympic.lmX2X4 = PRESS(lm(count~log_pop+comm_soviet))
olympic.lmX3X4 = PRESS(lm(count~host+comm_soviet))
olympic.lmX1X2X3 = PRESS(lm(count~log_gdp+log_pop+host))
olympic.lmX1X2X4 = PRESS(lm(count~log_gdp+log_pop+comm_soviet))
olympic.lmX2X3X4 = PRESS(lm(count~log_pop+host+comm_soviet))
olympic.lmX1X3X4 = PRESS(lm(count~log_gdp+host+comm_soviet))

olympic.lm_press <- rbind(olympic.lmX1$stat,
                          olympic.lmX3$stat,
                          olympic.lmX2$stat,
                          olympic.lmX4$stat,
                          olympic.lmX1X3$stat,
                          olympic.lmX1X2$stat,
                          olympic.lmX1X4$stat,
                          olympic.lmX2X3$stat,
                          olympic.lmX3X4$stat,
                          olympic.lmX2X4$stat,
                          olympic.lmX1X2X3$stat,
                          olympic.lmX1X2X4$stat,
                          olympic.lmX1X3X4$stat,
                          olympic.lmX2X3X4$stat,
                          olympic.lm$stat)

#Summary
cbind(olympic.lm2_leap$which, Cp=round(olympic.lm2_leap$Cp,2), aR2=round(olympic.lm2_r2$adjr2,2),
      AIC=matrix(unlist(lapply(allModelsResults, function(x) round(extractAIC(x),2))), ncol=2, byrow=TRUE)[,2],
      PRESS = olympic.lm_press)


#Choose the linear model with all 4 predictor variables
olympic.lmfinal <- lm(count ~ log_pop + log_gdp_pcap + host + comm_soviet)
summary(olympic.lmfinal)

#Residual analysis
plot(olympic.lmfinal)

#Diagnostics
vif(olympic.lmfinal)

#Influential cases
olympic.lm_inf=influence.measures(olympic.lm2)$is.inf
idx=which(apply(olympic.lm_inf,1,any))
olympic.lm_inf[idx,]

install.packages('dplyr')
library('dplyr')

Olympic_v2[idx,]
base[idx,c(2,4)]


#Transform Y using sqrt
olympic.lm3 <- lm(sqrt(count) ~ log_gdp + log_pop + host + comm_soviet)
plot(olympic.lm3)

#Residual analysis
library(car)

dev.new()
par(mfrow=c(2,2))
plot(olympic.lm4)

################POISSON####################3
#POISSON
Olympic.pois = glm(count~gdp+pop+comm_soviet+host,family = poisson)
Olympic.pois2 = glm(count~pop + log(exp(gdp) / exp(pop)) + host + comm_soviet,family = poisson)

plot(Olympic.pois)
plot(Olympic.pois2)

#Check for over-dispersion

#AIC, R-adjusted, VIF, PRESS, C-p statistic
library('qpcR')
library('MuMIn')
library('leaps')

model_comparison <- list(olympic.lm, olympic.lm4, Olympic.pois, Olympic.pois2)
lapply(model_comparison, function(x) extractAIC(x))
lapply(model_comparison, function(x) PRESS(x)$stat)

ps.leaps <- leaps(Olympic_v2$count, x=Olympic_v2[,3:7], method="Cp")
ps.leapsfull <- cbind(ps.leaps$which, ps.leaps$Cp)


#Outliers


