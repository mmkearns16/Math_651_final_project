
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

#paired scatterplot (add from Hillary's code)

#Linear regression
library('qpcR')
library('MuMIn')
library('leaps')


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
olympic.lm = PRESS(lm(count~log_gdp_pcap+log_pop+host+comm_soviet))
olympic.lmX1 = PRESS(lm(count~log_gdp_pcap))
olympic.lmX2 = PRESS(lm(count~log_pop))
olympic.lmX3 = PRESS(lm(count~host))
olympic.lmX4 = PRESS(lm(count~comm_soviet))
olympic.lmX1X2 = PRESS(lm(count~log_gdp_pcap+log_pop))
olympic.lmX1X3 = PRESS(lm(count~log_gdp_pcap+host))
olympic.lmX1X4 = PRESS(lm(count~log_gdp_pcap+comm_soviet))
olympic.lmX2X3 = PRESS(lm(count~log_pop+host))
olympic.lmX2X4 = PRESS(lm(count~log_pop+comm_soviet))
olympic.lmX3X4 = PRESS(lm(count~host+comm_soviet))
olympic.lmX1X2X3 = PRESS(lm(count~log_gdp_pcap+log_pop+host))
olympic.lmX1X2X4 = PRESS(lm(count~log_gdp_pcap+log_pop+comm_soviet))
olympic.lmX2X3X4 = PRESS(lm(count~log_pop+host+comm_soviet))
olympic.lmX1X3X4 = PRESS(lm(count~log_gdp_pcap+host+comm_soviet))

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
      Press = round(olympic.lm_press,2))


#Choose the linear model with all 4 predictor variables
olympic.lmfinal <- glm(log(count) ~ log_pop + log_gdp_pcap + host + comm_soviet, family=gaussian)
summary(olympic.lmfinal)

#Residual analysis
dev.new()
par(mfrow=c(2,2))
plot(olympic.lmfinal)
plot(x=exp(olympic.lmfinal$fitted.values), y=olympic.lmfinal$residuals, xlim=c(0,3))

#Diagnostics

library(car)

dev.new()
par(mfrow=c(2,2))
plot(olympic.lm4)

################POISSON####################3
#POISSON
olympic.pois = glm(count ~ log_pop + log_gdp_pcap + host + comm_soviet,family = poisson)
summary(olympic.pois)
dev.new()
par(mfrow=c(2,2))
plot(olympic.pois)

#Check for over-dispersion
P__disp(olympic.pois) #Dispersion > 1

#####################################negative binomial################
olympic.nb <- glm.nb(count ~ log_pop + log_gdp_pcap + host + comm_soviet)
summary(olympic.nb)
P__disp(olympic.nb)

dev.new()
par(mfrow=c(2,2))
plot(olympic.nb)

#####################################negative binomial with year dummy################
olympic.nb_fe <- glm.nb(count ~ log_pop + log_gdp_pcap + host + comm_soviet + factor(year))
summary(olympic.nb_fe)
plot(olympic.nb_fe)

#####################################negative binomial with interaction################
olympic.nb_int <- glm.nb(count ~ log_pop + log_gdp_pcap + host + comm_soviet + log_gdp_pcap * host)
summary(olympic.nb_int)
plot(olympic.nb_int)

#####################################negative binomial with interaction 2################
olympic.nb_int2 <- glm.nb(count ~ log_pop + log_gdp_pcap + host + comm_soviet + log_gdp_pcap * comm_soviet)
summary(olympic.nb_int2)
plot(olympic.nb_int2)

#####################################Residual analysis on individual years################
olympic.nb_resid_year2008 <- cbind(year, residuals = olympic.nb$residuals, fitted=olympic.nb$fitted)[which(year==2008),]
summary(olympic.nb_int)

class(olympic.nb_resid_year2008)
plot(x=olympic.nb_resid_year2008[,3], y=olympic.nb_resid_year2008[,2])


#####################################Model Summary####################################
modelList <- list(olympic.lmfinal, olympic.pois, olympic.nb)
aic <- unlist(lapply(modelList, function(x) x$aic))
sse <- unlist(lapply(modelList, function(x) anova(x)[5,4]))

modelCompare <- rbind(aic, sse)
colnames(modelCompare) <- c('Linear', 'Poisson', 'Negative Binomial')
rownames(modelCompare) <- rbind('aic', 'sse')
