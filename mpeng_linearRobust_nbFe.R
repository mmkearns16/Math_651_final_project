############Linear, robust###############

#Two different forms of robustness
olympic.lm_huber <- rlm(log(count) ~ log_pop + log_gdp_pcap + host + comm_soviet)
olympic.lm_bi <- rlm(log(count) ~ log_pop + log_gdp_pcap + host + comm_soviet, psi=psi.bisquare)

summary(olympic.lm_huber)
summary(olympic.lm_bi)

dev.new()
par(mfrow=c(2,2))

#Residual plot analysis
plot(olympic.lm_huber)
plot(olympic.lm_bi)

#Can include a note on comparing the weights of Huber vs. Bisquare
data.frame(Olympic_v2$year, Olympic_v2$country, olympic.lm_huber$w, olympic.lm_bi$w)

#Since there's more variability in the weight for Bisquare, include only the coefficients for Bisquare 

###########Negative binomial, with year fixed effect###############
olympic.nb_fe <- glm.nb(count ~ log_pop + log_gdp_pcap + host + comm_soviet + factor(year))
summary(olympic.nb_fe)

#Residual plot analysis
plot(olympic.nb_fe)

###########AIC and MSE###############
AIC(olympic.lm_huber)
AIC(olympic.lm_bi)
AIC(olympic.lmfinal)
AIC(olympic.pois)
AIC(olympic.nb)

###############Predicted vs.Actual###############
par(mfrow=c(1,2))

#Comparison of Predicted vs. Actual
plot(x=Olympic_v2$count, y = exp(olympic.lmfinal$fitted.values), col='blue', lwd=2, xlab='Actual',
     ylab='Predicted', main='OLS Linear Regression')
abline(a=0,b=1, col='red')

plot(x=Olympic_v2$count, y = olympic.nb$fitted.values, col='blue', lwd=2, xlab='Actual',
     ylab='Predicted', main='Negative Binomial Regression')
abline(a=0,b=1, col='red')

