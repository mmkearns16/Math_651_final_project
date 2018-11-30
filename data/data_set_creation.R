library(dplyr)
summer             <-read.csv('summer.csv', stringsAsFactors = F)
colnames(summer)   <-sapply(colnames(summer), tolower)
summer<-summer[which(summer$year>1990),]
#some athletes don't have a country
summer<-summer[which(summer$country!=''),]


################################################# COUNTRIES

file<-"C:/Users/Max/Documents/Georgetown/Math651_Regression/Math_651_final_project/raw_data/dictionary.csv"
countries<-read.csv(file, stringsAsFactors = F)[,c(1,2)]
colnames(countries)   <-sapply(colnames(countries), tolower)
colnames(countries)<-c('name', 'country')

#change a couple weird formats
countries[which(countries$name == 'Korea, North'),1]<-'North Korea'
countries[which(countries$name == 'Korea, South'),1]<-'South Korea'
countries[which(countries$name == 'Hong Kong*'),1]  <-'Hong Kong'
countries[which(countries$name == 'Puerto Rico*'),1]  <-'Puerto Rico'



extra_ioc_codes<-c('AHO',	'Netherlands Antilles',	
'ANZ',	 'Australasia',	
'BOH',	 'Bohemia',
'BWI',	 'British West Indies',	
'EUA',   'Germany United Team of Germany',	
'EUN',	 'International Olympic Committee Unified Team',	
'FRG',	 'West Germany',
'GDR',	 'East Germany',
'SCG',	 'Serbia and Montenegro',	
'TCH',	 'Czechoslovakia',	
'URS',	 'Soviet Union',
'YUG',	 'Yugoslavia',	
'ZZX',	 'Mixed teams',
'ROU',   'Romania', 
'ALG',   'Algeria', 
'IOP',   'Independent Olympic Participants',
'SRB',   'Serbia', 
'MNE',   'Montenegro',
'SGP',   'Singapore', 
'TTO',   'Trinidad and Tobago')

extra_ioc_codes<-data.frame(matrix(extra_ioc_codes, ncol = 2, byrow = T))[,c(2,1)]
colnames(extra_ioc_codes)<-c('name', 'country')

countries<-rbind(countries, extra_ioc_codes)


#stuff to take out later
bad_stuff<-c('Independent Olympic Participants', 'International Olympic Committee Unified Team')

######################################################## CREATING THE GENDER DATA SET
gender<-select(summer, year, country, gender)

gender %>%
  group_by(year, country, gender) %>%
  summarize(count = n()) -> gender

gender<-left_join(gender, countries)

gender<-gender[,c(1,3,4,5)]

colnames(gender)[4]<-'country'

head(gender)

gender<-gender[!(gender$country  %in% bad_stuff),]
#########################################################   CREATING THE SPORT DATA SET
sport<-select(summer, year, country, sport)

sport %>%
  group_by(year, country, sport) %>%
  summarize(count = n()) -> sport

sport<-left_join(sport, countries)

sport<-sport[,c(1,3,4,5)]

colnames(sport)[4]<-'country'

sport<-sport[!(sport$country  %in% bad_stuff),]
########################################################## CREATING THE REGULAR DATA SET (only by country)
reg<-select(summer, year, country)

reg %>%
  group_by(year, country) %>%
  summarize(count = n()) -> reg

reg<-left_join(reg, countries)

reg<-reg[,c(1,3,4)]

colnames(reg)[3]<-'country'

reg<-reg[!(reg$country  %in% bad_stuff),]


###################################################write to data file


setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")
write.csv(reg, 'base_data.csv')
write.csv(gender, 'gender_data.csv')
write.csv(sport, 'sport_data.csv')








