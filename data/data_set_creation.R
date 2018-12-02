library(dplyr)
setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/Math_651_final_project/data")
summer             <-read.csv('better_summer.csv', stringsAsFactors = F)
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
#write.csv(reg, 'base_data.csv')
#write.csv(gender, 'gender_data.csv')
#write.csv(sport, 'sport_data.csv')








summer[which(summer$country == 'Russian Federation'),1]<-'Russia'
summer[which(summer$country == 'Great Britain'),1]<-'United Kingdom'












################################################################# GDP



setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/raw_data")

gdp<-read.csv('GDP.csv', stringsAsFactors = F)
gdp<-gdp[,c(1, seq(37, 61, 4))]

gdp_1992<-gdp[,c(1,2)]
gdp_1996<-gdp[,c(1,3)]
gdp_2000<-gdp[,c(1,4)]
gdp_2004<-gdp[,c(1,5)]
gdp_2008<-gdp[,c(1,6)]
gdp_2012<-gdp[,c(1,7)]
gdp_2016<-gdp[,c(1,8)]

gdp_1992$year<-1992
gdp_1996$year<-1996
gdp_2000$year<-2000
gdp_2004$year<-2004
gdp_2008$year<-2008
gdp_2012$year<-2012
gdp_2016$year<-2016


colnames(gdp_1992)<-c('country', 'gdp', 'year')
colnames(gdp_1996)<-c('country', 'gdp', 'year')
colnames(gdp_2000)<-c('country', 'gdp', 'year')
colnames(gdp_2004)<-c('country', 'gdp', 'year')
colnames(gdp_2008)<-c('country', 'gdp', 'year')
colnames(gdp_2012)<-c('country', 'gdp', 'year')
colnames(gdp_2016)<-c('country', 'gdp', 'year')

gdp<-rbind(gdp_1992, gdp_1996, gdp_2000, gdp_2004, gdp_2008, gdp_2012, gdp_2016)

# fix north korea 
gdp[which(gdp$country=='Korea, Dem. Peopleâ€™s Rep.'),1]<-'North Korea'

#######################################################################################  Population
pop<-read.csv('Population.csv', sep = '\t', stringsAsFactors = F)

colnames(pop)[57]

pop<-pop[,c(1, seq(37, 61, 4))]


pop_1992<-pop[,c(1,2)]
pop_1996<-pop[,c(1,3)]
pop_2000<-pop[,c(1,4)]
pop_2004<-pop[,c(1,5)]
pop_2008<-pop[,c(1,6)]
pop_2012<-pop[,c(1,7)]
pop_2016<-pop[,c(1,8)]

pop_1992$year<-1992
pop_1996$year<-1996
pop_2000$year<-2000
pop_2004$year<-2004
pop_2008$year<-2008
pop_2012$year<-2012
pop_2016$year<-2016

colnames(pop_1992)<-c('country', 'pop', 'year')
colnames(pop_1996)<-c('country', 'pop', 'year')
colnames(pop_2000)<-c('country', 'pop', 'year')
colnames(pop_2004)<-c('country', 'pop', 'year')
colnames(pop_2008)<-c('country', 'pop', 'year')
colnames(pop_2012)<-c('country', 'pop', 'year')
colnames(pop_2016)<-c('country', 'pop', 'year')


pop<-rbind(pop_1992, pop_1996, pop_2000, pop_2004, pop_2008, pop_2012, pop_2016)

pop[which(pop$country=='Korea, Dem. People’s Rep.'),1]<-'North Korea'


############################################################################ JOINS


setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")

pop$country<-as.character(pop$country)
gdp$country<-as.character(gdp$country)


# only missed 6 joins, not bad 
demographics<-full_join(gdp, pop)

#fixing a couple names
demographics[which(demographics$country=='Bahamas, The'),1]<-'Bahamas'
demographics[which(demographics$country=='Iran, Islamic Rep.'),1]<-'Iran'
demographics[which(demographics$country=='Iran, Islamic Rep.'),1]<-'Iran'
demographics[which(demographics$country=='Korea, Rep.'),1]<-'South Korea'
demographics[which(demographics$country=='Slovak Republic'),1]<-'Slovakia'
demographics[which(demographics$country=='Russian Federation'),1]<-'Russia'
demographics[which(demographics$country=='Czech Republic' & demographics$year==1992),1]<-'Czechoslovakia'
demographics[which(demographics$country=='Egypt, Arab Rep.'),1]<-'Egypt'
demographics[which(demographics$country=='Hong Kong SAR, China'),1]<-'Hong Kong'
demographics[which(demographics$country=='Syrian Arab Republic'),1]<-'Syria'
demographics[which(demographics$country=='Kyrgyz Republic'),1]<-'Kyrgyzstan'
demographics[which(demographics$country=='Macedonia, FYR'),1]<-'Macedonia'
demographics[which(demographics$country=='Venezuela, RB'),1]<-'Venezuela'



##Base
base<-read.csv('base_data.csv', stringsAsFactors = F)
#there are 2 algerias 1992 for some reason
base[-1,]

base<-left_join(summer, demographics)
base<-base[,-1]


##Gender
gender<-read.csv('gender_data.csv', stringsAsFactors = F)
gender<-left_join(gender, demographics)
#fix algeria again
gender<-gender[-c(1,3),]
gender<-gender[,-1]

sum(is.na(gender$gdp))
sum(is.na(gender$pop))


##Sport 
sport<-read.csv('sport_data.csv', stringsAsFactors = F)
sport<-left_join(sport, demographics)

sport<-sport[,-1]
#algeria...
sport<-sport[-c(1,3),]

sum(is.na(sport$gdp))
sum(is.na(sport$pop))

setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")
#write.csv(base, 'base_data.csv')
#write.csv(gender, 'gender_data.csv')
#write.csv(sport, 'sport_data.csv')

#######################################################################Gender Inequality


setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/raw_data")

inequality<-read.csv('Gender Inequality Index (GII).csv', stringsAsFactors = F)

inequality<-inequality[,c(2,3,4,5,6,8)]


inequality$X95<-1995
inequality$X00<-2000
inequality$X05<-2005
inequality$X10<-2010
inequality$X12<-2012

colnames(inequality)<-c("country","index","index","index","index","index","year","year","year","year","year")

inequality<-rbind(inequality[,c(1,2,7)], inequality[,c(1,3,8)], inequality[,c(1,4,9)], inequality[,c(1,5,10)], inequality[,c(1,6,11)])
inequality$country<-trimws(inequality$country, which = 'left')

inequality[which(inequality$country=='Korea (Republic of)'),1]<-'South Korea'
inequality[which(inequality$country=='Czechia' & inequality$year==1995),1]<-'Czechoslovakia'
inequality[which(inequality$country=='Czechia' & inequality$year!=1995),1]<-'Czech Republic'
inequality[which(inequality$country=='Iran (Islamic Republic of)'),1]<-'Iran'
inequality[which(inequality$country=='The former Yugoslav Republic of Macedonia'),1]<-'Macedonia'
inequality[which(inequality$country=='Moldova (Republic of)'),1]<-'Moldova'
inequality[which(inequality$country=='Russian Federation'),1]<-'Russia'
inequality[which(inequality$country=='Syrian Arab Republic'),1]<-'Syria'
inequality[which(inequality$country=='Venezuela (Bolivarian Republic of)'),1]<-'Venezuala'
inequality[which(inequality$country=='Viet Nam'),1]<-'Vietnam'

colnames$nearest_olympics<-o

inequality[which(inequality$year==1995),4]<-1996
inequality[which(inequality$year==2000),4]<-2000
inequality[which(inequality$year==2005),4]<-2004
inequality[which(inequality$year==2010),4]<-2008
inequality[which(inequality$year==2012),4]<-2012

setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")
#write.csv(inequality, 'joinable_inequality.csv')



hosts<-c('USA', 1984,
'South Korea', 1988,
'Spain', 1992,
'United States', 1996,
'Australia', 2000,
'Greece', 2004,
'China', 2008,
'United Kingdom', 2012,
'Brazil', 2016,
'South Korea', 2018,
'Japan', 2020, 
'United States', 2024)

hosts<-data.frame(matrix(hosts, ncol = 2, byrow = T))
colnames(hosts)<-c('country', 'year')

setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")
#write.csv(hosts, 'host_cities.csv')

###################################################   USSR

USSR = c("Armenia","Moldova" ,"Estonia","Latvia","Lithuania","Georgia","Azerbaijan",
         "Tajikistan","Kyrgyzstan", "Belarus","Uzbekistan","Ukraine",
         "Kazakhstan","Russia")


base$soviet<-0
base$soviet<-ifelse(test = base$country %in% USSR, yes = 1, no = 0)

#####################################################    Host Cities

hosts<-read.csv('host_cities.csv', stringsAsFactors = F)

hosts<-hosts[,-1]
hosts[c(1,4),1]<-'United States'


hosts$country<-as.character(hosts$country)
hosts$year   <-as.numeric(as.character(hosts$year))


hosts_m_4<-hosts
hosts_p_4<-hosts
hosts_m_8<-hosts
hosts_p_8<-hosts

hosts_m_4$year<-hosts_m_4$year-4
hosts_p_4$year<-hosts_p_4$year+4
hosts_m_8$year<-hosts_m_8$year-8
hosts_p_8$year<-hosts_p_8$year+8

hosts_m_4$host<-1
hosts_p_4$host<-1
hosts_m_8$host<-1
hosts_p_8$host<-1

hosts$host<-1

hosts_exp<-rbind(hosts, hosts_m_4, hosts_p_4, hosts_m_8, hosts_p_8)

base<-left_join(base, hosts_exp)



head(base)

base$host<-ifelse(is.na(base$host), 0, 1)

setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/raw_data")
demo<-read.csv('democracy_data.csv', stringsAsFactors = F)
setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")


head(demo)
colnames(demo)[c(2,65)]
demo<-demo[,c(2,65)]

demo %>%
  group_by(ctryname) %>%
  summarize(com = sum(comm))->demo

demo<-demo[-1,]
demo$com<-ifelse(demo$com == 0, 0, 1)

country_names<-base%>% group_by(country) %>% summarize(count = n())
colnames(demo)<-c('country', 'comm')


demo[which(demo$country=='Viet Nam'),1]<-'Vietnam'
demo[which(demo$country=='Russian Federation'),1]<-'Russia'
demo[which(demo$country=='United States of America'),1]<-'United States'


View(left_join(country_names, demo))

##################################################################################
setwd("C:/Users/Max/Documents/Georgetown/Math651_Regression/MAth_651_final_project/data")
base<-read.csv('base_data.csv', stringsAsFactors = F)

base<-base[,-1]

base<-left_join(base, demo)

base$comm_soviet<-base$soviet+base$comm

yugo<-c('Croatia', 'Slovenia', 'Serbia', 'Macedonia', 'Bosnia', 'Kosovo', 'Montenegro')

base[which(base$country %in% yugo),9]<-'1'

base<-base[,-c(6,8)]




############################################################################# Remove NAs

base<-base[complete.cases(base),]

