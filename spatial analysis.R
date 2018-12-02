library(foreign)
d <- read.dta("C:/Users/Desktop/NFHS_Children.dta")

library(survey)
library(dplyr)

#for spatial analysis
library(tmap) 
library(sp) 

wt <- d$v005/1000000 #defining variable for national sample weights

#defining the survey design
sd <- svydesign(ids = d$v021, strata = d$v023, weights = wt, data = d)
#adjustment for primary sampling units with single observation
options(survey.lonely.psu="adjust")

#defining variable for stunting i.e. less height for age, indicator of malnutrition
#A child is labeled as stunted if his HAZ or height for age z score is less than -2SD (WHO)
#0=not stunted, 1=stunted, >=9996 have to be excluded from numerator and denominator hence made NAs
stunt <- ifelse(sd$variables$hw70>=9996,NA,ifelse(sd$variables$hw70>-200,0,1))

sd$variables <- mutate(sd$variables,stunt=as.factor(stunt)) #adding this column to the dataset

#proportion of stunting in India
tab_stunt <- svymean(~stunt,sd,na.rm=TRUE) 

#proportion of stunting by rural and urban
tab_stunt_ru <- data.frame(svyby(~stunt,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:4]

#proportion of stunting by state
tab_stunt_states <- data.frame(svyby(~stunt,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,2:4]

#mapping the state data
write.csv(tabnum_stunt_states, "stuntbystate.csv") #this line is to be run only once
#download spatial data for India from https://gadm.org/download_country_v3.html -> India -> R(sp)-level1 
gadm1 <- readRDS("gadm36_IND_1_sp.rds") 

states <-read.csv("stuntbystate.csv") 
#"delhi" to be changed to "nct delhi"
#sort the data set using state column, make the state column same as NAME_1 column in gadm@data

gadm1@data <- merge(gadm1@data,states, by.x ="NAME_1", by.y="state", all.x = TRUE)
map <-tm_shape(gadm1)+tm_fill(col = "stunt1", style = "quantile", palette = "Blues") + tm_layout(legend.outside.position = "bottom", legend.outside = TRUE)
