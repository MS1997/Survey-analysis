library(foreign)
d <- read.dta("C:/Users/Desktop/NFHS_Children.dta")

library(survey)
library(dplyr)

wt <- d$v005/1000000 #defining variable for national sample weights

#defining the survey design
sd <- svydesign(ids = d$v021, strata = d$v023, weights = wt, data = d)
#adjustment for primary sampling units with single observation
options(survey.lonely.psu="adjust")

age <-d$v008 - d$b3 # age of child in months 
agebreaks <- c(-1,1,3,5,8,11,17,23,35,59)
agelabels <- c("0-1","2-3","4-5","6-8","9-11","12-17","18-23","24-35","36-59")
age_groups <- cut(age, breaks  = agebreaks,lables=agelabels)
levels(age_groups)
#age_under2= as.numeric(age_groups)<8

age_under2 <- ifelse(age_groups == "(23,35]" | age_groups == "(35,59]", NA, age_groups)
#d1 <- select(d1, -age_under2)
d1 <- mutate(d1, age)
d1 <- mutate(d1, age_groups = as.factor(age_groups))
d1 <- mutate(d1, age_under2 = as.factor(age_under2))
count(d1,age_under2)

dummy <- d1%>%
  filter(b5 == "yes" ) #only living children
sd3 <- svydesign(ids = dummy$v021, strata = dummy$v023, weights = dummy$v005/1000000, data = dummy)
tab_children_under2 <- svytotal(~sd3$variables$age_under2,sd3, na.rm =TRUE)
#sd1$variables <- mutate(sd1$variables, everbf1 = as.factor(everbf1))

bf <- ifelse(dummy$age < dummy$m4 & dummy$m4 <94, dummy$age,dummy$m4)
#bf <- ifelse(dummy$m4>=24 & dummy$m4<94,0,dummy$m4)
dummy <- mutate(dummy,bf)
dummy <- select(dummy, -bf)

dummy1 <- dummy %>%
  group_by(caseid) %>%
  filter(bidx == min(bidx) & b9 == "respondent") #youngest child living with the mother


#ever breastfed
sd4 <- svydesign(ids = dummy1$v021, strata = dummy1$v023, weights = dummy1$v005/1000000, data = dummy1)
tab_under2_young <- svytotal(~sd4$variables$age_under2,sd4, na.rm = TRUE)
everbf <- ifelse((sd4$variables$bf == 94 | is.na(sd4$variables$bf) == TRUE), 0,1)
sd4$variables <- mutate(sd4$variables, everbf= as.factor(everbf))
tab_everbf <- svymean(~everbf, sd4)
tab_everbf_states <- svyby(~everbf, by=sd4$variables$v024, design = sd4, FUN = svymean, na.rm =TRUE)
tab_everbf_ru <- svyby(~everbf, by=sd4$variables$v025, design = sd4, FUN = svymean)

#breastfeeding during the first hour
first_hour <- ifelse(sd4$variables$everbf ==1 & sd4$variables$m34 == 0,1,0)
sd4$variables <- mutate(sd4$variables, first_hour= as.factor(first_hour))
tab_first_hour <- svymean(~first_hour, sd4)
tab_first_hour_states <- svyby(~first_hour, by=sd4$variables$v024, design = sd4, FUN = svymean, na.rm =TRUE)
tab_first_hour_ru <- svyby(~first_hour, by=sd4$variables$v025, design = sd4, FUN = svymean, na.rm =TRUE)

#breastfeeding during the first day
first_day <- ifelse(sd4$variables$everbf ==1 & (sd4$variables$m34<=123 & sd4$variables$m34>=0),1,0)
sd4$variables <- mutate(sd4$variables, first_day= as.factor(first_day))
tab_first_day <- svymean(~first_day, sd4)
tab_first_day_states <- svyby(~first_day, by=sd4$variables$v024, design = sd4, FUN = svymean, na.rm =TRUE)
tab_first_day_ru <- svyby(~first_day, by=sd4$variables$v025, design = sd4, FUN = svymean, na.rm =TRUE)


#recode number of visits into visits>4=1,<4 =0,dontknow=2
num_visits <- ifelse(d$m14==98,2,ifelse(d$m14<=3,0,1))
sd$variables <- mutate(sd$variables,num_visits)
sd <- update(sd, num_visits=as.factor(num_visits))
tab_num_visits<- svymean(~num_visits,sd,na.rm=TRUE)
tabnum_visits_ru <- data.frame(svyby(~num_visits,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:4]
tabnum_visits_states <- data.frame(svyby(~num_visits,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,1:4]


#recode number of visits into no anc=1, anc=0, dont know=2
no_anc <- ifelse(d$m14==98,2,ifelse(d$m14==0,1,0))
sd$variables <- mutate(sd$variables,no_anc)
sd <- update(sd, no_anc=as.factor(no_anc))
tab_no_anc<- svymean(~no_anc,sd,na.rm=TRUE)
tabno_anc_ru <- data.frame(svyby(~no_anc,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:4] 
tabno_anc_states <- data.frame(svyby(~no_anc,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,2:4]

#median month of first anc visit
median_visits <- svyquantile(~m13,sd,0.5,na.rm=TRUE) #3
mean_visits <- svymean(~m13,sd,na.rm=TRUE) #3.5

#recode first anc visit in first trimester=1
first_visit <- ifelse(sd$variables$no_anc!=1  & sd$variables$m13<=3,1,0) 
sd$variables <- mutate(sd$variables,first_visit)
sd <- update(sd, first_visit=as.factor(first_visit))
tab_first_visit<- svymean(~first_visit,sd,na.rm=TRUE)
tabfirst_visit_ru <- data.frame(svyby(~first_visit,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:3]
tabfirst_visit_states <- data.frame(svyby(~first_visit,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,2:3]

#recode for neonatal tetanus during last pregnancy
tetanus_during_prg <- ifelse(sd$variables$m1>1 & sd$variables$m1 <8 , 1, 0)
sd$variables <- mutate(sd$variables,tetanus_during_prg)
sd <- update(sd, tetanus_during_prg=as.factor(tetanus_during_prg))
tab_tetanus<- svymean(~tetanus_during_prg,sd,na.rm=TRUE)
tab_tetanus_ru <- data.frame(svyby(~tetanus_during_prg,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:3]
tab_tetanus_states <- data.frame(svyby(~tetanus_during_prg,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,2:3]

#recode for neonatal tetanus before pregnancy
#tot_tetanus <- ifelse(sd$variables$m1>0 & sd$variables$m1 <8,sd$variables$m1,ifelse(sd$variables$m1a>0 & sd$variables$m1a<8,sd$variables$m1a,0) ) #sd$variables$tetanus_during_prg+sd$variables$m1a
age <- sd$variables$v008- sd$variables$b3
tot_tetanus <- ifelse((sd$variables$m1>0 & sd$variables$m1 <8),sd$variables$m1,0)
sd$variables$m1a <- ifelse(is.na(sd$variables$m1a)==TRUE,9996,d$m1a)
tot_tetanus1 <- ifelse(sd$variables$m1a>0 & sd$variables$m1a<8,(sd$variables$m1a + tot_tetanus),tot_tetanus)
diff <- ifelse(sd$variables$m1d<20 & (sd$variables$m1==0 | sd$variables$m1>7) ,sd$variables$m1d-(age/12),0)
sd$variables <- mutate(sd$variables,diff,tot_tetanus1)
#last_tetanus <- ifelse((sd$variables$tetanus_during_prg==1 | (sd$variables$m1a>=2 & sd$variables$diff <=3)| (sd$variables$m1a>=3 & sd$variables$diff <=5)| (sd$variables$m1a>=4 & sd$variables$diff <=10)| (sd$variables$m1a>=5)),1,0)
last_tetanus <- ifelse(sd$variables$tetanus_during_prg==1 | (sd$variables$tot_tetanus1>=2 & sd$variables$diff <=2)| (sd$variables$tot_tetanus1>=3 & sd$variables$diff <=4)| (sd$variables$tot_tetanus1>=4 & sd$variables$diff <=9)| (sd$variables$tot_tetanus1>=5),1,0)
sd$variables <- mutate(sd$variables,last_tetanus)
sd <- update(sd, last_tetanus=as.factor(last_tetanus))
tab_last_tetanus<- svymean(~last_tetanus,sd,na.rm=TRUE)
tab_last_tetanus_ru <- data.frame(svyby(~last_tetanus,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:3]
tab_last_tetanus_states <- data.frame(svyby(~last_tetanus,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,2:3]

lastbirth <- sd$variables %>%
  group_by(caseid) %>%
  filter(bidx == min(bidx))
sd1<- svydesign(ids = lastbirth$v021, strata = lastbirth$v023, weights = lastbirth$v005, data = lastbirth)
iron <- ifelse((sd1$variables$m46<100  | sd1$variables$m46 >=998 | is.na(sd1$variables$m46)==TRUE) ,0, 1)
sd1$variables <- mutate(sd1$variables,iron)
sd1 <- update(sd1, iron=as.factor(iron))
tab_iron <- svymean(~iron,sd1)
tab_iron_ru <- data.frame(svyby(~iron,by=~v025, design = sd1, FUN = svymean,na.rm=TRUE))[,2:3]
tab_iron_states <- data.frame(svyby(~iron,by=~v024, design = sd1, FUN = svymean,na.rm=TRUE))[,2:3]

#checking for full antenatal care for last birth in 5 years
tetanus_atleast1 <- ifelse(sd1$variables$m1>=1 & sd1$variables$m1 <8 , 1, 0)
sd1$variables <- mutate(sd1$variables,tetanus_atleast1)
sd1 <- update(sd1, tetanus_atleast1=as.factor(tetanus_atleast1))
tab_tetanus1<- svymean(~tetanus_atleast1,sd1,na.rm=TRUE)
tab_tetanus1_ru <- data.frame(svyby(~tetanus_atleast1,by=~v025, design = sd1, FUN = svymean,na.rm=TRUE))[,2:3]
tab_tetanus1_states <- data.frame(svyby(~tetanus_atleast1,by=~v024, design = sd1, FUN = svymean,na.rm=TRUE))[,2:3]

num_visits1 <- ifelse(lastbirth$m14==98,2,ifelse(lastbirth$m14<=3,0,1))
sd1$variables <- mutate(sd1$variables,num_visits1)
sd1 <- update(sd1, num_visits1=as.factor(num_visits1))
tab_num_visits1<- svymean(~num_visits1,sd1,na.rm=TRUE)
tabnum_visits_ru <- data.frame(svyby(~num_visits,by=~v025, design = sd, FUN = svymean,na.rm=TRUE))[,2:4]
tabnum_visits_states <- data.frame(svyby(~num_visits,by=~v024, design = sd, FUN = svymean,na.rm=TRUE))[,1:4]

full_anc <- ifelse( sd1$variables$tetanus_atleast1==1 & sd1$variables$iron ==1,ifelse(sd1$variables$num_visits1==1,1,0),0)
sd1$variables <- mutate(sd1$variables,full_anc)
sd1 <- update(sd1, full_anc=as.factor(full_anc))
tab_full_anc<- svymean(~full_anc,sd1,na.rm=TRUE)
tabfull_anc_ru <- data.frame(svyby(~full_anc,by=~v025, design = sd1, FUN = svymean,na.rm=TRUE))[,2:4]
tabfull_anc_states <- data.frame(svyby(~full_anc,by=~v024, design = sd1, FUN = svymean,na.rm=TRUE))[,1:4]

#place of delivery
place_d <- ifelse(lastbirth$m15 == "home" | lastbirth$m15 == "respondent's home" | lastbirth$m15 == "other home" | lastbirth$m15 == "parents' home",0,1)
sd1$variables <- mutate(sd1$variables,place_d)
sd1 <- update(sd1, place_d=as.factor(place_d))
tab_place_d<- svymean(~place_d,sd1,na.rm=TRUE)
tab_place_d_byeach<- svymean(~m15,sd1,na.rm=TRUE)

t