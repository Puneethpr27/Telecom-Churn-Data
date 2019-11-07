#Capstone Project

setwd("C:\\Users\\puneeth r\\Documents\\churn jigsaw")

tm<-read.csv("sampletelecomfinal.csv", na.strings=c("", NA))
options(scipen=999)

library(dplyr)
library(gains)
library(irr)
library(caret)
library(ggplot2)
library(car)

names(tm)
summary(tm)

#Ommitting variables with more than 15% missing values and creating a new data set
tm1<-tm[,colMeans(is.na(tm))<=0.15]

names(tm1)

#Dropping Variables that are substituted by adding 2 independent variables
#Imputing Categorical Variables

#5 point summary for numeric variable
#frequency distribution for categorical variable


summary(tm1)

#variable1 mou_Mean Monthly minutes of use
summary(tm1$mou_Mean)
quantile(tm1$mou_Mean, p=0:100/100, na.rm=T)
tm1$mou_Mean[is.na(tm1$mou_Mean)]<-mean(tm1$mou_Mean, na.rm=T)
tm1%>%filter(mou_Mean>=4000)%>%nrow()
tm1%>%filter(mou_Mean<=4000)->tm1
summary(tm1)

#variable2 totmrc_Mean
#Mean total monthly recurring charge
summary(tm1$totmrc_Mean)
quantile(tm1$totmrc_Mean, p=0:100/100, na.rm=T)
tm1$totmrc_Mean[is.na(tm1$totmrc_Mean)]<-mean(tm1$totmrc_Mean, na.rm=T)
tm1%>%filter(totmrc_Mean<=0)%>%nrow()
table(tm1$totmrc_Mean, tm1$churn)

summary(tm1)

#variable3 rev_Range
#Range of revenue (charge amount)
summary(tm1$rev_Range)
quantile(tm1$rev_Range, p=0:100/100, na.rm=T)
tm1$rev_Range[is.na(tm1$rev_Range)]<-mean(tm1$rev_Range, na.rm=T)
tm1%>%filter(rev_Range>=500)%>%nrow()
tm1%>%filter(rev_Range<=500)->tm1

summary(tm1)

#variable4 mou_Range
#Range of number of minutes of use
summary(tm1$mou_Range)
quantile(tm1$mou_Range, p=0:100/100, na.rm=T)
tm1$mou_Range[is.na(tm1$mou_Range)]<-mean(tm1$mou_Range, na.rm=T)

tm1%>%filter(mou_Range>=3000)%>%nrow()
tm1%>%filter(mou_Range<=3000)->tm1

summary(tm1)

#variable5 change_mou
#Percentage change in monthly minutes of use vs previous three month average
summary(tm1$change_mou)
quantile(tm1$change_mou, p=0:100/100, na.rm=T)
tm1$change_mou[is.na(tm1$change_mou)]<-mean(tm1$change_mou, na.rm=T)
tm1%>%filter(change_mou>=1200)%>%nrow()
tm1%>%filter(change_mou<=1200)->tm1

summary(tm1)


#variable6 ovrrev_Mean
#Mean overage revenue
summary(tm1$ovrrev_Mean)
quantile(tm1$ovrrev_Mean, p=0:100/100, na.rm=T)
tm1$ovrrev_Mean[is.na(tm1$ovrrev_Mean)]<-mean(tm1$ovrrev_Mean, na.rm=T)
tm1%>%filter(ovrrev_Mean>=250)%>%nrow()
tm1%>%filter(ovrrev_Mean<=250)->tm1

summary(tm1)

#variable7 rev_Mean
#Mean monthly revenue (charge amount)
summary(tm1$rev_Mean)
quantile(tm1$rev_Mean, p=0:100/100, na.rm=T)
tm1$rev_Mean[is.na(tm1$rev_Mean)]<-mean(tm1$rev_Mean, na.rm=T)
tm1%>%filter(rev_Mean>=300)%>%nrow()
tm1%>%filter(rev_Mean<=300)->tm1

summary(tm1)


#variable8 ovrmou_Mean
#Mean overage minutes of use
summary(tm1$ovrmou_Mean)
quantile(tm1$ovrmou_Mean, p=0:100/100, na.rm=T)
tm1$ovrmou_Mean[is.na(tm1$ovrmou_Mean)]<-mean(tm1$ovrmou_Mean, na.rm=T)
tm1%>%filter(ovrmou_Mean>=600)%>%nrow()
tm1%>%filter(ovrmou_Mean<=600)->tm1

summary(tm1)


#variable9 avg6mou
#Average monthly minutes of use over the previous six months
summary(tm1$avg6mou)
quantile(tm1$avg6mou, p=0:100/100, na.rm=T)
tm1$avg6mou[is.na(tm1$avg6mou)]<-mean(tm1$avg6mou, na.rm=T)
tm1%>%filter(avg6mou>=3000)%>%nrow()
tm1%>%filter(avg6mou<=3000)->tm1

summary(tm1)


#variable10 avg6qty
#Average monthly number of calls over the previous six months
summary(tm1$avg6qty)
quantile(tm1$avg6qty, p=0:100/100, na.rm=T)
tm1$avg6qty[is.na(tm1$avg6qty)]<-mean(tm1$avg6qty, na.rm=T)
tm1%>%filter(avg6qty>=1300)%>%nrow()
tm1%>%filter(avg6qty<=1300)->tm1

summary(tm1)

#Categorical Variable 1 PRIZM_SOCIAL_ONE
#Social group letter only
summary(tm1$prizm_social_one)
tm1$prizm_social_one_1<-ifelse(is.na(tm1$prizm_social_one),"Missing",as.factor(tm1$prizm_social_one))
str(tm1$prizm_social_one)
tm1$prizm_social_one_1<-as.factor(tm1$prizm_social_one_1)
summary(tm1$prizm_social_one)
summary(tm1$prizm_social_one_1)
tm1$prizm_social_one_1<-factor(tm1$prizm_social_one_1,labels =c("C","R","S","T","U","Missing"))
summary(tm1$prizm_social_one_1)

summary(tm1)

#Categorical Variable 2 area
#Geographic area
#only 4 missing values so omit
summary(tm1$area)
ind<-which(is.na(tm1$area))
tm1<-tm1[-ind,]


summary(tm1)

#Categorical Variable 3 hnd_webcap
#Variable hnd_webcap
#Handset web capability
summary(tm1$hnd_webcap)
tm1$hnd_webcap_1<-ifelse(is.na(tm1$hnd_webcap),"Missing",as.factor(tm1$hnd_webcap))
str(tm1$hnd_webcap_1)
tm1$hnd_webcap_1<-as.factor(tm1$hnd_webcap_1)
summary(tm1$hnd_webcap)
summary(tm1$hnd_webcap_1)
tm1$hnd_webcap_1<-factor(tm1$hnd_webcap_1,labels =c("UNKW","WC","WCMB","Missing"))
summary(tm1$hnd_webcap_1)


summary(tm1)


#Categorical Variable 4 marital
#Marital status
summary(tm1$marital)
tm1$marital_1<-ifelse(is.na(tm1$marital),"Missing",as.factor(tm1$marital))
str(tm1$marital_1)
tm1$marital_1<-as.factor(tm1$marital_1)
summary(tm1$marital)
summary(tm1$marital)
tm1$marital_1<-factor(tm1$marital_1,labels =c("A","B","M","S","U","Missing"))
summary(tm1$marital_1)

#Categorical Variable 5 ethnic
#Ethnicity roll-up code
summary(tm1$ethnic)
tm1$ethnic_1<-ifelse(is.na(tm1$ethnic),"Missing",as.factor(tm1$ethnic))
str(tm1$ethnic_1)
tm1$ethnic_1<-as.factor(tm1$ethnic_1)
summary(tm1$ethnic)
summary(tm1$ethnic_1)
tm1$ethnic_1<-factor(tm1$ethnic_1,labels =c("B","C","D","F","G","H","I","J","M","N","O","P","R","S","U","X","Z","Missing"))
summary(tm1$ethnic_1)

summary(tm1)

#Categorical Variable 6 age1
#Age of first household member
summary(tm1$age1)
tm1$age1_1<-ifelse(tm1$age1==0,"Default",ifelse(tm1$age1<=30,"Young",ifelse(tm1$age1>30 & tm1$age1<=55,"Mid Age","Old")))
str(tm1$age1_1)
tm1$age1_1<-as.factor(tm1$age1_1)
summary(tm1$age1_1)
tm1$age1_1[is.na(tm1$age1_1)]<-"Default"
table(tm1$age1, tm1$age1_1)
summary(tm1$age1_1)

#Categorical Variable 7 age2
#Age of second household member
summary(tm1$age2)
tm1$age2_2<-ifelse(tm1$age2==0,"Default",ifelse(tm1$age2<=30,"Young",ifelse(tm1$age2>30 & tm1$age2<=55,"Mid Age","Old")))
str(tm1$age2_2)
tm1$age2_2<-as.factor(tm1$age2_2)
summary(tm1$age2_2)
tm1$age2_2[is.na(tm1$age2_2)]<-"Default"
summary(tm1$age2_2)


#continuos variable
#variable 11 hnd_price
summary(tm1$hnd_price)
unique(tm1$hnd_price)
tab1<-table(tm1$hnd_price, tm1$churn)
churnedratehand<-tab1[,2]/rowSums(tab1)
ind11<-which(is.na(tm1$hnd_price))
table(tm1$churn[ind11])/length(ind11)

#One estimate of NA will be 399.9899902
tm1$hnd_price[ind11]<-399.9899902

summary(tm1)


#categorical variable 8
#forgntvl
#Foreign travel dummy variable
summary(tm1$forgntvl)
unique(tm1$forgntvl)
tm1$forgntvl_1<-ifelse(is.na(tm1$forgntvl),"Missing",as.factor(tm1$forgntvl))
str(tm1$forgntvl)
tm1$forgntvl_1<-as.factor(tm1$forgntvl_1)
summary(tm1$forgntvl)
summary(tm1$forgntvl_1)
tm1$forgntvl_1<-factor(tm1$forgntvl_1,labels =c(0,1,"Missing"))
summary(tm1$forgntvl_1)



summary(tm1)

#categorical variable 9
#variable mtrcycle
#Motorcycle indicator
summary(tm1$mtrcycle)
unique(tm1$mtrcycle)
tm1$mtrcycle_1<-ifelse(is.na(tm1$mtrcycle),"Missing",as.factor(tm1$mtrcycle))
str(tm1$mtrcycle)
tm1$mtrcycle_1<-as.factor(tm1$mtrcycle_1)
summary(tm1$mtrcycle)
summary(tm1$mtrcycle_1)
tm1$mtrcycle_1<-factor(tm1$mtrcycle_1,labels =c(0,1,"Missing"))
summary(tm1$mtrcycle_1)

#categorical variable 10
#variable truck
#Truck indicator
summary(tm1$truck)
unique(tm1$truck)
tm1$truck_1<-ifelse(is.na(tm1$truck),"Missing",as.factor(tm1$truck))
str(tm1$truck)
tm1$truck_1<-as.factor(tm1$truck_1)
summary(tm1$truck)
summary(tm1$truck_1)
tm1$truck_1<-factor(tm1$truck_1,labels =c(0,1,"Missing"))
summary(tm1$truck_1)

#Continuous variable 12
#roam_Mean
#Mean number of roaming calls
summary(tm1$roam_Mean)
quantile(tm1$roam_Mean, p=0:100/100, na.rm=T)
tm1$roam_Mean[is.na(tm1$roam_Mean)]<-mean(tm1$roam_Mean, na.rm=T)
tm1%>%filter(roam_Mean>=50)%>%nrow()
tm1%>%filter(roam_Mean<=50)->tm1


summary(tm1)


#categorical variable 11
#variable car_buy
#New or used car buyer
summary(tm1$car_buy)
unique(tm1$car_buy)
tm1$car_buy_1<-ifelse(is.na(tm1$car_buy),"Missing",as.factor(tm1$car_buy))
str(tm1$car_buy)
tm1$car_buy_1<-as.factor(tm1$car_buy_1)
summary(tm1$car_buy)
summary(tm1$car_buy_1)
tm1$car_buy_1<-factor(tm1$car_buy_1,labels =c("New","UNKNOWN","Missing"))
summary(tm1$car_buy_1)


summary(tm1$da_Mean)

#Continuous variable 13
#da_Mean
#Mean number of directory assisted calls
summary(tm1$da_Mean)
quantile(tm1$da_Mean, p=0:100/100, na.rm=T)
tm1$da_Mean[is.na(tm1$da_Mean)]<-mean(tm1$da_Mean, na.rm=T)
tm1%>%filter(da_Mean>=20)%>%nrow()
tm1%>%filter(da_Mean<=20)->tm1

#Continuous variable 14
#da_Range
#Range of number of directory assisted calls
summary(tm1$da_Range)
quantile(tm1$da_Range, p=0:100/100, na.rm=T)
tm1$da_Range[is.na(tm1$da_Range)]<-mean(tm1$da_Range, na.rm=T)
tm1%>%filter(da_Range>=20)%>%nrow()
tm1%>%filter(da_Mean<=20)->tm1


#Continuous variable 15
#datovr_Mean   Omit the data
#Mean revenue of data overage
summary(tm1$datovr_Mean)
quantile(tm1$datovr_Mean, p=0:100/100, na.rm=T)
tm1$datovr_Mean[is.na(tm1$datovr_Mean)]<-mean(tm1$datovr_Mean, na.rm=T)
tm1%>%filter(datovr_Mean>=10)%>%nrow()
tm1%>%filter(datovr_Mean<=10)->tm1

#Continuous variable 16
#datovr_Range 
#Range of revenue of data overage
summary(tm1$datovr_Range)
quantile(tm1$datovr_Range, p=0:100/100, na.rm=T)
tm1$datovr_Range[is.na(tm1$datovr_Range)]<-mean(tm1$datovr_Range, na.rm=T)
tm1%>%filter(datovr_Range>=20)%>%nrow()
tm1%>%filter(datovr_Range<=20)->tm1




####Non Missing Values
summary(tm1$adjrev)
quantile(tm1$adjrev, p=1:100/100)
tm1%>%filter(adjrev>=5000)%>%nrow()
tm1%>%filter(adjrev<=5000)->tm1


#omit few variables
names(tm1)
unique(tm1$crclscod)
unique(tm1$csa)

summary(tm1)


names(tm1)

tele<-tm1[, -c(30,32,35,36,37,38,39,44,46,47,53,54)]

names(tele)
summary(tele)
str(tele)

#exporting the data
write.csv(tele, "tele.csv", row.names = T)


#######Logistic Regression##########
set.seed(200)
index<-sample(nrow(tele), 0.70*nrow(tele), replace=F)
train<-tele[index,]
test<-tele[-index,]


#checking the churn rate
table(tele$churn)/nrow(train)
table(tele$churn)/nrow(test)

names(tele)

#Building Logistic Regression excluding Customer_ID
mod<-glm(churn~., data=train[-53], family = "binomial")
summary(mod)


mod1<-glm(formula = churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + 
            drop_blk_Mean + owylis_vce_Range + mou_opkv_Range + months + 
            totcalls + eqpdays + custcare_Mean + callwait_Mean + iwylis_vce_Mean + 
            callwait_Range + ccrndmou_Range + adjqty + ovrrev_Mean + 
            rev_Mean + ovrmou_Mean + comp_vce_Mean + plcd_vce_Mean + 
            avg3mou + avgmou + avg3qty + avgqty + avg6mou + avg6qty + 
            asl_flag + refurb_new + models + hnd_price + actvsubs + uniqsubs + 
            opk_dat_Mean + roam_Mean + recv_sms_Mean + blck_dat_Mean + 
            mou_pead_Mean + da_Mean + da_Range + datovr_Mean + datovr_Range + 
            drop_dat_Mean + drop_vce_Mean + adjmou + totrev + adjrev + 
            avgrev + hnd_webcap_1 + ethnic_1 + age1_1 + car_buy_1, family = "binomial", data= train)

summary(mod1)


#creating dummies
summary(tele$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)

summary(train$area)
train$area_Cal_Nrth<-ifelse(train$area == "CALIFORNIA NORTH AREA", 1, 0)
test$area_Cal_Nrth<-ifelse(test$area == "CALIFORNIA NORTH AREA", 1, 0)

train$area_texas<-ifelse(train$area == "CENTRAL/SOUTH TEXAS AREA", 1, 0)
test$area_texas<-ifelse(test$area == "CENTRAL/SOUTH TEXAS AREA", 1, 0)

train$area_nrthflrda<-ifelse(train$area == "NORTH FLORIDA AREA", 1, 0)
test$area_nrthflrda<-ifelse(test$area == "NORTH FLORIDA AREA", 1, 0)

train$area_nrthwst<-ifelse(train$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)
test$area_nrthwst<-ifelse(test$area == "NORTHWEST/ROCKY MOUNTAIN AREA", 1, 0)

train$area_southflrda<-ifelse(train$area == "SOUTH FLORIDA AREA", 1, 0)
test$area_southflrda<-ifelse(test$area == "SOUTH FLORIDA AREA", 1, 0)

train$area_southwst<-ifelse(train$area == "SOUTHWEST AREA", 1, 0)
test$area_southwst<-ifelse(test$area == "SOUTHWEST AREA", 1, 0)

train$area_tenese<-ifelse(train$area == "TENNESSEE AREA", 1, 0)
test$area_tenese<-ifelse(test$area == "TENNESSEE AREA", 1, 0)

summary(train$refurb_new)
train$refurb_R<-ifelse(train$refurb_new == "R", 1, 0)
test$refurb_R<-ifelse(test$refurb_new == "R", 1, 0)

summary(train$ethnic_1)
train$ethnic_1_C<-ifelse(train$ethnic_1 == "C", 1, 0)
test$ethnic_1_C<-ifelse(test$ethnic_1 == "C", 1, 0)

train$ethnic_1_N<-ifelse(train$ethnic_1 == "N", 1, 0)
test$ethnic_1_N<-ifelse(test$ethnic_1 == "N", 1, 0)

train$ethnic_1_O<-ifelse(train$ethnic_1 == "O", 1, 0)
test$ethnic_1_O<-ifelse(test$ethnic_1 == "O", 1, 0)

train$ethnic_1_S<-ifelse(train$ethnic_1 == "S", 1, 0)
test$ethnic_1_S<-ifelse(test$ethnic_1 == "S", 1, 0)

train$ethnic_1_U<-ifelse(train$ethnic_1 == "U", 1, 0)
test$ethnic_1_U<-ifelse(test$ethnic_1 == "U", 1, 0)

train$ethnic_1_Z<-ifelse(train$ethnic_1 == "Z", 1, 0)
test$ethnic_1_Z<-ifelse(test$ethnic_1 == "Z", 1, 0)


summary(train$hnd_price)

train$hnd_price_79.98<-ifelse(train$hnd_price == "79.98999023", 1, 0)
test$hnd_price_79.98<-ifelse(test$hnd_price == "79.98999023", 1, 0)

train$hnd_price_105.08<-ifelse(train$hnd_price == "105.083038078331", 1, 0)
test$hnd_price_105.08<-ifelse(test$hnd_price == "105.083038078331", 1, 0)

train$hnd_price_129.98<-ifelse(train$hnd_price == "129.9899902", 1, 0)
test$hnd_price_129.98<-ifelse(test$hnd_price == "129.9899902", 1, 0)

train$hnd_price_149.98<-ifelse(train$hnd_price == "149.9899902", 1, 0)
test$hnd_price_149.98<-ifelse(test$hnd_price == "149.9899902", 1, 0)

train$hnd_price_199.98<-ifelse(train$hnd_price == "199.9899902", 1, 0)
test$hnd_price_199.98<-ifelse(test$hnd_price == "199.9899902", 1, 0)

train$hnd_price_249.98<-ifelse(train$hnd_price == "249.9899902", 1, 0)
test$hnd_price_249.98<-ifelse(test$hnd_price == "249.9899902", 1, 0)


summary(train$uniqsubs)

train$unq_2<-ifelse(train$uniqsubs == "2", 1, 0)
test$unq_2<-ifelse(test$uniqsubs == "2", 1, 0)

train$unq_3<-ifelse(train$uniqsubs == "3", 1, 0)
test$unq_3<-ifelse(test$uniqsubs == "3", 1, 0)

train$unq_4<-ifelse(train$uniqsubs == "4", 1, 0)
test$unq_4<-ifelse(test$uniqsubs == "4", 1, 0)

train$unq_5<-ifelse(train$uniqsubs == "5", 1, 0) 
test$unq_5<-ifelse(test$uniqsubs == "5", 1, 0)

train$unq_6<-ifelse(train$uniqsubs == "6", 1, 0) 
test$unq_6<-ifelse(test$uniqsubs == "6", 1, 0)

train$unq_7<-ifelse(train$uniqsubs == "7", 1, 0)
test$unq_7<-ifelse(test$uniqsubs == "7", 1, 0)

train$unq_9<-ifelse(train$uniqsubs == "9", 1, 0)
test$unq_9<-ifelse(test$uniqsubs == "9", 1, 0)

summary(train$prizm_social_one_1)

train$przm_social_R<-ifelse(train$prizm_social_one_1 == "R", 1, 0)
test$przm_social_R<-ifelse(test$prizm_social_one_1 == "R", 1, 0)

train$przm_social_T<-ifelse(train$prizm_social_one_1 == "T", 1, 0)
test$przm_social_T<-ifelse(test$prizm_social_one_1 == "T", 1, 0)


summary(train$age1_1)

train$age1_Mid_Age<-ifelse(train$age1_1 == "Mid Age", 1, 0)
test$age1_Mid_Age<-ifelse(test$age1_1 == "Mid Age", 1, 0)

train$age1_Old<-ifelse(train$age1_1 == "Old", 1, 0)
test$age1_Old<-ifelse(test$age1_1 == "Old", 1, 0)

train$age1_Young<-ifelse(train$age1_1 == "Young", 1, 0) # Not Required
test$age1_Young<-ifelse(test$age1_1 == "Young", 1, 0)


summary(train$age2_2)

train$age2_Old<-ifelse(train$age2_2 == "Old", 1, 0)
test$age2_Old<-ifelse(test$age2_2 == "Old", 1, 0)

###Logistic Regression 2

names(train)

mod1<-glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range +
            mou_opkv_Range + months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + avg3qty + avgqty +
            avg6mou + asl_flag_Y + area_Cal_Nrth + area_texas + area_nrthflrda + area_nrthwst + area_southflrda +
            area_southwst + area_tenese + refurb_R + ethnic_1_C + ethnic_1_N + ethnic_1_O + ethnic_1_S + ethnic_1_U + 
            ethnic_1_Z + hnd_price_79.98 + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + 
            hnd_price_199.98 + hnd_price_249.98 + unq_2 + unq_3 + unq_4 + unq_5 + unq_6 + unq_7 + unq_9 +
            truck_1 + adjmou + totrev  + przm_social_R + przm_social_T + age1_Mid_Age +
            age1_Old + age1_Young + age2_Old,data=train,family="binomial")
summary(mod1)

mod2<-  glm(formula = churn ~ mou_Mean + mou_Range + drop_vce_Range + 
             months + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + 
             avg3qty + asl_flag_Y + area_texas + area_southflrda + refurb_R + 
             ethnic_1_C + ethnic_1_O + ethnic_1_Z + hnd_price_79.98 + 
             hnd_price_199.98 + unq_2 + unq_3 + unq_4 + unq_9 + age1_Mid_Age + 
             age1_Old, family = "binomial", data = train)
summary(mod2)

mod3<-  glm(formula = churn ~ mou_Mean + mou_Range + drop_vce_Range
               + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + 
              avg3qty + asl_flag_Y + refurb_R + hnd_price_199.98 + unq_2 + unq_3 + age1_Mid_Age + 
              age1_Old, family = "binomial", data = train)
summary(mod3)


#All Variables are significant
#Model can be finalised on checking multi collinerity
vif(mod3)

#Removing mou_Mean as it is above 5 and rerunning the model
mod4<-  glm(formula = churn ~  mou_Range + drop_vce_Range
            + eqpdays + iwylis_vce_Mean + ovrrev_Mean + avgmou + 
              avg3qty + asl_flag_Y + refurb_R + hnd_price_199.98 + unq_2 + unq_3 + age1_Mid_Age + 
              age1_Old, family = "binomial", data = train)
summary(mod4)


#variables mou_Range iwylis_vce_Mean  are insignificant
#so removing them and rerunning the model
mod5<-  glm(formula = churn ~   drop_vce_Range
            + eqpdays  + ovrrev_Mean + avgmou + 
              avg3qty + asl_flag_Y + refurb_R + hnd_price_199.98 + unq_2 + unq_3 + age1_Mid_Age + 
              age1_Old, family = "binomial", data = train)
summary(mod5)

#checking multicollinearity
vif(mod5)

pred<-predict(mod5, type="response", newdata = test)


head(pred)

#checking churn rate 1 which says that the customer has left
table(tele$churn)/nrow(tele)
pred1<-ifelse(pred>=0.2355173,1,0)

head(pred1)

kappa2(data.frame(test$churn, pred1))

table(test$churn)

#So far good fit model

#######################################################################################################################

#Gains Chart
gains(test$churn, predict(mod5, type="response", newdata = test, groups=10))

#the Gains Chart shows that the top 20% of the probabilities contain 26.6% customers that are likely to churn.

test$prob<-predict(mod5, type="response", newdata = test)

#selecting customers with high churn rate
quantile(test$prob, prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

#Top 20% of the probability scores lie between 0.2882659 and 0.5832088
#We can use this probablity to extract the data of customers who are highly likely to churn.


targeted<-test[test$prob>0.2882659&test$prob<=0.5832088 & test$churn=="1", "Customer_ID"]
targeted<-as.data.frame(targeted)
nrow(targeted)

write.csv(targeted, "targeted customer ID.csv", row.names = F)
##########################################################################################################################
#Finding aswers to the Capstone Project

head(sort(abs(mod5$coefficients),decreasing = T),10)
summary(mod1)

summary(tele$asl_flag)
summary(tele$refurb_new)

###########################################################################################################################
#2b. Are data usage connectivity issues turning out to be costly? In other words, is it leading to churn?

summary(mod)
###########################################################################################################################

#5.) Finding Revenue saves target by creating gains chart

pred5<-predict(mod5, type="response", newdata=test)
test$prob<-predict(mod5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
table(pred6,test$churn)

str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<559.521,"Low_Revenue",ifelse(test$totrev>=912.114 & 
                                                                  test$totrev<1869.089,"Medium_Revenue","High_Revenue"))

table(Revenue_Levels)

table(pred6,Revenue_Levels)

#This table can be extracted to select the subscribers on revenue save

test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<559.521,"Low_Revenue",ifelse(test$totrev>=912.114 & 
                                                                       test$totrev<1869.089,"Medium_Revenue","High_Revenue"))

Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

write.csv(Targeted1,"High Revenue Target Customer ID.csv",row.names = F)

save.image("Project_Churn.RData")

