library(tidyverse)
library(mice)
w1 = da21600.0001 %>% select(AID,BIO_SEX,H1GI4,H1GI6A,H1GI6B,H1GI6C,H1GI6D,H1GI6E,H1GH59A,H1GH59B,H1GH60,PA12,PA55,H1WP7)
w1$BMI = 703*(w1$H1GH60)/(
  (
((as.integer(w1$H1GH59A)+3)*12) + ((as.numeric(w1$H1GH59B)-1)))^2)

w1$w1obs = ifelse(w1$BMI >= 30,"Obese",ifelse(w1$BMI < 30,"Non-Obese",w1$BMI))

w1$H1GI6A = as.numeric(w1$H1GI6A)- 1
w1$H1GI6B = as.numeric(w1$H1GI6B)- 1
w1$H1GI6C = as.numeric(w1$H1GI6C)- 1
w1$H1GI6D = as.numeric(w1$H1GI6D)- 1
w1$H1GI6E = as.numeric(w1$H1GI6E)- 1
w1$racecount = w1$H1GI6A+w1$H1GI6B+w1$H1GI6C+w1$H1GI6D+w1$H1GI6E
w1$RACE = ifelse(as.numeric(w1$H1GI6A) == "1","White",NA)
w1$RACE = ifelse(as.numeric(w1$H1GI6B) == "1","Black",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI6C) == "1","Indian",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI6D) == "1","Asian",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI6E) == "1","Other",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$racecount) > 1,"Multi-Race",w1$RACE)
w1$RACE = ifelse(as.numeric(w1$H1GI4) == "2","Hispanic",w1$RACE)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) < 4,"Less than High School",NA)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) %in% c(4,5),"High School Graduate",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) %in% c(6,7),"Some College",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) == 8 ,"College Graduate",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) == 9 ,"College Graduate +",w1$PAEDUC)
w1$PAEDUC = ifelse(as.numeric(w1$PA12) == 10 ,"Less than High School",w1$PAEDUC)
w1$EDUC1 = "Less than High School"
w1$w1HI = ifelse(w1$PA55<75,"Less than $75000",NA)
w1$w1HI = ifelse(w1$PA55<100 & w1$PA55>75,"$75000 - $99999",w1$w1HI)
w1$w1HI = ifelse(w1$PA55>100,"$100000 and over",w1$w1HI)


w2 = da21600.0022
w2$w2obs = ifelse(as.numeric(w2$H4BMICLS) %in% c(4,5,6),"Obese",NA)
w2$w2obs = ifelse(as.numeric(w2$H4BMICLS) %in% c(1,2,3),"Non-Obese",w2$w2obs)
w2$w2HI = ifelse(as.numeric(w2$H4EC1)<10,"Less than $75000",NA)
w2$w2HI = ifelse(as.numeric(w2$H4EC1) == 10,"$75000 - $99999",w2$w2HI)
w2$w2HI = ifelse(as.numeric(w2$H4EC1) %in% c(11,12),"$100000 and over",w2$w2HI)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2)<3,"Less than High School",NA)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2)==3,"High School Graduate",w2$EDUC2)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2)%in%c(4,5,6),"Some College",w2$EDUC2)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2) == 7,"College Graduate",w2$EDUC2)
w2$EDUC2 = ifelse(as.numeric(w2$H4ED2) %in% c(8,9,10,11,12,13),"College Graduate +",w2$EDUC2)
w2$H4DA5 = as.numeric(w2$H4DA5)



w3obs = da21600.0034 %>% select(AID,H5BMICLS)
w3obs$w3obs = ifelse(w3obs$H5BMICLS <4,"Non-Obese",NA)
w3obs$w3obs = ifelse(w3obs$H5BMICLS %in% c(4,5,6),"Obese",w3obs$w3obs)

w3 = da21600.0032
w3$w3HI = ifelse(as.numeric(w3$H5EC2)<10,"Less than $75000",NA)
w3$w3HI = ifelse(as.numeric(w3$H5EC2) == 10,"$75000 - $99999",w3$w3HI)
w3$w3HI = ifelse(as.numeric(w3$H5EC2) %in% c(11,12),"$100000 and over",w3$w3HI)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) == 2, "Less than High School",NA)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) %in% c(3,4), "Less than High School",w3$EDUC3)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) %in% c(5,6,7,8,9), "Some College",w3$EDUC3)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) ==10, "College Graduate",w3$EDUC3)
w3$EDUC3 = ifelse(as.numeric(w3$H5OD11) %in% c(11,12,13,14,15,16), "College Graduate +",w3$EDUC3)
w3$H5ID27 = as.numeric(w3$H5ID27)

ww = da21600.0042 %>% select(AID,GSW145)

ready = ww %>% merge(w3,by="AID") %>% merge(w2,by = "AID") %>% merge(w1,by ="AID") %>% merge(w3obs,by = "AID") %>% select(AID,w1obs,w2obs,w3obs,PAEDUC,RACE,BIO_SEX,EDUC1,EDUC2,EDUC3,H4DA5,H5ID27,H1WP7,w1HI,w2HI,w3HI,GSW145)



ready$H4DA5 <- as.numeric(ready$H4DA5)
ready$H5ID27 <- as.numeric(ready$H5ID27)

str(ready)
ready$w1obs = as.factor(ready$w1obs)
ready$w2obs = as.factor(ready$w2obs)
ready$w3obs = as.factor(ready$w3obs)
ready$w1HI = as.factor(ready$w1HI)
ready$w2HI=as.factor(ready$w2HI)
ready$w3HI=as.factor(ready$w3HI)
ready$EDUC1=as.factor(ready$EDUC1)
ready$EDUC2=as.factor(ready$EDUC2
                      )
ready$EDUC3=as.factor(ready$EDUC3)
ready$PAEDUC = as.factor(ready$PAEDUC)
ready$RACE = as.factor(ready$RACE)
levels(ready$w1HI) = c("Less than $75000","$75000 - $99999","$100000 and over")
levels(ready$w2HI)= c("Less than $75000","$75000 - $99999","$100000 and over")
levels(ready$w3HI)= c("Less than $75000","$75000 - $99999","$100000 and over")
levels(ready$PAEDUC) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
levels(ready$EDUC1) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
levels(ready$EDUC2) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
levels(ready$EDUC3) = c("Less than High School", "High School Graduate","Some College","College Graduate","College Graduate +")
ready$AID = as.numeric(ready$AID)
str(ready)




ini = mice(ready,seed = 1)
pred1 = ini$predictorMatrix
pred1[,'AID'] = 0
ready = complete(mice(ready,seed = 1,pred = pred1))



## Univariate Analysis

ready %>% group_by(autonomy,baselineHI) %>% summarise(counts = n()) %>% ggplot(aes(x = autonomy, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + facet_wrap(~baselineHI)

ready %>% group_by(w1obs,baselineHI) %>% summarise(counts = n()) %>% ggplot(aes(x = w1obs, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + facet_grid(~baselineHI)

ready %>% group_by(w2obs,baselineHI) %>% summarise(counts = n()) %>% ggplot(aes(x = w2obs, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + facet_grid(~baselineHI)
ready %>% group_by(w3obs,baselineHI) %>% summarise(counts = n()) %>% ggplot(aes(x = w3obs, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + facet_grid(~baselineHI)







ready$BIO_SEX = as.factor(ifelse(ready$BIO_SEX == '(1) (1) Male',"Male","Famale"))
colnames(ready)[11] = "w2spt"
colnames(ready)[12] = 'w3spt'
colnames(ready)[13] = 'autonomy'
ready$autonomy = as.factor(ifelse(ready$autonomy == "(0) (0) No","No","Yes"))

library(table1)
ready$baselineHI = ready$w1HI
table1(~.-baselinHI-AID-GSW145|baselineHI,data = ready)

long = ready %>% pivot_longer(cols = c(EDUC1,EDUC2,EDUC3),values_to
                              = 'EDUC')


long$HI = NA
for(i in seq(0,5280,3)){
  long$HI[i] = long$w3HI[i]
}

for(i in seq(2,5280,3)){
  long$HI[i] = long$w2HI[i]
}

for(i in seq(1,5280,3)){
  long$HI[i] = long$w1HI[i]
}

long$obs = NA
for(i in seq(0,5280,3)){
  long$obs[i] = long$w3obs[i]
}

for(i in seq(2,5280,3)){
  long$obs[i] = long$w2obs[i]
}

for(i in seq(1,5280,3)){
  long$obs[i] = long$w1obs[i]
}
long$obs = as.factor(ifelse(long$obs == 1,"non-obese","obese"))

long$HI = as.factor(
  ifelse(long$HI == 1,"Less than $75000", 
         ifelse(
           long$HI == 2, "$75000 - $99999","$100000 and over"
         ))
)
long = long %>% select(-w1obs,-w2obs,-w3obs,-w1HI,-w2HI,-w3HI,-name)
long$wave = rep(c(1,2,3),1760)



long








long$AID = as.factor(long$AID)

library(geepack)
write.csv(long,"~/Downloads/tt.csv")
long = read.csv("~/Downloads/tt.csv")
long$obs = ifelse(long$obs=="non-obese",0,1)


high = long %>% filter(baselineHI == "$100000 and over")
hightbl = gee(obs~PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy+EDUC,corstr = "exchangeable",id = AID,family = binomial,data = high)
tbl_regression(hightbl)


low = long %>% filter(baselineHI == "Less than $75000")
lowtbl = gee(obs~PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy+EDUC,id = AID,data = low,family='binomial')
tbl_regression(lowtbl)


mid = long %>% filter(baselineHI == "$75000 - $99999")
midtbl = gee(obs~PAEDUC+RACE+BIO_SEX+w2spt+w3spt+autonomy+EDUC,id = AID,data = mid,family='binomial')
tbl_regression(midtbl)


### data are ready to be analyzed.




