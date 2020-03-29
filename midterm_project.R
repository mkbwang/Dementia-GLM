
url = "https://raw.githubusercontent.com/skybullbobby/Dementia-GLM/master/data/mds_center9661.csv"
mds_center9661 = read.csv(url)

'change the char variables into numeric'
var_numeric_name=c('CLINDEM', 'DEP', 'SEX', 'RACE', 'RLDEM','MARISTAT')
catn1 = factor(mds_center9661$CLINDEM, labels=(1:length(levels(factor(mds_center9661$CLINDEM)))))
mds_center9661$CLINDEM_num=as.numeric(catn1)
catn1 = factor(mds_center9661$DEP, labels=(1:length(levels(factor(mds_center9661$DEP)))))
mds_center9661$DEP_num=as.numeric(catn1)
catn1 = factor(mds_center9661$SEX, labels=(1:length(levels(factor(mds_center9661$SEX)))))
mds_center9661$SEX_num=as.numeric(catn1)
catn1 = factor(mds_center9661$RACE, labels=(1:length(levels(factor(mds_center9661$RACE)))))
mds_center9661$RACE_num=as.numeric(catn1)
catn1 = factor(mds_center9661$RLDEM, labels=(1:length(levels(factor(mds_center9661$RLDEM)))))
mds_center9661$RLDEM_num=as.numeric(catn1)
catn1 = factor(mds_center9661$MARISTAT, labels=(1:length(levels(factor(mds_center9661$MARISTAT)))))
mds_center9661$MARISTAT_num=as.numeric(catn1)

'new clean dataset'
library(tidyr)
library(dplyr)
library(car)
library(DescTools)
library(MASS)
mds_data=mds_center9661%>%
  dplyr::select(EDUC,FEVALAGE,CLINDEM_num, DEP_num, SEX_num, RACE_num, RLDEM_num,MARISTAT_num)

'not related'
mds_cor=cor(mds_data)
vif(lm(CLINDEM_num ~ DEP_num + EDUC + FEVALAGE + SEX_num + RACE_num + RLDEM_num + MARISTAT_num, data = mds_data))
'put all the variables into the model'
'three data processing issues'

'categorical variable'
hist(mds_data$MARISTAT_num)
freq.table=table(mds_data$MARISTAT_num)
pro.table=prop.table(freq.table)
'according to the proportion of each category as well as the their meaning'
'try to make the marrital status into binary variable, married or not'
var_binary_name=c('CLINDEM_num', 'DEP_num', 'SEX_num', 'RACE_num', 'RLDEM_num')
for (i in 1:length(var_binary_name)) {
  mds_data[,var_binary_name[i]]=mds_data[,var_binary_name[i]]-1
}
mds_data$marry=ifelse(mds_data$MARISTAT_num==2,1,0)
hist(mds_data$marry)

# maristat in numeric though having 5 different values?
model.maristat=glm(CLINDEM_num ~ DEP_num + EDUC + FEVALAGE + SEX_num + RACE_num + RLDEM_num + MARISTAT_num, data = mds_data,family = binomial())
summary(model.maristat)
PseudoR2(model.maristat)
model.marry=glm(CLINDEM_num ~ DEP_num + EDUC + FEVALAGE + SEX_num + RACE_num + RLDEM_num + marry, data = mds_data,family = binomial())
summary(model.marry)
PseudoR2(model.marry)
'better goodness of fit'
'become significant predictor'

'continuous variable: whether derive into categorical or dummy variable'
var_continuous_name=c('FEVALAGE', 'EDUC')
par(mfrow=c(1,2))
for (i in 1:length(var_continuous_name)) {
  hist(mds_center9661[[var_continuous_name[i]]],xlab=var_continuous_name[i],main=paste('histogram for',var_continuous_name[i]))
}
summary(mds_data$EDUC)
'split into categorical data: <=12, 13-17, >=18'
'ordinal variables'
mds_data$edu.level=rep(0,nrow(mds_data))
for (i in 1:nrow(mds_data)) {
  if(mds_data$EDUC[i]<=12){
    mds_data$edu.level[i]=1
  }
  if(mds_data$EDUC[i]>=13 && mds_data$EDUC[i]<=17){
    mds_data$edu.level[i]=2
  }
  if(mds_data$EDUC[i]>=18){
    mds_data$edu.level[i]=3
  }
}
prop.table(table(mds_data$edu.level))
hist(mds_data$edu.level)
'split into dummy variables'
mds_data$edu.low=ifelse(mds_data$edu.level==1,1,0)
mds_data$edu.median=ifelse(mds_data$edu.level==2,1,0)
mds_data$edu.high=ifelse(mds_data$edu.level==3,1,0)
model.edu.cate=glm(CLINDEM_num ~ DEP_num + edu.level + FEVALAGE + SEX_num + RACE_num + RLDEM_num + marry, data = mds_data,family = binomial())
summary(model.edu.cate)
PseudoR2(model.edu.cate)
'better result than using the continuous educ'
model.edu.dummy=glm(CLINDEM_num ~ DEP_num + edu.low+edu.median + FEVALAGE + SEX_num + RACE_num + RLDEM_num + marry, data = mds_data,family = binomial())
summary(model.edu.dummy)
PseudoR2(model.edu.dummy)
'lose df, but no obvious better results'
'suggest using categorical educ'

summary(mds_data$FEVALAGE)
'try early onset age: 65'
length(which(mds_data$FEVALAGE<=65))/length(mds_data$FEVALAGE)
mds_data$age.early=ifelse(mds_data$FEVALAGE<=65,1,0)
cor(mds_data$age.early,mds_data$FEVALAGE)
vif(lm(CLINDEM_num ~ DEP_num + edu.level + FEVALAGE+age.early + SEX_num + RACE_num + RLDEM_num + marry, data = mds_data))
    model.age.double=glm(CLINDEM_num ~ DEP_num + edu.level+ FEVALAGE+age.early + SEX_num + RACE_num + RLDEM_num + marry, data = mds_data,family = binomial())
summary(model.age.double)
PseudoR2(model.age.double)
'worse goodness of fit'
model.age.early=glm(CLINDEM_num ~ DEP_num + edu.level+age.early + SEX_num + RACE_num + RLDEM_num + marry, data = mds_data,family = binomial())
summary(model.age.early)
PseudoR2(model.age.early)
'no better; use continuous age here'

'interaction terms: consider interaction with interested variables'
model.interaction=glm(CLINDEM_num ~ DEP_num + edu.level+FEVALAGE + SEX_num + RACE_num + RLDEM_num + marry+ DEP_num:edu.level+DEP_num:FEVALAGE + DEP_num:SEX_num + DEP_num:RACE_num + DEP_num:RLDEM_num + DEP_num:marry, data = mds_data,family = binomial())
summary(model.interaction)
'drop not significant interaction terms'
model.interaction.1=glm(CLINDEM_num ~ DEP_num + edu.level+FEVALAGE + SEX_num + RACE_num + RLDEM_num + marry + DEP_num:SEX_num, data = mds_data,family = binomial())
summary(model.interaction.1)

'try to select with stepwise regression: mainly select the interaction terms'
var_name_adjust=c('edu.level','FEVALAGE' , 'SEX_num' ,'RACE_num' ,'RLDEM_num','marry')
for (i in 1:length(var_name_adjust)) {
  mds_data[[14+i]]=as.vector(unlist(mds_data$DEP_num*mds_data[,var_name_adjust[i]]))
}
# could have name the interaction terms better
names(mds_data)[15:20]=c('edu.level.1','FEVALAGE.1' , 'SEX_num.1' ,'RACE_num.1' ,'RLDEM_num.1','marry.1')
full.model <- glm(CLINDEM_num ~., data = mds_data,family = binomial())
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
'add interaction term with age'
model.interaction.2=glm(CLINDEM_num ~ DEP_num + edu.level+FEVALAGE + SEX_num + RACE_num + RLDEM_num + marry + DEP_num:SEX_num+DEP_num:FEVALAGE, data = mds_data,family = binomial())
summary(model.interaction.2)
'not significant and hard to interpret'

'use model.interaction.1'
'centralize the continuous variable; not influence the inference, but interpretation'
summary(mds_data$FEVALAGE)
(range(mds_data$FEVALAGE)[2]-range(mds_data$FEVALAGE)[1])/sd(mds_data$FEVALAGE)
'6 sd (9.4751) in range; divided by sd better interpretation'
mds_data$FEVALAGE.center=(mds_data$FEVALAGE-mean(mds_data$FEVALAGE))/sd(mds_data$FEVALAGE)
mds_clean_data=mds_data%>%
  dplyr::select(CLINDEM_num,DEP_num , edu.level,FEVALAGE.center , SEX_num , RACE_num , RLDEM_num , marry , SEX_num.1)
model=glm(CLINDEM_num ~ DEP_num + edu.level+FEVALAGE.center + SEX_num + RACE_num + RLDEM_num + marry + SEX_num.1, data = mds_clean_data,family = binomial())
summary(model)
PseudoR2(model)

save(mds_data, file = "/Users/qiuhualiu/Desktop/650_midterm_project/mds_data.RData")
save(mds_clean_data, file = "/Users/qiuhualiu/Desktop/650_midterm_project/mds_clean_data.RData")



'summary the mds_centerdata: no missing data'
summary(mds_center9661)
'check if the ID repeated'
length(unique(mds_center9661$NACCID))
'equal to length of mds data; no repeated ID'
'check the continuous variables:FEVALAGE, EDUC'
'EDUC: valid range (0-36), same unit, assume no measurement error, no external data, no missing data'
'FEVALAGE: valid range (45-107), same unit, assume no measurement error, no external data, no missing data'
'histogram of continuous variables'
var_continuous_name=c('FEVALAGE', 'EDUC')
par(mfrow=c(1,2))
for (i in 1:length(var_continuous_name)) {
  hist(mds_center9661[[var_continuous_name[i]]],xlab=var_continuous_name[i],main=paste('histogram for',var_continuous_name[i]))
}
'not obvious skewness'
'checking the binary data:CLINDEM, DEP, SEX, RACE, RLDEM'
'no value except 0 and 1'
'no obvious imbalance'
var_binary_name=c('CLINDEM_num', 'DEP_num', 'SEX_num', 'RACE_num', 'RLDEM_num')
par(mfrow=c(3,2))
for (i in 1:length(var_binary_name)) {
  hist(mds_center9661[[var_binary_name[i]]],xlab=var_binary_name[i],main=paste('histogram for',var_binary_name[i]))
}
'checking category: MARISTAT'
'all integrety'
'some imbalanced: group 3 and 4'
var_cate_name=c('MARISTAT_num')
par(mfrow=c(2,1))
for (i in 1:length(var_cate_name)) {
  hist(mds_center9661[[var_cate_name[i]]],xlab=var_cate_name[i],main=paste('histogram for',var_cate_name[i]))
}
nrow(mds_center9661[which(mds_center9661$MARISTAT_num==1),])/nrow(mds_center9661)
nrow(mds_center9661[which(mds_center9661$MARISTAT_num==3),])/nrow(mds_center9661)
nrow(mds_center9661[which(mds_center9661$MARISTAT_num==4),])/nrow(mds_center9661)
'both response and interested variables are binary variables'

