library(ggplot2)


'derive the model'
mds_clean_data$edu.level=as.factor(mds_clean_data$edu.level)
model=glm(CLINDEM_num ~ DEP_num + edu.level+FEVALAGE.center + SEX_num + RACE_num + RLDEM_num + marry + SEX_num.1, data = mds_clean_data,family = binomial())
sum=summary(model)

'get the estimated parameter'
exp(sum$coefficients[,1])
mean(mds_center9661$FEVALAGE)
coeff=sum$coefficients[,1]
se=sum$coefficients[,2]
est.table=cbind(Estimate=coeff,lower.bound.95=coeff-1.96*se,upper.bound.95=coeff+1.96*se)
exp.table=exp(est.table)

'see the significance of wald test'
exp_table1 = as.data.frame(exp.table[-1,]) %>% mutate(vars = rownames(.)) 
ggplot(exp_table1,aes(x=vars, y=Estimate, ymin=lower.bound.95, ymax=upper.bound.95)) + geom_pointrange(size=1) +geom_hline(yintercept=1, lty=2) +
  coord_flip() +ggtitle("Main Effects for Model") + xlab("Variable") + ylab("Odds Ratios") +
  theme(plot.title = element_text(size=12,hjust=0.5), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12), axis.text.x = element_text(size=12), axis.text.y = element_text(size=12))
# Bonferroni Correction
sign=cbind(p.value.Wald.test=sum$coefficients[,4],con.Wald.test=ifelse(sum$coefficients[,4]<0.05,1,0))

'test the association between depression and dementia'
null.model.1=glm(CLINDEM_num ~ edu.level+FEVALAGE.center + SEX_num + RACE_num + RLDEM_num + marry , data = mds_clean_data,family = binomial())
anova(model, null.model.1,test="LRT")

'test the significance of confounders'
null.model.2=glm(CLINDEM_num ~ edu.level, data = mds_clean_data,family = binomial())
anova(model, null.model.2,test="LRT")

'check the assumption: linear association between logit of probability and predictors'
probabilities = predict(model, type = "response")
predictors <- colnames(mds_clean_data)
# Bind the logit and tidying the data for plot
mds_clean_data$edu.level=as.numeric(mds_clean_data$edu.level)
mydata <- mds_clean_data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
'accept from the insignificance variable: RLDEM; other variables: quite linear association'

