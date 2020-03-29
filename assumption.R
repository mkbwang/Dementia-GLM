model=glm(CLINDEM_num ~ DEP_num + edu.level+FEVALAGE.center + SEX_num + RACE_num + RLDEM_num + marry + SEX_num.1, data = mds_clean_data,family = binomial())
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
