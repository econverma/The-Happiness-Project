#load libraries and options

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")


options(digits=3)
#-------------------------------------------------------------------------------------------
# Data Wrangling
#load happiness report data

#datasets

#https://raw.githubusercontent.com/econverma/The-Happiness-Project/master/WHR20.csv
#https://raw.githubusercontent.com/econverma/The-Happiness-Project/master/gini%20updated.csv

file<-read_csv(url("https://raw.githubusercontent.com/econverma/The-Happiness-Project/master/WHR20.csv"))


#select columns
data<-file%>%
  select(c(`Country name`,`Regional indicator`,`Ladder score`,`Logged GDP per capita`,`Social support`,`Healthy life expectancy`,`Freedom to make life choices`,Generosity,`Perceptions of corruption`,`Dystopia + residual`))

#rename columns 
colnames(data)<-c("country","region","score","log_pcgdp","soc_support","life_exp","freedom","generosity","corruption","dys_res")
#explore data set

head(data) %>% knitr::kable()

#changing country and region from charaters to factors

data$country<-as.factor(data$country)
data$region<-as.factor(data$region)


#load the income inequailty data from World Bank and combine the two data sets

gini_updated<-read_csv(url("https://raw.githubusercontent.com/econverma/The-Happiness-Project/master/gini%20updated.csv"))
ginidata<-data%>%left_join(gini_updated,by="country")%>%as.data.frame()

ginidata<-ginidata%>%mutate(gini=Gini/100)

ginidata<-ginidata%>%select("country","region","score","log_pcgdp","soc_support","life_exp","freedom","generosity","corruption","dys_res","gini")

#examine the data and remove missing values

head(ginidata) %>% knitr::kable()
summary(ginidata)

ginidata<-ginidata%>%remove_missing()

#changing country and region from charaters to factors

ginidata$country<-as.factor(ginidata$country)
ginidata$region<-as.factor(ginidata$region)

dim(ginidata)

# Table of variables and what they represent
Variables<-c("Country","Region","Score","Log_pcgdp","Soc_support","Life_exp","Freedom","Generosity","Corruption","Dys_res","Gini Coefficient")

Explanation<-c("Countries where the surveys were conducted",
               "Region of the world", 
               " Overall happiness score from 0 to 10",
               " Per-capita GDP in constant 2011 dollars in purchasing parity parity terms",
               "National average of survey response on presence or absence of social support",
               "Life expectancy at birth", 
               "National average of survey response on presence or absence of individual freedom",
               "National average of survey response to donating to charity",
               "National average of survey response to widespread corruption in government or business",
               "Lowest benchmark value plus residual",
               "Income inequality within a country")

variable_table <- cbind(data.frame(Variables),data.frame( Explanation))
names(variable_table) <- c("Variable", "Explanation")
variable_table %>% knitr::kable()

#====================================================================================


##Data Visualization


# 1. Create a correlation plot among all the variables



cor = cor(ginidata[c(3:11)], method = "spearman")
corrplot::corrplot(cor,method="number",type="lower")


#2. Scatterplot by countries by variables

#economy and happiness
ggplot(data, aes(x=log_pcgdp, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Per-capita GDP')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")

#Health and Happiness
ggplot(data, aes(x=life_exp, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Life Expectancy')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")

#Family and friends support and happiness
ggplot(data, aes(x=soc_support, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Social Support')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")


#Freedom and Happiness
ggplot(data, aes(x=freedom, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Freedom')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")

#Generosity and Happiness
ggplot(data, aes(x=generosity, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Generosity')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")

#Happiness and perception of corruption
ggplot(data, aes(x=corruption, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Corruption')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")

#Happiness and Income Inequality
ggplot(ginidata, aes(x=gini, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Income Inequality')+
  geom_text(aes(color=region),size=3)+theme(legend.position = "none")

#3. Scatter-plots of Income Ineqaulty by region and countries: FACET MODEL

ggplot(ginidata, aes(x=gini, y=score))+
  geom_point()+ylab('Happiness Score')+xlab('Income Inequality')+
  geom_text(aes(color=region,label=country),size=3)+theme(legend.position = "none")+
  facet_grid(~abbreviate(region))


# 4. aggregate by region and create box-plots by income inequality and happiness scores

#By happiness scores
boxplot_score<-ggplot(ginidata, aes(x=region, y=score,fill=region))+
  geom_boxplot()+ylab('Happiness Score')+xlab('Region')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none")

#By income inequality

boxplot_gini<-ggplot(ginidata, aes(x=region, y=gini,fill=region))+
  geom_boxplot()+ylab('Income Inequality')+xlab('Region')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none")

#arrange the box-plots side-by-side
ggarrange(boxplot_score,boxplot_gini,ncol=2)


##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##METHOD AND ANALYSIS


##create training and testing set with p=0.5


#remove country,region and dys-res from the data
hdata<-ginidata%>%select("score","log_pcgdp","soc_support","life_exp","freedom","generosity","corruption","gini")


set.seed(1, sample.kind="Rounding")


#creat training and testing set with p=0.5 since the data set is small.

test_index <- createDataPartition(y = hdata$score, times = 1, p = 0.5, list = FALSE)
train_set <- hdata[-test_index,]
test_set <- hdata[test_index,]

# Which Model will be appropriate?
#check for multi-variate normal distribution
sdc_score<-ggplot(ginidata,aes(x=score))+geom_density()
sdc_pcgdp<-ggplot(ginidata,aes(x=log_pcgdp))+geom_density()
sdc_ss<-ggplot(ginidata,aes(x=soc_support))+geom_density()
sdc_life<-ggplot(ginidata,aes(x=life_exp))+geom_density()
sdc_freedom<-ggplot(ginidata,aes(x=freedom))+geom_density()
sdc_generosity<-ggplot(ginidata,aes(x=generosity))+geom_density()
sdc_corruption<-ggplot(ginidata,aes(x=corruption))+geom_density()
sdc_gini<-ggplot(ginidata,aes(x=gini))+geom_density()

ggarrange(sdc_score,sdc_pcgdp,sdc_ss,sdc_life,sdc_freedom,sdc_generosity,sdc_corruption,sdc_gini,nrow=4,ncol=2)

#MODEL 1: GLM Model
#Fitting Generalized Linear Model Regression to the training set

glm_method = glm(formula = score ~ .,
                  data = train_set)

summary(glm_method)


#Predicting on the Test set 
p_hat_glm = predict(glm_method, newdata = test_set)


RMSE_glm<-RMSE(p_hat_glm,test_set$score)

#Creation of a table to store our results
rmse_table <- data_frame(Model= "GLM Model", RMSE= RMSE_glm)

rmse_table %>% knitr::kable()

#Model 2: Decision Tree Model

# Fitting Decision Tree Regression to the dataset

#finding the best tune
set.seed(1)
train_rpart <- train(score ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = train_set)
ggplot(train_rpart)
train_rpart$bestTune


dt_method = rpart(formula = score ~ .,
                     data = train_set,
                     control = rpart.control(cp=0.0229,minsplit = 10))


# Predicting the Test set results
p_hat_dt = predict(dt_method, newdata = test_set)


RMSE_dt<-RMSE(p_hat_dt,test_set$score)

rmse_table <- bind_rows(rmse_table, data_frame(Model= "Decision Tree Model", RMSE=RMSE_dt))

rmse_table %>% knitr::kable()

# Plotting the tree
plot(dt_method, margin = 0.1)
text(dt_method, cex = 0.75)

# Model 3. Random Forest Model

#Fitting the Random Forest Model

rf_method<-randomForest(score~.,data=train_set)

plot(rf_method)#to check the number of trees that will reduce the rmse

imp<-importance(rf_method)#to find out the most important explainatory variables

imp

varImpPlot(rf_method)#plot the variables according to the importance

p_hat_rf = predict(rf_method, newdata = test_set)

RMSE_rf<-RMSE(p_hat_rf,test_set$score)

rmse_table <- bind_rows(rmse_table, data_frame(Model= "Random Forest Model", RMSE=RMSE_rf))

rmse_table %>% knitr::kable()
#--------------------------------------------------------------------


# Final results table

rmse_table %>% knitr::kable()


