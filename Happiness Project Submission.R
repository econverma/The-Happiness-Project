#load libraries
library(tidyverse)
library(readr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(caret)
library(corrplot)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tinytex)

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
class(data)
str(data)

#country variable include countries where the surveys were conducted.
#regions are the geographical regions where countries are located.
#score is the overall happiness score from 0 (least happy) to 10(most happy)
#log_pcgdp are the World Bank per-capita GDP in constant 2011 dollars in Purchasing parity parity terms
#soc_support is national average to the question"If you were in trouble, can you count on friends/family to help". Ansswer is binary 0 or 1.
#life_exp is based on WHO data on life expectancy at birth
#freedom is national average to the question"Are you satisfied or dissatisfied with the freedom to choose what you do in your life" Answers are binary.
#generosity is the residual of regressing national average of responses to "have you donated to a charity in the past month?" to GDP per capita.
#corruption is average of responses to "Is their widespread corruption in Government/Business?"
#dys_res is the value of 1.97 as the lowest benchmark value + any residual


#changing country and region from charaters to factors

data$country<-as.factor(data$country)
data$region<-as.factor(data$region)

#load the income inequailty data from World Bank and combine the two data sets

gini_updated<-read_csv(url("https://raw.githubusercontent.com/econverma/The-Happiness-Project/master/gini%20updated.csv"))
ginidata<-data%>%left_join(gini_updated,by="country")%>%as.data.frame()

ginidata<-ginidata%>%mutate(gini=Gini/100)

ginidata<-ginidata%>%select("country","region","score","log_pcgdp","soc_support","life_exp","freedom","generosity","corruption","dys_res","gini")

#examine the data and remove missing values

summary(ginidata)

ginidata<-ginidata%>%remove_missing()

#changing country and region from charaters to factors

ginidata$country<-as.factor(ginidata$country)
ginidata$region<-as.factor(ginidata$region)

dim(ginidata)

#====================================================================================


##Data Visualization


# 1. Create a correlation plot among all the variables



cor = cor(ginidata[c(3:11)], method = "spearman")
corrplot::corrplot(cor,method="number")


#2. Scatterplot by countries by variables

#economy and happiness
ggplot(data, aes(x=log_pcgdp, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Per=capita GDP')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")

#Health and Happiness
ggplot(data, aes(x=life_exp, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Life Expectancy')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")

#Family and friends support and happiness
ggplot(data, aes(x=soc_support, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Social Support')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")


#Freedom and Happiness
ggplot(data, aes(x=freedom, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Freedom')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")

#Generosity and Happiness
ggplot(data, aes(x=generosity, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Generosity')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")

#Happiness and perception of corruption
ggplot(data, aes(x=corruption, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Corruption')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")

#Happiness and Income Inequality
ggplot(ginidata, aes(x=gini, y=score, label=country))+
  geom_point()+
  geom_smooth()+ylab('Happiness Score')+xlab('Income Inequality')+
  geom_text(aes(color=country),size=2)+theme(legend.position = "none")

#3. Scatter-plots of Income Ineqaulty by region and countries: FACET MODEL

ggplot(ginidata, aes(x=gini, y=score))+
  geom_point()+ylab('Happiness Score')+xlab('Income Inequality')+
  geom_text(aes(color=country,label=country),size=2)+theme(legend.position = "none")+
  facet_grid(~abbreviate(region))


# 4. aggregate by region and create box-plots by income inequality and happiness scores

#By happiness scores
g1<-ggplot(ginidata, aes(x=region, y=score,fill=region))+
  geom_boxplot()+ylab('Happiness Score')+xlab('Region')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none")

#By income inequality

g2<-ggplot(ginidata, aes(x=region, y=gini,fill=region))+
  geom_boxplot()+ylab('Income Inequality')+xlab('Region')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  theme(legend.position = "none")

#arrange the box-plots side-by-side
ggarrange(g1,g2,ncol=2)


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
h1<-ggplot(ginidata,aes(x=score))+geom_density()
h2<-ggplot(ginidata,aes(x=log_pcgdp))+geom_density()
h3<-ggplot(ginidata,aes(x=soc_support))+geom_density()
h4<-ggplot(ginidata,aes(x=life_exp))+geom_density()
h5<-ggplot(ginidata,aes(x=freedom))+geom_density()
h6<-ggplot(ginidata,aes(x=generosity))+geom_density()
h7<-ggplot(ginidata,aes(x=corruption))+geom_density()
h8<-ggplot(ginidata,aes(x=gini))+geom_density()

ggarrange(h1,h2,h3,h4,h5,h6,h7,h8,nrow=4,ncol=2)

#MODEL 1: GLM Model
#Fitting Generalized Linear Model Regression to the training set

glm_method = glm(formula = score ~ .,
                  data = train_set)

summary(glm_method)


#Predicting the Test set results
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

plot(rf_method)

p_hat_rf = predict(rf_method, newdata = test_set)

RMSE_rf<-RMSE(p_hat_rf,test_set$score)

rmse_table <- bind_rows(rmse_table, data_frame(Model= "Random Forest Model", RMSE=RMSE_rf))

rmse_table %>% knitr::kable()
#--------------------------------------------------------------------


# Final results table

rmse_table %>% knitr::kable()

