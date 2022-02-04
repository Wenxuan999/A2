setwd("/Users/wenxuan/Desktop/613/A1/Data")
install.packages("plm")
library(dplyr)
library(plm)
install.packages("Tidyverse")
library(tidyverse)

#=============================================
# Exercise 1: 
#=============================================
#1 Calculate the correlation between Y and X.
datind2009=read.csv("datind2009.csv")
datind2009_1=datind2009[complete.cases(datind2009$wage), ]
a=rep(1,20232)
b=cbind(a,datind2009_1$age)
cor(datind2009_1$wage,datind2009_1$age)

#2 Calculate the coefficients on this regression
X=b
Y=datind2009_1$wage
result=solve(t(X)%*%X)%*%t(X)%*%Y
result

#3.1 Using the standard formulas of the OLS.
#Using the standard formulas of the OLS.

df=length(datind2009_1$age)-2
yhat=result[1,1] + result[2,1]*datind2009_1$age
error_term=datind2009_1$wage-yhat
theta2=sum((datind2009_1$wage-yhat)^2)/df
se1=sqrt(theta2*solve(t(X)%*%X)[1,1])
se2=sqrt(theta2*solve(t(X)%*%X)[2,2])
se1
se2

#3.2 Using bootstrap with 49 and 499 replications respectively. Comment on the difference between the two strategies.

for (i in 1:49) {
sample1=sample(1:20232,size=20232,replace=T)
data_choose =datind2009_1[sample1,]
df=length(datind2009_1$age)-2

X=cbind(rep(1,20232),data_choose$age)
Y=data_choose$wage
beta=solve(t(X)%*%X)%*%t(X)%*%Y
yhat=beta[1,1] + beta[2,1]*data_choose$age
error_term=data_choose$wage-yhat
theta2=sum((data_choose$wage-yhat)^2)/df
se1[i]=sqrt(theta2*solve(t(X)%*%X)[1,1])
se2[i]=sqrt(theta2*solve(t(X)%*%X)[2,2])
}

se49_1=mean(se1)
se49_1
se49_2=mean(se2)
se49_2

for (i in 1:499) {
  sample1=sample(1:20232,size=20232,replace=T)
  data_choose =datind2009_1[sample1,]
  df=length(datind2009_1$age)-2
  
  X=cbind(rep(1,20232),data_choose$age)
  Y=data_choose$wage
  beta=solve(t(X)%*%X)%*%t(X)%*%Y
  yhat=beta[1,1] + beta[2,1]*data_choose$age
  error_term=data_choose$wage-yhat
  theta2=sum((data_choose$wage-yhat)^2)/df
  se1[i]=sqrt(theta2*solve(t(X)%*%X)[1,1])
  se2[i]=sqrt(theta2*solve(t(X)%*%X)[2,2])
}

se499_1=mean(se1)
se499_1
se499_2=mean(se2)
se499_2


#the method using 499 is more accurate since it repeat more times

#=============================================
# Exercise 2: 
#=============================================
#1.Plot the wage of each age group across years. Is there a trend?
datind2005=read.csv("datind2005.csv")
datind2006=read.csv("datind2006.csv")
datind2007=read.csv("datind2007.csv")
datind2008=read.csv("datind2008.csv")
datind2009=read.csv("datind2009.csv")
datind2010=read.csv("datind2010.csv")
datind2011=read.csv("datind2011.csv")
datind2012=read.csv("datind2012.csv")
datind2013=read.csv("datind2013.csv")
datind2014=read.csv("datind2014.csv")
datind2015=read.csv("datind2015.csv")
datind2016=read.csv("datind2016.csv")
datind2017=read.csv("datind2017.csv")
datind2018=read.csv("datind2018.csv")
Append1=rbind(datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,datind2014,datind2015,datind2016,datind2017,datind2018)

Append1=Append1 %>%mutate(ag = 0,
ag = ifelse(18 <= age & 25 >= age, 1, ag),
ag = ifelse(26 <= age & 30 >= age, 2, ag),
ag = ifelse(31 <= age & 35 >= age, 3, ag),
ag = ifelse(36 <= age & 40 >= age, 4, ag),
ag = ifelse(41 <= age & 45 >= age, 5, ag),
ag = ifelse(46 <= age & 50 >= age, 6, ag),
ag = ifelse(51 <= age & 55 >= age, 7, ag),
ag = ifelse(56 <= age & 60 >= age, 8, ag),
ag = ifelse(60 < age, 9, ag))
Append1
#2.Plot the wage of each age group across years. Is there a trend?
Append1=Append1[complete.cases(Append1$wage), ]
#group 18-25
group1=Append1%>%filter(Append1$ag=="1")
group1=group1%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic1=ggplot(group1,aes(x=year,y=mean) )+geom_line()
pic1
#group 26-30
group2=Append1%>%filter(Append1$ag=="2")
group2=group2%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic2=ggplot(group2,aes(x=year,y=mean) )+geom_line()
pic2
#group 31-35
group3=Append1%>%filter(Append1$ag=="3")
group3=group3%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic3=ggplot(group3,aes(x=year,y=mean) )+geom_line()
pic3
#group 36-40
group4=Append1%>%filter(Append1$ag=="4")
group4=group4%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic4=ggplot(group4,aes(x=year,y=mean) )+geom_line()
pic4
#group 41-45
group5=Append1%>%filter(Append1$ag=="5")
group5=group5%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic5=ggplot(group5,aes(x=year,y=mean) )+geom_line()
pic5
#group 46-50
group6=Append1%>%filter(Append1$ag=="6")
group6=group6%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic6=ggplot(group6,aes(x=year,y=mean) )+geom_line()
pic6
#group 51-55
group7=Append1%>%filter(Append1$ag=="7")
group7=group7%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic7=ggplot(group7,aes(x=year,y=mean) )+geom_line()
pic7
#group 56-60
group8=Append1%>%filter(Append1$ag=="8")
group8=group8%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic8=ggplot(group8,aes(x=year,y=mean) )+geom_line()
pic8
#group 60+
group9=Append1%>%filter(Append1$ag=="9")
group9=group9%>%
  group_by(year) %>%
  mutate(mean= mean(wage))
pic9=ggplot(group9,aes(x=year,y=mean) )+geom_line()
pic9
#trend:the salary tends to increase over years, but when people get older, the mean salary is getting smaller



#After including a time fixed effect, how do the estimated coefficients change?

reg2=lm(Append1$wage ~ Append1$age)
summary(reg2)
reg3= plm(Append1$wage ~ Append1$age, data=Append1,index=c("year"), model="within")
summary(reg3)
#from -182.4896 to -186.8793

#=============================================
# Exercise 3: 
#=============================================
#1Exclude all individuals who are inactive.
datind2007=read.csv("datind2007.csv")
datind2007_1=datind2007[complete.cases(datind2007$empstat), ]
datind2007_2=filter(datind2007_1,datind2007_1$empstat!="Inactive")
datind2007_2

#2Write a function that returns the likelihood of the probit of being employed.
datind2007_2$status = ifelse(datind2007_2$empstat == "Employed", 1, 0)
datind2007_3=datind2007_2[complete.cases(datind2007_2$empstat), ]

flike = function(par,x1,yvar)
{
  xbeta           = par[1] + par[2]*x1 
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

reg4 = glm(datind2007_3$status~datind2007_3$age,family = binomial(link = "probit"))
summary(reg4)
test_coefficients = reg4$coefficients
x=datind2007_3$age
y = datind2007_3$status
like(reg4$coefficients,x,y)
logLik(reg4)





#3
#-0.0678642 means that when age increases, the labor market participation will decrease.
opt1  = optim(reg4$coefficients,fn=flike,method="BFGS",control=list(trace=5,REPORT=1,maxit=10000),x=datind2007_3$age,y=datind2007_3$status,hessian=TRUE)
opt1$par
#4
reg5 = glm(datind2007_3$status~datind2007_3$age+datind2007_3$wage,family = binomial(link = "probit"))


#=============================================
# Exercise 4: 
#=============================================
#1
datind2005=read.csv("datind2005.csv")
datind2006=read.csv("datind2006.csv")
datind2007=read.csv("datind2007.csv")
datind2008=read.csv("datind2008.csv")
datind2009=read.csv("datind2009.csv")
datind2010=read.csv("datind2010.csv")
datind2011=read.csv("datind2011.csv")
datind2012=read.csv("datind2012.csv")
datind2013=read.csv("datind2013.csv")
datind2014=read.csv("datind2014.csv")
datind2015=read.csv("datind2015.csv")

Append2=rbind(datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,datind2014,datind2015)
Append2_1=Append2[complete.cases(Append2$empstat), ]
Append2_2=filter(Append2_1,Append2_1$empstat!="Inactive")
Append2_2$status = ifelse(Append2_2$empstat == "Employed", 1, 0)
#creat the fixed effect variables
Append2_2$y2006=ifelse(Append2_2$year == "2006", 1, 0)
Append2_2$y2007=ifelse(Append2_2$year == "2007", 1, 0)
Append2_2$y2008=ifelse(Append2_2$year == "2008", 1, 0)
Append2_2$y2009=ifelse(Append2_2$year == "2009", 1, 0)
Append2_2$y2010=ifelse(Append2_2$year == "2010", 1, 0)
Append2_2$y2011=ifelse(Append2_2$year == "2011", 1, 0)
Append2_2$y2012=ifelse(Append2_2$year == "2012", 1, 0)
Append2_2$y2013=ifelse(Append2_2$year == "2013", 1, 0)
Append2_2$y2014=ifelse(Append2_2$year == "2014", 1, 0)
Append2_2$y2015=ifelse(Append2_2$year == "2015", 1, 0)
Append2_2

#2  Write and optimize the probit, logit, and the linear probability models.
#Probit
flike = function(par,x,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,yvar)
{
  xbeta           = par[1] + par[2]*x + par[3]*x1 + par[4]*x2+par[5]*x3+par[6]*x4+par[7]*x5+par[8]*x6+par[9]*x7+par[10]*x8+par[11]*x9+par[12]*x10
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

#Optimize
res1      = optim(runif(12,min=-0.1,max=0),fn=flike,method="BFGS",control=list(trace=5,REPORT=1,maxit=100000),x=Append2_2$age,x1=Append2_2$y2006,x2=Append2_2$y2007,x3=Append2_2$y2008, x4=Append2_2$y2009,x5=Append2_2$y2010,x6=Append2_2$y2011,x7=Append2_2$y2012,x8=Append2_2$y2013,x9=Append2_2$y2014,x10=Append2_2$y2015,yvar=Append2_2$status,hessian=TRUE)
res1$par[2]
#compare with the glm probit function, we can find that the coefficient is correct
reg5= glm(Append2_2$status ~ Append2_2$age+Append2_2$y2006+Append2_2$y2007+Append2_2$y2008+Append2_2$y2009+Append2_2$y2010+Append2_2$y2011+Append2_2$y2012+Append2_2$y2013+Append2_2$y2014+Append2_2$y2015, data=Append2_2,family = binomial(link = "probit"))
reg5$coefficients[2]

#Logit
flike2 = function(par,x,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,yvar)
{
  xbeta           = par[1] + par[2]*x + par[3]*x1 + par[4]*x2+par[5]*x3+par[6]*x4+par[7]*x5+par[8]*x6+par[9]*x7+par[10]*x8+par[11]*x9+par[12]*x10
  pr              = exp(xbeta)/(1+exp(xbeta)) 
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

start2    = runif(12,min=-0.1,max=0)
res2      = optim(start2,fn=flike2,method="BFGS",control=list(trace=5,REPORT=1,maxit=100000),x=Append2_2$age,x1=Append2_2$y2006,x2=Append2_2$y2007,x3=Append2_2$y2008, x4=Append2_2$y2009,x5=Append2_2$y2010,x6=Append2_2$y2011,x7=Append2_2$y2012,x8=Append2_2$y2013,x9=Append2_2$y2014,x10=Append2_2$y2015,yvar=Append2_2$status,hessian=TRUE)
res2$par[2]
#compare with the glm logit function, we can find that the coefficient is correct
reg6= glm(Append2_2$status ~ Append2_2$age+Append2_2$y2006+Append2_2$y2007+Append2_2$y2008+Append2_2$y2009+Append2_2$y2010+Append2_2$y2011+Append2_2$y2012+Append2_2$y2013+Append2_2$y2014+Append2_2$y2015, data=Append2_2,family =binomial)
reg6$coefficients[2]

#linear model
flike3 = function(par,x,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,yvar)
{
  y_hat          = par[1] + par[2]*x + par[3]*x1 + par[4]*x2+par[5]*x3+par[6]*x4+par[7]*x5+par[8]*x6+par[9]*x7+par[10]*x8+par[11]*x9+par[12]*x10
  yvar = as.numeric(y_hat)
  error                  = Append2_2$status - yvar
  return(sum(error^2))
}

start3=runif(12,min=-0.1,max=0)
res3 = optim(start3,fn=flike3,method="BFGS",control=list(trace=5,maxit=100000),x=Append2_2$age,x1=Append2_2$y2006,x2=Append2_2$y2007,x3=Append2_2$y2008, x4=Append2_2$y2009,x5=Append2_2$y2010,x6=Append2_2$y2011,x7=Append2_2$y2012,x8=Append2_2$y2013,x9=Append2_2$y2014,x10=Append2_2$y2015,yvar=Append2_2$status)
res3$par[2]
#compare with the linear model function, we can find that the coefficient is correct
reg7= lm(Append2_2$status ~ Append2_2$age+Append2_2$y2006+Append2_2$y2007+Append2_2$y2008+Append2_2$y2009+Append2_2$y2010+Append2_2$y2011+Append2_2$y2012+Append2_2$y2013+Append2_2$y2014+Append2_2$y2015, data=Append2_2)
reg7$coefficients[2]





#3 Interpret and compare the estimated coefficients. How significant are they?
#Calculate the standard error to get the T value

#For linear model
# Calculate the correlation between Y and X.
a=rep(1,190296)
b=cbind(a,Append2_2$age,Append2_2$y2006,Append2_2$y2007,Append2_2$y2008,Append2_2$y2009,Append2_2$y2010,Append2_2$y2011,Append2_2$y2012,Append2_2$y2013,Append2_2$y2014,Append2_2$y2015)
X=b
Y=Append2_2$status
result=solve(t(X)%*%X)%*%t(X)%*%Y
result
df=length(Append2_2$age)-12
yhat=result[1,1] + result[2,1]*Append2_2$age+result[3,1]*Append2_2$y2006+result[4,1]*Append2_2$y2007+result[5,1]*Append2_2$y2008+result[6,1]*Append2_2$y2009+result[7,1]*Append2_2$y2010+result[8,1]*Append2_2$y2011+result[9,1]*Append2_2$y2012+result[10,1]*Append2_2$y2013+result[11,1]*Append2_2$y2014+result[12,1]*Append2_2$y2015
error_term=Append2_2$status-yhat
theta2=sum((Append2_2$status-yhat)^2)/df
se1=sqrt(theta2*solve(t(X)%*%X)[1,1])
se2=sqrt(theta2*solve(t(X)%*%X)[2,2])
se1
se2
T_linear=result[2,1]/se2
T_linear
#which is significant at 1% level

#For Logit model
Se_l= sqrt(solve(res2$hessian))
Se_l[2,2]
T_logit=res2$par[2]/Se_l[2,2]
T_logit
#which is significant at 1% level

#For Probit model
Se_P= sqrt(solve(res1$hessian))
Se_P[2,2]
T_probit=res1$par[2]/Se_P[2,2]
T_probit
#which is significant at 1% level

#=============================================
# Exercise 5: 
#=============================================
#1.Compute the marginal effect of the previous probit and logit models.
pdf1=mean(dnorm(predict(reg5, type = "link")))
marginal.effects1=pdf1*reg5$coefficients[2]
marginal.effects1

pdf2=mean(dlogis(predict(reg6, type = "link")))
marginal.effects2=pdf2*reg6$coefficients[2]
marginal.effects2

#2.Construct the standard errors of the marginal effects. Hint: Boostrap may be the easiest way.
#Probit
boot=40
bootvals <- matrix(rep(NA,boot*12), nrow=boot)
set.seed(111)
for(i in 1:boot){
  samp1 <- Append2_2[sample(1:dim(Append2_2)[1],replace=T,dim(Append2_2)[1]),]
  res4=optim(runif(12,min=-0.1,max=0),fn=flike,method="BFGS",control=list(trace=5,maxit=100000),x=samp1$age,x1=samp1$y2006,x2=samp1$y2007,x3=samp1$y2008, x4=samp1$y2009,x5=samp1$y2010,x6=samp1$y2011,x7=samp1$y2012,x8=samp1$y2013,x9=samp1$y2014,x10=samp1$y2015,yvar=samp1$status)
  yhat=res4$par[1] + res4$par[2]*samp1$age+res4$par[3]*samp1$y2006+res4$par[4]*samp1$y2007+res4$par[5]*samp1$y2008+res4$par[6]*samp1$y2009+res4$par[7]*samp1$y2010+res4$par[8]*samp1$y2011+res4$par[9]*samp1$y2012+res4$par[10]*samp1$y2013+res4$par[11]*samp1$y2014+res4$par[12]*samp1$y2015
  pdf1=mean(dnorm(yhat))
  bootvals[i,] = pdf1*res4$par
  }

sd(bootvals[,2] )



#Logit
boot=40
bootvals1 <- matrix(rep(NA,boot*12), nrow=boot)
set.seed(111)
for(i in 1:boot){
  samp2 <- Append2_2[sample(1:dim(Append2_2)[1],replace=T,dim(Append2_2)[1]),]
  res5=optim(runif(12,min=-0.1,max=0),fn=flike2,method="BFGS",control=list(trace=5,maxit=100000),x=samp2$age,x1=samp2$y2006,x2=samp2$y2007,x3=samp2$y2008, x4=samp2$y2009,x5=samp2$y2010,x6=samp2$y2011,x7=samp2$y2012,x8=samp2$y2013,x9=samp2$y2014,x10=samp2$y2015,yvar=samp2$status)
  yhat=res5$par[1] + res5$par[2]*samp2$age+res5$par[3]*samp2$y2006+res5$par[4]*samp2$y2007+res5$par[5]*samp2$y2008+res5$par[6]*samp2$y2009+res5$par[7]*samp2$y2010+res5$par[8]*samp2$y2011+res5$par[9]*samp2$y2012+res5$par[10]*samp2$y2013+res5$par[11]*samp2$y2014+res5$par[12]*samp2$y2015
  pdf2=mean(dlogis(yhat))
  bootvals1[i,] = pdf2*res5$par
}

sd(bootvals1[,2] )


