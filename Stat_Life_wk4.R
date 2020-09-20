#QQplot Exercises
#Q1

load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
par("mar")
par(mar=c(1,1,1,1))
for (i in 1:9) {
  x <- dat[,i]
  qqnorm(x, xlab="quantiles", main=paste0("Col No=",i))
  qqline(x)
  }
hist(dat[,4])

#Q2
hist(dat[,9])

#Boxplot Exercises

head(InsectSprays)
names(InsectSprays)

#Q1

boxplot(InsectSprays$count ~ InsectSprays$spray) 

#Q2
library(tidyverse)
library(rafalib)
install.packages("UsingR")
data(nym.2002, package="UsingR")

boxplot(nym.2002$time ~ nym.2002$gender) 
time.male <- nym.2002 %>% filter(gender=="Male") %>% select(time) %>% unlist
time.female <- nym.2002 %>% filter(gender=="Female") %>% select(time) %>% unlist
mypar(1,2)
hist(time.male)
hist(time.female)
mean(time.female) -  mean(time.male)
#or
mypar(1,3)
males <- filter(nym.2002, gender=="Male") %>% select(time) %>% unlist
females <- filter(nym.2002, gender=="Female") %>% select(time) %>% unlist
boxplot(females, males)
hist(females,xlim=c(range( nym.2002$time)))
hist(males,xlim=c(range( nym.2002$time)))

#Scatterplot Exercises

#Q1
males <- filter(nym.2002, gender=="Male") %>% select(time, age)
females <- filter(nym.2002, gender=="Female") %>% select(time, age) 
cor(males$age, males$time)

#Q2
cor(females$age, females$time)

#Q3
groups <- split(nym.2002$time,round(nym.2002$age)) 
boxplot(groups)

#or
library(rafalib)
mypar(2,2)
plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5) * 5
boxplot(females$time~group)
group <- floor(males$age/5) * 5
boxplot(males$time~group)

#Symmetry of Log
time = sort(nym.2002$time)

#Q1
min(time) / median(time)

#Q2
max(time) / median(time)

#Median, MAD, Spearman Exercises
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)

#Q1
mean(c(chick$weight.4,3000))/mean(chick$weight.4)

#Q2
median(c(chick$weight.4,3000))/median(chick$weight.4)

#Q3
sd(c(chick$weight.4,3000))/sd(chick$weight.4)

#Q4
mad(c(chick$weight.4,3000))/mad(chick$weight.4)

#Q5
cor(c(chick$weight.4, 3000), c(chick$weight.21,3000))/cor(chick$weight.4, chick$weight.21)

#Wilcoxon Test Exercises

#Q1
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
chick <- na.omit(chick)
head(chick)
x <- chick %>% filter(Diet == "1")%>% select(weight.4) %>% unlist
y <- chick %>% filter(Diet == "4")%>% select(weight.4) %>% unlist
t.test(x,y)
wilcox.test(x,y)
#Q1
t.test(c(x,200),y)

#Q2
wilcox.test(c(x,200),y)

#Q3
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

#Q4
wilcox.test(1:3,4:6)

#Q5
wilcox.test(1:3,c(400,500,600))