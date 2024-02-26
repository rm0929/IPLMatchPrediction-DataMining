#importing dataset
ipl_data<-read.csv('D:\\College\\Sem 5\\DM\\Project\\IPL Matches 2008-2020.csv')
View(ipl_data)

#preprocessing data
ipl_data<-ipl_data[-17]
ipl_data<-ipl_data[-16]

head(ipl_data)

DL<-ifelse(is.na(ipl_data$method), "N", "Y")
DL
ipl_data<-data.frame(ipl_data,DL)

View(ipl_data)

ipl_data<-ipl_data[-15]
ipl_data$city<-ifelse(is.na(ipl_data$city),"Dubai",ipl_data$city)

ipl_data$result_margin<-ifelse(ipl_data$result=="tie",0,ipl_data$result_margin)

unique(ipl_data$winner)

ipl_data$team1[ipl_data$team1=="Rising Pune Supergiant"]<-"Rising Pune Supergiants"
ipl_data$team2[ipl_data$team2=="Rising Pune Supergiant"]<-"Rising Pune Supergiants"
ipl_data$winner[ipl_data$winner=="Rising Pune Supergiant"]<-"Rising Pune Supergiants"
ipl_data$toss_winner[ipl_data$toss_winner=="Rising Pune Supergiant"]<-"Rising Pune Supergiants"

ipl_data$team1[ipl_data$team1=="Delhi Daredevils"]<-"Delhi Capitals"
ipl_data$team2[ipl_data$team2=="Delhi Daredevils"]<-"Delhi Capitals"
ipl_data$winner[ipl_data$winner=="Delhi Daredevils"]<-"Delhi Capitals"
ipl_data$toss_winner[ipl_data$toss_winner=="Delhi Daredevils"]<-"Delhi Capitals"

colSums(is.na(ipl_data))

ipl_data<-na.omit(ipl_data)

#TASK 1

#creating class  
match_toss_win<-ifelse(ipl_data$toss_winner==ipl_data$winner,"Y","N")
unique(match_toss_win)
ipl_data<-data.frame(ipl_data,match_toss_win)

#creating testing and training data
set.seed(2)
id<-sample(2,nrow(ipl_data),prob=c(.7,.3),replace = TRUE)
print(id)
train_data<-ipl_data[id==1,]
test_data<-ipl_data[id==2,]
summary(test_data)

library(e1071)
library(rpart)
library(caret)
library(rattle)

model<-naiveBayes(match_toss_win~.,train_data)
summary(model)
p<-predict(model,test_data)
confusionMatrix(table(p,test_data$match_toss_win))


#TASK 2
#creating new class
first_bat_win<-ifelse(ipl_data$toss_decision=="bat",ifelse(ipl_data$toss_winner==ipl_data$winner,"Y","N"),ifelse(ipl_data$toss_winner!=ipl_data$winner,"Y","N"))
ipl_data<-data.frame(ipl_data,first_bat_win)

train<-sample(1:nrow(ipl_data),size = ceiling(0.70*nrow(ipl_data)),replace = FALSE);
c_train<-ipl_data[train,]
c_test<-ipl_data[-train,]
c1_test<-c_test[,-16]

tree<-rpart(first_bat_win~team1+team2+toss_winner+winner+toss_decision,data=c_train)
summary(tree)
fancyRpartPlot(tree,caption=NULL)
p<-predict(object = tree,c1_test,type="class")
print(p)
confusionMatrix(table(p,c_test$first_bat_win))

library(dplyr)
library(ggplot2)
#data visualization

partial_data_1 <- table(ipl_data$team1)
partial_data_2 <- table(ipl_data$team2)
data3<-ipl_data$team1+ipl_data$team2
total<- partial_data_1+partial_data_2
total
total_data1<-data.frame(total)
total_data1
ggplot(data=total_data1,aes(x=Var1,y=Freq))+geom_point()
ggplot(data=ipl_data,aes(winner))+geom_bar(color="black",fill="darkgreen")
ggplot(data=ipl_data,aes(toss_winner))+geom_bar()
#plot2<-rbind(ipl_data$toss_winner, ipl_data$winner)
ggplot(data = ipl_data,aes(x=winner,fill=toss_winner))+geom_bar(position = position_dodge())
ggplot(data=ipl_data,aes(venue))+geom_bar()
barplot(total)

mtw1<-table(ipl_data$match_toss_win)
fbw<-table(ipl_data$first_bat_win)
barplot(mtw1)

x1<-table(ipl_data$city)
x1<-x1[order(-x1)]
x1
ggplot(data=ipl_data,aes(winner)+geom_bar()
barplot(head(x1))
