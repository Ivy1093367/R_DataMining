####取用dataset####
#setwd("C:/Users/陳俐卉/Documents")
setwd("C:/Users/陳俐卉/Desktop/R_Datamining/student_alcohol_dataset")
getwd()
#sleep_health_dataset <- read.csv("sleep_health_dataset/sleepdata.csv")
student_data_all=read.csv("student-mat.csv")
#View(student_data_all)
#student_data <- filter(student_data_all, school=="GP")
gmeans <- (student_data_all$G1+student_data_all$G2+student_data_all$G3)/3
student_data <- cbind(student_data_all,gmeans)
View(student_data)
#student_data <- student_data[,c(2:4,7:10,13,14,24:30,34)]



####檢查資料####
summary(student_data)
str(student_data)
sum_null <- 0
for (i in 1:34) {
  for (j in 1:395) {
    if(is.null(student_data[j,i])){
      sum_null <- sum_null+1
    }
  }
}
print(sum_null)  #檢查空值
sum(is.na(student_data))   #印出缺失值欄位的數量
#which(is.na(student_data))  #印出哪幾個欄位缺值

####作檢定####
#查看相關係數
target_column <- "gmeans"  # 將 "target" 替換為你的目標欄位名稱
# 選取其他欄位
#other_columns <- setdiff(names(student_data), target_column)
# 計算相關係數矩陣
cor_matrix <- cor(student_data[, c(3,7,8,13:15,24:30)], student_data[, target_column])
cor_matrix_two <- cor(student_data[, c(3,7,8,13:15,24:30)])
# 輸出相關係數矩陣
print(cor_matrix)
print(cor_matrix_two)

####anova-檢查高相關性欄位是否對成績有影響####
aov_result<-aov(gmeans~Walc,data=student_data)
summary(aov_result)
aov_result<-aov(gmeans~Dalc,data=student_data)
summary(aov_result)

####迴歸模型####
#全變數
Reg_model <- lm(gmeans~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+health+absences,data=student_data)
summary(Reg_model)
#相關性高的變數(移除低相關欄位)
Reg_model_new <- lm(gmeans~age+Medu+Fedu+failures+traveltime+goout,data=student_data)
summary(Reg_model_new)

#### t檢定####
#看性別會不會影響成績
stu_female <- filter(student_data,sex=="F")
stu_male <- filter(student_data,sex=="M")
var.test(x = stu_female$gmeans, y = stu_male$gmeans)
t_test_result <- t.test(student_data[,"gmeans"] ~ student_data[,"sex"], data = student_data,var.equal=TRUE)
boxplot(student_data[,"gmeans"] ~ student_data[,"sex"], col="lightblue" ,main="Math Score" ,ylab="")
#看居住地(都市和鄉村)會不會影響成績
stu_radd <- filter(student_data,address=="R")
stu_uadd <- filter(student_data,address=="U")
var.test(x = stu_radd$gmeans, y = stu_uadd$gmeans)
t_test_result <- t.test(student_data[,"gmeans"] ~ student_data[,"address"], data = student_data,var.equal=TRUE)
boxplot(student_data[,"gmeans"] ~ student_data[,"address"], col="lightblue" ,main="Math Score" ,ylab="")

library(ggplot2)
#sex
ggplot(data = student_data,
       mapping = aes(x = G3,y = gmeans,color = sex))+
  geom_point() + 
  stat_ellipse(level = 0.95,geom = "polygon",
               aes(fill = sex),
               alpha = 0.2)+
  theme_bw()
#address
ggplot(data = student_data,
       mapping = aes(x = G3,y = gmeans,color = address))+
  geom_point() + 
  stat_ellipse(level = 0.95,geom = "polygon",
               aes(fill = address),
               alpha = 0.2)+
  theme_bw()

####類別型-anova、knn####
library(dplyr)
#母工作類別
Mjob_athome <- filter(student_data,Mjob=="at_home")
Mjob_health <- student_data %>% filter(Mjob=="health")
Mjob_other <- student_data %>% filter(Mjob=="other")
Mjob_services <- student_data %>% filter(Mjob=="services")
Mjob_teacher <- student_data %>% filter(Mjob=="teacher")

aov_result<-aov(gmeans~Mjob,data=student_data)
summary(aov_result)
#事後檢定:想知道誰不一樣，Tukey可以同時滿足比較才不會一直損失信心水準
TukeyResult <- TukeyHSD(aov_result,conf.level = .99)

par(mfrow=c(1,1))
boxplot(student_data$gmeans~student_data$Mjob, col="lightblue",main="Mjob",ylab="")
plot(TukeyResult, las=2)

ggplot(data = student_data,
       mapping = aes(x = G1,y = G2,color = Mjob))+
  geom_point() + 
  stat_ellipse(level = 0.95,geom = "polygon",
               aes(fill = Mjob),
               alpha = 0.2)+
  theme_bw()
#盒形圖
Myboxplot_stu <- function(InXidx){
  boxplot(student_data[,InXidx]~student_data$Mjob,
          col=c(2,3,4,5,6),
          ylab = "",
          xlab = "",
          las=1, main=names(student_data)[InXidx])
}
par(mfrow=c(2, 2))
lapply(c(31:34),FUN = Myboxplot_stu)

#分類
library(caTools)
library(caret)
TrainFlag <- sample.split(student_data$Mjob,
                          SplitRatio = 0.75)
stu_train <- subset(student_data,TrainFlag==TRUE)
stu_test <- subset(student_data,TrainFlag==FALSE)

table(student_data$Mjob,TrainFlag)

NormalParam <- preProcess(stu_train[,17:20])
stu_train_scaleByTrain <- predict(NormalParam,stu_train[,17:20])
stu_test_scaleByTrain <- predict(NormalParam,stu_test[,17:20])

library(class)
stu_Mjob_predict <- knn(train = stu_train_scaleByTrain,
                            test = stu_test_scaleByTrain,
                            cl = factor(stu_train[,6]),
                            k = 5,
                            prob = TRUE)

stu_test_ConMatrix <- table(stu_test$Mjob,stu_Mjob_predict)
stu_test_Accuracy <- sum(diag(stu_test_ConMatrix))/sum(stu_test_ConMatrix)  #對角/全部加總
#k-fold cross validation
foldflag <- createFolds(y = student_data$Mjob,
                        k=20)

All_pred_Accuracy <- lapply(foldflag,FUN = function(dataidx){
  fold_train <- student_data[-dataidx,]
  fold_test <- student_data[dataidx,]
  fold_pred <- knn(train = fold_train[,17:20],
                   test = fold_test[,17:20],
                   cl = fold_train[,6],
                   k = 5,
                   prob = TRUE)
  fold_ConMatrix <- table(fold_test[,6],fold_pred)
  fold_Accuracy <- sum(diag(fold_ConMatrix))/sum(fold_ConMatrix)
  
  return(fold_Accuracy)
}
)
All_pred_Accuracy
as.numeric(All_pred_Accuracy)
mean(as.numeric(All_pred_Accuracy))

#父工作類別
aov_result<-aov(gmeans~Fjob,data=student_data)
summary(aov_result)
#事後檢定:想知道誰不一樣，Tukey可以同時滿足比較才不會一直損失信心水準
TukeyResult <- TukeyHSD(aov_result,conf.level = .99)

par(mfrow=c(1,2))
boxplot(student_data$gmeans~student_data$Fjob, col="lightblue",main="Fjob",ylab="")
plot(TukeyResult, las=2)

#盒形圖
Myboxplot_stu <- function(InXidx){
  boxplot(student_data[,InXidx]~student_data$Fjob,
          col=c(2,3,4,5,6),
          ylab = "",
          xlab = "",
          las=1, main=names(student_data)[InXidx])
}
par(mfrow=c(2, 2))
lapply(c(31:34),FUN = Myboxplot_stu)

#分類
TrainFlag <- sample.split(student_data$Fjob,
                          SplitRatio = 0.75)
stu_train <- subset(student_data,TrainFlag==TRUE)
stu_test <- subset(student_data,TrainFlag==FALSE)

table(student_data$Fjob,TrainFlag)


NormalParam <- preProcess(stu_train[,17:20])
stu_train_scaleByTrain <- predict(NormalParam,stu_train[,17:20])
stu_test_scaleByTrain <- predict(NormalParam,stu_test[,17:20])

library(class)
stu_Fjob_predict <- knn(train = stu_train_scaleByTrain,
                        test = stu_test_scaleByTrain,
                        cl = factor(stu_train[,7]),
                        k = 5,
                        prob = TRUE)

stu_test_ConMatrix <- table(stu_test$Fjob,stu_Fjob_predict)
stu_test_Accuracy <- sum(diag(stu_test_ConMatrix))/sum(stu_test_ConMatrix)  #對角/全部加總
#k-fold cross validation
foldflag <- createFolds(y = student_data$Fjob,
                        k=20)

All_pred_Accuracy <- lapply(foldflag,FUN = function(dataidx){
  fold_train <- student_data[-dataidx,]
  fold_test <- student_data[dataidx,]
  fold_pred <- knn(train = fold_train[,17:20],
                   test = fold_test[,17:20],
                   cl = fold_train[,7],
                   k = 5,
                   prob = TRUE)
  fold_ConMatrix <- table(fold_test[,7],fold_pred)
  fold_Accuracy <- sum(diag(fold_ConMatrix))/sum(fold_ConMatrix)
  
  return(fold_Accuracy)
}
)
All_pred_Accuracy
as.numeric(All_pred_Accuracy)
mean(as.numeric(All_pred_Accuracy))

####Assoiciation Rule####
#轉換資料grade->level
boxplot(student_data$gmeans, horizontal = TRUE, col="lightblue",main="gmeans")
glevel <- c()
grade_level <- function(InXidx){
  if(student_data[InXidx,34]<=5){
    glevel <- c(glevel, "level1")
  }else if(5<student_data[InXidx,34] && student_data[InXidx,34]<=10){
    glevel <- c(glevel, "level2")
  }else if(10<student_data[InXidx,34] && student_data[InXidx,34]<=15){
    glevel <- c(glevel, "level3")
  }else{
    glevel <- c(glevel, "level4")
  }
}
lg <- length(student_data$gmeans)
glevel <- sapply(c(1:lg),FUN = grade_level)
length(glevel)
student_data <- cbind(student_data,glevel)
View(student_data)
#轉換資料age->level
boxplot(student_data$age, horizontal = TRUE, col="lightblue",main="age")
agelevel <- c()
age_level <- function(InXidx){
  if(student_data[InXidx,3]<=17){
    agelevel <- c(agelevel, "15~17y")
  }else if(17<student_data[InXidx,3] && student_data[InXidx,3]<=19){
    agelevel <- c(agelevel, "18~19y")
  }else{
    agelevel <- c(agelevel, "20~22y")
  }
}
agelevel <- sapply(c(1:lg),FUN = age_level)
length(agelevel)
student_data <- cbind(student_data,agelevel)
View(student_data)
#轉換資料absences->level
par(mfrow=c(1, 1))
boxplot(student_data$absences, horizontal = TRUE, col="lightblue",main="absences")
abslevel <- c()
abs_level <- function(InXidx){
  if(student_data[InXidx,30]<=3){
    abslevel <- c(abslevel, "0~3")
  }else if(3<student_data[InXidx,30] && student_data[InXidx,30]<=6){
    abslevel <- c(abslevel, "4~6")
  }else if(6<student_data[InXidx,30] && student_data[InXidx,30]<=10){
    abslevel <- c(abslevel, "7~10")
  }else{
    abslevel <- c(abslevel, "11up")
  }
}
abslevel <- sapply(c(1:lg),FUN = abs_level)
length(abslevel)
student_data <- cbind(student_data,abslevel)
View(student_data)

#rule-整理資料
library(arules)
library(arulesViz)
student_data2 <- student_data[,c(1:2,4:29,35:37)]
View(student_data2)
for(x in 1:31){
  student_data2[,x] <- as.factor(student_data2[,x])
}

#rule
rules <- apriori(student_data2, 
                # min support & confidence, 最小規則長度(lhs+rhs)
                parameter = list(minlen=2, supp=0.3, conf=0.5),  
                appearance = list(default="lhs",
                                  rhs=c("glevel=level1", "glevel=level2", "glevel=level3", "glevel=level4") 
                                  # 右手邊顯示的特徵
                )
) 
inspect(rules)
sort.rules <- sort(rules, by="lift")
inspect(sort.rules)
plot(sort.rules)
plot(head(sort(rules,
               by = "lift"),
          n=8),
     method = "graph",
     engine = "htmlwidget")

#rule->glevel2
rules <- apriori(student_data2, 
                 # min support & confidence, 最小規則長度(lhs+rhs)
                 parameter = list(minlen=2, supp=0.1, conf=0.5),  
                 appearance = list(default="lhs",
                                   rhs=c("glevel=level2") 
                                   # 右手邊顯示的特徵
                 )
) 
inspect(rules)
sort.rules <- sort(rules, by="lift")
inspect(sort.rules)
plot(head(sort(rules,
               by = "lift"),
          n=8),
     method = "graph",
     engine = "htmlwidget")
plot(sort.rules)

#rule->glevel1
rules <- apriori(student_data2, 
                 # min support & confidence, 最小規則長度(lhs+rhs)
                 parameter = list(minlen=2, supp=0.1, conf=0.5),  
                 appearance = list(default="lhs",
                                   rhs=c("glevel=level1") 
                                   # 右手邊顯示的特徵
                 )
) 
inspect(rules)

#rule->glevel4
rules <- apriori(student_data2, 
                 # min support & confidence, 最小規則長度(lhs+rhs)
                 parameter = list(minlen=2, supp=0.1, conf=0.5),  
                 appearance = list(default="lhs",
                                   rhs=c("glevel=level4") 
                                   # 右手邊顯示的特徵
                 )
) 
inspect(rules)

