# logistic
summary(train)
#install.packages("corrplot")
library(corrplot)
matrix=cor(train[,-14])
corrplot(corr=matrix,method='number') 
corrplot(matrix, method = "color") 

obj=glm(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+duration+campaign+poutcome,data = train)
summary(obj)

fit.reduced <- glm(y ~  age + job + marital + education + contact+month+duration+campaign+poutcome  ,family=binomial(),data=train)
summary(fit.reduced)

anova(obj,fit.reduced,test="Chisq")# 模型是否有差异
#logistics回归方程模型的系数展示
coef(obj)

mydata = train
mydata$y[mydata$y == '1'] = 1
mydata$y[mydata$y == '0'] = 0
mydata$y = as.numeric(mydata$y)

# logistic 拟合建立模型
index = sample(x = 1:2,size = nrow(mydata), replace = TRUE, prob = c(0.7,0.3))
train = mydata[index == 1, ]
test = mydata[index == 2, ]
logistic.model = glm(y~., data = train, family = binomial(link = 'logit'))

train_predict0 = predict(logistic.model, train, type='response')
train_predict = ifelse(train_predict0>0.5, 1, 0)
train_table = table(actual = train$y, pre = train_predict)

test_predict0 = predict(logistic.model, test, type='response')
test_predict = ifelse(test_predict0>0.5, 1, 0)
test_table = table(actual = test$y, pre = test_predict)

#fit.reduced 
train_predict1 = predict(fit.reduced , train, type='response')
train_predict1 = ifelse(train_predict1>0.5, 1, 0)
train_table = table(actual = train$y, pre = train_predict)

test_predict1 = predict(fit.reduced , test, type='response')
test_predict1 = ifelse(test_predict1>0.5, 1, 0)
test_table = table(actual = test$y, pre = test_predict)

cat('train accuracy:', sum(diag(train_table)/sum(train_table)), '\n', 
    'test accuracy:', sum(diag(test_table)/sum(test_table)))

predict_value=test_predict0
predict_value1=test_predict1

true_value=test[,14]
#true_value
#predict_value=test[,15]
#混淆矩阵中的量
#真实值预测值全为1 / 预测值全为1 --- 提取出的正确信息条数/提取出的信息条数 
precision=sum(true_value & predict_value)/sum(predict_value)
#真实值预测值全为1 / 真实值全为1 --- 提取出的正确信息条数 /样本中的信息条数 
recall=sum(predict_value & true_value)/sum(true_value)
#P和R指标有时候会出现的矛盾的情况，这样就需要综合考虑他们，最常见的方法就是F-Measure（又称为F-Score）
F_measure=2*precision*recall/(precision+recall)    #F-Measure是Precision和Recall加权调和平均，是一个综合评价指标 

plot(predict_value,test$y)

#输出以上各结果
print(precision) 
print(recall) 
print(F_measure) 
print("----------") 
#混淆矩阵，显示结果依次为TP、FN、FP、TN 
t=table(test_predict,test$y)
t
mean(test_predict==test$y)
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))

#混淆矩阵，显示结果依次为TP、FN、FP、TN 
t=table(test_predict1,test$y)
t
mean(test_predict1==test$y)
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))

#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(pROC)
library(ggplot2)
library(magrittr)
#install.packages("ROCR")
library(ROCR)

pred <- prediction(predict_value,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)

pred <- prediction(predict_value1,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)



#线性判别分析
library(MASS)
lda.fit=lda(y~age+job+marital+education+default+housing+loan+contact+month+day_of_week+duration+campaign+poutcome,data=train)
lda.pred=predict(lda.fit,test)
names(lda.pred)
lda.pred
lda.class=lda.pred$class
#plot(lda.fit)
#lda.fit

#预测
lda.class
#实际
test$y

t=table(lda.class,test$y)
t
mean(lda.class==test$y)
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))

x=lda.pred$posterior[,2]
a=as.numeric(x)

pred1 <- prediction(a,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred1,'auc')@y.values        #AUC值  
perf1 <- performance(pred1,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf1,colorize=TRUE)

modelroc <- roc(b,a) 
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", print.thres=TRUE)        #画出ROC曲线，标出坐标，并标出AUC的值 


# 决策树
#install.packages("tree")
library(tree)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("rattle")
library(rattle)

chose=ifelse(train$y>0.5,"1", "0")
bank=data.frame(train,chose)
tree.bank=tree(chose~.-y,bank)
summary(tree.bank)

plot(tree.bank)
text(tree.bank,pretty=0)

tree.bank=tree(chose~.-y,bank)
tree.pred=predict(tree.bank,test,type='class')
t=table(tree.pred,test$y)
t
mean(tree.pred==test$y)
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))

#ROC
pred <- prediction(as.numeric(tree.pred),true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)

#剪枝
set.seed(3)
cv.bank=cv.tree(tree.bank,FUN = prune.misclass)
names(cv.bank)

cv.bank

par(mfrow=c(1,2))
plot(cv.bank$size,cv.bank$dev,type="b")
plot(cv.bank$k,cv.bank$dev,type="b")

prune.bank=prune.misclass(tree.bank,best=4)
plot(prune.bank)
text(prune.bank,pretty=0)

tree.pred=predict(prune.bank,test,type="class")
t=table(tree.pred,test$y)
t
mean(tree.pred==test$y)
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))

fit    = tree(y ~., train)
tree.bank=tree(y~.,train)
tree.pred=predict(tree.bank,test)

#ROC
pred <- prediction(tree.pred,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)

plot(tree.pred,test$y)


##随机森林
install.packages("randomForest")
library(randomForest)
set.seed(1)
modelf=randomForest(y~.,data=mydata,subset=train,mtry=13,importance=TRUE)


# KNN
library(class)
set.seed(1)  #设置一个随机种子
knn.pred = knn(train, test,train$y, k = 1)  
t=table(test$y,knn.pred)
mean(knn.pred ==  test$y)
t
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))

a=as.numeric(knn.pred)
pred <- prediction(a,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)
```

correct = rep(0,15)
for(i in 1:15){
  fit_pre = knn(train, test,train$y,k=i)
  correct[i] = mean(fit_pre ==  test$y)
}
print(correct)

#best=13
knn.pred1 = knn(train, test,train$y, k = 3)  
t=table(test$y,knn.pred1)
mean(knn.pred1 ==  test$y)
t
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))
print(sensitivity <- tp/(tp + fn))
print(specificity <- tn/(tn + fp))


a=as.numeric(knn.pred1)
pred <- prediction(a,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
#performance(pred,'auc')@y.values        #AUC值  
#perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
#plot(perf,colorize=TRUE)
fit_pre = knn(train, test,train$y,k=1)
a=as.numeric(fit_pre)
pred <- prediction(a,true_value)
performance(pred,'auc')@y.values  
fit_pre = knn(train, test,train$y,k=3)
a=as.numeric(fit_pre)
pred <- prediction(a,true_value)
performance(pred,'auc')@y.values 
fit_pre = knn(train, test,train$y,k=5)
a=as.numeric(fit_pre)
pred <- prediction(a,true_value)
performance(pred,'auc')@y.values 
fit_pre = knn(train, test,train$y,k=7)
a=as.numeric(fit_pre)
pred <- prediction(a,true_value)
performance(pred,'auc')@y.values 
fit_pre = knn(train, test,train$y,k=9)
a=as.numeric(fit_pre)
pred <- prediction(a,true_value)
performance(pred,'auc')@y.values 
fit_pre = knn(train, test,train$y,k=15)
a=as.numeric(fit_pre)
pred <- prediction(a,true_value)
performance(pred,'auc')@y.values 


knn_roc <- roc(test$y,a)
plot(knn_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='knn算法ROC曲线')


# SVM

library(e1071)
svm_model = svm(y~.,data=train,knernel = "radial")
summary(svm_model)

#svm.pred=predict(svm_model,test,decision.values = TRUE)
svm.fit=svm(y~.,data=mydata,kenel="linear",cost=10,scale=FALSE)

set.seed(1)
tune.out=tune(svm,y~.,data=mydata,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
#tune.out=tune(svm(y~.,data=mydata,kenel='linear'),ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))


summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

#head(test)
t=table(test$y,svm.pred)
t
mean(svm.pred==test$y)
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))

#install.packages("Pandoc")
#install.packages("miktex")
install.packages("rticles")

## 决策树2

chose=ifelse(train$y>0.5,"1", "0")
bank=data.frame(train,chose)
tree.bank=tree(chose~.-y,bank)

model <- rpart(chose~.-y,data=bank)
fancyRpartPlot(model)

pred<-predict(model,test,type="class")
t=table(pred,test$y)
t
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))

a=as.numeric(pred)
pred <- prediction(a,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)

#剪枝
model$cptable #查看交叉验证结果

#根据交叉验证结果，找出估计误差最小时的cp值，并重新建立模型。

xerr <-model$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <-model$cptable[minxerr, "CP"] #选择交叉验证的估计误差最小时对应的cp
#新模型
model.prune <- prune(model,cp=mincp) 
fancyRpartPlot(model.prune)

pred<-predict(model.prune,test,type="class")
t=table(pred,test$y)
t
tp <- t[1, 1]
tn <- t[2, 2]
fp <- t[2, 1]
fn <- t[1, 2]
print(accuracy <- (tp + tn)/(tp + tn + fp + fn))
print(precision <- tp/(tp + fp))
print(recall <- tp/(tp + fn))

a=as.numeric(pred)
pred <- prediction(a,true_value)   #预测值(0.5二分类之前的预测值)和真实值    
performance(pred,'auc')@y.values        #AUC值  
perf <- performance(pred,'tpr','fpr')  #y轴为tpr(true positive rate),x轴为fpr(false positive rate)
plot(perf,colorize=TRUE)










