w<-read.table("E:/数据挖掘/car.txt",header=F,sep=",") #读取已经下载的数据文件
names(w)<-c("buying","maint","doors","persons","lug-boot","safety","class") #给每个变量命名
n<-length(w[,"class"])  #取得数据样例个数
samp<-sample(1:n,n/2)  #随机选取一半的样例作为训练集
tsamp<-setdiff(1:n,samp) #将另一半作为测试集
#使用决策树方法分类数据集
library(rpart)
a<-rpart(class~.,w,subset = samp) #使用训练集生成决策树
t(table(predict(a,w[samp,],type="class"),w[samp,"class"])) #输出训练集分类结果     t(table(predict(a,w[tsamp,],type="class"),w[tsamp,"class"])) #输出测试集分类结果   #使用6折交叉分类方法分类数据集
d=1:n;d2=list();Z=6    #观测值数目1728个，Z为折数
n2=levels(w$class);K=length(n2)   #目标变量class有K=4类
for(i in 1:K)d2[[i]]=d[w$class==n2[i]]   #每个d2[[i]]是i类下标集
#下面每个k[[i]]是i类中每折的数目
k=NULL
for(i in 1:K)k=c(k,round(length(d2[[i]])/Z))
y=list(NULL,NULL,NULL,NULL)
for(i in 1:K){x=list();u=d2[[i]]
for(j in 1:(Z-1)){x[[j]]=sample(u,k[i])
u=setdiff(u,x[[j]])};x[[Z]]=u
for(h in 1:Z)y[[i]][[h]]=x[[h]]}
m=list(NULL,NULL,NULL,NULL,NULL,NULL)
for(i in 1:Z)for(j in 1:K)m[[i]]=c(m[[i]],y[[j]][[i]])
#m[[i]]是第i折的测试下标集 
#下面再做6折交叉验证，这时会有6棵决策树。输出训练集和测试集的平均误判率
E0=rep(0,6);E1=E0
for(i in 1:5){m2=m[[i]]
n0=n-length(m2);n1=length(m2)
a=rpart(class~.,w[-m2,])
E0[i]=sum(w[-m2,"class"]!=predict(a,w[-m2,],type="class"))/n0
E1[i]=sum(w[m2,"class"]!=predict(a,w[m2,],type="class"))/n1}
mean(E0);mean(E1)
#使用组合方法分类数据集
library(mlbench)
library(ggplot2)
library(caret)
library(lattice)
library(adabag)
h1<-rep(0,100)
h2<-h1
h3<-h1
h4<-h1
#使用装袋方法分类数据
#分别得出分类器个数从1到100时的提升分类方法，计算分类方法对训练集和测试集的分类结果，并得出分类误差
for(i in 1:100){b<-bagging(class~.,data=w[samp,],mfinal=i);
b1<-predict.bagging(b,newdata=w[samp,],type="class");
b2<-predict.bagging(b,newdata=w[tsamp,],type="class");
h1[i]<-b1$error;
h2[i]<-b2$error
}
b<-bagging(class~.,data=w[samp,],mfinal=60) #得出分类器个数为60时的装袋方法
b1<-predict.bagging(b,newdata=w[samp,],type="class")
b2<-predict.bagging(b,newdata=w[tsamp,],type="class")
barplot(b$importance) #画出重要性图
#使用提升方法分类数据
#分别得出分类器个数从1到100时的袋装分类方法，计算分类方法对训练集和测试集的分类结果，并得出分类误差
for(i in 1:100){c<-boosting(class~.,data=w[samp,],mfinal=i);
c1<-predict.boosting(b,newdata=w[samp,],type="class");
c2<-predict.boosting(b,newdata=w[tsamp,],type="class");
h3<-c1$error;
h4<-c2$error
}
c<-boosting(class~.,data=w[samp,],mfinal=29) #得出分类器个数为29时的提升方法
barplot(c$importance)  #画出重要性图
#使用随机森林方法分类数据
library(dandomForest)
d<-randomForest(class~.,data=w,importance=T) #使用全部数据生成随机森林分类方法
par(mfrow=c(2,2))
for(i in 1:2) barplot(t(round(importance(d),2))[i,],cex.names=0.5) #画出四种类型分类时各个变量的重要性图
d1<-predict(d,w[ssamp,]) #使用随机森林方法对训练集分类
d2<-predict(d,w[tsamp,]) #使用随机森林方法对测试集分类
table(observed= wsamp, "class"],predicted=d1) #输出对训练集的分类结果
table(observed= w[tsamp, "class"],predicted=d2) #输出对测试集的分类结果
