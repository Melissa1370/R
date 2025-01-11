########## (Packages) ########################################################

library(skimr)
library(dplyr)
library(moments)
library(car)
library(DMwR2)
library(fitdistrplus)
library(fpc)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggsignif)
library(rstatix)
library(writexl)
library(Rweka)
library(RWekajars)
library(rJava)
library(arules)
library(matrix)
library(arulesViz)
library(ggraph)
library(igraph)
library(polyclip)
library(graphlayouts)
library(shinythemes)
library(shiny)
library(seriation)
library(e1071)
library(caret)
library(lattice)
library(clusterCrit)

########## (1) ################################################################

setwd("E:/data course/5-R/Project-Shahgholizadeh/Project")
data<-read.csv(file.choose())
data

########## (2) ################################################################

names(data)
head(data)
tail(data)
str(data)
summary(data)
skim(data)

########## (3) ################################################################

names(data)[16]<-"Action"

########## (4) ################################################################

is.na(data$Product_Date)
data[data==""]<-NA
sum_of_nas<-sum(is.na(data$Product_Date))
mean_nas<-sum_of_nas/nrow(data)
data<-data[,-7]

########## (5) ################################################################

mean(data$TAT01)
mean(data$TAT02)

########## (6) ################################################################

duplicated(data$Serial_No)
dup_rows<-which(duplicated(data$Serial_No))
sum_dup<-sum(duplicated(data$Serial_No))
dup_rate<-sum_dup/nrow(data)

########## (7) ################################################################
class(data$Cost_Type)
t<-table(data$Cost_Type)
barplot(t, main = "Warranty", ylab = "N of Products", 
        col = rainbow(length(row.names(t))))
########## (Cleaning Data) ####################################################

#### Based on CRISP_DM Model:

#### Noise  -> there is not any noise in data
skim(data)




#### Outlier Detection ########################################################
#### LOF Approach

data1<-data[,c(10,11,18:21)]    #numeric columns
h1<-data.matrix(data1)
lof<-lofactor(data=h1,k=5)
h2<-data.frame(h1)
h2$lof<-lof
hist(h2$lof,breaks=100)

sum(is.na(h2$lof))     # 1791
w<-which(is.na(h2$lof))
h3<-h2[-w,]
range(h3)
hist(h3$lof,breaks=100)
h4<-h2[w,]
z<-which(is.finite(h2$lof))
h5<-h2[z,]
range(h5)
hist(h5$lof,breaks=20)
h6<-h5[h5$lof<=1.4,]      #844 obs
hist(h6$lof,breaks=20)
na_inf<-h2[-z,]
# lof method is not logical in this data set because 844 obs remain

#### IQR Method
#### 1.5 IQR TAT01
hist(h2$TAT01,probability = TRUE,breaks=20)
lines(density(h2$TAT01),col="red")
qqnorm(h2$TAT01,pch=20)
qqline(h2$TAT01,col="red")
shapiro.test(h2$TAT01)     # is not normal
jarque.test(h2$TAT01)      # is not normal
anscombe.test(h2$TAT01)    # is not normal 
descdist(h2$TAT01,discrete = FALSE)
boxplot(h2$TAT01)
IQR(h2$TAT01)
out_TAT01<-boxplot.stats(h2$TAT01)$out
outi_TAT01<-which(h2$TAT01 %in% c(out_TAT01))
sum(h2$TAT01 %in% c(out_TAT01))              #377
outi_TAT01

##### 3 IQR TAT01
xout_TAT01<-boxplot.stats(h2$TAT01,coef=3)$out
xouti_TAT01<-which(h2$TAT01 %in% c(xout_TAT01))
sum(h2$TAT01 %in% c(xout_TAT01))          #184
xouti_TAT01

##### 1.5 IQR TAT02
hist(h2$TAT02,probability = TRUE,breaks=20)
lines(density(h2$TAT02),col="red")
qqnorm(h2$TAT02,pch=20)
qqline(h2$TAT02,col="red")
shapiro.test(h2$TAT02)     # is not normal
jarque.test(h2$TAT02)      # is not normal
anscombe.test(h2$TAT02)    # is not normal 
descdist(h2$TAT02,discrete = FALSE)
boxplot(h2$TAT02)
IQR(h2$TAT02)
out_TAT02<-boxplot.stats(h2$TAT02)$out
outi_TAT02<-which(h2$TAT02 %in% c(out_TAT02))
sum(h2$TAT02 %in% c(out_TAT02))              #358
outi_TAT02

##### 3 IQR TAT02
xout_TAT02<-boxplot.stats(h2$TAT02,coef=3)$out
xouti_TAT02<-which(h2$TAT02 %in% c(xout_TAT02))
sum(h2$TAT02 %in% c(xout_TAT02))          #203
xouti_TAT02

##### 1.5 IQR Labor_Charge_Amount
hist(h2$Labor_Charge_Amount,probability = TRUE,breaks=20)
lines(density(h2$Labor_Charge_Amount),col="red")
qqnorm(h2$Labor_Charge_Amount,pch=20)
qqline(h2$Labor_Charge_Amount,col="red")
shapiro.test(h2$Labor_Charge_Amount)     # is not normal
jarque.test(h2$Labor_Charge_Amount)      # is not normal
anscombe.test(h2$Labor_Charge_Amount)    # is not normal 
descdist(h2$Labor_Charge_Amount,discrete = FALSE)
boxplot(h2$Labor_Charge_Amount)       # no outlier
IQR(h2$Labor_Charge_Amount)
out_Labor_Charge_Amount<-boxplot.stats(h2$Labor_Charge_Amount)$out
outi_Labor_Charge_Amount<-which(h2$Labor_Charge_Amount %in% c(out_Labor_Charge_Amount))
sum(h2$Labor_Charge_Amount %in% c(out_Labor_Charge_Amount))       #0  
outi_Labor_Charge_Amount

##### 1.5 IQR Parts_Amount
hist(h2$Parts_Amount,probability = TRUE,breaks=20)
lines(density(h2$Parts_Amount),col="red")
qqnorm(h2$Parts_Amount,pch=20)
qqline(h2$Parts_Amount,col="red")
shapiro.test(h2$Parts_Amount)     # is not normal
jarque.test(h2$Parts_Amount)      # is not normal
anscombe.test(h2$Parts_Amount)    # is not normal 
descdist(h2$Parts_Amount,discrete = FALSE)
boxplot(h2$Parts_Amount)
IQR(h2$Parts_Amount)
out_Parts_Amount<-boxplot.stats(h2$Parts_Amount)$out
outi_Parts_Amount<-which(h2$Parts_Amount %in% c(out_Parts_Amount))
sum(h2$Parts_Amount %in% c(out_Parts_Amount))              #731
outi_Parts_Amount

##### 3 IQR Parts_Amount
xout_Parts_Amount<-boxplot.stats(h2$Parts_Amount,coef=3)$out
xouti_Parts_Amount<-which(h2$Parts_Amount %in% c(xout_Parts_Amount))
sum(h2$Parts_Amount %in% c(xout_Parts_Amount))          #713
xouti_Parts_Amount

##### 1.5 IQR Discount_Amount
hist(h2$Discount_Amount,probability = TRUE,breaks=20)
lines(density(h2$Discount_Amount),col="red")
qqnorm(h2$Discount_Amount,pch=20)
qqline(h2$Discount_Amount,col="red")
shapiro.test(h2$Discount_Amount)     # is not normal
jarque.test(h2$Discount_Amount)      # is not normal
anscombe.test(h2$Discount_Amount)    # is not normal
descdist(h2$Discount_Amount,discrete = FALSE)
boxplot(h2$Discount_Amount)
IQR(h2$Discount_Amount)
out_Discount_Amount<-boxplot.stats(h2$Discount_Amount)$out
outi_Discount_Amount<-which(h2$Discount_Amount %in% c(out_Discount_Amount))
sum(h2$Discount_Amount %in% c(out_Discount_Amount))              #539
outi_Discount_Amount

##### 3 IQR Discount_Amount
xout_Discount_Amount<-boxplot.stats(h2$Discount_Amount,coef=3)$out
xouti_Discount_Amount<-which(h2$Discount_Amount %in% c(xout_Discount_Amount))
sum(h2$Discount_Amount %in% c(xout_Discount_Amount))          #465
xouti_Discount_Amount

##### 1.5 IQR Total_Invoice_Amount
hist(h2$Total_Invoice_Amount,probability = TRUE,breaks=20)
lines(density(h2$Total_Invoice_Amount),col="red")
qqnorm(h2$Total_Invoice_Amount,pch=20)
qqline(h2$Total_Invoice_Amount,col="red")
shapiro.test(h2$Total_Invoice_Amount)     # is not normal
jarque.test(h2$Total_Invoice_Amount)      # is not normal
anscombe.test(h2$Total_Invoice_Amount)    # is not normal 
descdist(h2$Total_Invoice_Amount,discrete = FALSE)
boxplot(h2$Total_Invoice_Amount)
IQR(h2$Total_Invoice_Amount)
out_Total_Invoice_Amount<-boxplot.stats(h2$Total_Invoice_Amount)$out
outi_Total_Invoice_Amount<-which(h2$Total_Invoice_Amount %in% c(out_Total_Invoice_Amount))
sum(h2$Total_Invoice_Amount %in% c(out_Total_Invoice_Amount))       #518
outi_Total_Invoice_Amount

##### 3 IQR Total_Invoice_Amount
xout_Total_Invoice_Amount<-boxplot.stats(h2$Total_Invoice_Amount,coef=3)$out
xouti_Total_Invoice_Amount<-which(h2$Total_Invoice_Amount %in% 
                                    c(xout_Total_Invoice_Amount))
sum(h2$Total_Invoice_Amount %in% c(xout_Total_Invoice_Amount))          #447
xouti_Total_Invoice_Amount

#### 1.5 IQR overall
overall_out<-c(outi_TAT01,outi_TAT02,outi_Discount_Amount,
               outi_Parts_Amount)     #2005
overall_dup<-duplicated(overall_out,incomparables = FALSE)
overall_dup
sum(overall_dup==FALSE)   #1174
sum(overall_dup==TRUE)    #831
overall_t<-overall_out[overall_dup==FALSE]      # 1174 overall outlier
overall_t 
h7<-h2[-overall_t,]         # 1962 remains

#### 3 IQR overall
overall_xout<-c(xouti_TAT01,xouti_TAT02,xouti_Discount_Amount,
               xouti_Parts_Amount)     #1565
overall_xdup<-duplicated(overall_xout,incomparables = FALSE)
overall_xdup
sum(overall_xdup==FALSE)   #946
sum(overall_xdup==TRUE)    #619
overall_xt<-overall_xout[overall_xdup==FALSE]      # 946 overall extreme outlier
overall_xt 
h8<-h2[-overall_xt,]    # 2190 remains

#### visualization of data with deleting extreme outliers
hist(h8$TAT01,breaks=20)
boxplot(h8$TAT01, range = 1.5)

hist(h8$TAT02,breaks=20)
boxplot(h8$TAT02, range = 1.5)

hist(h8$Discount_Amount,breaks=20)
boxplot(h7$Discount_Amount, range = 1.5)

hist(h8$Parts_Amount,breaks=20)
boxplot(h7$Parts_Amount, range = 1.5)

hist(h8$Total_Invoice_Amount,breaks=20)
boxplot(h8$Total_Invoice_Amount, range = 1.5)
 #write_xlsx(h7, "C:/Users/LENOVO/OneDrive/Desktop/qq.xlsx")

#### DBSCAN algorithm

dbscan_result <- dbscan(data[c(10,11,18:20)], eps = 0.5, MinPts = 2)
dbscan_result
dbscan_result$cluster
table(dbscan_result$cluster)

clus<-data.frame(dbscan_result$cluster)
ind<-which(clus>0)
sum(clus>0)
data_dbscan<-data[ind,]      # 2268 obs

#### visualization of data with deleting abnormal obs in DBSCAN method
hist(data_dbscan$TAT01,breaks=20)
boxplot(data_dbscan$TAT01, range = 1.5)

hist(data_dbscan$TAT02,breaks=20)
boxplot(data_dbscan$TAT02, range = 1.5)

hist(data_dbscan$Discount_Amount,breaks=20)
boxplot(data_dbscan$Discount_Amount, range = 1.5)

hist(data_dbscan$Parts_Amount,breaks=20)
boxplot(data_dbscan$Parts_Amount, range = 1.5)

hist(data_dbscan$Total_Invoice_Amount,breaks=20)
boxplot(data_dbscan$Total_Invoice_Amount, range = 1.5)

#### clusters in DBSCAN
fviz_cluster(dbscan_result, data = data[c(10,11,18:20)])

# with the same Amount of deletion the 3-IQR method performs better so we delete extreme 
# outliers and keep all others

clean_data<-data[-overall_xt,]    #2190 obs

#### Missing Data
is.na.data.frame(clean_data)
sum(is.na.data.frame(clean_data))  # 11

sum(is.na(clean_data$Product_Group))  #5
sum(is.na(clean_data$Serial_No))   #6

na_Product_Group<-which(is.na(clean_data$Product_Group))
na_Serial_No<-which(is.na(clean_data$Serial_No))

#write_xlsx(clean_data, "C:/Users/LENOVO/OneDrive/Desktop/qq.xlsx")
cdata<-clean_data[-c(na_Product_Group,na_Serial_No),]   #2179 obs

########## (8) ################################################################
names(cdata)

# correlation of numeric features
cor_table<-round(cor(cdata[,c(2,10,11,12,18,19,20)]),2)    #without target variable
cor_table

plot(cdata$Discount_Amount,cdata$Labor_Charge_Amount)     # high correlation between two column

# model1= numeric features only, R2=0.9998
m1<-lm(Total_Invoice_Amount~TAT01+TAT02+Service_type+Job_Satus+Labor_Charge_Amount
       +Parts_Amount+Discount_Amount,data=cdata)
summary(m1)

#model2=numeric features without TAT01,TAT02, R2=0.9998
m2<-lm(Total_Invoice_Amount~Service_type+Job_Satus+Labor_Charge_Amount
       +Parts_Amount+Discount_Amount,data=cdata)
summary(m2)

hist(m2$residuals,probability = TRUE)
lines(density(m2$residuals),col="red")

qqnorm(m2$residuals,main="Q-Q plot of residuals",pch=20)
qqline(m2$residuals,col="red")

jarque.test(m2$residuals)
anscombe.test(m2$residuals)

plot(m2)

car::vif(m2)       #Labor_Charge_Amount/Parts_Amount/Discount_Amount

#### scaling
csdata<-as.data.frame(scale(cdata[,c(2,10,11,12,18,19,20,21)]))
csdata

m2p<-lm(Total_Invoice_Amount~Service_type+Job_Satus+Labor_Charge_Amount
       +Parts_Amount+Discount_Amount,data=csdata)
summary(m2p)
car::vif(m2p)      # scaling cant omit or decrease multi-colinearity

#### omitting discount_amount  (multi-colinearity)
m3<-lm(Total_Invoice_Amount~TAT01+TAT02+Service_type+Job_Satus+Labor_Charge_Amount
       +Parts_Amount ,data=cdata)
summary(m3)       #R2=0.9998
car::vif(m3)      #there is not any multi-colinearity


########## (9) ################################################################

class(cdata$Cost_Type)
cdata$Cost_Type<-as.factor(cdata$Cost_Type)
cdata$Cost_Type_factor<-factor(cdata$Cost_Type)     # new column

cor(cdata$Total_Invoice_Amount,as.numeric(cdata$Cost_Type))      # R=0.31
cor(cdata$Total_Invoice_Amount,as.numeric(cdata$Cost_Type))^2    # R2=0.098


m4<-lm(Total_Invoice_Amount~TAT01+TAT02+Service_type+Labor_Charge_Amount
       +Parts_Amount+Cost_Type,data=cdata)
summary(m4)     # R2=0.9998

#### omitting some variables
m5<-lm(Total_Invoice_Amount~Labor_Charge_Amount+Parts_Amount+Cost_Type_factor,data=cdata)
summary(m5)
car::vif(m5)     # R2=0.9998

########## (9) ################################################################

rdata<-cdata[,c(2,3,4,6,(12:17))]

#### factor
rdata$Cost_Type=as.factor(rdata$Cost_Type)
rdata$Product_Group=as.factor(rdata$Product_Group)
rdata$City=as.factor(rdata$City)
rdata$Defect_Des=as.factor(rdata$Defect_Des)
rdata$Symptom_Desc=as.factor(rdata$Symptom_Desc)
rdata$Action=as.factor(rdata$Action)
rdata$Labor_Charge_Desc=as.factor(rdata$Labor_Charge_Desc)
rdata$Engineer=as.factor(rdata$Engineer)

#### categorized service_type column
rdata$Service_type1[rdata$Service_type==0]<-"Customer Location"
rdata$Service_type1[rdata$Service_type==1]<-"Repair shop Location"

#### categorized job_status column
rdata$Job_Satus1[rdata$Job_Satus==0]<-"Not Delivered"
rdata$Job_Satus1[rdata$Job_Satus==1]<-"Delivered"

#### factor
rdata$Service_type1=as.factor(rdata$Service_type1)
rdata$Job_Satus1=as.factor(rdata$Job_Satus1)

rdata1<-rdata[,c(2,3,4,6:12)]
rdata1[rdata1==""]<-NA
is.na(rdata1)
which(is.na(rdata1))

#### Apriori Algorithm
arules_model<-apriori(rdata1,parameter=list(supp=0.5,conf=0.7))
arules_model
inspect(arules_model[1:20])

rdata1_matrix<-as(rdata1,"transactions")
itemFrequencyPlot(rdata1_matrix,topN=15,main="itemFrequencyPlot",type="absolute",ylab="ylab")

inspect(subset(arules_model,lift>1))
inspect(subset(arules_model,support()>0.5))

#### Interactive rules
rules<-apriori(rdata1,parameter=list(supp=0.5,conf=0.7,minlen=2,target="rules"))
inspectDT(rules)

plot(rules,method="scatterplot",engine="html")
plot(rules,method="graph",engine="html")
plot(rules,method="matrix",engine="html")

ruleExplorer(rules)


########## (9) ################################################################

cdata
sdata<-cdata[,c(2,10,11,12,18:21)]
sdata

# Normalizing data
# Define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply Min-Max normalization to the columns
sdata1 <- as.data.frame(lapply(sdata, min_max_norm))

# test and train
set.seed(12345)
train_cases<-sample(1:nrow(sdata1),nrow(sdata1)*0.7)
train<-sdata1[train_cases,]
test<-sdata1[-train_cases,]

########### Naive-Bayes model
seg_nb<-naiveBayes(sdata1$Total_Invoice_Amount~.,data=sdata1)
seg_nb

# Prediction
test$test_pred<-predict(seg_nb,test)

table(test$Total_Invoice_Amount,test$test_pred)

mean(test$Total_Invoice_Amount==test$test_pred)    # 0.957

############## KNN model

# test and train
set.seed(12345)
train_cases_knn<-sample(1:nrow(sdata1),nrow(sdata1)*0.7)
train_knn<-sdata1[train_cases_knn,]
test_knn<-sdata1[-train_cases_knn,]

# model
sdata1$Service_type<-as.factor(sdata1$Service_type)
knn<-knn3(sdata1$Total_Invoice_Amount~.,data=sdata1,k=10)

test_knn$pred_knn<-predict(knn,test_knn)
table(test_knn$pred_knn,test_knn$Total_Invoice_Amount)
mean(test_knn$Total_Invoice_Amount==test_knn$pred_knn)   #0.719

# Optimum k

list1<-list()
ind<-1
for(i in seq(1,100,5)) {
  knn<-knn3(sdata1$Total_Invoice_Amount~.,data=sdata1,k=i)
  pred_knn<-predict(knn,test_knn,type="class")
  list1[ind]<-mean(test_knn$Total_Invoice_Amount==test_knn$pred_knn)
  ind<-ind+1
}

plot(seq(1,100,5),unlist(list1),type="l")

############ K-means Model

sdata2<-sdata1[,-8]      # for unsupervised model omitting target column
set.seed(1234)
seg_km<-kmeans(sdata2,centers=5)
seg_km$cluster
table(seg_km$cluster)

# Elbow plot

set.seed(1234)

wss<-function(k){
  kmeans(sdata2,k,nstart=10)$tot.withinss
}

k_values<-1:10
wss_values<-data.frame(k=k_values)

for(i in k_values){
wss_values$wss[i]<-wss(wss_values$k[i])  
}
wss_values

plot(wss_values$k,wss_values$wss,type="b",pch=20,frame=FALSE,xlab="N of clusters",ylab="Total within 
cluster sum of squares")    # optimal k=5 based on elbow method

sdata2$segment<-seg_km$cluster   # adding segment column to data


tapply(sdata2$TAT01,sdata2$segment,mean)
tapply(sdata2$TAT02,sdata2$segment,mean)
tapply(sdata2$Job_Satus,sdata2$segment,mean)
tapply(sdata2$Labor_Charge_Amount,sdata2$segment,mean)
tapply(sdata2$Parts_Amount,sdata2$segment,mean)
tapply(sdata2$Discount_Amount,sdata2$segment,mean)

boxplot(sdata2$TAT01,sdata2$segment)
boxplot(sdata2$TAT02,sdata2$segment)
boxplot(sdata2$Labor_Charge_Amount,sdata2$segment)

fviz_cluster(seg_km,geom="point",data=sdata2[,-1])+ggtitle("N of clusters: k=5")

ggplot(data=sdata2,aes(sdata2$Parts_Amount,sdata2$Discount_Amount,color=factor(segment)))+geom_point()

# silhouette kpi

for (i in 1:ncol(sdata2)){
  sdata2[,i]<-as.numeric(sdata2[,i])
}
str(sdata2)
sdata3<-as.matrix(sdata2)

sil=intCriteria(traj=sdata3,seg_km$cluster,"silhouette")
sil            # 0.654

# silhouette plot

list1=list()
for (i in 2:15){
  set.seed(1234)
  km<-kmeans(sdata3,centers=i)
  list[i]<-intCriteria(traj=sdata3,km$cluster,"silhouette")
}

plot(seq(2:15),unlist(list1),type="l")   # error





