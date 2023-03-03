customerData <- read.csv("Bank_Customers_Data.csv")
LoanApprover <- read.csv("Loan_Approved.csv")
head(customerData)
tail(customerData)
str(customerData)
summary(customerData)
sd(customerData$Age)
sd(customerData$Annual.Income..thousand.)
GENDER <- table(customerData$Gender)
barplot(GENDER,main = "Gender Comparison",xlab = "Gender",ylab = "Count",col =c("pink","lightblue"))
pct = round(GENDER/sum(GENDER)*100)
lbs = paste(c("Female","Male")," ",pct,"%",sep = " ")
library(plotrix)
pie3D(GENDER,labels = lbs,main="Pie Chart Depicting Ratio Of Female And Male",col = c("red","orange"))


summary(customerData$Age)
hist(customerData$Age,col = "lightblue",main = "Hitogram to show count of Age Class",xlab = "Age Class",ylab = "Frequency",labels = TRUE)
boxplot(customerData$Age,col = "#ff0066",main="Boxplot for Descriptive Analysis of Age")
summary(customerData$Annual.Income..thousand.)
hist(customerData$Annual.Income..thousand.,col = "orange",main = "Histogram For Annual Income",xlab = "Annual Income Class",ylab = "Frequency",labels = TRUE)
plot(density(customerData$Annual.Income..thousand.),col="orange",main = "Density Plot For Annual Income",xlab = "Annual Income Class",ylab = "Density")
polygon(density(customerData$Annual.Income..thousand.),col = "#ccff66")
summary(customerData$Spending.Score..1.100.)
boxplot(customerData$Spending.Score..1.100.,horizontal = TRUE,col = "lightblue",main="Boxplot For Descriptive Analysis Of Spending Score")
hist(customerData$Spending.Score..1.100.,main = "Histogram For Spending Score",xlab = "Spending Score Class",ylab = "Frequency",col = "#2475B0",labels = TRUE)

barplot(table(LoanApprover$Married),main="APPOVAL OF  LOAN IN MARKET SEGMENTATION" , col = c("red","Green"))

SEGMTS<- table(customerData$segment)
barplot(SEGMTS,main ="UBA BANK Market Segmentation", xlab="Segmentation", ylab="count", col=c("pink", "lightblue", "ORANGE"))
pct = round(SEGMTS/sum(SEGMTS)*100)
lbs = paste(c("corporate Banking","retail Banking", "Sacco Banking")," ",pct,"%",sep = " ")
library(plotrix)
pie3D(SEGMTS,labels = lbs,main="Pie Chart Depicting Ratio Of RETAIL, CORPORATE AND SOCCO BANKING, MARKET SEGMENTATION",col = c("pink", "lightblue", "ORANGE"))
savingAccount <- read.csv("Saving_Account.csv")
currentAccount<- read.csv("current_Account.csv")
head(savingAccount)
tail(savingAccount)
str(savingAccount)
summary(savingAccount)

barplot(table(savingAccount$Student_Account),main="Student Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Student_Account),main="Student Account under Current", col = c("pink","lightblue"))

barplot(table(savingAccount$Children.s),main="children's Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Children.s),main="children's Account under Current", col = c("pink","lightblue"))

barplot(table(savingAccount$Joint_Account),main="joint Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Joint_Account),main="joint Account under Current", col = c("pink","lightblue"))

barplot(table(savingAccount$Business_Account),main="Business Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Business_Account),main="Business Account under Current", col = c("pink","lightblue"))

barplot(table(savingAccount$Young_Worker_Account),main="Y_Worker Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Young_Worker_Account),main="Y_WORKER Account under Current", col = c("pink","lightblue"))


barplot(table(savingAccount$Young_Adult_Account),main="Y_Adult Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Young_Adult_Account),main="Y_Adult Account under Current", col = c("pink","lightblue"))

barplot(table(savingAccount$Pensioneer.Account),main="Pensnr Account under saving" , col = c("red","orange"))
barplot(table(currentAccount$Pensioneer.Account),main="Pensnr Account under Current", col = c("pink","lightblue"))


library(purrr)
set.seed(123)
iss <- function(k){
  kmeans(customerData[,3:5],k,iter.max = 100,nstart = 100,algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10

iss_values <- map_dbl(k.values,iss)
plot(k.values,iss_values,type = "b",pch=19,frame=FALSE,xlab = "Number Of Clusters K",ylab = "Total Intra Clusters Sum Of Squares",col="#1287A5")
library(cluster)
library(gridExtra)
library(grid)
k2 <- kmeans(customerData[,3:5],2,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s2 <- plot(silhouette(k2$cluster,dist(customerData[,3:5],"euclidean")),col = "#1287A5")
k3 <- kmeans(customerData[,3:5],3,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s3 <- plot(silhouette(k3$cluster,dist(customerData[,3:5],"euclidean")),col="#1287A5")
k4 <- kmeans(customerData[,3:5],4,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s4 <- plot(silhouette(k4$cluster,dist(customerData[,3:5],"euclidean")),col="#1287A5")
k5 <- kmeans(customerData[,3:5],5,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s5 <- plot(silhouette(k5$cluster,dist(customerData[,3:5],"euclidean")),col="#1287A5")
k6 <- kmeans(customerData[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s6 <- plot(silhouette(k6$cluster,dist(customerData[,3:5],"euclidean")),col="#1287A5")
k7 <- kmeans(customerData[,3:5],7,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s7 <- plot(silhouette(k7$cluster,dist(customerData[,3:5],"euclidean")),col = "#1287A5")
k8 <- kmeans(customerData[,3:5],8,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s8 <- plot(silhouette(k8$cluster,dist(customerData[,3:5],"euclidean")),col = "#1287A5")
k9 <- kmeans(customerData[,3:5],9,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s9 <- plot(silhouette(k9$cluster,dist(customerData[,3:5],"euclidean")),col = "#1287A5")
k10 <- kmeans(customerData[,3:5],10,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s10 <- plot(silhouette(k10$cluster,dist(customerData[,3:5],"euclidean")),col = "#1287A5")
library(NbClust)
library(factoextra)
set.seed(125)
stat_gap <- clusGap(customerData[,3:5],FUN=kmeans,nstart=25,K.max = 10,B=50)
fviz_gap_stat(stat_gap)
clusterK <- kmeans(customerData[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
clusterK
pclust <- prcomp(customerData[,3:5],scale. = FALSE)
summary(pclust)
pclust$rotation[,1:2]
kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster <- clusterK$cluster 
dignm <- as.character(digCluster)
plot(pclust$x[,1:2],col=kCols(digCluster),pch=19,xlab = "K-Means",ylab = "Classes")
legend("bottomleft",unique(dignm),fill = unique(kCols(digCluster)))


par(mfrow=c(1,2))

