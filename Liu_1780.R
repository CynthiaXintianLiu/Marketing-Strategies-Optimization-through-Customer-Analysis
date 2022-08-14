#################################
# Stat 102B project
# Group member:Xintian Liu(1780),Shang Ma(6049)
# Copyright 9/7/2021  Xintian Liu,Shang Ma.All rights reserved
#################################

#######Part I Data processing and description
#######1st step:read and standardize data
data=read.csv("D:\\桌面\\B学习文件\\Summer school\\Statistics\\Dataset\\Liu_1780.csv")
#Our data do not have any missing value
#We use 14 numerical variables as a matrix to do further analysis  
x=subset(data,select=c(wheelbase,carlength,carwidth,carheight
                       ,curbweight,enginesize,boreratio
                       ,stroke,compressionratio,horsepower,peakrpm,citympg,highwaympg,price))
X=as.matrix(x)
X.cs1=scale(x,scale=T)
head(X.cs1) #centered and scaled data

#######2rd step:describe the data and process the dataset 
summary(x)#find mean, medians, sd and IQR of each variable
dim(x)
hist(X.cs1[,1])#draw the histograms of each variable
#Check the outleir by box plot
#box plot shows the outliers
boxplot(X.cs1)
#replace outliers with means
fun.outlier <- function(x,iqr=1.5){
  for (i in 1:ncol(x)){
    outlier.low <- quantile(x[,i],probs=c(0.25))-IQR(x[,i])*iqr
    outlier.high <- quantile(x[,i],probs=c(0.75))+IQR(x[,i])*iqr
    x[,i][which(x[,i]>outlier.high|x[,i]<outlier.low)]<-NA
    x[,i]
  }
  return(x)
}
new_data1=fun.outlier(X.cs1)
X.cs=na.omit(new_data1)
list <-which(rowSums(is.na(new_data1)) > 0)
newdata=data[-list,]

#describe the new data set
summary(X.cs)
dim(X.cs)
##we still use 194 observations and the 14 variables.

#######Part II. Cluster Analysis
#######1st step:choose variables for cluster analysis
### In Cluster Analysis,
### Based on our general knowledge, we chose six popular variables
### that show car’s properties well and are the most important factors that have an influence on car classification
x3=subset(X.cs,select=c(carlength,carwidth,carheight,curbweight,enginesize,price))

#######2rd step:cluster analysis using nearest neighbor
D1=dist(x3);D1
nn=hclust(D1,method="single",members=NULL)
plot(nn,main="nearest neighbors ")
clusters.nn=cutree(nn,k =3)#choose cluster number=3
table(clusters.nn)
#find means, medians and standard deviations of the variables
apply(x3[clusters.nn==1,],2, function(x) c(mean=mean(x3), median=median(x3), sd=sd(x3), size=length(x3)))
apply(matrix(x3[clusters.nn==2,], nrow = 1), 2, function(x3) c(mean=mean(x3), median=median(x3), sd=sd(x3), size=length(x3)))
apply(matrix(x3[clusters.nn==3,], nrow = 1), 2, function(x3) c(mean=mean(x3), median=median(x3), sd=sd(x3), size=length(x3)))

#######3th step:cluster analysis using k-means methods
Car_kmeans=kmeans(x3,3,nstart=20)
Car_kmeans # Do k-means clustering, k=3 
Car_cluster=Car_kmeans$cluster
Car_cluster
table(Car_cluster)

table(Car_cluster, newdata$symboling)
c=prop.table(table(Car_cluster, newdata$symboling), 1)
#write.csv(c,"C:\\Users\\Administrator\\Desktop\\correlation.csv")

#find each carname falls into which clusters
#Cause our research purpose is to find the competitors 
#of specific type of car
#find means, medians and standard deviations of the variables
apply(x3[Car_cluster==1,], 2, function(x3) c(mean=mean(x3), median=median(x3), sd=sd(x3), size=length(x3)))
apply(x3[Car_cluster==2,], 2, function(x3) c(mean=mean(x3), median=median(x3), sd=sd(x3), size=length(x3)))
apply(x3[Car_cluster==3,], 2, function(x3) c(mean=mean(x3), median=median(x3), sd=sd(x3), size=length(x3)))

######4th step: scatter plots of pairs of variables
###We choose some pairs of variables that could show
###the cars' characteristics for labeling 
#Plot 1. scatter plot of Car length & Curb Weight
plot(x3[,1], x3[,4],
     col=c("red","blue","yellow")[unclass(Car_cluster)],pch=c(23,24,25)[unclass (Car_cluster)],main="Car length & Curb Weight",xlab="Car length", ylab="Curb Weight")
legend("topleft",c("Car length","Curb Weight"),pch=c(23,24))
###Plot 2. scatter plot of Curb Weight & Price
plot(x3[,4], x3[,6],
     col=c("red","blue","yellow")[unclass(Car_cluster)],pch=c(23,24,25)[unclass (Car_cluster)],main="Curb Weight & Price",xlab="Curb Weight", ylab="Price")
###Plot 3. scatter plot of Car Length & Engine Size
plot(x3[,1], x3[,6],
     col=c("red","blue","yellow")[unclass(Car_cluster)],pch=c(23,24,25)[unclass (Car_cluster)],main="Car length & Engine Size",xlab="Car Length", ylab="Engine Size")

#######Part III:Principal components analysis 
######1st step:dimension reduction
#this plot show the correlations between variables,most correlations
#>0.5. So it is necessary to do PCA
#The plot isn't included in report cause it doesn't look nice.
pairs(X.cs,lower.panel=NULL,col=c("blue"),pch=c(8),
      main="pairs plot(standard deviation units)",
      xlim=c(-3.5,3.5),ylim=c(-3.5,3.5))
###we use all the numerical variables and see if we 
#can reduce some dimension based on threshold choice
eigen(var(X.cs))
eigenvalues=eigen(var(X.cs))$values
eigenvalues
(cumsum(100*eigenvalues/(sum(eigenvalues))))
###we want to choose PCs that account for at least
#90% of the variance, so we save first 9 PCs, which
#account for 91.87785%
#reduce 14 variables to 9 variables

######2rd step:principal components analysis
eigen(var(X.cs))
eigenvectors=eigen(var(X.cs))$vectors
V=eigenvectors
PC=X.cs%*%V
head(PC)#create pc matrix
##take the first 9 PCs as a new matrix
PC_matrix=as.matrix(PC[,1:9]);dim(PC_matrix)
colnames(PC_matrix)=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9")
head(PC_matrix)

##correlations of the PC and original variables 
cor(PC_matrix,X.cs)
t(cor(PC_matrix,X.cs))#we can see the correlations of
#original variables and PCs chosen
write.csv(t(cor(PC_matrix,X.cs)),"D:\\桌面\\correlation.csv")
biplot(prcomp(X.cs[,1:14]))#biplot have too many observations 
#that it does not look nice so it's not included in our report


######Part IV. Principal components regression for prediction

######1st step:Split the data into a training set and 
#a small test set of 10 rows of data
#We define the independent variable,y-price
Inde_vari=X.cs[11:194,14]
t(cor(PC,X.cs))
#find correlations between y and PCs
##using the criteria learned in the lesson on PC regression,
#we find principal components that have the biggest correlation
#with independent variable. They are X1,X11,X12.The correlation 
# of those variables is -0.7121251,-0.3489914,0.3667739
#So we use X1,X11,X12 as dependent variables to do regression analysis
test=PC[1:10,];dim(test)#test set
New_test_matrix=test[,c(1,11,12)]#matrix for dependent variables
colnames(New_test_matrix)<-c("x1","x2","x3")
train=PC[11:194,];dim(train)#training set
New_matrix=cbind(train[,c(1,11,12)],Inde_vari)#matrix for dependent variables and y
colnames(New_matrix)<-c("x1","x2","x3","y")
######2rd step:normalize the columns of the matrix of PC before doing the PC regression
PC[,1] <- PC[,1]/sqrt(as.numeric(crossprod(PC[,1])))
PC[,2] <- PC[,2]/sqrt(as.numeric(crossprod(PC[,2])))
PC[,3] <- PC[,3]/sqrt(as.numeric(crossprod(PC[,3])))
PC[,4] <- PC[,4]/sqrt(as.numeric(crossprod(PC[,4])))
PC[,5] <- PC[,5]/sqrt(as.numeric(crossprod(PC[,5])))
PC[,6] <- PC[,6]/sqrt(as.numeric(crossprod(PC[,6])))
PC[,7] <- PC[,7]/sqrt(as.numeric(crossprod(PC[,7])))
PC[,8] <- PC[,8]/sqrt(as.numeric(crossprod(PC[,8])))
PC[,9] <- PC[,9]/sqrt(as.numeric(crossprod(PC[,9])))
PC[,10] <- PC[,10]/sqrt(as.numeric(crossprod(PC[,10])))
PC[,11] <- PC[,11]/sqrt(as.numeric(crossprod(PC[,11])))
PC[,12] <- PC[,12]/sqrt(as.numeric(crossprod(PC[,12])))
PC[,13] <- PC[,13]/sqrt(as.numeric(crossprod(PC[,13])))
PC[,14] <- PC[,14]/sqrt(as.numeric(crossprod(PC[,14])))


######3th step:regression function using train data set
lm1<-lm(y~x1+x2+x3,data=data.frame(New_matrix))
lm1
##regression function:
#y = -0.03227-0.23372*x1-0.49985*x2+0.58580*x3
summary(lm1)#To see coefficient and significance
par(mfrow=c(2,2))
plot(lm1)#To see the residual

######4th step:predict the values of the dependent 
predict_test<-predict(lm1,data.frame(New_test_matrix),interval="prediction")
head(predict_test,10)

##draw a plot to see the prediction visually
##compare the predicted value and test values
par(mfrow=c(1,1))
plot(x=c(1:10),y=predict_test[1:10,1],
     main="Prediction and Real ones",
     xlab="the number of observation",ylab="Prediction and Real ones",col="blue",pch=21)

par(new=TRUE)
plot(x=c(1:10),y=X.cs[1:10,14],xaxt='n',yaxt='n',ann=F,col="red",pch=21)
## observation 10 and 9 got the same values
## values show similar trend

######5th step:Calculate the root mean square error
RMSE <- sqrt(mean((X.cs[1:10,14]-predict_test[1:10,1])^2))
RMSE
######6th step:change back to original variable
predict_test<-as.data.frame(predict_test)
predict_test$fit<-predict_test$fit*sd(x$price)+mean(x$price)
predict_test$lwr<-predict_test$lwr*sd(x$price)+mean(x$price)
predict_test$upr<-predict_test$upr*sd(x$price)+mean(x$price)
head(predict_test)


######Part V.Maximum Likelihood estimation 
######1st step:find the model fits the data well
y=X[,14]/1000#denote independent variable-price/1000 as y
hist(y, prob=T,ylim=c(0,0.1))#using histogram to find initial value to start the program
y_point=c(seq(0,max(y),by=0.1))
points(y_point,dlnorm(y_point, meanlog=2.5, sdlog=0.4,log=FALSE), col="red", type="o",pch=21, bg="red")
n=length(y); n
#The histogram is right-skewed. So we choose log-normal distribution
#to fit our data.

#using contour plot to find initial value for double check
x1=seq(1,3,by=0.01)
x2=seq(0.3,3,by=0.01)
f=matrix(0,nrow=length(x1),ncol=length(x2)) 
for(i in 1:length(x1)){ 
  for(j in 1:length(x2)) {
    f[i,j]=-sum(log(y))-n*log(x2[j])-(n/2)*(log(2*pi))-(1/(2*(x2[j])^2))*sum((log(y)-x1[i])^2)
  }}
contour(x1,x2,f,nlevels=30,xlab="mu",ylab="sigma") 
##mu=2.5,sigma=0.5 seem to be the value we find

######2rd step:numerical optimization
xt=c(100,0)   # initial values for xt
eps=0.0000001  # tolerance criterion for xtp1-xt 
xtp1=c(2.5,0.5)    # initial values for mu and sigma
xHist=matrix(xtp1,2,1) # save history of xt
# objective function 
f=-sum(log(y))-n*log(xtp1[2])-(n/2)*(log(2*pi))-(1/(2*(xtp1[2])^2))*sum((log(y)-xtp1[1])^2)
# History of the objective function
fHist=f

while(sum((xtp1-xt)^2)>eps){
  xt=xtp1#compute first and second derivatives
  xt
  gradient=as.vector(c(sum(log(y)-xt[1])/xt[2]^2,
                       -n/xt[2]+sum((log(y)-xt[1])^2)/xt[2]^3))
  gradient
  hessian=matrix(c(-n/xt[2]^2,
                   -2*(sum(log(y)-xt[1]))/xt[2]^3,
                   -2*(sum(log(y)-xt[1]))/xt[2]^3,
                   n/xt[2]^2-3*(sum((log(y)-xt[1])^2)/xt[2]^4)),ncol=2,nrow=2)
  hessian
  ###  
  # compute xtp1 solve(hessian*gradient=hessian^{-1}*gradient)
  ###   
  xtp1=xt-solve(hessian,gradient)
  xtp1
  #save history
  xHist=matrix(c(xHist,xtp1),2)
  f=-sum(log(y))-n*log(xtp1[2])-(n/2)*(log(2*pi))-(1/(2*(xtp1[2])^2))*sum((log(y)-xtp1[1])^2)
  fHist=c(fHist,f)
}
xHist
mu.mle=xHist[1,5];mu.mle
sigma.mle=xHist[2,5];sigma.mle
###we can see find the MLE estimators 
#mu.mle=2.42405,sigma.mle=0.4826281  the number of iterations=5
fHist#keep unchanged at the iterations=4
hessian
eigen(hessian) 
# both eigenvalues are negative,which means the estimate
#is a maximum likelihood estimate
gradient
#gradient is close to 0 at the iterations=5

######3th step:find confidence intervals
diag(solve(-hessian))#the variances
sqrt(diag(solve(-hessian)))#the standard errors
### Confidence intervals ###
CI_mu <- mu.mle + c(-1,1)*qnorm(0.975)*sqrt(diag(solve(-hessian)))[1]
CI_mu
CI_sigma <- sigma.mle + c(-1,1)*qnorm(0.975)*sqrt(diag(solve(-hessian)))[2]
CI_sigma
######4th step: find the probability of predicted values like those predicted in part IV 
### We chose the cumulative Distribution function for the possibility
### set the lower.tail to False to obtain P[X>=x]
### To get the possibility by this function,
### we must use the values of centered and scaled data
### we used the mle of mu and sigma as our original values
PP=predict_test
r.lower=1-pnorm(PP,mean=2.42405,sd=0.4826281,lower.tail = F)
r.higher=pnorm(PP,mean=2.42405,sd=0.4826281,lower.tail = F)
r.lower
r.higher
