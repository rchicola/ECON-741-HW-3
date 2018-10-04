file_path<-"/home/appertjt/Documents/Grad School/econometrics/Code/homework3/ECON-741-HW-3/munged_data.csv"

#read the file in

data<-read.csv(file_path)

#load data into a dataframe

df1.d<-data.frame(data)


#convert all columns to numeric values
df1.d<-transform(df1.d, incwage=as.numeric(incwage), uhrswork=as.numeric(uhrswork), age=as.numeric(age))

#get a summary of the dataframe

summary(df1.d)

#get a column for age squared
df1.d<-transform(df1.d, age2=age^2)
#assign the independent and dependent variables
y<-as.matrix(df1.d["incwage"])
x1<-as.matrix(subset(df1.d, select=c("age", "age2")))

#run a 2nd order linear regression
#create a column of constants
#set up the constant
cons<-matrix(nrow=nrow(x1), ncol=1,1)
xmat<-cbind(x1, cons)

#find betaHat

betaHat<-(solve(t(xmat)%*%(xmat)))%*%(t(xmat)%*%y)

#find yhat
yHat=xmat%*%betaHat

#find the residuals
epsilonHat=y-yHat

#find sigmaHat
sigmaHat<-(t(epsilonHat)%*%epsilonHat)/(nrow(xmat)-ncol(xmat))

#get the standard errors
se=sigmaHat[1]*solve(t(xmat)%*%(xmat))
print (solve(t(xmat)%*%(xmat)))
print(sqrt(diag(se)))


