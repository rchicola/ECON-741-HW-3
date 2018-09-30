file_path<-"/home/appertjt/Documents/Grad School/econometrics fall/Code/homework3/ECON-741-HW-3/CH6_HW-1.dta"

library(foreign)
library("readstata13")

#read the file in

data<-read.dta13(file_path)

#load data into a dataframe

df1.d<-data.frame(data)
df1.d

#convert all columns to numeric values
df1.d<-transform(df1.d, incwage=as.numeric(incwage), uhrswork=as.numeric(uhrswork), age=as.numeric(age))

#get a summary of the dataframe

summary(df1.d)

#Need the following for the polynomials question:
#  Age >15 and < 66
#  Earnings >0
#  Hours>1000
df1.d<-df1.d[df1.d["incwage"]<999999,]  #filtering out 
df1.d<-df1.d[df1.d['incwage']>0,]  #wages >0
df1.d<-df1.d[df1.d["age"]>15,]  #older than 15
df1.d<-df1.d[df1.d["age"}<66,]  #younger than 66
df1.d["uhrswork"]<-df1.d["uhrswork"]*50
df1.d<-df1.d[df1.d["uhrswork"]>1000,]

summary(df1.d)

###############################
### Run the first regression###
### Age on Wages###############
###############################


#run a 4th order linear regression
reg_age = lm(incwage ~ poly(age, 4), data=df1.d)
summary(reg_age)




