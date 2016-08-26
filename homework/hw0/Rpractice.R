getwd()
setwd("F:/2015 spring/cs 249 ml/hw1")
getwd()
list.files()
state<-read.csv(  "Data_sb49.csv"  )
state<-na.omit(state)
if(state$Open[1]>state$Close[1])
{status<-"up"}
else if (state$Open[1]<state$Close[1])
{status<-"down"}
else{status<-"flat"}

status

cong<-state$Open>state$Close

status<-ifelse(cong,"up","down")
cong<-state$Open<state$Close
status<-ifelse(cong,"down",status)
status
table(status)

x<-sample(-10:20,10)
y<-sample(-10:20,10)
x
y
any(x==y)
x>0|y>0

fluctuation<-(state$High-state$Low)/state$Open
head(fluctuation)
big.change<-abs(fluctuation)>0.08
head(big.change)
which(big.change)
x<-c()
for (i in 1:10)
{x<-append(x^2,i^2)}
x
state
the.ticker<-unique(state$Close)
for (ticker in the.ticker){
      these<-state$Close==ticker
}

a<-1
b<-2
c<-"dd"
d<-"ss"
ls()
rm(b)
rm(list=ls())
x<-1:10
x
as.numeric(x)
as.factor(x)
as.character(x)
as.matrix(x)

head(state,3)
apply(state,2,sum,na.rm=TRUE)
