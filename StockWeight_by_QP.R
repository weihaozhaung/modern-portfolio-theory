rm(list = ls());gc()
library(dplyr)
library(tidyverse)
library(lubridate)
library(quadprog)
options(scipen = 999)

 setwd("C:/Users/weiha/Desktop/finance management/現投/hw03")
# 
# data<- data.table::fread("20181120.txt")
# index<-data.table::fread("TAI50.csv")
# 
# save(data,index,file="allstocks.RData")
# data1<-data.table::fread("20181120_1.txt")
# lastcode <- unique(data1$證券代碼)
load("allstocks.RData")

# code_date<-data.table::fread("twn50.txt")
# unique(code_date$年月日)
# code_date$成份股 <- str_extract(code_date$成份股,"[0-9]{1,4}")
# colnames(code_date)<-c("code","name","date","stock","weight")

colnames(index)<-c("code","name","date","close")
colnames(data) <- c("code","name","date","mv","close")
index$date<-as.Date(as.character(index$date),format="%Y%m%d")
data<-data %>% filter(code!=50)
#-----------experiment--------------------#
#data<-data %>% filter(code!=2330)
#----------------------------------------#
data<-data %>% group_by(code)%>% filter(n()>=527)
#save(data,file="newly_issued_or_failed.RData")
#load("newly_issued_or_failed.RData")
#-------------n()?need?-------------------#
data$code<-as.character(data$code)
data$date<-as.Date(as.character(data$date), format = "%Y%m%d")
#####################Which Date###################################
#dateofBiggestvalue<-ymd("20170601")

Tracking_error<-function(dateofBiggestvalue,stock_num,traindate_start,traindate_end,testdate_start,testdate_end){
#----------------------------------------------------------------#
  dateofBiggestvalue<-ymd(dateofBiggestvalue)
  tw <- data %>% 
  filter(date == dateofBiggestvalue)%>% 
  arrange(desc(mv)) %>% pull(code)   
eval(parse(text=paste0("tw",stock_num,"<-as.character(as.numeric(tw[1:stock_num]) %>% sort())")))# weight 分配位置 跟 股票名單一致

data<-data %>% select(code,name,date,close) %>% bind_rows(.,index)
###################################Start Here#############################
stock_num<-stock_num

traindate_start<-ymd(traindate_start)
traindate_end<-ymd(traindate_end)

testdate_start<-ymd(testdate_start)
testdate_end<-ymd(testdate_end)
#-------------------------------------------------------------------------#

stock_data <- data  %>% group_by(code) %>% 
  mutate(Ret = (close / lag(close, 1)-1)) 
eval(parse(text=paste0("stock_data<-stock_data %>% filter(code %in% tw",stock_num,"|code==as.character('TWN50'))")))

 tw.data <- stock_data  %>% 
   select(code, date, Ret) %>% filter(date>=traindate_start,date<=traindate_end ) %>% 
   spread(code, Ret) %>% na.omit()

r=t(tw.data[-c(1,ncol(tw.data))])
rt=tw.data[,-c(1,ncol(tw.data))] %>% as.matrix()
D=r %*% rt *2

dt=t(tw.data[,ncol(tw.data)]) %*% rt *2
d=t(dt)

At=rbind(rep(1,stock_num),diag(1,stock_num,stock_num))
A=t(At)

b0=t(c(1,rep(0.0001,stock_num)))


weight<-solve.QP(Dmat=D, dvec=d, Amat=A, bvec=b0, meq=1)
weight=weight$solution

testData <- stock_data %>% 
  filter(date >= testdate_start, date <=testdate_end) %>% 
  group_by(code) 

eval(parse(text=paste0("testData<-testData %>% filter(code %in% tw",stock_num,")")))

 testData<-testData %>%  arrange(code, date) %>% 
  select(code, date, Ret) %>% 
  spread(code, Ret) %>% na.omit()

test.index <- stock_data %>% 
  filter(code == "TWN50", date >= testdate_start, date <= testdate_end) %>% 
  select(code, date, Ret) 

te<-sd(as.matrix(testData[, -1]) %*% as.matrix(weight) - as.matrix(test.index[,3]))*sqrt(252)
newdata<-list(TE=te,w=weight,code=unique(stock_data$code))
return(newdata)
#print(te)
}


#-------------TRACKING ERROR AND MAXIMUM WEIGHT AND CODE--------------------------------------------------------------------------------------------#
max_weight_code<-function(dBV,stock_num,tr_star,tr_end,ts_star,ts_end){
print(c(Tracking_error(dateofBiggestvalue=dBV,stock_num=i,traindate_start=tr_star,traindate_end=tr_end,testdate_start=ts_star,testdate_end=ts_end)[[1]],
        Tracking_error(dateofBiggestvalue=dBV,stock_num=i,traindate_start=tr_star,traindate_end=tr_end,testdate_start=ts_star,testdate_end=ts_end)[[2]][which.max(Tracking_error(dateofBiggestvalue=dBV,stock_num=i,traindate_start=tr_star,traindate_end=tr_end,testdate_start=ts_star,testdate_end=ts_end)[[2]])],
      Tracking_error(dateofBiggestvalue=dBV,stock_num=i,traindate_start=tr_star,traindate_end=tr_end,testdate_start=ts_star,testdate_end=ts_end)[[3]][which.max(Tracking_error(dateofBiggestvalue=dBV,stock_num=i,traindate_start=tr_star,traindate_end=tr_end,testdate_start=ts_star,testdate_end=ts_end)[[2]])]))
}
for(i in 1:100){

print(paste0(i,"__",Tracking_error(dateofBiggestvalue=20180430,stock_num=i,traindate_start=20180201,traindate_end=20180429,testdate_start=20180701,testdate_end=20180801)[[1]]))
  #選股日，股數，TRAINDATA,TRAININDEX,TESTDATA,TESTINDEX
}
i=50#SET STOCK NUMS
dBV<-20180502
tr_star<-20160930
tr_end <- 20170930
ts_star<-20171001
ts_end <- 20180930

max_weight_code(dBV,i,tr_star,tr_end,ts_star,ts_end)
