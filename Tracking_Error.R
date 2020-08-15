rm(list=ls());gc()
library(tidyverse)
library(ggplot2)
library(data.table)
library(lubridate)
setwd("D:/下載/modern")
#----------------------------------------------------------------#
sharenums<-fread("sharenums.txt")
colnames(sharenums)<-c("code","name","date","stkname","sharenums")
sharenums$date<-ymd(sharenums$date)
adjstks<-fread("adjstocks.txt")
colnames(adjstks)<-c("code","name","date","close","mv")
adjstks$date<-ymd(adjstks$date)
Y9997<-fread("9997.txt")
colnames(Y9997)<-c("code","name","date","close")
Y9997$date<-ymd(as.character(Y9997$date))
adjstks$code<-as.character(adjstks$code)
#----------------------------------------------------------------#
choose_code_date<-ymd(20180430)

stknum<-400
ts.start<-20180501
ts.end<-20180731
adjstks.codedata<-filter(adjstks,date==choose_code_date)
eval(parse(text=paste0("code",stknum,"<-adjstks.codedata %>% arrange(desc(mv)) %>% slice(1:",stknum,") %>% select(code,mv) %>% mutate(weight=mv/sum(mv))")))

#----------------------------------------------------------------#
adjstks.select<-adjstks %>% group_by(code) %>% mutate(ret=close/lag(close)-1) %>% na.omit() %>% filter(date>ymd(ts.start),date<ymd(ts.end))
Y9997.select<-Y9997%>% mutate(ret=close/lag(close)-1)
Y9997.select<-Y9997.select %>% filter(date>ymd(ts.start),date<ymd(ts.end))

eval(parse(text=paste0("data.select<-filter(adjstks.select,code%in%code",stknum,"$code) %>% left_join(code",stknum,",by=c('code')) %>% group_by(date) %>% summarise(totalret=sum(weight*ret))")))
Y9997.select$date<-ymd(Y9997.select$date)
data.selectq3<-data.select%>% left_join(Y9997.select,by="date")
sqrt(252)*sd(data.select$ret-data.select$totalret)


