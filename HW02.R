rm(list = ls());gc()
library(dplyr)
library(reshape2)
library(ggplot2)
library(data.table)

setwd("E:\\NSYSU\\現投")
stockData <- fread("10_stock.txt", sep = "\t")
colnames(stockData) <- c("code", "name", "date", "open", "high", "low", "close", "volume")

retData <- stockData %>% 
  group_by(code) %>% 
  mutate(dayLogRet = log(close / lag(close, 1))) %>% 
  na.omit() %>% 
  dcast(., formula = date ~ code, value.var = "dayLogRet")

# 第一題
# 無風險利率1.04%
rf <- 0.0104
ret_2016_2017 <- retData %>% filter(date >= 20161012, date <= 20171011)
ret_2017_2018 <- retData %>% filter(date >= 20171012, date <= 20181011)

# 2016~2017年化溢酬 & 年化變異數
apply(ret_2016_2017[, 2:11], 2, mean)*250 - rf
apply(ret_2016_2017[, 2:11], 2, var)*250
# 2017~2018年化溢酬 & 年化變異數
apply(ret_2017_2018[, 2:11], 2, mean)*250 - rf
apply(ret_2017_2018[, 2:11], 2, var)*250
################################# 火腿法 #################################
yr <- retData[, 2:11]*250*100
v <- cov(retData[, 2:11]*100)*250
#average<-apply(data,2,mean)*250
Ff <- apply(yr[, 1:10], 2, mean)
f <- matrix(Ff, 10, 1)
e <- matrix(1, 10, 1)

A <- t(e) %*% solve(v) %*% e
B <- t(e) %*% solve(v) %*% f
C <- t(f) %*% solve(v) %*% f
delta <- A * C - B^2

mu <- seq(-100,100,by=0.05)
var.p=NULL
for(i in 1:length(mu)){
  var.p <- c(var.p,(A*mu[i]*mu[i]-2*B*mu[i]+C)/delta)
}

efficient_frontier1 <- tibble(mu = mu, Var = sqrt(var.p), group = as.factor(c(rep(1, 2250), rep(2, 1751))))

################################# 切線法 #################################

risk_year <- sapply(retData[, 2:11],function(x) round(mean(x)*250,8))

rf_vector <- seq(-500,500,by=0.1)
efficient_frontier2 <- data.frame(miu=NA,Var=NA)

for(i in 1:length(rf_vector))
{
  
  V <- cov(retData[, 2:11])*(250)
  f <- as.vector(risk_year)
  e = rep(1, 10)
  
  weight = as.vector(solve(V) %*% (f-rf_vector[i]*e) %*% (1/(t(e) %*% solve(V) %*% (f-rf_vector[i]*e))))
  
  var = as.vector(t(weight) %*% V %*% weight)
  miu = as.vector(weight %*% f)
  
  efficient_frontier2[i,1] <- miu
  efficient_frontier2[i,2] <- var
}  

efficient_frontier2 <- filter(efficient_frontier2, miu<1 & miu>-1)
# efficient_frontier2$group <- as.factor(c(rep(1, 10000), rep(2, 10000)))
# efficient_frontier2 <- efficient_frontier2 %>% filter(Var <= (30/100)^2)
################################# 畫圖 #################################
hq.sd <- (t(solve(V) %*% (f-0.0104) %*% (1/(t(f-0.0104) %*% solve(V) %*% as.matrix(e)))) %*% V %*% solve(V) %*% (f-0.0104) %*% (1/(t(f-0.0104) %*% solve(V) %*% as.matrix(e))))^0.5*100
hq.mean <- as.vector(solve(V) %*% (f-0.0104) %*% (1/(t(f-0.0104) %*% solve(V) %*% as.matrix(e))))%*%(f-0.0104)*100
# 火腿法畫圖
plot(sqrt(var.p), mu,type = "l", xlab = "投資組合標準差(%)", ylab="預期報酬(%)"
     , main="火腿法效率前緣(年化)", xlim=c(0,50), ylim=c(-100, 100))#xlim=c(20,40)
points(x=sqrt(1/A), y=B/A, pch=19, col="red")      # C點
points(x = 0, y = 1.04, pch=19, col="blue")        # Rf
points(x = 13, y = 0, pch=19, col="blue")          # Z點
points(x = hq.sd, y = hq.mean, pch=19, col="blue") # Q點
abline(a = 1.04, b=2.45)                           # 切線


# 切線法畫圖
plot(x=as.vector((efficient_frontier2$Var)^(0.5)*100),y=as.vector(efficient_frontier2$miu*100), type = 'l',
     xlab = "投資組合標準差(%)", ylab="預期報酬(%)", main="切線法效率前緣(年化)", xlim=c(0,50), ylim=c(-100, 100))
points(x=sqrt(1/A), y=B/A,pch=19, col="red")        # C點
points(x = 0, y = 1.04, pch=19, col="blue")         # Rf
points(x = 13, y = 0, pch=19, col="blue")           # Z點
points(x = hq.sd, y = hq.mean, pch=19, col="blue")  # Q點
abline(a = 1.04, b=2.45)                            # 切線

# solve(V) %*% f / sum(solve(V) %*% (f-0.0104))
# solve(V) %*% (f-0.0104) %*% (1/(t(f-0.0104) %*% solve(V) %*% as.matrix(e)))

# ggplot(efficient_frontier1, aes(x = Var, y = mu, color=group))+
#   geom_point()+
#   geom_path()
# 
# ggplot(efficient_frontier2, aes(x = sqrt(Var)*100, y = miu, color=group))+
#   geom_point()+
#   geom_path()
