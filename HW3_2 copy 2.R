library(qcc)

# hist(data)

# qqnorm(data,main="QQ plot of normal data",pch=19)
# qqline(data)

##### Non normality (Beta dist) #####

ARL_sw <- vector()
ARL_CUSUM <-  vector()
ARL_CUSUM_5 <-  vector()
ARL_EWMA <-  vector()

for(i in 1:100){
  data <-rbeta(1000, 5, 1)
  
  shewharts<-qcc(data = data, type = "xbar.one", sizes = 5, nsigma = 3, digits = 2,plot = F) 
  ARL_sw <- append(ARL_sw,min(shewharts$violations$beyond.limits))
  
  
  x_bar <- mean(data)
  target_K <- 0.5
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM <- append(ARL_CUSUM, min(cusum_norm$violations$upper))
  }
  
  target_K <- 1
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM_5 <- append(ARL_CUSUM_5, min(cusum_norm$violations$upper))
  }
  
  q <- ewma(data, lambda=0.3, plot = F)
  ARL_EWMA <- append(ARL_EWMA, min(q$violations))
}


cat(mean(ARL_sw), mean(ARL_CUSUM), mean(ARL_CUSUM_5), mean(ARL_EWMA))


##### Non normality (Geometric dist) #####

ARL_sw <- vector()
ARL_CUSUM <-  vector()
ARL_CUSUM_5 <-  vector()
ARL_EWMA <-  vector()

for(i in 1:100){
  data <-rgeom(1000, prob=0.7)
  
  shewharts<-qcc(data = data, type = "xbar.one", sizes = 5, nsigma = 3, digits = 2,plot = F) 
  ARL_sw <- append(ARL_sw,min(shewharts$violations$beyond.limits))
  
  
  x_bar <- mean(data)
  target_K <- 0.5
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM <- append(ARL_CUSUM, min(cusum_norm$violations$upper))
  }
  
  
  target_K <- 1
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM_5 <- append(ARL_CUSUM_5, min(cusum_norm$violations$upper))
  }
  
  q <- ewma(data, lambda=0.3, plot = F)
  ARL_EWMA <- append(ARL_EWMA, min(q$violations))
}


cat(mean(ARL_sw), mean(ARL_CUSUM),  mean(ARL_CUSUM_5), mean(ARL_EWMA))


##### Non normality (F dist) #####


ARL_sw <- vector()
ARL_CUSUM <-  vector()
ARL_CUSUM_5<- vector()
ARL_EWMA <-  vector()

for(i in 1:100){
  data <-rf(1000, 50,100)
  
  shewharts<-qcc(data = data, type = "xbar.one", sizes = 5, nsigma = 3, digits = 2,plot = F) 
  ARL_sw <- append(ARL_sw,min(shewharts$violations$beyond.limits))
  
  
  x_bar <- mean(data)
  target_K <- 0.5
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM <- append(ARL_CUSUM, min(cusum_norm$violations$upper))
  }
  
  target_K <- 1
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM_5 <- append(ARL_CUSUM_5, min(cusum_norm$violations$upper))
  }

  q <- ewma(data, lambda=0.3, plot = F)  
  if(min(q$violations)== Inf){
  }
  else{
    ARL_EWMA <- append(ARL_EWMA, min(q$violations))
  }
}


cat(mean(ARL_sw), mean(ARL_CUSUM), mean(ARL_CUSUM_5), mean(ARL_EWMA))

##### Normality #####


ARL_sw <- vector()
ARL_CUSUM <-  vector()
ARL_CUSUM_5 <- vector()
ARL_EWMA <-  vector()

for(i in 1:100){
  data <- rnorm(1000, mean=0, sd=1)
  
  shewharts<-qcc(data = data, type = "xbar.one", sizes = 5, nsigma = 3, digits = 2,plot = F) 
  if(min(shewharts$violations$beyond.limits)== Inf){
  }
  else{
    ARL_sw <- append(ARL_sw,min(shewharts$violations$beyond.limits))
  }
  
  x_bar <- mean(data)
  target_K <- 0.5
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM <- append(ARL_CUSUM, min(cusum_norm$violations$upper))
  }
  
  target_K <- 1
  cusum_norm <- cusum(data, se.shift = target_K, center = x_bar, plot = F)
  if(min(cusum_norm$violations$upper) == Inf){
  }
  else{
    ARL_CUSUM_5 <- append(ARL_CUSUM_5, min(cusum_norm$violations$upper))
  }
  
  q <- ewma(data, lambda=0.3, plot = F)  
  if(min(q$violations)== Inf){
  }
  else{
    ARL_EWMA <- append(ARL_EWMA, min(q$violations))
  }
}


cat(mean(ARL_sw), mean(ARL_CUSUM), mean(ARL_CUSUM_5), mean(ARL_EWMA))



