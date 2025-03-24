# 1
# 100, 200, 300, 400,200,200, 100,400,250,230,70,80,90이란 값들이 존재한다. 
# 해당 값에서 평균, 표준편차, 중앙값, 사분위수(Q1, Q3)와 사분위수 범위를 구하시오.

data<-c(100, 200, 300, 400,200,200, 100,400,250,230,70,80,90)
mean_value<-mean(data)
sd_value<-sd(data)
median_value <-median(data)

Q1 <- quantile(data, 0.25)
Q3<- quantile(data, 0.75)
iqr_value<-Q3-Q1

#2
#Score.txt파일을 불러와 .csv파일로 저장하시오.
