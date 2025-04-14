##----- 1. 기술통계 -----## 
# 데이터셋 설정(벡터)
data <- c(10, 15, 20, 25, 30, 15, 20, 25, 25, 10)
# 데이터셋 평균
mean_value <- mean(data)
# 데이터셋 중앙값
median_value <- median(data)
# 데이터 셋 가장 큰값
max_value <-max(data)
# 데이터 셋 가장 작은값
min_value <-min(data)
# 최빈값 구하기 기본함수 아님(사용자 정의 혹은 패키지함수)
mode_value <- find_mode(data)

## 평균의 정의 
#산술평균, 절단평균, 기하평균, 가중평균, 조화평균 
x<-c(1,2,3,4,5)
mean(x)#산술평균
mean(c(2,3,4))#절단평균-값제외
mean(x*0.5+x*0.1)#가중평균
#기하평균
#조화평균

##----- 2. 데이터 시각화(EDA) -----##
install.packages("ggplot2")## 그래프 시각화 패키지
library("ggplot2")## 패키지 부착
# 사용되는 자료형 
x <-c(1,2,3,4); y<-c(2,3,4,5)# 벡터
df<-data.frame(x1=x, y1=y )# 데이터프레임
df1<-data.frame("1열" = c(1,2,3,4), "2열" = c(0.1,0.2,0.3,0.4))# 데이터 프레임

# 사용되는 함수(base)
seq(1,10,2)# 1~10까지 1의 간격으로 배열 생성
rep("?",5)# 값이 5번 반복되는 배열 생성
paste("1",1:5)# 값 연결 x에 y를 연결한다  y는 배열 가능
paste("학생", 1:10)

# ggplot2
ggplot()# <- 기본 구조
geom_abline() # <-geom_형식에 따라서 그래프가 달라짐
labs()#그래프 레이블 옵션
xlab()# x레이블 옵션
ylab()# y레이블 옵션
# df <-data.frame("x" =c(1,2,3), "y" = c(0,1,2))
# ggplot(df, aes(group=x, y=y)) + geom_boxplot([박스플롯 옵션])
## 박스플롯은 x값의 종류(다른 갯수)가 박스의 갯수가 됨
## ggplot(data = df, aes(x = 그룹변수, y = 수치변수))
df <-data.frame("x1" =c(c(1,1,1),c(2,2,2)), "y1" = c(c(0,1,2),c(0,1,2)))
ggplot(df, aes(group =x1, y=y1)) + geom_boxplot()

