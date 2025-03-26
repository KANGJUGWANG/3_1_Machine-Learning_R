## 탐색적 데이터 분석
## 데이터의 분포를 시각적으로 확인하는 방법
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)
# line chart
time <- seq(1, 24) # 24시간
temperature <- c(22, 21, 20, 19, 18, 18, 19, 20, 22, 24, 26, 28, 29, 28, 27, 25, 24, 23, 22, 22, 21, 21, 20, 19)
data <- data.frame(time=time, temp=temperature)
ggplot(data, aes(x=time, y=temp)) + 
  geom_line() + # 선 그래프 추가
  labs(title="시간에 따른 온도 변화") +
  xlab("시간") +
  ylab("온도")

# scatter plot_1(점만 찍기)
df <- data.frame( x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7)) 
ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

# scatter plot_2(점에 선 연결)
df <- data.frame( x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7)) 
ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 3) + 
  geom_line(aes(color = " Connected Points"), size = 0.5)+
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

# scatter plot_3(2변수로 그리기)
df <- data.frame( x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7)) 
df2 <- data.frame(x= c(5,6,7,8,9), y=c(18,12,16,77,63))
ggplot() + 
  geom_point(data = df, aes(x=x,y=y),color = "blue", size = 3) + 
  geom_line(data = df , aes(x=x,y=y, color = " Connected Points"), size = 0.5)+
  geom_point(data = df2, aes(x=x,y=y), color = "blue", size = 3)+
  geom_line(data = df2 , aes(x=x,y=y, color =" Connected Ponts 2"), size =0.5)+
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

# # scatter plot_4(회귀선 추가)
df <- data.frame( x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7)) 
ggplot(df, aes(x = x, y = y)) + 
  geom_point(color = "blue", size = 3) + 
  geom_smooth(method = "lm", se = FALSE, aes(color = "Trendline")) +
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

# boxplot
df <- data.frame(
  group = c(rep("Group 1", 60), rep ( "Group 2 ",60)),
  values = c(rnorm(60, mea = 0 ,sd =1), rnorm(60, mean = 2, sd =1))
)
ggplot (df,aes(x = group, y = values))+
  geom_boxplot(fill = c("lightblue","lightgreen"), outlier.color = "red")+
  labs(title = "Boxlot Example")+
  xlab("Group")+
  ylab("Values")

#bar chart 
city <- c("Seoul" ," Busan", "Daegu","Seoul" ," Busan", "Daegu","Ulsan")
pm25 <- c(18,21,21,17,8,11,25)
df <- data.frame(city = city, pm25,pm25)
ggplot (df, aes(x= city , y=pm25, fill = city))+
  geom_bar(stat = "identity") + 
  labs(title = "지역별 초미세먼지 농도") +
  xlab("City")+
  ylab("농도")

#bar chart 
city <- c("Seoul" ," Busan", "Daegu","Seoul" ," Busan", "Daegu","Ulsan")
vari <- c("오전", "오후","오전", "오후","오전", "오후", "오후")
pm25 <- c(18,21,21,17,8,11,25)
df <- data.frame(city = city, pm25,pm25, vari = vari)
ggplot (df, aes(x= city , y=pm25, fill = vari))+
  geom_bar(stat = "identity") + 
  labs(title = "지역별 초미세먼지 농도") +
  xlab("City")+
  ylab("농도")


df <- data.frame(values = c(5,7,10,12,14,18,20,22,25,27,30))
ggplot(df, aes(x = values)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Histogram") +
  xlab("Values") +
  ylab("Density")

df <- data.frame(values = c(5, 7, 10, 12, 14, 18, 10, 22, 25, 27, 30))
# 히스토그램과 밀도 곡선 그리기
ggplot(df, aes(x = values)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "steelblue", color = "white") +
  geom_density(alpha = 0.3, fill = "red") + # 밀도 곡선 추가
  labs(title = "Histogram with Density Plot") +
  xlab("Values") +
  ylab("Density")

city <- c("Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", "Ulsan")
pm25 <- c(18, 21, 21, 17, 8, 11, 25)
colours()
colors <- c("red", "orange", "yellow", "green", "lightblue", "blue", "violet")
df <- data.frame(city = city, pm25 = pm25, colors = colors)
ggplot(df, aes(x= "", y = pm25, fill = city)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Concentration of Ultrafine Dust by Region") +
  xlab("") +
  ylab("")
#컬러를 지정하기 위해서
scale_fill_manual(values=colors)



# 가상의 성적 데이터 생성
students <- paste("Student", 1:10) # 10명의 학생
subjects <- c("Math", "Science", "English") # 3개 과목
grades <- matrix(sample(50:100, 30, replace=TRUE), nrow=10, ncol=3, dimnames=list(students, subjects)) # 과목별 성적
# 데이터 프레임으로 변환
grades_melted <- melt(grades, id.vars = rownames(grades))
grades_melted
colnames(grades_melted) <- c("Student", "Subject", "Grade")
# Heatmap 생성
ggplot(grades_melted, aes(x=Subject, y=Student, fill=Grade)) + 
  geom_tile(color = "white") + # 타일 테두리 추가
  scale_fill_gradient(low="green", high="red") + # 성적에 따른 색상 그라데이션 지정
  #theme_minimal() + # 미니멀한 테마 적용
  labs(title="학생별 과목 성적 Heatmap")+
  xlab("과목") +
  ylab("학생")





## 기초 통계 -추론통계(평균의 분포)
# 확률 변수: 이산확률 변수, 연속확률 변수(원소를 셀수없는 경우 포함)
# 확률의 종류(?): 확룰 질량, 확률 밀도
# 확률의 분포 :이산 확률 분포, 연속 확률 분포 
# 중심 극한정리 : 표본의 커질수록 모집단의 분포와 상관없이 표본의 분포는 정규분포에 가까워진다
# 중심극한정리 조건 : n>=30 , 모집단은 유한하며 알려짐, 표본의 관측차는 독립적임
# 모집단을 모두 계산이 불가 -> 표본을 구해 모집단을 추정(예측)한다
# 모집단과 표본의 통계량 계산은 분산을 제외하면 동일함
# 표본의 분산은 자유도를 고려해 /n 이아닌 /n-1로 계산함
## 단순 이해 -> 표본은 모집단보다 크기가 작다 -> 표본을 /n을 하면 모집단을 과소평가 하는게 된다
## -> /n-1 을 하게되면 분산의 값이 커지면서 과대평가할수있음(????)
# 표준 정규분포 = 평균이 0이고 분산이 1인 정규분포
## x축이 다른 분포를 비교할수 있을까? -> 표본의 사이즈가 커지면 정규분포를 이룬다(가정)
## -> 표준 분포를 평균이 0, 분산이 1인 분포로 변경 -> 기준이 다른 분포끼리도 비교 가능 
# 

(157-160)/(sqrt(49/10))
1-0.9131

(20-15)/sqrt(100/25)
0.9938

(15-25)/sqrt(144/35)
1-0

