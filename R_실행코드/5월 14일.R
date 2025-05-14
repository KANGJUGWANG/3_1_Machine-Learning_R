## 테스트 4 풀이
# 문제 1
# 문제 해석
# ...요소들이 적합하게 분포 되어있는지.. <- 카이제곱 분포의 적합도 검정
# ...어떤 요소들이 적합하지 않은지 판단하시오(유의수준 95%) <- 적합도 검정이후 사후검정

# 과정 
# 데이터 로드 -> 적합도 검정 -> 귀무사설 기각, 대립가설을 채택하는가? -> 사후검정 
data1 <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/카이제곱문제/color.csv")

data_table <- table(data1);data_table
expected<- rep(sum(data_table)/ length(data_table), length(data_table));expected
chi_squared <- chisq.test(x= data_table, p= expected/ sum(expected))
print(chi_squared)
"p-value가 0.05보다 작음으로 대립가설을 채택한다 = 분포의 차이가 일정하지 않다"
std_residuals <- (data_table- expected)/sqrt(expected)
value <- std_residuals^2
p_values <- 1-pchisq(value, df=3)

# 문제 2 
data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/카이제곱문제/incafe.csv")
cross_tab<-table(data$카페분위기, data$만족도)
chi_squared <- chisq.test(cross_tab)
chi_squared
## install.packages("chisq.posthoc.test")
## library(chisq.posthoc.test)

chisq.posthoc.test(cross_tab,method = "bonferroni")


## 5월 14일 학습 내용 정리 
# 카이제곱 분포의 독립성 분석
data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/smartphone_sleep1.csv")

cross_tab<-table(data$스마트폰사용목적, data$수면만족도)
chi_squared <- chisq.test(cross_tab)
print(chi_squared)


data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/smartphone_sleep2.csv")
cross_tab<-table(data$스마트폰사용목적, data$수면만족도)
chi_squared<-chisq.test(cross_tab)
print(chi_squared)
## X-squared = 66.85, df = 8, p-value = 2.076e-11
chisq.posthoc.test(cross_tab, method = "bonferroni")
# 0.05보다 작다 -> 독립이 아니다 (관련성이 있다).

# f 분포 : 집단의 분산 비율
## n개의 집단간의 분산을 비교하여 일지하는지 확인하는 방법
# 집단의 수가 2개일때 f-test
# 분산의 동질성 검정 : 분산분석(f-tset)
# 분산 분석의 귀무가설 : 두집단의 분산이 같다, 대립가설 : 두 집단의 분산이 다르다.
# f-test(ex)
set_a <- c(10.1,10.2,10.3,10.0,10.1,10.2,10.3,10.0,10.1,10.2)
set_b <- c(9.8,10.5,10.2,9.7,10.4,10.3,9.6,10.6,9.9,10.7)
# 양측검정(두 집단의 분산이 같은가?)
result <- var.test(set_a, set_b)
print(result)
# 단측 검정 (집단 a는 집단 b의 분산보다 작다)
var.test(set_a, set_b, alternative = "less")
# 단측 검정 
var.test(set_a, set_b, alternative = "greater")
# ex2)
df= read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/Machine_set.csv")
set_a <- subset(df, machine == 'A')$value
set_b <- subset(df, machine == 'B')$value
result<- var.test(set_a, set_b)
print(result)
var.test(set_a, set_b, alternative = "less")
var.test(set_a, set_b, alternative = "greater")

# 기준 집단에 대하여 복수의 집단들의 분산을 비교한다
set_a <-c(10.2,10.3,10.1,10.0,10.3,10.2,10.1,10.2,10.1,10.3)
set_b <-c(10.1,10.0,10.2,10.3,10.0,10.1,10.2,10.1,10.2,10.1)
set_c <-c(10.2,10.3,10.3,10.3,10.4,10.3,10.2,10.4,10.3,10.4)
var.test(set_b ,set_a,alternative ="less")
var.test(set_c, set_a, alternative = "less")
# 집단 a를 기준으로 집단 b,c의 분산이 집단 a와 같은가? (귀무가설 : 같다, 대립가설(좌측검정) : 작다)

#### 
# 지금까지의 가설검정 추록통계의 방법
# 두 연속형 집단의 평군차이 비교 (t-test, z-test)
# 두 명목형 집단의 연관성 비교(chi square-test) [1집단의 2변수(관측값- 예측값, 관측값 - 관측값) 비교]
# 두 연속형(명목형 가능) 집단의 분산 차이 비교(f-test)

# anova분석 = 다집단의 분산분석 = 집단 "평균" 차이가 있는가?
# 분산분석인 이유 : 개별 집단의 분산과 집단들의 평균 분산을 활용해 집단간의 평균 차이를 구한다
## 여기부터 다음시간에
