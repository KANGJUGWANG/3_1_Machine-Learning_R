## 상관관계 분석
# 상관관계는 공분산(변수 사이의 관계)을 통해 분석한다
# 상관관계 분석에 기본이 되는 공분산은 극단치의 영향을 많이 받는다
# 극단치 제거 = eda(박스플롯)->시각적 확인을 통한 극단치 제거

# 상관관계 분석을 진행하는 이유
## 상관관계가 높은 변수들끼의 분석(아노바,f)은 분석에 혼란을 야기시킬수있음

#상관관계 분석
#상관계수와 p-value
#귀무가설(h0) : 변수간의 상관관계가 0이다 = p-value는 0이다.
#대립가설(h1) : 변수간의 상관관계가 0이 아니다(양측)
# + 변수간의 상관관계가 0보다 크다(작다) <-(단측)
# 



#ex 
# 키와 몸무게 데이터 생성 (피어슨 상관과계 분석)
heights <- c(160,162,155,180,170,175,165,171,177,172)
weights <- c(55,60,53,72,70,73,62,64,69,65)
#install.packages(psych)
library("psych")
result_pearson = corr.test(heights, weights, method="pearson")
result_pearson$p ## p-value
result_pearson$r ## 상관관계 계수

# 스피어만 상관관계 분석
# 범주형 -> 순위형 변수 의 분석에 사용됨
## 순위형 + 순위형변수의 분석에서 비선형 상관관계를 파악할떄 사용함
#분석 가능한 변수(순위형-순위형, 순위형-수치형)
# ex (고객 만족도(순위), 서비스 재이용 횟수(수치))
# ex2 (공부 강도(순위), 성적 등수(순위))
# 불가능한 분석
# 범주형(이진)-수치형<-불가능
# 명목형<-이건 그냥 불가능
 
# 스피어만 상관관계분석
# ex) 학생의 자기 평가와 평가자의 평가(순위형 - 순위형)
s_eval <-c(3,4,2,5,1,4,3,2,5,3)
e_eval <-c(2,5,2,4,1,4,3,2,5,3)
reslut_pearson=corr.test(s_eval, e_eval,method = "spearman")
reslut_pearson$p
reslut_pearson$r


## 실습
# 상관관계 분석
data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/spearman/pearson.csv");data
data2 <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/spearman/spearman.csv");data2
# 피어슨 상관관계 분석
indep_pearson<- data[,c("수면시간", "휴대폰사용시간","카페인섭취량","운동시간"), header=None]
target_var_peareson<-data[, "집중력점수", drop = FALSE]

indep_spearman<- data2[,c("스트레스수준", "자기효능감수준","학교만족도","가족지원수준","소속감수준")]
target_var_spearman<-data2[, "집중력등급", drop = FALSE]
result_pearson = corr.test(target_var_peareson$집중력점수,indep_pearson$수면시간+indep_pearson$휴대폰사용시간, method="pearson")





## 1. p벨류의 값에 따라서 어떻게 해석이 가능한다
# 2. p벨류와 상관계수가 어떤 관계를 가지는가?
# 3. 각각의 상관계수의 방법이 어떤 변수들의 분석에 사용되는가?

