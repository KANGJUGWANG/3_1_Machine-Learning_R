## anova분석 
# n개의 집단의 평균이 동일한다(모든 집단이)
# 귀무가설 : 모든 집단의 평균은 동일하다
# 대립가설 : 1개 이상의 집단의 평균이 동일하지 않는다
# 대립가설 채택시 사후검정 : 몇개의 집단이 어느정도로 차이가 발생하는가
# 사후검정이 필요 없을때 : 집단이 2개(두 집단의 평균이 다르다), 대립가설 기각 시

data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/5월 21일 anova/class_scores.csv", stringsAsFactors = TRUE);data

# 아노바분석
anova_result <- aov(Score ~ Class, data = data)

# 분석 결과 출력 
summary(anova_result)
# p-value가 0.05보다 작다 -> 대립가설 채택 

# 사후검증 출력 
# install.packages("multcomp")
library(multcomp)
tukey_result <- glht(anova_result, linfct = mcp(Class = "Tukey"))
summary(tukey_result)
# Tukey : 집단이 등분산일때(샘플수가 같고 분산이 유사하다)
# 모든 집단간에 p-value가 0.05보다 작다->모든 집단의 평균은 일치하지 않는다.


##### 추론통계의 변수
# 독립변수 : 가설의 원인이 되는 변수, 종속변수에 영향을 미치는 변수(원인)
# 종속변수 : 가설의 결과가 되는 변수, 독립변수로 영향을 받는 변수(결과)
## 여러 명명법
# 원인 : 독립변수, 설명변수, 예측변수
# 결과 : 종속변수, 반응변수, 결과변수, 표적변수

## 이해 설명 
# 통계적 독립이 아닐때 상관관계 있음 -> 두 변수가 같이 있다면 내용이 달라짐
# 수면시간과 카페인의 섭취는 관련성이 없지만 같이 비교하면 관계를 가짐


# 상관관계가 있을때 통계적 독립을 설명 안됨-> 두 변수의 작용으로 내용이 달라지지만 통계적 독립관계는 아님
# 학습시간이 늘어나면 성적도 늘어나지만 학습시간과 성석은 관련성이 없다고 할수없음


## 이원 분산분석 
grow <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/5월 21일 anova/growth.csv", stringsAsFactors = TRUE);grow
## 3개의 변수(fertilizer,water <- 독립변수 / grwth <- 종속변수)

# 이원산분석(독립변수들간에 상호작용 없음)
anova_result<-aov(growth~fertilizer+water, data=grow)
summary(anova_result)

# 이원산 분석(독립변수들간에 사용작용 있음) - 상호작용은 상관관계 아님
anova_result<-aov(growth~fertilizer*water, data=grow)
summary(anova_result)

##############################################################
## 이원산 분석의 사후검정(이해 못함) < -시험 출제 나옴
# 왜 하는건가? <-어떤 변수들이 어떻게 영향이 있었는지 세부 내역 확인을 위해서
# 어떤 내용을 확인할수 있는가? <- 차이가 발생한 부분이 어디서 어떤 그룹간에 있는지 확인 가능하다
# 일원산 분석과 어떤 차이인가
# f-test와 어떤개 다른건가?(사후검정이)
##############################################################
install.packages("car")
library(car)## 등분산 검정
install.packages("rstatix")
library(rstatix)#games_howell_test : 등분산이 아닐경우 검정

# 등분산 검정
# p-value<0.05일때 등분산이 아님(games_howell_test() 사용)
leveneTest(growth~fertilizer, data = grow)
# 등분산이 아님

# fertilizer 사후검정
tukey_result <- glht(anova_result, linfct = mcp(fertilizer = "Tukey"))
games_howell_test(grow, growth~fertilizer)

#사후검정(water)
leveneTest(growth ~water, data= grow)

#등분산이 아니니까 games_howell_test 적용
tukey_result <-glht(anova_result, linfct = mcp(water = "Tukey"))
summary(tukey_result)
games_howell_test(grow,growth~water)

# 변수의 그룹화를 통한 사후검정
grow$group <- interaction(grow$fertilizer, grow$water)
group_model <- aov(growth~group , data=grow)
summary(group_model)

leveneTest(growth ~ group, data = grow) ## 0.05보다 크기때문에 등분산 관계

tukey_result <- glht(grup_model, linfct = mcp(group = 'Tukey'))
summary(tukey_result)
# 등분산 관계일때 games_howell_test의 결과는 어떤 차이인가?
games_howell_test(grow, growth ~ group)


# 예제 1 
data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/5월 21일 anova/cafe.csv", stringsAsFactors = TRUE)

data
# 명목형 3개(CoffeeType, CafeMood, SeatLocation)
# 연속형 1개(SatisFaction)

# 종속형 1개 (SatisFaction)
anova_result <-aov(Satisfaction ~ CoffeeType+CafeMood+SeatLocation, data = data)
summary(anova_result)
# 카페 타입, 카페 분위기, 카페자리는 사용자의 만족도에 차이가 있다

## 어떤 변수가 어떠한 차이가 있는가?
data$group <- interaction(data$CoffeeType, data$CafeMood, data$SeatLocation) 
leveneTest(Satisfaction~group, data = data) #독립변수는 등분산이다? 이걸 해야하는가? 언제함?

tukey_result <- glht(anova_result, linfct = mcp(CoffeeType = "Tukey"))
summary(tukey_result)
tukey_result <- glht(anova_result, linfct = mcp(CafeMood = "Tukey"))
summary(tukey_result)
tukey_result <- glht(anova_result, linfct = mcp(SeatLocation = "Tukey"))
summary(tukey_result)



## aov에서 +왜 *는 어떤 차이이고 언제 사용하는가


