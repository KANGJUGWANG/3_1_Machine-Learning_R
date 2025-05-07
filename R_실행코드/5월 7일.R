## 이전 내용 복습 
# 카이제곱 분포 
# 카이제곱 분포는 집단을 이루는 변수의 변동성(분포)를 기준으로 분포로 정리한것

# 카이제곱 검정 
# 귀무가설(H0) : 집단의 분포가 차이없다.
# 대립가설(H1) : 집단의 분포에 차이가 있다.

# 카이제곱 검정의 적합도 검정
# 관측된 값이 예상되는 분포(기대 분포)와 일치하는가?
# 1개의 집단의 1개의 변수에 대하여 예측값과 관측값의 일치를 비교한다
# ex) 주사위를 굴렸을때 눈의 수에따른 관측값
# -> 확률변수 : 주사위 눈의 수
# -> 예측값 = 눈의 수는 동일한 비율로 발생한다
# -> 실제 관측값 : 9,9,9,11,11,11
# => 1개의 집단(주사위를 굴렸을때 관측된 눈의 수에 따른 횟수)
# => 1개의 변수(눈의 수에 따른 횟수)
# => 확인하고 싶은것 : 예측(예상)되는 값과 일치하는 분포를 가지는가?

#적합도 검정 이후 => 사후검정
#사후검정 : 적합도 검정의 상세 결과를 확인한다.


## 오늘 강의 내용
# 카이제곱 검정의 독립성 검정
# n개의 변수(1이상)에 대하여 변수간의 독립성을 가지는가?
# 귀무가설 : 변수가 독립관계이다.
# 대립가설 : 변수가 독립관계가 아니다.

# ex 1) 아이스크림의 맛과 색상은 관련성이 있다? 없다?
observed  <- matrix(c(25,15,20,30,35,25,15,20,15), nrow =3, byrow =TRUE)
rownames(observed) <-c("Red", "Blue", "Green")
colnames(observed) <- c("chocolate", "vanilla", "Strawberry")
chi_squared <- chisq.test(observed)
print(chi_squared)
# p-value가 0.05보다 크므로 귀무가설을 채택하고 대립가설을 기각한다
# 그러므로 아이스크림의 맛과 생상은 관련성이 없다.

# ex 2) 성별에 따라서 좋아하는 음식은 서로 관련성이 있다? 없다?
data <- data.frame(Gender = c("Male", "Female","Male","Male","Female","Female","Male","Male","Female","Female"), Food = c("국밥", "마라탕", "국밥","피자","피자","국밥","국밥","마라탕","피자","피자"))
cross_tab <-table(data$Gender, data$Food)
cross_tab
chi_square_test_result <-chisq.test(cross_tab)
print(chi_square_test_result)

data<- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/gender_food.csv")
g_f_tab <- table(data$Gender, data$Food)
chi_s_test <- chisq.test(g_f_tab)
print(chi_s_test)

#독립성 검정의 사후검정
install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)
cross_tab <- table(data$Gender, data$Food)
cross_tab
results<- chisq.posthoc.test(cross_tab, method = "bonferroni")# bonferroni<-유의수준(0.05)/조합쌍의수(6) 
results
choose(3,2)# bonferroni는 너무 낮아지기 때문에 유의수준/조합수(choose()결과)로 검정하겠다 할때 사용

# 성별과 음식의 독립성을 사후검정결과 
# 국밥과 피자는 귀무가설을 기각하며, 마라탕은 대립가설을 기각한다.
# 마라탕과 성별은 독립이며 국밥, 피자는 성별과 독립이 아니다.

#카이제곱 검정
# 귀무가설 : 부부와 집안일은 관련성이 없다.
# 대립가설 : 부부와 집안일은 관련성이 있다.
cross_tab <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/housetasks.csv",row.names = 1)
cross_tab
chi_square_test_result <-chisq.test(cross_tab)
print(chi_square_test_result)
# 검정결과 귀무가설을 기각하고 대립가설을 채택한다(부부와 집안일은 차이가 있다.)
# 카이제곱 검정의 사후검정(독립성 검정)
## 귀무가설 : 독립이다.
# 대립가설 : 독립이 아니다.
results <- chisq.posthoc.test(cross_tab,method = "bonferroni")
results

## (실습)결혼 상태과 교육 레벨의 카이제곱 검정
# 귀무가설 : 결혼상태와 교육레벨은 독립이다.
# 대립가설 : 결혼 상태와 교육 레벨은 독립이 아니다.
data <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/전자상거래행동분석(명목형 2, 연속형1).csv", skip = 2)
cross_tab <- table(data$Married, data$Bachelor.s);cross_tab
chi_square_test_result <- chisq.test(cross_tab);chi_square_test_result
## p_value가 0.6098임으로 귀무가설을 기각하지 못하여 귀무가설을 채택하고 그에 따라서 결혼 상태와 교육 레벨은 독립이다.

# 사후검정
results <- chisq.posthoc.test(cross_tab,method = "bonferroni")
results
# 귀무가설(변수간의 관계가 독립이다)을 채택하여 사후검정에 의미가 없다.


## 내용 정리
# 카이제곱 검정? 집단(명목형 변수)의 분포를 검정한다.
# 카이제곱 검정의 2가지 유형 : 적합도 검정, 독립성 검정
# 적합도 검정 : 1개의 집단의 1개 변수(관측값과 예측값)의 분포를 비교
# 귀무가설 : 집단의 관측값과 예측값의 차이가 없다.
# 대립가설 : 집단의 관측값과 예측값의 차이가 있다.
# 독립성 검정 : 1개의 집단의 n개 변수의 분포를 비교
# 귀무가설 : 변수간의 관계가 독립이다.
# 대립가설 : 변수간의 관계가 독립이 아니다.
# 카이제곱 검정 이후 사후 검정
# 사후검정은 [p-value<유의수준]인 상황(대립가설 지지)일때 왜 차이가있는지, 독립이아닌지를 개별 값에대하여 확인하기 위한 방법
# 즉 p-value에 영향을 미친 변수들의 범주 정도를 확인하기 위한 방법
