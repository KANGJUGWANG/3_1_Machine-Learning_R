## 이전내용
# 평균의 분포=정규분포 
# 집단의 평균에 대하여 비교한다 = 정규분포
# 수치형 변수에 대하여 평균을 구해 비교한다


##4월 30일
#분산의 분포 = 카이제곱 분포
# 범주형 변수에 대하여 분포를 구해 비교한다
# 즉 집단의 변동성에 대하여 고려한다

# 카이제곱 검정의 가설설정
# H0(귀무가설) : 두 범주형 변수 사이에 유의미한 관계(연관)이 없다
# H1(대립가설) : 두 범주형 변수 사이에 유의미한 관계(연관)이 있다 

# 범주형 데이터는 음수가 없다 => z.test처럼 양측검정이 불가하다 단측검정만 가능
# 카이제곱 분포는 자유도에 따라서 분포의 형태가 정규 분포에 가까워진다

## 카이제곱 검정의 2개지 유형 

# 적합도 검정 : 기대 빈도와 관측 빈도의 분포를 비교(사전 검정)
# 귀무가설 : 실제 빈도와 기대 빈도의 유의미한 차이가 없다.
# 대립가설 : 실제 빈도와 기대 빈도의 유의미한 차이가 있다.
# 변수의 수 -1 = 자유도 
# 카이 제곱 계산 ((관찰값1 - 기대값1)^2)/기대값1 

candy_data <- data.frame(
  Color = c("Red","Blue","Green"),
  Observd = c(30, 50, 20));candy_data
total_candies <- sum(candy_data$Observd);print(total_candies)
expected<-rep(total_candies/3,3);print(expected)
test_result <- chisq.test(candy_data$Observd, p=expected/sum(expected))
print(test_result)
# 결과 해석 : 기대값과 관측값의 분포의 차이가 있다.(p_value가 0.05보다 작기 때문)
# 적합도 검정(사후 검정)
std_residuals<- (candy_data$Observd - expected) / sqrt(expected)
chi_square_values <- std_residuals ^2 
p_values <- 1- pchisq(chi_square_values, df=2)

candy_data$Residuals <- std_residuals
candy_data$Chi_square <- chi_square_values
candy_data$p_value <-p_values
print(candy_data)
# 결과 해석


## 실습(적합도 검정)
#datas <- read.csv("C:/Users/PC502/Downloads/국경통과데이터(명목 2).csv", skip = 2)
# local
datas <- read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/국경통과데이터(명목 2).csv", skip = 2)
datas
observed <- table(datas$US.Canada.Border,datas$Maine)
observed
expected <-rep(sum(observed)/length(observed),length(observed));expected
test_result <- chisq.test(x = observed, p=expected/sum(expected))
print(test_result)
"귀무가설 : 기대 빈도와 실제 빈도는 차이가 없다 "
" 대립가설 : 기대 빈도와 실제 빈도는 차이가 있다."
"p-value가 0.05보다 작기 때문에 귀무가설을 기각하고 대립 가설을 채택한다"
"미국 - 캐나다 국경을 이용하는 도시와 미국 - 맥시코 국경을 이용하는 도시는 차이가 있다"






# 독립성 검정:
