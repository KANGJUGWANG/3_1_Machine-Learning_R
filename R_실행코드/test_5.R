# 1. 데이터를 찾아 특정 변수의 요소들의 값에 대한 분산의 동질성 검사를 하시오.
data <- read.csv("C:/Users/pc502/Downloads/전자상거래행동분석(명목형 2, 연속형1).csv", skip = 1,header = FALSE, encoding = "utf-8");data
table(data$V2)
set_single <- subset(data, V2 == 'Single')
set_married <- subset(data, V2 == 'Married')

set_single <- set_single[seq(1,200),1]
set_married<- set_married[seq(1,200),1]
result <- var.test(set_single, set_married)
print(result)

## 혼인과 미혼에 따른 전자상거래 금액의 동질성 검정 
# H0 : 혼인 여부에 따른  전사장거래 금액의 분산의 같다
# H1 : 혼인 여부에 따른 전자 상거래 금액의 분산은 다르다
# p-value = 0.7601임으로 혼인 여부에 따른 전자상거래 금액의 분산은 같다