install.packages("chisq.posthoc.test")
library(chisq.posthoc.test)
## 문제 1
# color.csv 파일에서 색깔 변수에 포함된 요소들이 적합하게 분포되어 있는지를 확인하고 
# 어떤 요소들이 적합하지 않은지(많거나, 적은지) 판단하시오

## 적합도 검정과 사후검정
data1<- read.csv("C:/Users/PC502/Desktop/새 폴더 (2)/카이제곱문제/color.csv")
data_table <- table(data1);data_table
expected<- rep(sum(data_table)/ length(data_table), length(data_table));expected
chi_squared <- chisq.test(x= data_table, p= expected/ sum(expected))
print(chi_squared)
"p-value가 0.05보다 작음으로 대립가설을 채택한다 = 분포의 차이가 일정하지 않다"
std_residuals <- (data_table- expected)/sqrt(expected)
value <- std_residuals^2
p_values <- 1-pchisq(value, df=3)
df<-data.frame
print(df)
"빨강의과 초록의 p-value가 0.05보다 낮음으로 빨강이 많으며, 초록이 적다는것을 확인할수있다."

## 문제 2 (residuals = std_residuals, chi_square=value,p_value =p_values)
# incafe.csv 파일에서 카페의 분위기가 고객의 만족도에 관련성이 있는지를 판단하고, 
# 관련성이 있다면 어떤 한 요소들이 카페의 만족도를 높이는데 영향을 주는지 판단하시오(유의수순 95%)
data2<- read.csv("C:/Users/PC502/Desktop/새 폴더 (2)/카이제곱문제/incafe.csv")
tb <- table(data2)
c_s_test<-chisq.test(tb);c_s_test
" p-value가 0.05보다 낮은걸 확인할수 있으며 귀무가설을 기각하고 대립가설을 지지하므로 카페 분위기와 만족도는 독립관계가 아니다"
results <- chisq.posthoc.test(tb, method = "bonferroni");results
"음악과 카페 분위기는 독립 관계이며"
"북적이는, 자연채광, 조용한 카페 분위기는 고객의 만족도와 독립 관계가 아니며"
"자연채광과 조용한이 고객의 만족도를 높이는걸 확인할수있다."

