################ 시험 전에 삭제할 내용
## + 표시 주석 전부 삭제
## 문자열 인덱싱 페키지 library(stringr)
## 대용량 데이터 library(data.table)
## 변수값 분리 library(tidyr)
## Z-test library(BSDA)
# Array 인덱싱
a_array_2d <- array(data = c(1, 2, 3, 4, 5, 6), dim = c(2, 3))
a_array_2d[1:2, 2]
my_array_3d <- array(1:24, dim = c(3, 4, 2))
my_array_3d[, 2:3, ]

################
mean(data)
median(data)
max(data)
min(data)
summary(data)
sd(data)
var(data)
typeof(data)
mode(data)
substr(a_str, 1, 2)


# CSV 파일 읽기
read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/gender_food.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
fread('C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/gender_food.csv', header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

rbindlist(list(data1,data2), fill=TRUE)
rbind(data1,data2)
dim()
colnames()

# 결측치 제거
na.omit(df_create)

# 변수 타입에 따라 데이터프레임 분리
combined_df[, sapply(df, is.numeric), with=FALSE]
combined_df[, sapply(df, is.character), with=FALSE]

# 변수 타입 변환
as.integer()
as.numeric()
as.factor()
as.logical()
as.character()

# 변수 이름 변경
names(df)[names(df) == "city"] <- "CityName"
names(name)[names(name) == "name"] <- "Name"

# 조건에 맞는 데이터 선택 (subset 함수 사용)
 subset(df_create, pm25 > 15, select = c(CityName, pm25))

# 변수 값 분리 (tidyr 패키지 separate 함수 사용)
library(tidyr)
separate(data, col = "full_name", into = c("name", "_name"), sep = "_")

# 데이터 변환 (dplyr 패키지 mutate, across, ifelse, case_when 사용)
library(dplyr)
mutate(a_mutate, across(c("X20s", "X30s", "X40s", "X50s", "X60s"), ~ case_when(. >= 6.0 ~ "High", . >= 2.5 & . < 6.0 ~ "Medium", . < 2.5 ~ "Low", TRUE ~ as.character(.))))

# CSV 파일 저장 (data.table 패키지 사용)
fwrite(completed_data, 'C:/Users/USER/Desktop/창균/건강조사/test.csv')
write.csv(df_create, "output_df.csv", row.names = FALSE)
library(ggplot2)
 df_score_boxplot <- data.frame(
   group = c(rep("Group 1", length(a_score$eng)), rep("Group 2", length(a_score$math))),
   values = c(a_score$eng, a_score$math))
 ggplot(df_score_boxplot, aes(x = group, y = values)) +
   geom_boxplot(fill = c("lightblue", "lightgreen"), outlier.color = "red") +
   labs(title = "Boxplot Example") +
   xlab("Group") +
   ylab("Values")

# 5. 추론 통계 (Inferential Statistics) - 가설 검정 및 분석

# t-test (두 그룹 평균 비교)
t.test(test, ttest, alternative = "two.sided")

# Z-test (두 그룹 평균 비교, 모집단 표준 편차를 알거나 샘플 크기가 큰 경우)
library(BSDA)
sd()
z.test(x = test, y = ttest, sigma.x = sd_a_ztest, sigma.y = sd_b_ztest, alternative = "less")
# F-test (두 집단 분산 비교 - 분산 동질성 검정)
result_ftest <- var.test(set_a_ftest, set_b_ftest)

# 카이제곱 적합도 검정 (Chi-Square Goodness-of-fit Test)
chisq.test(data$인구, p=c(0.6,0.15,0.2,0.05))$residuals

# 카이제곱 독립성 검정 (Chi-Square Independence Test)
matrix(c(25, 15, 20, 30, 35, 25, 15, 20, 15), nrow = 3, byrow = TRUE)
rownames() <- c("Red", "Blue", "Green")
colnames() <- c("Chocolate", "Vanilla", "Strawberry")
chisq.test(observed_chi_ind)
print(chi_squared_ind)
(candy_data$Observed - expected) / sqrt(expected)
std_residuals^2
1 - pchisq(chi_square_values, df = 2) 



# 데이터프레임으로 교차표 생성 후 독립성 검정
data_chi_ind_df <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Female"),
                              Food = c("국밥", "마라탕", "국밥", "피자", "피자", "국밥", "국밥", "마라탕", "피자", "피자"))
cross_tab_chi_ind <- table(data_chi_ind_df$Gender, data_chi_ind_df$Food)
print(cross_tab_chi_ind)
chi_square_test_result_ind <- chisq.test(cross_tab_chi_ind)
print(chi_square_test_result_ind)

# 카이제곱 사후 검정 (chisq.posthoc.test 사용 - Bonferroni 수정)
library(chisq.posthoc.test)
chisq.posthoc.test(cross_tab_chi_ind, method = "bonferroni")


# 카이제곱 검정 잔차 시각화 (corrplot 사용)
library(corrplot)
corrplot(chi_square_test_result_ind$residuals, is.cor = FALSE)


# 이원 분산 분석 (Two-way ANOVA)
aov(growth ~ fertilizer * water, data = grow_data)
summary(anova_result_interaction)

# Games-Howell 검정 전 등분산 검정 (car 패키지 leveneTest 사용)
library(car)
leveneTest(growth ~ fertilizer, data = grow_data)
leveneTest(growth ~ water, data = grow_data)

# ANOVA 사후 검정 (Tukey HSD - multcomp 패키지 glht 사용)
library(multcomp)
summary(glht(anova_result, linfct = mcp(Class = "Tukey")))


# ANOVA 사후 검정 (Games-Howell - rstatix 패키지 사용)
library(rstatix)
games_howell_test(grow_data, growth ~ fertilizer)
games_howell_test(grow_data, growth ~ water)


# 이원 ANOVA 상호작용 항에 대한 사후 검정 준비 (interaction 사용)
interaction(grow_data$fertilizer, grow_data$water)
group_model <- aov(growth ~ group, data = grow_data)
leveneTest(growth ~ group, data = grow_data)
games_howell_test(grow_data, growth ~ group)

# 상관 분석 (Pearson - psych 패키지 corr.test 사용)
library(psych)
heights_corr <- c(160, 162, 155, 180, 170, 175, 165, 171, 177, 172)
weights_corr <- c(55, 60, 53, 72, 70, 73, 62, 64, 69, 65)
result_pearson <- corr.test(heights_corr, weights_corr, method = "pearson")
result_pearson$p
result_pearson$r

# 상관 분석 (Spearman - psych 패키지 corr.test 사용)
s_eval_corr <- c(3, 4, 2, 5, 1, 4, 3, 2, 5, 3)
e_eval_corr <- c(2, 5, 2, 4, 1, 4, 3, 2, 5, 3)
result_spearman <- corr.test(s_eval_corr, e_eval_corr, method = "spearman")
result_spearman$p
result_spearman$r

# 독립변수/종속변수 선택 코드 구조
# data_selection <- read.csv("pearson.csv")
data[, c("")]
data[, "", drop = FALSE]