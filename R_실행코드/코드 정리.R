# 1. 기초 통계 (기술 통계) 계산

# 평균, 중앙값, 최대값, 최소값, 최빈값 계산
data <- c(10, 15, 20, 25, 30, 15, 20, 25, 25, 10)
mean_value <- mean(data)
median_value <- median(data)
max_value <- max(data)
min_value <- min(data)
summary(data)
# find_mode 함수는 R 기본 함수가 아니므로, 실제 사용 시 정의 필요
# mode_value <- find_mode(data) 

# 범위 계산
data <- c(10, 15, 20, 25, 30, 15, 20, 25, 25, 10)
range_value <- max(data) - min(data)

# 사분위수 (Q1, Q3) 및 사분위수 범위 (IQR) 계산
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)
iqr_value <- Q3 - Q1

# 분산 및 표준 편차 계산 (수동 계산 예시)
data <- c(10, 15, 20, 25, 30, 15, 20, 25, 25, 10)
mean_data <- mean(data)
squared_diff <- (data - mean_data)^2
variance <- sum(squared_diff) / length(data)
std_dev <- sqrt(variance)

# 분산 및 표준 편차 계산 (R 함수 사용)
data <- c(10, 15, 20, 25, 30, 15, 20, 25, 25, 10)
sd_value <- sd(data)
var_value <- var(data)

# 2. 데이터 구조 (Lists, Vectors, Arrays) 및 인덱싱

# List 생성
List1 <- list(1, 2, 3)
List2 <- list(1.6, 2.3, 3.5)
List3 <- list("apple", "banana", "orange", 1, 1.5, TRUE)

# List 타입 확인
typeof(List1)
mode(List1)

# Vector 생성
Vector1 <- c(1, 2, 3)
char_vector <- c("apple", "banana", "orange")
logical_vector <- c(TRUE, FALSE, TRUE)

# Vector 타입 확인
mode(Vector1)

# Vector 연산
Vector_1_ops <- c(1.6, 2.3, 3.5)
Vector_2_ops <- c(2.6, 5.3, 7.5)
Vector_1_ops + Vector_2_ops

# Array 생성
array1 <- array(data = c(1, 2, 3, 4, 5, 6), dim = c(2, 3))
array2 <- array(data = c(1, 2, 3, 4, 5, 6), dim = c(2, 2, 2))

# 문자열 인덱싱
library(stringr)
a_str <- "abcdefg"
substr(a_str, 1, 2)

# List 인덱싱
a_list <- list(1, 2, 3, 4, 5, 6)
a_list
a_list[]

# Vector 인덱싱
a_vec <- c(1, 2, 3, 4, 5, 6)
a_vec

# Array 인덱싱
a_array_2d <- array(data = c(1, 2, 3, 4, 5, 6), dim = c(2, 3))
a_array_2d
a_array_2d[1, 2]
a_array_2d[1:2]
a_array_2d[1:3]
a_array_2d[1:2, 2]
a_array_2d[1:2, 2:3]
my_array_3d <- array(1:24, dim = c(3, 4, 2))
my_array_3d
my_array_3d[3, , 2]
my_array_3d[, 2:3, ]
my_array_3d[, , 2]

# 3. 데이터프레임 생성, 기본 조작 및 전처리

# 데이터프레임 생성
city <- c("Seoul", "Busan", "Daegu", "Seoul", "Busan", "Daegu", "Ulsan")
pm25 <- c(18, 21, 21, 17, 8, 11, 25)
df_create <- data.frame(city = city, pm25 = pm25)

# CSV 파일 읽기
# data_csv <- read.csv("data.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# 대용량 데이터 불러오기 (data.table 패키지 사용)
library(data.table)
# hn_2009 <- fread('C:/Users/USER/Desktop/창균/건강조사/HN_2019.csv')
# hn_2010 <- fread('C:/Users/USER/Desktop/창균/건강조사/HN_2010.csv')
# hn_2011 <- fread('C:/Users/USER/Desktop/창균/건강조사/HN_2011.csv')

# 여러 데이터프레임 하나로 합치기 (행 기준)
# combined_df <- rbindlist(list(hn_2009, hn_2010, hn_2011), fill=TRUE)

# 데이터프레임 차원 확인
# dim(combined_df)

# 특정 변수만 선택
# col_names_hn_2009 <- colnames(hn_2009)
# col_2009_subset <- combined_df[, col_names_hn_2009, with = FALSE]

# 결측치 제거
df_clean_na <- na.omit(df_create)

# 변수 타입에 따라 데이터프레임 분리
# df_numeric_separated <- combined_df[, sapply(combined_df, is.numeric), with=FALSE]
# df_character_separated <- combined_df[, sapply(combined_df, is.character), with=FALSE]

# 변수 타입 변환
name_age_df <- data.frame(name = c("철수", "영희"), Age = c("20", "25"))
name_age_df$Age <- as.integer(name_age_df$Age)
name_age_df$Age <- as.numeric(name_age_df$Age)
name_age_df$Age <- as.factor(name_age_df$Age)
name_age_df$Age <- as.logical(name_age_df$Age)
name_age_df$Age <- as.character(name_age_df$Age)

# 데이터프레임 변수 요약
summary(name_age_df)

# 변수 이름 변경
names(df_create)[names(df_create) == "city"] <- "CityName"
names(name_age_df)[names(name_age_df) == "name"] <- "Name"

# 조건에 맞는 데이터 선택 (subset 함수 사용)
subset_df <- subset(df_create, pm25 > 15, select = c(CityName, pm25))

# 변수 값 분리 (tidyr 패키지 separate 함수 사용)
library(tidyr)
data_sep <- data.frame(full_name = c("John_Doe", "Jane_Smith"))
separated_data <- separate(data_sep, col = "full_name", into = c("first_name", "last_name"), sep = "_")

# 결측치 처리 (mice 패키지 사용)
library(mice)
# sub_df_numeric <- data.frame(V1 = c(1,2,NA,4), V2 = c(NA,2,3,4))
# imputed_data <- mice(sub_df_numeric, m=5, maxit=10, method="pmm", seed=1235)

# mice 결과에서 데이터 선택
# completed_data <- complete(imputed_data, 1)

# 데이터 변환 (dplyr 패키지 mutate, across, ifelse, case_when 사용)
library(dplyr)
a_mutate <- data.frame(X20s = c(1, 7, 3), X30s = c(2, 5, 8), X40s = c(4, 9, 6), X50s = c(3, 2, 7), X60s = c(5, 1, 9))
a_mutate_ifelse <- mutate(a_mutate, across(c("X20s", "X30s", "X40s", "X50s", "X60s"), ~ ifelse(. > 6.0, "High", .)))
a_mutate_case_when_num <- mutate(a_mutate, across(c("X30s", "X40s", "X50s", "X60s"), ~ case_when(. >= 6.0 ~ 1, . >= 2.5 & . < 6.0 ~ 2, . < 2.5 ~ 3, TRUE ~ .)))
a_mutate_case_when_char <- mutate(a_mutate, across(c("X20s", "X30s", "X40s", "X50s", "X60s"), ~ case_when(. >= 6.0 ~ "High", . >= 2.5 & . < 6.0 ~ "Medium", . < 2.5 ~ "Low", TRUE ~ as.character(.))))

# CSV 파일 저장 (data.table 패키지 사용)
# fwrite(completed_data, 'C:/Users/USER/Desktop/창균/건강조사/test.csv')

# CSV 파일 저장 (기본 함수 사용)
write.csv(df_create, "output_df.csv", row.names = FALSE)

# 4. 탐색적 데이터 분석 (EDA) - 시각화 (ggplot2 사용)

library(ggplot2)

# 라인 그래프 (Line Chart)
time_line <- seq(1, 24)
temperature_line <- c(22, 21, 20, 19, 18, 18, 19, 20, 22, 24, 26, 28, 29, 28, 27, 25, 24, 23, 22, 22, 21, 21, 20, 19)
data_line <- data.frame(time = time_line, temp = temperature_line)
ggplot(data_line, aes(x = time, y = temp)) +
  geom_line() +
  labs(title = "시간에 따른 온도 변화") + xlab("시간") + ylab("온도")

# 산점도 (Scatter Plot)
df_scatter <- data.frame(x = c(1, 2, 3, 4, 5), y = c(6, 8, 5, 9, 7))
ggplot(df_scatter, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatter Plot") +
  xlab("X") +
  ylab("Y")

# 산점도와 연결선
ggplot(df_scatter, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3) +
  geom_line(aes(color = "Connected Points"), size = 0.5) +
  labs(title = "Scatter Plot") + xlab("X") + ylab("Y")

# 여러 데이터셋 산점도
df_scatter2 <- data.frame(x = c(5, 6, 7, 8, 9), y = c(18, 12, 16, 77, 63))
ggplot() +
  geom_point(data = df_scatter, aes(x = x, y = y), color = "blue", size = 3) +
  geom_line(data = df_scatter, aes(x = x, y = y, color = "Connected Points"), size = 0.5) +
  geom_point(data = df_scatter2, aes(x = x, y = y), color = "blue", size = 3) +
  geom_line(data = df_scatter2, aes(x = x, y = y, color = "Connected Points 2"), size = 0.5) +
  labs(title = "Scatter Plot") + xlab("X") + ylab("Y")

# 산점도와 추세선
ggplot(df_scatter, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(color = "Trendline")) +
  labs(title = "Scatter Plot") + xlab("X") + ylab("Y")

# 상자수염 그림 (Boxplot) - 단일 그룹
df_boxplot_single <- data.frame(values = c(5, 7, 10, 12, 14, 18, 20, 22, 25, 27, 30))
ggplot(df_boxplot_single, aes(x = values)) +
  geom_boxplot(fill = "steelblue", color = "white") +
  labs(title = "Boxplot of Values") + xlab("Values")

# 상자수염 그림 (Boxplot) - 그룹 비교
df_boxplot_group <- data.frame(
  group = c(rep("Group 1", 60), rep("Group 2", 60)),
  values = c(rnorm(60, mean = 0, sd = 1), rnorm(60, mean = 2, sd = 1)))
ggplot(df_boxplot_group, aes(x = group, y = values)) +
  geom_boxplot(fill = c("lightblue", "lightgreen"), outlier.color = "red") +
  labs(title = "Boxplot Example") + xlab("Group") + ylab("Values")

# Boxplot 그리기 예제 (문제 풀이 코드)
# a_score <- read.csv("C:/Users/USER/Desktop/Score_2.csv")
# df_score_boxplot <- data.frame(
#   group = c(rep("Group 1", length(a_score$eng)), rep("Group 2", length(a_score$math))),
#   values = c(a_score$eng, a_score$math))
# ggplot(df_score_boxplot, aes(x = group, y = values)) +
#   geom_boxplot(fill = c("lightblue", "lightgreen"), outlier.color = "red") +
#   labs(title = "Boxplot Example") +
#   xlab("Group") +
#   ylab("Values")

# 막대 그래프 (Bar Chart) - 단일 범주
city_bar <- c("Seoul", "Busan", "Daegu", "Seoul", "Busan", "Daegu", "Ulsan")
pm25_bar <- c(18, 21, 21, 17, 8, 11, 25)
df_bar <- data.frame(city = city_bar, pm25 = pm25_bar)
ggplot(df_bar, aes(x = city, y = pm25, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "지역별 초미세먼지 농도") + xlab("City") + ylab("농도")

# 막대 그래프 (Bar Chart) - 그룹별 누적 막대
city_stacked <- c("Seoul", "Busan", "Daegu", "Seoul", "Busan", "Daegu", "Ulsan")
vari_stacked <- c("오전", "오후", "오전", "오후", "오전", "오후", "오후")
pm25_stacked <- c(18, 21, 21, 17, 8, 11, 25)
df_stacked_bar <- data.frame(city = city_stacked, pm25 = pm25_stacked, vari = vari_stacked)
ggplot(df_stacked_bar, aes(x = city, y = pm25, fill = vari)) +
  geom_bar(stat = "identity") +
  labs(title = "지역별 초미세먼지 농도") + xlab("City") + ylab("농도")

# 히스토그램 (Histogram)
df_hist <- data.frame(values = c(5, 7, 10, 12, 14, 18, 20, 22, 25, 27, 30))
ggplot(df_hist, aes(x = values)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Histogram") + xlab("Values") + ylab("Density")

# 히스토그램과 밀도 곡선
df_hist_density <- data.frame(values = c(5, 7, 10, 12, 14, 18, 10, 22, 25, 27, 30))
ggplot(df_hist_density, aes(x = values)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "steelblue", color = "white") +
  geom_density(alpha = 0.3, fill = "red") +
  labs(title = "Histogram with Density Plot") + xlab("Values") + ylab("Density")

# 파이 차트 (Pie Chart)
city_pie <- c("Seoul", "Busan", "Daegu", "Incheon", "Gwangju", "Daejeon", "Ulsan")
pm25_pie <- c(18, 21, 21, 17, 8, 11, 25)
colors_pie <- c("red", "orange", "yellow", "green", "lightblue", "blue", "violet")
df_pie <- data.frame(city = city_pie, pm25 = pm25_pie, colors = colors_pie)
ggplot(df_pie, aes(x = "", y = pm25, fill = city)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Concentration of Ultrafine Dust by Region") + xlab("") + ylab("") +
  scale_fill_manual(values = colors_pie)

# 히트맵 (Heatmap)
library(reshape2)
students_hm <- paste("Student", 1:10)
subjects_hm <- c("Math", "Science", "English")
grades_hm <- matrix(sample(50:100, 30, replace = TRUE), nrow = 10, ncol = 3, dimnames = list(students_hm, subjects_hm))
grades_melted <- melt(grades_hm, id.vars = rownames(grades_hm))
colnames(grades_melted) <- c("Student", "Subject", "Grade")
ggplot(grades_melted, aes(x = Subject, y = Student, fill = Grade)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "학생별 과목 성적 Heatmap") + xlab("과목") + ylab("학생")

# 5. 추론 통계 (Inferential Statistics) - 가설 검정 및 분석

# t-test (두 그룹 평균 비교)
group_a_ttest <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b_ttest <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79)
t_test_result <- t.test(group_a_ttest, group_b_ttest, alternative = "two.sided")

# Z-test (두 그룹 평균 비교, 모집단 표준 편차를 알거나 샘플 크기가 큰 경우)
library(BSDA)
group_a_ztest <- c(85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83, 85, 88, 90, 92, 91, 87, 89, 86, 84, 83)
group_b_ztest <- c(78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79, 78, 82, 80, 85, 84, 87, 83, 81, 80, 79)
sd_a_ztest = sd(group_a_ztest)
sd_b_ztest = sd(group_b_ztest)
result_ztest <- z.test(x = group_a_ztest, y = group_b_ztest, sigma.x = sd_a_ztest, sigma.y = sd_b_ztest, alternative = "two.sided")

# F-test (두 집단 분산 비교 - 분산 동질성 검정)
set_a_ftest <- c(10.1, 10.2, 10.3, 10.0, 10.1, 10.2, 10.3, 10.0, 10.1, 10.2)
set_b_ftest <- c(9.8, 10.5, 10.2, 9.7, 10.4, 10.3, 9.6, 10.6, 9.9, 10.7)
result_ftest <- var.test(set_a_ftest, set_b_ftest)

# 카이제곱 적합도 검정 (Chi-Square Goodness-of-fit Test)
candy_data_chi <- data.frame(Color = c("Red", "Blue", "Green"), Observed = c(30, 50, 20))
total_candies_chi <- sum(candy_data_chi$Observed)
expected_chi <- rep(total_candies_chi / 3, 3)
test_result_chi_gof <- chisq.test(candy_data_chi$Observed, p = expected_chi / sum(expected_chi))
print(test_result_chi_gof)

# 적합도 검정 다른 코드 형태
# observed_chi_gof_alt <- table(데이터$변수)
# expected_chi_gof_alt <- rep(sum(observed_chi_gof_alt) / length(observed_chi_gof_alt), length(observed_chi_gof_alt))
# test_result_chi_gof_alt <- chisq.test(x = observed_chi_gof_alt, p = expected_chi_gof_alt / sum(expected_chi_gof_alt))
# print(test_result_chi_gof_alt)

# 카이제곱 독립성 검정 (Chi-Square Independence Test)
observed_chi_ind <- matrix(c(25, 15, 20, 30, 35, 25, 15, 20, 15), nrow = 3, byrow = TRUE)
rownames(observed_chi_ind) <- c("Red", "Blue", "Green")
colnames(observed_chi_ind) <- c("Chocolate", "Vanilla", "Strawberry")
chi_squared_ind <- chisq.test(observed_chi_ind)
print(chi_squared_ind)

# 데이터프레임으로 교차표 생성 후 독립성 검정
data_chi_ind_df <- data.frame(Gender = c("Male", "Female", "Male", "Male", "Female", "Female", "Male", "Male", "Female", "Female"),
                              Food = c("국밥", "마라탕", "국밥", "피자", "피자", "국밥", "국밥", "마라탕", "피자", "피자"))
cross_tab_chi_ind <- table(data_chi_ind_df$Gender, data_chi_ind_df$Food)
print(cross_tab_chi_ind)
chi_square_test_result_ind <- chisq.test(cross_tab_chi_ind)
print(chi_square_test_result_ind)

# 카이제곱 사후 검정 (chisq.posthoc.test 사용 - Bonferroni 수정)
library(chisq.posthoc.test)
# results_chi_posthoc <- chisq.posthoc.test(cross_tab_chi_ind, method = "bonferroni")
# print(results_chi_posthoc)

# 카이제곱 검정 잔차 시각화 (corrplot 사용)
library(corrplot)
# corrplot(chi_square_test_result_ind$residuals, is.cor = FALSE)

# 일원 분산 분석 (One-way ANOVA)
# data_anova <- read.csv("class_scores.csv", stringsAsFactors = TRUE)
# anova_result_one_way <- aov(Score ~ Class, data = data_anova)
# summary(anova_result_one_way)

# ANOVA 사후 검정 (Tukey HSD - multcomp 패키지 glht 사용)
library(multcomp)
# tukey_result_anova <- glht(anova_result_one_way, linfct = mcp(Class = "Tukey"))
# summary(tukey_result_anova)

# 이원 분산 분석 (Two-way ANOVA)
# grow_data <- read.csv("growth.csv", stringsAsFactors = TRUE)
# anova_result_two_way <- aov(growth ~ fertilizer + water, data = grow_data)
# summary(anova_result_two_way)
# anova_result_interaction <- aov(growth ~ fertilizer * water, data = grow_data)
# summary(anova_result_interaction)

# ANOVA 사후 검정 (Games-Howell - rstatix 패키지 사용)
library(rstatix)
# games_howell_test(grow_data, growth ~ fertilizer)
# games_howell_test(grow_data, growth ~ water)

# Games-Howell 검정 전 등분산 검정 (car 패키지 leveneTest 사용)
library(car)
# leveneTest(growth ~ fertilizer, data = grow_data)
# leveneTest(growth ~ water, data = grow_data)

# 이원 ANOVA 상호작용 항에 대한 사후 검정 준비 (interaction 사용)
# grow_data$group <- interaction(grow_data$fertilizer, grow_data$water)
# group_model <- aov(growth ~ group, data = grow_data)
# leveneTest(growth ~ group, data = grow_data)
# games_howell_test(grow_data, growth ~ group)

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
# indep_vars <- data_selection[, c("독립변수1", " 독립변수2", " 독립변수3", "독립변수4")]
# target_var <- data_selection[, "종속변수", drop = FALSE]