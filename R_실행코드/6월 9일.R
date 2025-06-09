rep(1/6, 6)# 독립성 검정 예시 데이터 생성
# (실제 CSV 파일로 저장 및 로드하는 것을 가정하지만, 여기서는 직접 생성)
data_independence <- data.frame(
  Gender = c(rep("남자", 45), rep("남자", 55), rep("여자", 60), rep("여자", 40)),
  Preference = c(rep("선호", 45), rep("비선호", 55), rep("선호", 60), rep("비선호", 40))
)

# 데이터프레임의 범주형 변수를 사용하여 교차표 생성
contingency_table_independence <- table(data_independence$Gender, data_independence$Preference)
print("--- 독립성 검정 교차표 ---")
print(contingency_table_independence)

# 카이제곱 독립성 검정 수행
# chisq.test() 함수에 교차표를 직접 전달
chi_sq_independence_result <- chisq.test(contingency_table_independence)

print("--- 독립성 검정 결과 ---")
print(chi_sq_independence_result)

# 결과 해석 예시:
if (chi_sq_independence_result$p.value < 0.05) {
  print("p-value가 0.05보다 작으므로, 성별과 음료 선호도는 통계적으로 유의미한 연관성이 있습니다.")
} else {
  print("p-value가 0.05보다 크므로, 성별과 음료 선호도 사이에 통계적으로 유의미한 연관성을 찾기 어렵습니다.")
}

# (선택 사항) 잔차 분석: 어떤 셀에서 차이가 나는지 확인
# corrplot 패키지가 설치되어 있다면 시각화 가능
# install.packages("corrplot")
# library(corrplot)
# print("--- 독립성 검정 잔차 시각화 (corrplot) ---")
# corrplot(chi_sq_independence_result$residuals, is.cor = FALSE, title = "Chi-square Test Residuals")


# 적합도 검정 예시 데이터 (관측 빈도 벡터)
# (실제 CSV 파일로 저장 및 로드하는 것을 가정하지만, 여기서는 직접 생성)
# CSV 파일 형태로 생각한다면, 각 행이 증상, 열이 빈도일 수 있습니다.
# 예를 들어, symptom_counts.csv 파일에 "Symptom,Count\n메스꺼움,120\n두통,80\n피로,100" 이런 식으로 저장될 수 있습니다.
# read.csv("symptom_counts.csv")$Count 로 로드하면 됩니다.

observed_counts_gof <- c(120, 80, 100)
names(observed_counts_gof) <- c("메스꺼움", "두통", "피로") # 시각적 이해를 돕기 위한 이름

# 우리가 기대하는 비율 (모든 증상이 동일한 비율로 나타날 것이라는 귀무가설)
# 전체 환자 수 300명에 대해 각 증상이 100명씩 나타날 것으로 기대.
# 따라서 각 증상별 기대 확률은 1/3 (0.333...)
expected_probabilities_gof <- rep(1/3, length(observed_counts_gof))
# 또는 총 합이 300인 경우 expected_counts <- c(100, 100, 100) 으로 직접 기대 빈도를 사용해도 됩니다.

print("--- 적합도 검정 관측 빈도 ---")
print(observed_counts_gof)
print("--- 적합도 검정 기대 확률 (귀무가설) ---")
print(expected_probabilities_gof)

# 카이제곱 적합도 검정 수행
# chisq.test() 함수에 관측 빈도 벡터와 기대 확률 p를 전달
chi_sq_gof_result <- chisq.test(x = observed_counts_gof, p = expected_probabilities_gof)

print("--- 적합도 검정 결과 ---")
print(chi_sq_gof_result)

# 결과 해석 예시:
if (chi_sq_gof_result$p.value < 0.05) {
  print("p-value가 0.05보다 작으므로, 부작용 발생 비율이 동일하다는 귀무가설을 기각합니다. 즉, 증상들은 동일한 비율로 발생하지 않습니다.")
} else {
  print("p-value가 0.05보다 크므로, 부작용 발생 비율이 동일하다는 귀무가설을 기각할 수 없습니다. 즉, 증상들은 동일한 비율로 발생한다고 볼 수 있습니다.")
}






# 1. 원본 데이터프레임 형태 (가장 일반적인 경우)
# 실제 CSV 파일에서 read.csv로 로드하면 이 형태가 됩니다.
data_df <- data.frame(
  Gender = factor(c(rep("Male", 45), rep("Male", 55), rep("Female", 60), rep("Female", 40))),
  Preference = factor(c(rep("Like", 45), rep("Dislike", 55), rep("Like", 60), rep("Dislike", 40)))
)
# 각 행이 한 명의 응답자를 의미하도록 확장 (chisq.test(Gender, Preference)에서 필요)
# 위 데이터는 요약된 빈도표이므로, 실제 개별 응답자 데이터프레임으로 변환
expanded_data_df <- data.frame(
  Gender = c(rep("Male", 45), rep("Male", 55), rep("Female", 60), rep("Female", 40)),
  Preference = c(rep("Like", 45), rep("Dislike", 55), rep("Like", 60), rep("Dislike", 40))
)
expanded_data_df$Gender <- factor(expanded_data_df$Gender)
expanded_data_df$Preference <- factor(expanded_data_df$Preference)


# 2. 교차표 (Contingency Table) 형태
# table() 함수나 matrix() 함수로 직접 생성할 수 있습니다.
# row: Gender, col: Preference
contingency_table <- as.table(matrix(c(45, 55, 60, 40), nrow = 2, byrow = TRUE,
                                     dimnames = list(Gender = c("Male", "Female"),
                                                     Preference = c("Like", "Dislike"))))


# 예시 데이터 (expanded_data_df 사용)
print("--- 독립성 검정 (두 벡터 전달) ---")
result_vector <- chisq.test(expanded_data_df$Gender, expanded_data_df$Preference)
print(result_vector)
cat("\n")
# 예시 데이터 (contingency_table 사용)
print("--- 독립성 검정 (table 객체 전달) ---")
result_table_object <- chisq.test(contingency_table)
print(result_table_object)
cat("\n")
# 예시 데이터 (contingency_table을 행렬로 변환)
matrix_data <- as.matrix(contingency_table)
print("--- 독립성 검정 (행렬 전달) ---")
print(matrix_data)
result_matrix <- chisq.test(matrix_data)
print(result_matrix)
cat("\n")

