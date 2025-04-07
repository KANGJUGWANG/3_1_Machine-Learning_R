# 인터넷에서 원하는 데이터 셋을 선택하고 가설을 새운뒤 t_test와 z_test를 활용하여 가설을 검정하라
install.packages("BSDA")## z_test를 사용할수있는 라이브러리리
library("BSDA")
data_set =read.csv("C:/Users/PC502/Downloads/archive/student_performance_dataset.csv");data_set
## 학생들의 지난 시험 점수와 기말 시험 점수는 차이가 없다.
# H0(귀무가설) : 이전 시험과 기말시험 성적은 차이가 있다.
# H1(대립가설) : 이전 시험과 기말시험 성적은 차이가 없다.
past_scores = data_set$Past_Exam_Scores;past_scores
final_scores = data_set$Final_Exam_Score;final_scores
sd_past = sd(past_scores)
sd_final = sd(final_scores)
t_test = t.test(x= past_scores, y= final_scores, alternative = "two.sided");t_test
z_test = z.test(x = past_scores, y=final_scores, sigma.x = sd_past, sigma.y = sd_final, alternative = "two.sided");z_test
# : p-value가 0.05보다 작기 때문에 귀무가설을 기각하고 대립가설을 채택한다.
