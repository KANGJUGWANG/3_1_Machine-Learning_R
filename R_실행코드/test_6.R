# 1. youtube.csv 파일을 활용해 viewercount(종속변수)에 영행을 미치는 독립변수를 찾고, 독립변수들의 요소를 토대로 어떠한 조건일때, viewercount가 가강 높은지에 대해 서술하시오.
# *독립변수가 유의미한 결과가 나오더라도 사후검증에서 요소들이 모두 유의하지 않을 수도 있음(전체 분산은 크지만 요소쌍의 차이는 작을수 있기 때문)

# 데이터 불러오기
data = read.csv("C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/5월 26일 test-6/youtube.csv",stringsAsFactors = TRUE);data

anova_result<-aov(ViewerCount ~ ContentType+ThumbnailType+UploadTime, data = data)
summary(anova_result)
anova_result<-aov(ViewerCount ~ ContentType*ThumbnailType*UploadTime, data = data)
summary(anova_result)


library(multcomp)
library(car)## 등분산 검정
library(rstatix)#games_howell_test : 등분산이 아닐경우 검정


# 등분산 검정
# p-value<0.05일때 등분산이 아님(games_howell_test() 사용)
leveneTest(ViewerCount~ContentType, data = data)
# ViewerCount와 ContentType는 등분산이다
anova_result<-aov(ViewerCount ~ ContentType, data = data)
tukey_result <- glht(anova_result, linfct = mcp(ContentType = "Tukey"))
summary(tukey_result)


leveneTest(ViewerCount~ThumbnailType, data = data)
# ViewerCount와 ThumbnailType는 등분산 관계가 아님(games-howell적용)


leveneTest(ViewerCount~UploadTime, data = data)
# ViewerCount와 UploadTime는 등분산 관계이다.
anova_result<-aov(ViewerCount ~ UploadTime, data = data)
tukey_result <- glht(anova_result, linfct = mcp(UploadTime = "Tukey"))
summary(tukey_result)



#### 변수들 간에 평균의 차이가 있는가?
# 이원산 분석(독립변수들간에 사용작용 있음)
anova_result<-aov(ViewerCount ~ ContentType*ThumbnailType*UploadTime, data = data)
summary(anova_result)
## p-value가 0.05보다 작음으로 종속변수(ViewerCount)와 그 외의 독립변수들은 모두 평균의 차이가 있다.
## 콘텐츠유형과 썸내일이 ViewerCount에 차이를 발새시킨다.
data$group <- interaction(data$ContentType, data$ThumbnailType, data$UploadTime) 
leveneTest(ViewerCount~group, data = data) #변수 조합은 등분산이다
## 독립변수의 조합은 종속변수와 등분산 관계이다.
tukey_result <- glht(anova_result, linfct = mcp(ContentType = "Tukey"))
summary(tukey_result)
# ContentType에서는 
# game > tutorial > review = vlog의 차이로 ViewerCount가 발생한다.  
tukey_result <- glht(anova_result, linfct = mcp(ThumbnailType = "Tukey"))
summary(tukey_result)
# ThumbnailType에서는 
# person > product > text 의 차이로  ViewerCount가 발생한다.
tukey_result <- glht(anova_result, linfct = mcp(UploadTime = "Tukey"))
summary(tukey_result)
# UploadTime에서는 
# night > moring = afternoon의 차이로 viewercount가 발생한다.