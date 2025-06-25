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

read.csv("C:/Users/rkdwn/Downloads/gender_food.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8", skip = 2)
fread('C:/Users/rkdwn/Documents/GitHub/Machine-Learning_R_3_1/데이터/gender_food.csv', header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

rbindlist(list(data1,data2), fill=TRUE)
rbind(data1,data2)
dim()
colnames()

na.omit(df_create)

combined_df[, sapply(df, is.numeric), with=FALSE]
combined_df[, sapply(df, is.character), with=FALSE]

as.integer()
as.numeric()
as.factor()
as.logical()
as.character()

names(df)[names(df) == "city"] <- "CityName"
names(name)[names(name) == "name"] <- "Name"

subset(df_create, pm25 > 15, select = c(CityName, pm25))

separate(data, col = "full_name", into = c("name", "_name"), sep = "_")

mutate(a_mutate, across(c("X20s", "X30s", "X40s", "X50s", "X60s"), ~ case_when(. >= 6.0 ~ "High", . >= 2.5 & . < 6.0 ~ "Medium", . < 2.5 ~ "Low", TRUE ~ as.character(.))))

fwrite(completed_data, 'C:/Users/USER/Desktop/창균/건강조사/test.csv')
write.csv(df_create, "output_df.csv", row.names = FALSE)

df_score_boxplot <- data.frame(
   group = c(rep("Group 1", length(a_score$eng)), rep("Group 2", length(a_score$math))),
   values = c(a_score$eng, a_score$math))
ggplot(df_score_boxplot, aes(x = group, y = values)) +
   geom_boxplot(fill = c("lightblue", "lightgreen"), outlier.color = "red") +
   labs(title = "Boxplot Example") +
   xlab("Group") +
   ylab("Values")

t.test(test, ttest, alternative = "two.sided")

z.test(x = test, y = ttest, sigma.x = sd(test), sigma.y = sd(ttest), alternative = "greater")

var.test(set_a_ftest, set_b_ftest)

chisq.test(data$인구, p=c(0.6,0.15,0.2,0.05))$residuals
(candy_data$Observed - expected) / sqrt(expected)
std_residuals^2
1 - pchisq(chi_square_values, df = 2) 

matrix(c(25, 15, 20, 30, 35, 25, 15, 20, 15), nrow = 3, byrow = TRUE)
rownames() <- c("Red", "Blue", "Green")
colnames() <- c("Chocolate", "Vanilla", "Strawberry")
table(data_chi_ind_df$Gender, data_chi_ind_df$Food)
chisq.test(cross_tab_chi_ind)

chisq.posthoc.test(cross_tab_chi_ind, method = "bonferroni")

corrplot(chi_square_test_result_ind$residuals, is.cor = FALSE)

aov(growth ~ fertilizer * water, data = grow_data)
summary(anova_result_interaction)

leveneTest(growth ~ fertilizer, data = grow_data)
leveneTest(growth ~ water, data = grow_data)

summary(glht(anova_result, linfct = mcp(Class = "Tukey")))
games_howell_test(grow_data, growth ~ fertilizer)
games_howell_test(grow_data, growth ~ water)

interaction(grow_data$fertilizer, grow_data$water)
aov(growth ~ group, data = grow_data)
leveneTest(growth ~ group, data = grow_data)
games_howell_test(grow_data, growth ~ group)

corr.test(heights_corr, weights_corr, method = "pearson")$p
corr.test(s_eval_corr, e_eval_corr, method = "spearman")$r

data[df, c("a","b")]
data[df, "c", drop = FALSE]






data1 <- read.csv("C:/Users/rkdwn/Downloads/기말데이터/Food.csv", header = TRUE, stringsAsFactors = TRUE, fileEncoding = "UTF-8")
data1
t1 <- table(data1$FavoriteColor, data1$Gender)
t2 <- table(data1$Food, data1$Gender)
chisq.test(t1)
chisq.test(t2)
chisq.posthoc.test(t1, method = "bonferroni")

chisq.posthoc.test(t2, method = "bonferroni")


data <- read.csv("C:/Users/rkdwn/Downloads/기말데이터/Measurement.csv", header = TRUE, stringsAsFactors = TRUE, fileEncoding = "UTF-8")
data
a <- aov(Measurement ~ Category1*Category2*Category3*Category4, data= data)

a <- aov(Measurement ~ Category1+Category2+Category3+Category4, data= data)
summary(a)
leveneTest(Measurement ~ Category1, data = data)# 등분산 
summary(glht(a, linfct = mcp(Category1 = "Tukey")))



data <- read.csv("C:/Users/rkdwn/Downloads/기말데이터/Factory.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
data

var.test(data$Reference, data$Comparison1)
var.test(data$Reference, data$Comparison2)


data <- read.csv("C:/Users/rkdwn/Downloads/기말데이터/Analysis.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
data
colnames(data)
a<-data[, c("Independent1","Independent2","Independent3","Independent4","Independent5")]
b<-data[, "Dependent", drop = FALSE]

corr.test(a, b, method = "pearson")$p
corr.test(a, b, method = "pearson")$r

data <- fread("C:/Users/rkdwn/Downloads/기말데이터/cafe_experience.csv", header = TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")
data
a<-data[, "Experience_Score", drop = FALSE]
b<-data[, c("Coffee_country","Seat_Type","Coffee_Type","Visit_Time","Cafe_Lighting")]
colnames(data)
c <- corr.test(b, a, method = "pearson");c
b
###########
1. 카이제곱 두가지
7. 스피어만