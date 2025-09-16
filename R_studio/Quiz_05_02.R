# H0: There is NO significant difference between teaching methods Lecture, Group Work, Online for growth of exam scores for 40 students
# H1: There is significant difference between teaching methods Lecture, Group Work, Online for growth of exam scores for 40 students

exam_scores <- read.csv("C:/Users/AnushangaKularachchi/Downloads/ExamScores.csv")

# Extracting Growth speeds of Fertilizer A, B and C
E_L <- exam_scores$Lecture
E_G <- exam_scores$GroupWork
E_O <- exam_scores$Online


## 1. Checking for normality using Shapiro-Wilk normality test 
shapiro.test(E_L)
shapiro.test(E_G)
shapiro.test(E_O)


## 2. Testing for equal variance using leveneTest()
#install.packages("car")
library(car)
help("leveneTest")

# Combining growth speeds of fertilizer A, B and C
exam_scores <- c(E_L, E_G, E_O)
Marks_Growth <- rep(c("E", "G", "O"), each = 40)

data <- data.frame(exam_scores, Marks_Growth)