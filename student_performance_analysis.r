install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")

library(dplyr)
library(ggplot2)

#1. LOAD AND INSPECT THE DATA SET
#load the dataset
student_data <- read.csv("StudentPerformanceFactors.csv")

#View the first few rows
head(student_data)

#inspect the structure of the data
str(student_data)

#Get the dimensions(row by column)
dim(student_data)

#get all column names
names(student_data)

#2. CLEAN THE DATASET
#checks for number of missing values in each colunm
colSums(is.na(student_data))

#check all variables and fix abnormal or unwanted data
unique(student_data$Hours_Studied)
unique(student_data$Attendance)
unique(student_data$Parental_Involvement)
unique(student_data$Access_to_Resources)
unique(student_data$Extracurricular_Activities)
unique(student_data$Sleep_Hours)
unique(student_data$Previous_Scores)
unique(student_data$Motivation_Level)
unique(student_data$Internet_Access)
unique(student_data$Tutoring_Sessions)
unique(student_data$Family_Income)
unique(student_data$Teacher_Quality)

#drops all rows with the Teacher_Quality column empty and assign to a new frame
student_data <- student_data[student_data$Teacher_Quality != "", ]
unique(student_data$Teacher_Quality)

unique(student_data$School_Type)
unique(student_data$Peer_Influence)
unique(student_data$Physical_Activity)
unique(student_data$Learning_Disabilities)

unique(student_data$Parental_Education_Level)
#drops all rows with the Parental_Education_Level column empty
student_data <- student_data[student_data$Parental_Education_Level != "", ]

unique(student_data$Distance_from_Home)
#drops all rows with the Distance_from_Home column empty
student_data <- student_data[student_data$Distance_from_Home != "", ]

unique(student_data$Gender)
unique(student_data$Exam_Score)
max(student_data$Exam_Score)

#drop the row with an exam score above 100
student_data <- student_data[student_data$Exam_Score != 101, ]

dim(student_data)
colSums(is.na(student_data))
str(student_data)

#EDA(EXPLORATORY DATA ANALYSIS)
summary(student_data)
table(student_data$Gender)

#RELATIONSHIPS
#Create a composite metric called level of dedication to school work
#create a composite metric called level of Academic Infrastructure Index(AII)
#Family_Income vs Level of Exam Score
#Gender vs level of dedication
#Level of Dedication vs Exam Score
#Parental Involvement vs Gender
#Gender vs Study time
#AII vs exam score
#Access to Resources and Internet access vs Level of Motivation
#Motivation level vs Exam Score
#Teacher Quality vs Exam Score
#School type and teacher quality
#School type and exam score
#Previous Score vs Exam Score

#CREATING A COMPOSITE METRIC CALLED LEVEL OF DEDICATION(LOD)

#normalize Hours_studied and store in a new column
student_data <- student_data %>%
  mutate(
    norm_hours = (
      (Hours_Studied) - min(Hours_Studied))/(max(Hours_Studied) - min(Hours_Studied)
    )
  )

#normalize Attendance and store in a new column
student_data <- student_data %>%
  mutate(norm_attendance = (Attendance / 100))
max(student_data$LOD)

#Calculate LOD as normalized
student_data <- student_data %>%
  mutate(LOD = (norm_hours + norm_attendance) / 2)


#CREATING A COMPOSITE METRIC CALLED Academic Infrastructure Index(AII)

#convert Access to Resources to weights(Low = 1, Medium = 2, High = 3)
student_data$Access_to_Resources <- recode(
  student_data$Access_to_Resources,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)

#convert Teacher Quality to weights(Low = 1, Medium = 2, High = 3)
student_data$Teacher_Quality <- recode(
  student_data$Teacher_Quality,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)

#convert Internet Access to boolean(Yes = 1, No = 0)
student_data$Internet_Access <- recode(
  student_data$Internet_Access,
  "Yes" = 1,
  "yes" = 1,
  "No" = 0,
  "no" = 0,
)

#derive Academic Infrastructure Index(AII) and store in a new column 
student_data <- student_data %>%
  mutate(AII = ((Access_to_Resources + Internet_Access + Teacher_Quality) / 3))
max(student_data$AII)

#convert motivation level to weights(Low = 1, Medium = 2, High = 3)
student_data$Motivation_Level <- recode(
  student_data$Motivation_Level,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)

#RELATIONSHIP BETWEEEN FAMILY INCOME AND EXAM SCORE
student_data$Family_Income <- recode(
  student_data$Family_Income,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)
aggregate(Exam_Score ~ Family_Income, data = student_data, mean)
cor(student_data$Exam_Score, student_data$Family_Income)
model <- lm(student_data$Exam_Score ~ student_data$Family_Income, data = student_data)
summary(model)

#represent the regression on box plot
boxplot(
  student_data$Exam_Score ~ student_data$Family_Income,
  data = student_data,
  main = "Family Income vs Exam Score",
  xlab = "Family Income",
  ylab = "Exam Score",
  col = "lightblue"
)

#RELATIONSHIP BETWEEN GENDER & LEVEL OF DEDICATION(LOD)
aggregate(LOD ~ Gender, data = student_data, mean)
t.test(LOD ~ Gender, data = student_data)
plot(student_data$LOD ~ as.factor(student_data$Gender),data = student_data, col = "lightblue")

#Level of Dedication vs Exam Score
cor(student_data$LOD, student_data$Exam_Score)
model1 <- lm(student_data$Exam_Score ~ student_data$LOD, data = student_data)
summary(model1)
plot(student_data$Exam_Score ~ student_data$LOD,data = student_data, col = "lightblue")
abline(model1, col = "red", lwd = 2)


#Parental Involvement vs Gender
student_data$Parental_Involvement <- recode(
  student_data$Parental_Involvement,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)
table(student_data$Gender, student_data$Parental_Involvement)
prop.table(table(student_data$Gender, student_data$Parental_Involvement), 1)
ggplot(student_data, aes(x = Gender, fill = Parental_Involvement)) + geom_bar(position = "dodge") + ylab("Count") + ggtitle("Parental Involment by Gender")
mosaicplot(table(student_data$Gender, student_data$Parental_Involvement), color = TRUE)

#Gender vs Study time
aggregate(Hours_Studied ~ Gender, data = student_data, mean)
t.test(Hours_Studied ~ Gender, data = student_data)
plot(student_data$LOD ~ as.factor(student_data$Gender),data = student_data, col = "lightblue")


# 3. LOD vs Exam Score
# (Shows if higher dedication predicts better scores)
cor(student_data$LOD, student_data$Exam_Score)
plot(student_data$LOD, student_data$Exam_Score,
     main = "LOD vs Exam Score",
     xlab = "Level of Dedication", ylab = "Exam Score", pch = 19)

# 4. Hours Studied vs Exam Score
# (Direct relationship between time studying and results)
cor(student_data$Hours_Studied, student_data$Exam_Score)
plot(student_data$Hours_Studied, student_data$Exam_Score,
     main = "Hours Studied vs Exam Score",
     xlab = "Hours Studied", ylab = "Exam Score", pch = 19)

# 5. Attendance vs Exam Score
# (See if higher attendance is linked to performance)
cor(student_data$Attendance, student_data$Exam_Score)
plot(student_data$Attendance, student_data$Exam_Score,
     main = "Attendance vs Exam Score",
     xlab = "Attendance (%)", ylab = "Exam Score", pch = 19)

# 6. Motivation Level vs Exam Score
# (Checks if more motivated students score higher)
cor(student_data$Motivation_Level, student_data$Exam_Score)
boxplot(Exam_Score ~ Motivation_Level, data = student_data,
        main = "Exam Score by Motivation Level",
        xlab = "Motivation Level", ylab = "Exam Score")

# 7. Parental Involvement vs Exam Score
# (See if parents' support affects results)
table(student_data$Parental_Involvement)
boxplot(Exam_Score ~ Parental_Involvement, data = student_data,
        main = "Exam Score by Parental Involvement",
        xlab = "Parental Involvement", ylab = "Exam Score")


View(student_data)