install.packages("dplyr")

library(dplyr)
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



#dropped the row with an exam score above 100
student_data <- student_data[student_data$Exam_Score != 101, ]

dim(student_data)
colSums(is.na(student_data))
str(student_data)

#EDA(EXPLORATORY DATA ANALYSIS)
summary(student_data)
table(student_data$Gender)

#RELATIONSHIPS
#Create a composite metric called level of dedication to school work
#create a composite metric called level of exam performance
#Family_Income vs Level of Exam performance
#Gender vs level of dedication
#Level of Dedication vs Exam Score
#Parental Involvement vs Gender
#Gender vs Study time

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

#convert motivation level to weights(Low = 1, Medium = 2, High = 3)
student_data$Motivation_Level <- recode(
  student_data$Motivation_Level,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)

# student_data <- student_data %>%
#   mutate(
#     norm_motivation_level = (
#       Motivation_Level - min(Motivation_Level)) / (max(Motivation_Level) - min(Motivation_Level)
#     )
#   )


student_data <- student_data %>%
  mutate(
    norm_hours = (
      (Hours_Studied) - min(Hours_Studied))/(max(Hours_Studied) - min(Hours_Studied)
    )
  )

student_data <- student_data %>%
  mutate(LOD = (norm_hours + norm_attendance) / 2)

#RELATIONSHIP BETWEEEN FAMILY INCOME AND EXAM SCORE
student_data$Family_Income <- recode(
  student_data$Family_Income,
  "High" = 3,
  "Medium" = 2,
  "Low" = 1,
)
aggregate(Exam_Score ~ Family_Income, data = student_data, mean)
cor(student_data$Family_Income, student_data$Exam_Score)
boxplot(student_data$Family_Income ~ student_data$Exam_Score,data = student_data)

#RELATIONSHIP BETWEEN GENDER & LEVEL OF DEDICATION(LOD)
aggregate(Exam_Score ~ Parental_Education_Level, data = student_data, mean)
t.test(LOD ~ Gender, data = student_data)
boxplot(student_data$LOD ~ student_data$Exam_Score,data = student_data)
View(student_data)

