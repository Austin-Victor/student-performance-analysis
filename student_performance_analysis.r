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

dim(student_data)
colSums(is.na(student_data))
str(student_data)

#EDA(EXPLORATORY DATA ANALYSIS)
summary(student_data)
table(student_data$Access_to_Resources)

#RELATIONSHIPS
#Create a composite metric called level of dedication to school work
#create a composite metric called level of exam performance
#Family_Income vs Level of Exam performance
#Gender vs level of dedication
#Level of Dedication vs Exam Score
#Parental Involvement vs Gender
#Gender vs Study time

student_data <- student_data %>%
  mutate(
    norm_hours = (
      (Hours_Studied) - min(Hours_Studied))/(max(Hours_Studied) - min(Hours_Studied)
    )
  )

student_data <- student_data %>%
  mutate(norm_attendance = (Attendance / 100))

student_data$Motivation_Level <- as.num


#RELATIONSHIP BETWEEEN FAMILY INCOME AND EXAMSCORE

View(student_data)
