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
clean1_data <- student_data[student_data$Teacher_Quality != "", ]
unique(clean1_data$Teacher_Quality)

unique(clean1_data$School_Type)
unique(clean1_data$Peer_Influence)
unique(clean1_data$Physical_Activity)
unique(clean1_data$Learning_Disabilities)

unique(clean1_data$Parental_Education_Level)
#drops all rows with the Parental_Education_Level column empty
clean1_data <- clean1_data[clean1_data$Parental_Education_Level != "", ]

unique(clean1_data$Distance_from_Home)
#drops all rows with the Distance_from_Home column empty
clean1_data <- clean1_data[clean1_data$Distance_from_Home != "", ]

unique(clean1_data$Gender)
unique(clean1_data$Exam_Score)

dim(clean1_data)
colSums(is.na(clean1_data))
str(clean1_data)

#EDA(EXPLORATORY DATA ANALYSIS)
summary(clean1_data)
table(clean1_data$Access_to_Resources)



#RELATIONSHIP BETWEEEN FAMILY INCOME AND EXAMSCORE
low_income_grades <- clean1_data %>%
  filter(Family_Income == "low") %>%
  select(Exam_Score)
View(clean1_data)
