# Top-10-Violations-all-up-and-where-GRADE-a-
## Steps
## -----
## (1) Define Paths, Files Names and Load Libraries
## (2) Read Data
## (3) Process Data
# ***********************************************************************************************************************
# (1) Define Paths, Files Names and Load Libraries
# ***********************************************************************************************************************
# Clear Workspace
rm(list=ls())

# Define Data Path
root        <- "D:/users/sghosh2/Desktop/NW 490 Special Topics Data Vizualization/Team project/"
data_path   <- "Raw Data/"
models_path <- "Models/"
out_path    <- "Final/"

# Load Libraries
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(plyr)
library(vcd)

# Define Input File Name
data     <- "DOHMH_New_York_City_Restaurant_Inspection_Results1.csv"

#Export data file name
export_dataset <- "Team_Project_Processed_data.csv"

# ***********************************************************************************************************************
# (2) Read Data
# ***********************************************************************************************************************
# Read Data
setwd(paste0(root, data_path))

data <- read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results2.csv", header = TRUE, stringsAsFactors = FALSE)

# ***********************************************************************************************************************
# (3) Process Data
# ***********************************************************************************************************************
# list the variables in the data frame
print(names(data))

#Format inspection dates as date
data$date <- as.Date(data$INSPECTION_DATE, "%m/%d/%Y")
data$year <- year(data$date)
data$month <- month(data$date)
data$grade_date <- as.Date(data$GRADE_DATE, "%m/%d/%Y")

#Drop incomplete data
data<-data[!(data$year==2010) & !(data$year==2011) & !(data$year==2015),]

# show the structure of the data frame
print(str(data))

#Create a dataframe of inspections (each inspection has some lines with and without a grade)
inspect = unique(data[,c("CAMIS", "date", "month", "year", "GRADE", "grade_date",
                         "CRITICAL_FLAG",   "VIOLATION_DESCRIPTION" )])

# standard R graphics to the screen
with(inspect, hist(CAMIS))

#Keep only inspections with letter grades
inspect = subset(inspect, GRADE %in% c("A","B","C"))
inspect$A_grade <-0
inspect$A_grade[inspect$GRADE == "A"] <- 1
inspect$B_grade <-0
inspect$B_grade[inspect$GRADE == "B"] <- 1
inspect$C_grade <-0
inspect$C_grade[inspect$GRADE == "C"] <- 1

#Code the Critical Flag
inspect = subset(inspect, CRITICAL_FLAG %in% c("Critical", "Not Critical"))
inspect$Critical <-0
inspect$Critical[inspect$CRITICAL_FLAG == "Critical"] <- 1
inspect$NCritical <-0
inspect$NCritical[inspect$CRITICAL_FLAG == "Not Critical"] <- 1

#Collapse counts by year/month
monthyear<-group_by(inspect, year, month)
counts_by_month <- summarise(monthyear,
                             countA = sum(A_grade, na.rm=TRUE),
                             countB = sum(B_grade, na.rm=TRUE),
                             countC = sum(C_grade, na.rm=TRUE)
)

#Calculate totals and percentages
counts_by_month<- mutate(counts_by_month,
                         total_count = countA + countB + countC,
                         perc_A = countA/total_count,
                         perc_B = countB/total_count,
                         perc_C = countC/total_count)

# 1  Create Stacked bar plots Criticality by GRADE
counts <- table(inspect$CRITICAL_FLAG, inspect$GRADE )
barplot1 <- barplot(counts, main="Restaurant Inspections by Violation Criticality and Grade",
        ylab="Criticality", xlab="Grade",
        col=c("red","blue"), horiz =TRUE,
        legend = rownames(counts)) 

# print to pdf file for use in many applications
pdf(file = "Violations by Criticality.pdf", width = 11, height = 8.5)
print(barplot1)
##############################
# 2 Top 10 Critical Violations
Critical <- subset(inspect, CRITICAL_FLAG %in% c("Critical"))
Critical_collapsed <- data.frame(table(Critical$VIOLATION_DESCRIPTION))
Critical_collapsed <- Critical_collapsed[order(-Critical_collapsed$Freq),]
Critical_collapsed_10 <- Critical_collapsed[1:10, ]

#Put in line breaks at 45th character to display nicely
Critical_collapsed_10$split <- gsub('(.{1,45})(\\s|$)', '\\1\n', Critical_collapsed_10$Var1)

#Sort the violations by order
Critical_collapsed_10$Var1 <- factor(Critical_collapsed_10$Var1, levels = Critical_collapsed_10$Var1, ordered = TRUE)
Critical_collapsed_10$row.names <- Critical_collapsed_10$Freq


#Plot
Critical_Top10V <- ggplot(Critical_collapsed_10,aes(x=Var1, y=Freq))+
    geom_bar(stat="identity", fill="blue") +
    coord_flip() +
    labs(y = "Number of violations") +
    labs(x = "Violation category") +
    ggtitle("Top10 critical violations")  +
    theme(axis.title.x = element_text(size = rel(0.7)), 
          axis.title.x = element_text(size = rel(0.7)))
print(Critical_Top10V)          

# print to pdf file for use in many applications
pdf(file = "D:/users/sghosh2/Desktop/NW 490 Special Topics Data Vizualization/Team project/Final/Top10 Critical Violations.pdf", width = 11, height = 8.5)
print(Critical_Top10V)
#################################

# 3 Top 10 Non Critical Violations
NCritical <- subset(inspect, CRITICAL_FLAG %in% c("Not Critical"))
NCritical_collapsed <- data.frame(table(NCritical$VIOLATION_DESCRIPTION))
NCritical_collapsed <- NCritical_collapsed[order(-NCritical_collapsed$Freq),]
NCritical_collapsed_10 <- NCritical_collapsed[1:10, ]

#Put in line breaks at 45th character to display nicely
NCritical_collapsed_10$split <- gsub('(.{1,45})(\\s|$)', '\\1\n', NCritical_collapsed_10$Var1)

#Sort the violations by order
NCritical_collapsed_10$Var1 <- factor(NCritical_collapsed_10$Var1, levels = NCritical_collapsed_10$Var1, ordered = TRUE)
NCritical_collapsed_10$row.names <- NCritical_collapsed_10$Freq

#Plot
NCritical_Top10V <- ggplot(NCritical_collapsed_10,aes(x=Var1, y=Freq))+
    geom_bar(stat="identity", fill="blue") +
    coord_flip() +
    labs(y = "Number of violations") +
    labs(x = "Violation category") +
    ggtitle("Top10 Non Critical violations")  +
    theme(axis.title.x = element_text(size = rel(0.7)), 
          axis.title.x = element_text(size = rel(0.7)))
print(NCritical_Top10V)          

# print to pdf file for use in many applications
pdf(file = "D:/users/sghosh2/Desktop/NW 490 Special Topics Data Vizualization/Team project/Final/Top10 Non Critical Violations.pdf", width = 11, height = 8.5)
print(NCritical_Top10V)
##############333

# Subset dataset where GRADE =A and Critical_Flag=Critical
Agrade <- subset(inspect, GRADE %in% c("A"))
Agrade_C <- subset(inspect, CRITICAL_FLAG %in% c("Critical"))

# Data Check- Histogram of Agrade, should be binary 
with(inspect, hist(A_grade))

# Create Critical/Not Critical bars where GRADE =A
barplot(table(factor(Agrade$CRITICAL_FLAG)), main='Violation Criticality where GRADE=A',
        col=c("red", "green"))

###################################
#  4 Top 10 Critical Violations where GRADE=A
AgradeVC_collapsed <- data.frame(table(Agrade_C$VIOLATION_DESCRIPTION))
AgradeVC_collapsed <- AgradeVC_collapsed[order(-AgradeVC_collapsed$Freq),]
AgradeVC_collapsed_10 <- AgradeVC_collapsed[1:10, ]

#Put in line breaks at 45th character to display nicely
AgradeVC_collapsed_10$split <- gsub('(.{1,45})(\\s|$)', '\\1\n', AgradeVC_collapsed_10$Var1)

#Sort the violations by order
AgradeVC_collapsed_10$Var1 <- factor(AgradeVC_collapsed_10$Var1, levels = AgradeVC_collapsed_10$Var1, ordered = TRUE)
AgradeVC_collapsed_10$row.names <- AgradeVC_collapsed_10$Freq


#Plot
Agrade_Top10VC <- ggplot(AgradeVC_collapsed_10,aes(x=Var1, y=Freq))+
                         geom_bar(stat="identity", fill="blue") +
                         coord_flip() +
                         labs(y = "Number of violations") +
                         labs(x = "Violation category") +
                         ggtitle("Top10 critical violations for A-graded inspections")  +
                         theme(axis.title.x = element_text(size = rel(0.7)), 
                         axis.title.x = element_text(size = rel(0.7)))
print(Agrade_Top10VC)          

# print to pdf file for use in many applications
pdf(file = "D:/users/sghosh2/Desktop/NW 490 Special Topics Data Vizualization/Team project/Final/Violations by Criticality.pdf", width = 11, height = 8.5)
print(Agrade_Top10VC)

# 5 Top Vermin Violations
table(inspect$VIOLATION_DESCRIPTION)
#Code the Vermin Violations where GRADE =A  (dataset= Agrade)
Agrade <- subset(inspect, GRADE %in% c("A"))
table(Agrade$VIOLATION_DESCRIPTION)

Vermin = subset(Agrade, VIOLATION_DESCRIPTION %in% 
                    c("Evidence of mice or live mice present in facility's food and/or non-food ar",
                      "Evidence of rats or live rats present in facility's food and/or non-food ar",
                      "Facility not vermin proof. Harborage or conditions conducive to attracting",
                      "Facility not vermin proof. Harborage or conditions conducive to vermin exis"))
Vermin$Mice <-0
Vermin$Mice[Vermin$VIOLATION_DESCRIPTION == "Evidence of mice or live mice present in facility's food and/or non-food ar"] <- 1
Vermin$Mice[Vermin$VIOLATION_DESCRIPTION == "Evidence of rats or live rats present in facility's food and/or non-food ar"] <- 2
Vermin$Mice[Vermin$VIOLATION_DESCRIPTION == "Facility not vermin proof. Harborage or conditions conducive to attracting"] <- 3
Vermin$Mice[Vermin$VIOLATION_DESCRIPTION == "Facility not vermin proof. Harborage or conditions conducive to vermin exis"] <- 4

#Collapse Vermin related violations
Vermin_collapsed <- data.frame(table(Vermin$VIOLATION_DESCRIPTION))
Vermin_collapsed <- Vermin_collapsed[order(-Vermin_collapsed$Freq),]

#Put in line breaks at 45th character to display nicely
Vermin_collapsed$spit <- gsub('(.{1,45})(\\s|$)', '\\1\n', Vermin_collapsed$Var1)

#Sort the violations by order
Vermin_collapsed$spit <- factor(Vermin_collapsed$spit, levels = Vermin_collapsed$spit, ordered = TRUE)
Vermin_collapsed$row.names <- Vermin_collapsed$Freq

#Plot
TopVermin <- ggplot(Vermin_collapsed,aes(x=spit, y=Freq))+
    geom_bar(stat="identity", fill="blue") +
    coord_flip() +
    labs(y = "Number of violations") +
    labs(x = "Violation category") +
    ggtitle("Vermin related violations for A-graded inspections")  +
    theme(axis.title.x = element_text(size = rel(0.7)), 
          axis.title.x = element_text(size = rel(0.7)))
print(TopVermin)          

# print to pdf file for use in many applications
pdf(file = "D:/users/sghosh2/Desktop/NW 490 Special Topics Data Vizualization/Team project/Final/Vermin Related Violations where GRADE is A.pdf", width = 11, height = 8.5)
print(TopVermin)          

# 6 Top Insect/Roach related Violations
#Code the Insect Violations where GRADE =A  (dataset= Agrade)
Agrade <- subset(inspect, GRADE %in% c("A"))
table(Agrade$VIOLATION_DESCRIPTION)

Insect = subset(Agrade, VIOLATION_DESCRIPTION %in% 
                    c("Filth flies or food/refuse/sewage-associated (FRSA) flies present in facili",
                      "Live roaches present in facility's food and/or non-food areas."))
Insect$bugs <-0
Insect$bugs[Insect$VIOLATION_DESCRIPTION == "Filth flies or food/refuse/sewage-associated (FRSA) flies present in facili"] <- 1
Insect$bugs[Insect$VIOLATION_DESCRIPTION == "Live roaches present in facility's food and/or non-food areas."] <- 2

#Collapse Insect related violations
Insect_collapsed <- data.frame(table(Insect$VIOLATION_DESCRIPTION))
Insect_collapsed <- Insect_collapsed[order(-Insect_collapsed$Freq),]

#Put in line breaks at 45th character to display nicely
Insect_collapsed$spit <- gsub('(.{1,45})(\\s|$)', '\\1\n', Insect_collapsed$Var1)

#Sort the violations by order
Insect_collapsed$spit <- factor(Insect_collapsed$spit, levels = Insect_collapsed$spit, ordered = TRUE)
Insect_collapsed$row.names <- Insect_collapsed$Freq

#Plot
TopInsect <- ggplot(Insect_collapsed,aes(x=spit, y=Freq))+
    geom_bar(stat="identity", fill="blue") +
    coord_flip() +
    labs(y = "Number of violations") +
    labs(x = "Violation category") +
    ggtitle("Insect related violations for A-graded inspections")  +
    theme(axis.title.x = element_text(size = rel(0.7)), 
          axis.title.x = element_text(size = rel(0.7)))
print(TopInsect)          

# print to pdf file for use in many applications
pdf(file = "D:/users/sghosh2/Desktop/NW 490 Special Topics Data Vizualization/Team project/Final/Insect Related Violations where GRADE is A.pdf", width = 11, height = 8.5)
print(TopInsect)          

