library("psychTools")
library("readr")

install.packages("dplyr")
library("dplyr")

setwd("/Users/sonyarashkovan/Desktop")
interviewdata <- read.csv("interviewdata.csv")

readLines("interviewdata.csv", n = 5)
interviewdata <- read.table("interviewdata.csv", header = FALSE, sep = ",", quote = "\"", fill = TRUE)
# Read without header and inspect structure
str(interviewdata)
#missing values
interviewdata <- read.table("interviewdata.csv", header = TRUE, sep = ",", quote = "\"", fill = TRUE)
interviewdata <- read.csv("interviewdata.csv", fill = TRUE)

#further cleaning and checking the data 
interviewdata <- read.csv("interviewdata.csv", header = FALSE, fill = TRUE)
View(interviewdata)

#fixing errir in column names
interviewdata <- read.csv("interviewdata.csv", header = FALSE, fill = TRUE)
names(interviewdata) <- paste0("V", 1:ncol(interviewdata))  

library(readr)
interviewdata <- read_csv("interviewdata.csv")
# figured out the issue: one of the cells is the name of the whole data set, but it takes up a column name without values associated wiht them (chr (1): interview_data)
# fix: delete the first row 
interviewdata <- read.csv("interviewdata.csv", skip = 1)
str(interviewdata)
head(interviewdata)

#Data inspection & cleaning 
View(interviewdata)
dim(interviewdata)
summary(interviewdata)
describe(interviewdata)
#need psych package for describe
install.packages(“psych”)
library("psych")
describe(interviewdata)
names(interviewdata)

#clean
complete.cases(interviewdata)
sum(complete.cases(interviewdata))
complete <- complete.cases(interviewdata)
na.omit(interviewdata)

summary(interviewdata) <- coicop_1_lable 
# column is not an object so needs "operator"
interviewdata$coicop_1_label
summary(interviewdata$coicop_1_label) # be careful with spelling! 

# Descriptive stats & visualization
unique(interviewdata$ccode)           # list of countries
unique(interviewdata$coicop_1_label) # list of category labels

#setting the countries to more usable names
kenya_data <- subset(interviewdata, ccode == "KEN")
southaftica_data <- subset(interviewdata, ccode == "ZAF)

#filter for the spending categories of interest 
target_labels <- c("Alcohol", "Transport", "Food", "Recreation", "Housing")

#Kenya
kenya_labels <- subset(kenya_data, coicop_1_label %in% target_labels)
barplot(table(kenya_labels$coicop_1_label), main = "Kenya COICOP Labels", xlab = "Category", ylab = "Frequency")
#frequency is very unhelpful to us , let's figure out how to add spending into this 

kenya_summary <- interviewdata %>% filter(ccode == "KEN", coicop_1_label %in% target_labels) %>% group_by(coicop_1_label) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE))

barplot(kenya_summary$avg_spend, names.arg = kenya_summary$coicop_1_label, main = "Avg Nominal Spending by Category (Kenya)", xlab = "Category", ylab = "Avg Nominal Spending", col = "skyblue", las = 2)
#fix formatting 
par(mar = c(7, 1, 4, 2))  # bottom, left, top, right

barplot(kenya_summary$avg_spend, names.arg = kenya_summary$coicop_1_label, main = "Avg Nominal Spending by Category (Kenya)", xlab = "Category", ylab = "Avg Nominal Spending", col = "skyblue", las = 2)

#play wiht ggplot to hand-adjust the plot
install.packages("ggplot2")
library (ggplot2)
ggplot(data = kenya_summary, aes(x = coicop_1_label, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Avg Nominal Spending by Category (Kenya)", x = "Category", y = "Avg Nominal Spending") + theme_minimal()

#awesome :) 

# South Africa 
SA_summary <- subset(southaftica_data, coicop_1_label %in% target_labels)
SA_summary <- interviewdata %>% filter(ccode == "ZAF", coicop_1_label %in% target_labels) %>% group_by(coicop_1_label) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE))
ggplot(data = SA_summary, aes(x = coicop_1_label, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Avg Nominal Spending by Category (South Africa)", x = "Category", y = "Avg Nominal Spending") + theme_minimal()

#comparison of countries
kenya_summary$ccode <- "Kenya"
SA_summary$ccode <- "South Africa"
comparison_df <- rbind(kenya_summary, SA_summary)
library(ggplot2)

ggplot(comparison_df, aes(x = coicop_1_label, y = avg_spend, fill = ccode)) + geom_col(position = "dodge") + labs(title = "Avg Nominal Spending by Category: Kenya vs South Africa", x = "Spending Category", y = "Avg Nominal Spending") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# spending by income level in Kenya 
unique(interviewdata$spending_group)

target_spending <- c("[12,40)", "[40,80)", "[80,120)")
#filter
kenya_spending <- interviewdata %>% filter(ccode == "KEN", spending_group %in% target_spending)
#summarize
spending_summary <- interviewdata %>% filter(ccode == "KEN", spending_group %in% target_spending) %>% group_by(spending_group) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE))
#plot 
ggplot(spending_summary, aes(x = spending_group, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Avg Nominal Spending by Income Group (Kenya)", x = "Income Group", y = "Avg Spending") + theme_minimal()


kenya_spending <- subset(kenya_data, spending_group %in% target_spending)
ggplot(data = kenya_spending, aes(x = spending_group, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Frequency of Spending Groups in Kenya", x = "Category", y = "Frequency") + theme_minimal()

# spending by age group in Kenya 
unique(interviewdata$age_group)
target_age <- c("[15,30)", "[30,45)", "[45,65)", "65+")
# summarize 
age_summary <- interviewdata %>% filter(ccode == "KEN", age_group %in% target_age) %>% group_by(age_group) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE))
ggplot(age_summary, aes(x = age_group, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Average Spending by Age Group in Kenya", x = "Age Group", y = "Avg Nominal Spending") + theme_minimal()

# spending by income group in South Africa 
unique(interviewdata$spending_group)
target_spending <- c("[12,40)", "[40,80)", "[80,120)")
SA_spending <- interviewdata %>% filter(ccode == "ZAF", spending_group %in% target_spending)
spending_summary <- interviewdata %>% filter(ccode == "ZAF", spending_group %in% target_spending) %>% group_by(spending_group) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE))
ggplot(spending_summary, aes(x = spending_group, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Avg Nominal Spending by Income Group (South Africa)", x = "Income Group", y = "Avg Spending") + theme_minimal()

#spending by age group in South Africa
unique(interviewdata$spending_group)
target_age <- c("[15,30)", "[30,45)", "[45,65)", "65+")
age_summary <- interviewdata %>% filter(ccode == "ZAF", age_group %in% target_age) %>% group_by(age_group) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE))
ggplot(age_summary, aes(x = age_group, y = avg_spend)) + geom_col(fill = "skyblue") + labs(title = "Average Spending by Age Group in South Africa", x = "Age Group", y = "Avg Nominal Spending") + theme_minimal()

#compare income groups 
names(kenya_spending)
names(SA_spending)

target_spending <- c("[12,40)", "[40,80)", "[80,120)")
kenya_spending <- interviewdata %>% filter(ccode == "KEN", spending_group %in% target_spending) %>% group_by(spending_group) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE)) %>% mutate(ccode = "Kenya")
SA_spending <- interviewdata %>% filter(ccode == "ZAF", spending_group %in% target_spending) %>% group_by(spending_group) %>% summarise(avg_spend = mean(exp_nominal, na.rm = TRUE)) %>% mutate(ccode = "South Africa")
comparison_df <- bind_rows(kenya_spending, SA_spending)
names(comparison_df)

ggplot(comparison_df, aes(x = spending_group, y = avg_spend, fill = ccode)) + geom_col(position = "dodge") + labs(title = "Avg Nominal Spending by Income Group: Kenya vs South Africa", x = "Spending Group", y = "Avg Nominal Spending") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ANOVA testing for South Africa
SA_data <- interviewdata %>% filter(ccode == "ZAF", spending_group %in% target_spending)
anova_result <- aov(exp_nominal ~ spending_group, data = SA_data)
summary(anova_result)
TukeyHSD(anova_result)
