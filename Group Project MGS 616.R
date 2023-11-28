# Importing libraries and Data

#1

library(skimr)
library(DataExplorer)
library(dplyr)
library(ggplot2)
library(scales)
library(car)
library(rstatix)
library(tidyr) 
library(randomForest)
library(caret)
library(datasets)
library(caTools)
library(e1071)

Customer_df = read.table("C:/Users/annu3/OneDrive - University at Buffalo/MGS 616 Predictive Analytics/data.csv", sep = "\t", header = T)
Customer_df=data.frame(Customer_df)
#Customer_df = drop_na()

#2

# data cleaning

#Plot the introduction to the dataset
plot_intro(Customer_df,title='Customer_df dataset')

#3
skim(Customer_df)

#4
head(Customer_df,4)

#5
plot_bar(Customer_df,ncol=2)

#6
# As we can see, columns z_costcontact and z_revenue have single vaue throughout the data
# Thus we will remove those columns

Customer_df = Customer_df %>% select(-c(Z_CostContact,Z_Revenue))


#7 

# Distribution of Yearly_Income Variable

ggplot(Customer_df, aes(x=Yearly_Income)) +
  geom_density(color="darkblue", fill="lightblue") + 
  theme_bw()


#8 not required 
# Filling the missing value with median value

Customer_df$Yearly_Income[is.na(Customer_df$Yearly_Income)]=median(Customer_df$Yearly_Income,na.rm=T)

# As we can see that the distribution of the Yearly_Income variable is rightly skewed
# Thus, we can use median to fill the missing values



#9
# 2nd cycle represents the master's degree 
# Replacing 2nd cycle with master's degree

Customer_df = Customer_df %>% 
  mutate(Customer_Education_Level = replace(Customer_Education_Level, Customer_Education_Level == '2n Cycle','Master'))


#10 
# Grouping the categories in marital status into 2 categories such as 'single' and 'in relationship'

Customer_df = Customer_df %>%
  mutate(Customer_Marital_Status=replace(Customer_Marital_Status, Customer_Marital_Status == 'Alone' | 
                                           Customer_Marital_Status == 'Absurd' | Customer_Marital_Status == 'YOLO' | 
                                           Customer_Marital_Status == 'Widow' | 
                                           Customer_Marital_Status == 'Divorced','Single')) %>%
  mutate(Customer_Marital_Status=replace(Customer_Marital_Status, Customer_Marital_Status == 'Married ' | Customer_Marital_Status == 'Married' |
                                           Customer_Marital_Status == 'Together','In Relationship'))

#11
#Calculating the current year

current_date= Sys.Date()
current_year = format(current_date,format="%Y")
current_year = as.integer(current_year)

# Adding the Age Column in Customer_df and removing the Birth_Year column 

Customer_df = Customer_df %>% mutate(Age = 2022- Birth_Year) %>% select(- Birth_Year)


#12

# Adding the Age_Range column

Customer_df = Customer_df %>% mutate(Age_Range = if_else(Age <= 18,"Children",
                                                         if_else(19 <= Age & Age <= 35,"Youth",
                                                                 if_else(35 < Age & Age <= 45,"Middle Aged","Old"))))


#13

# Calculating total Spent for each customer

Customer_df = Customer_df %>%
  mutate(Total_Spent = Expense_wine + Expense_fruits + Expense_meat + Expense_fish + Expense_sweets + 
           Expense_gold)


#14
# Calculating the total no of purchases made by customers

Customer_df = Customer_df %>%
  mutate(Total_Purchases = Purchases_website + Purchases_catalogue + Purchases_stores + 
           Purchases_discount)


#15
# Calculating the Enrollment Year of Customers 

Customer_df = Customer_df %>% separate(Customer_enrolment_Date,c('Date','Month','Year'),sep="-") %>% 
  select(- c('Month','Date')) %>%
  rename(Enrollment_Year = Year)

Customer_df['Enrollment_Year'] = as.integer(Customer_df$Enrollment_Year)

Customer_df = Customer_df %>%
  mutate(Seniority = 2022 - Enrollment_Year)

#16
# Calculating total no of accepted offers for each customer

Customer_df = Customer_df %>%
  mutate(Total_Offers = Accepted_campaign1 + Accepted_campaign2 + Accepted_campaign3 + Accepted_campaign4 + Accepted_campaign5 )

#17
head(Customer_df,2)

#18
# Percentages of Age Group

age_range = Customer_df %>% 
  select(Age_Range) %>%
  group_by(Age_Range) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

#19
ggplot(age_range, aes(x=factor(Age_Range,levels=c('Youth','Middle Aged','Old')),y=num,fill=Age_Range))+ 
  geom_bar(stat='identity') + 
  labs(x='Age Group',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c("#41B7C4", "#CCEDB1","#F5CA63" )) +
  theme_bw()

#20
ggplot(data = Customer_df, aes(x=Total_Spent)) + 
  geom_histogram(bins = 6,aes(y = stat(width*density)),color = "darkblue", fill = "lightblue", 
                 alpha = 0.5) + 
  scale_x_continuous(breaks=seq(0,2500,by=500)) +
  labs(x='Total Spent',y='Probability') +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  theme_bw()

#21
ggplot(Customer_df,aes(x=Total_Spent))+ 
  stat_ecdf(geom='step',size=1,color="#00AFBB",linetype='dashed') +
  scale_y_continuous(breaks=seq(0,1.0,by=0.1))+
  theme_bw()

#22

ggplot(data = Customer_df, aes(x= Yearly_Income)) + 
  geom_histogram(bins = 6,aes(y = stat(width*density)),color = "darkblue", fill = "lightblue", alpha = 0.5) + 
  labs(x='Yearly_Income',y='Probability') +
  scale_x_continuous(limits=c(0,200000),breaks=seq(0,200000,by=20000)) + # Ignoring Outlier
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  theme_bw()

#23
ggplot(Customer_df,aes(x=Yearly_Income))+ 
  stat_ecdf(geom='step',size=1,color="#00AFBB",linetype='dashed') +
  scale_x_continuous(limits=c(0,200000),breaks=seq(0,200000,by=20000)) + # Ignoring Outliers
  scale_y_continuous(breaks=seq(0,1.0,by=0.1))+
  theme_bw()

#24

# Correlation Test between Inocme and Total Spent Variable

cor.test(Customer_df$Yearly_Income, Customer_df$Total_Spent)


#25

ggplot(Customer_df,aes(x=Yearly_Income,y=Total_Spent))+
  geom_point(shape=21, color="black", fill="#69b3a2", size=2)+
  scale_x_continuous(limits=c(0,200000),breaks=seq(0,200000,by=20000)) + # Ignoring Outliers
  geom_smooth(method=lm, se=FALSE, color='darkred',linetype='dashed')+
  theme_bw()
#se : logical value. If TRUE, confidence interval is displayed around smooth. method: linear model


#26
ggplot(Customer_df, aes(x=Customer_Education_Level,y=Total_Spent))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#27
ggplot(Customer_df, aes(x=Customer_Marital_Status,y=Total_Spent))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#28
ggplot(Customer_df, aes(x=Age_Range,y=Total_Spent))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#29
products_df = Customer_df %>% select(starts_with("Expense"))
Product_Name = data.frame(Product_Name=rep(c('Wine','Fruit','Meat','Fish','Sweet','Gold'),each=5000))
Total_Spent = data.frame(Total_Spent = unlist(products_df),row.names=NULL)
products_df = data.frame(Product_Name,Total_Spent)
head(products_df,3)

#30
ggplot(products_df, aes(x=Product_Name,y=Total_Spent))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#31
ggplot(data = Customer_df, aes(x= Total_Purchases)) + 
  geom_histogram(bins = 6,aes(y = stat(width*density)),color = "darkblue", fill = "lightblue", 
                 alpha = 0.5) + 
  labs(x='No of Purchases',y='Probability') +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  scale_x_continuous(limits=c(0,50),breaks=seq(0,50,by=5)) + # Ignoring Outliers
  theme_bw()


#32
ggplot(Customer_df,aes(x=Total_Purchases))+ 
  stat_ecdf(geom='step',size=1,color="#00AFBB",linetype='dashed')+
  scale_y_continuous(breaks=seq(0,1.0,by=0.1))+
  theme_bw()

#33

purchase_df = Customer_df %>% select(starts_with("Purchases"))
Purchase_Name = data.frame(Purchase_Name=rep(c('Deal','Web','Catalog','Store'),each=5000))
Total_Purchases = data.frame(Total_Purchases = unlist(purchase_df),row.names=NULL)
purchase_df = data.frame(Purchase_Name,Total_Purchases)
head(purchase_df,3)

#34
ggplot(purchase_df, aes(x=Purchase_Name,y=Total_Purchases))+
  geom_boxplot(fill = 2,alpha = 0.5,color = 1,outlier.colour = 2)+
  theme_bw()

#35
# Percentages of purchase modes

purchase_df %>% 
  group_by(Purchase_Name) %>% 
  summarize(num=sum(Total_Purchases)) %>% 
  mutate(percentage=round(num*100/sum(num),2))

#36 

Customer_df %>% 
  summarize(Average_Web_Visit = mean(Monthly_Webvisits))

#37

ggplot(data = Customer_df, aes(x= Monthly_Webvisits)) + 
  geom_histogram(bins = 6,aes(y = stat(width*density)),color = "darkblue", fill = "lightblue", alpha = 0.5) + 
  labs(x='No of Web Visits',y='Probability') +
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) + #from scales
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,by=1)) + # Ignoring Outliers
  theme_bw()

#38
ggplot(Customer_df,aes(x=Monthly_Webvisits))+ 
  stat_ecdf(geom='step',size=1,color="#00AFBB",linetype='dashed')+
  scale_y_continuous(breaks=seq(0,1.0,by=0.1))+
  scale_x_continuous(limits=c(0,20),breaks=seq(0,20,by=1)) + # Ignoring Outliers
  theme_bw()

#39

# Percentages of Seniority

seniority = Customer_df %>% 
  select(Seniority) %>%
  rename(Total_Years = Seniority) %>%
  group_by(Total_Years) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

seniority['Total_Years'] = as.factor(seniority$Total_Years)


#40

ggplot(seniority, aes(x=factor(Total_Years,levels=c(8,9,10)),y=num,fill=Total_Years)) + 
  geom_bar(stat='identity') + 
  labs(x='No of Years ',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c("#41B7C4", "#CCEDB1","#F5CA63" )) +
  theme_bw()

#41
Customer_df %>% 
  group_by(Enrollment_Year) %>% 
  summarize(Total_Spent=sum(Total_Spent)) %>% 
  mutate(percentage=round(Total_Spent*100/sum(Total_Spent),2))

#42
# Correlation coefficient between Inocme and Total_Purchases Variable

cor.test(Customer_df$Yearly_Income, Customer_df$Total_Purchases)


#43
ggplot(Customer_df,aes(x=Yearly_Income,y=Total_Purchases))+
  geom_point(shape=21, color="black", fill="#FCA08D", size=2)+
  scale_x_continuous(limits=c(0,200000),breaks=seq(0,200000,by=20000)) + # Ignoring Outliers
  geom_smooth(method=lm, se=FALSE, color='darkblue',linetype='dashed')+
  theme_bw()
#se : logical value. If TRUE, confidence interval is displayed around smooth. method: linear model


#44
# Percentages of Accepted Offers

offers = Customer_df %>% 
  select(Total_Offers) %>%
  group_by(Total_Offers) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

offers['Total_Offers'] = as.factor(offers$Total_Offers)

#45
ggplot(offers, aes(x=factor(Total_Offers,levels=c(0,1,2,3,4)),y=num,fill=Total_Offers)) + 
  geom_bar(stat='identity') + 
  labs(x='Accepted No of Offers ',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c('#146C36','#F4D166','#2E5A87','#C43241','#4993C0' )) +
  theme_bw()

#46
cmp_df = Customer_df %>% select('Accepted_campaign1','Accepted_campaign2','Accepted_campaign3','Accepted_campaign4','Accepted_campaign5')
Campaign = data.frame(Campaign = rep(c('Campaign 1','Campaign 2','Campaign 3','Campaign 4','Campaign 5'),each=5000))
No_Of_Offers = data.frame(No_Of_Offers = unlist(cmp_df),row.names=NULL)
cmp_df = data.frame(Campaign, No_Of_Offers)
head(cmp_df,3)

#47
cmp_df = cmp_df %>% filter(No_Of_Offers == 1) %>% 
  group_by(Campaign) %>% 
  summarize(num=n()) %>% 
  mutate(percentage=round(num*100/sum(num),2))

#48
ggplot(cmp_df, aes(x=factor(Campaign),y=num,fill=Campaign)) + 
  geom_bar(stat='identity') + 
  labs(x='Campaign ',y='Count')+
  geom_text(aes(label=paste0(num ,' (',percentage,'%',')')),vjust=-.5)+
  scale_fill_manual(values = c('#146C36','#F4D166','#2E5A87','#C43241','#4993C0' )) +
  theme_bw()

#49
Customer_df %>% 
  group_by(Total_Offers) %>% 
  summarize(avg_spend = median(Yearly_Income))

# since Yearly_Income variable is skewed, we use median

#50
single = Customer_df %>% filter(Customer_Marital_Status == 'Single') %>% group_by(Customer_Education_Level) %>% 
  summarize(single = mean(Total_Spent))
in_relationship = Customer_df %>% filter(Customer_Marital_Status == 'In Relationship') %>% group_by(Customer_Education_Level) %>% 
  summarize(in_relationship = mean(Total_Spent))

joined_df1 = data.frame(single, in_relationship)

joined_df1 = joined_df1 %>% select(single, in_relationship)
joined_df1


#51
# Chi-Square test of Independence

chisq.test(joined_df1)

#52
# Cramer’s V is a measure of the strength of association between two nominal variables.
# It’s appropriate to calculate V when you’re working with any table larger than a 2 x 2 contingency table
# calculation of cramer's v

sqrt(26.893/2240)


#53
single = Customer_df %>% filter(Customer_Marital_Status == 'Single') %>% group_by(Age_Range) %>% 
  summarize(single = mean(Total_Spent)) 
in_relationship = Customer_df %>% filter(Customer_Marital_Status == 'In Relationship') %>% group_by(Age_Range) %>% 
  summarize(in_relationship = mean(Total_Spent))

joined_df2 = data.frame(single, in_relationship)
rownames(joined_df2) = c('Middle Aged','Old','Youth')
joined_df2 = joined_df2 %>% select(single, in_relationship)
joined_df2

#54
# Chi-Square test of Independence

chisq.test(joined_df2)

#55

# It’s appropriate to calculate φ only when you’re working with a 2 x 2 contingency table
# calculation of phi

sqrt(42.439/2240)

#56
# Testing equality of variance by levene's test
# Null Hypothesis(H0) - All groups have equal variance
# Alternate Hypothesis(H1) - Variances are not equal for altleast one pair

leveneTest(Total_Spent ~ Customer_Education_Level, data = Customer_df)

# From car library


#57
# Welch Anova Test

oneway.test(Total_Spent ~ Customer_Education_Level, data=Customer_df,var.equal = FALSE)

#58

# Games-Howell Post Hoc Test

games_howell_test(Total_Spent ~ Customer_Education_Level,data=Customer_df,conf.level = 0.95)

# From rstatix library



#59
# Testing equality of variance by levene's test
# Null Hypothesis(H0) - All groups have equal variance
# Alternate Hypothesis(H1) - Variances are not equal for altleast one pair

leveneTest(Total_Spent ~ Customer_Marital_Status, data = Customer_df)

# From car library

#60

# 2 sample t test

t.test(Total_Spent ~ Customer_Marital_Status, data = Customer_df, var.equal=T)

#61
# Testing equality of variance by levene's test
# Null Hypothesis(H0) - All groups have equal variance
# Alternate Hypothesis(H1) - Variances are not equal for altleast one pair
# From car library

leveneTest(Total_Spent ~ Age_Range, data = Customer_df)

#62
# one-way ANOVA

model <- aov(Total_Spent ~ Age_Range, data = Customer_df)
summary(model)

#63
# Tukey Test

TukeyHSD(model, conf.level=.95)

#64
# Using the products_df we created in EDA

products_df = products_df %>%
  mutate(Customer_Id = rep(1:5000, times= 6))
#products_df['Product_Name'] = as.factor(products_df$Product_Name)
head(products_df)

#65
# Repeated Measures ANOVA
# data: data frame, dv: (numeric) the dependent (or outcome) variable name,
# wid: variable name specifying the case/sample identifier, within: grouping variable

model = anova_test(data = products_df, dv = Total_Spent, wid = Customer_Id, within = Product_Name)
model

# From rstatix library


#66
friedman.test(y=products_df$Total_Spent, groups= products_df$Product_Name, blocks=products_df$Customer_Id)


#67
pairwise.wilcox.test(products_df$Total_Spent,products_df$Product_Name, p.adj = 'bonf')

# Linear Regression
y <- c(Customer_df$Total_Spent)
x <- c(Customer_df$Yearly_Income)
relation <- lm(y~x)
print(relation)
print(summary(relation))
salary <- data.frame(x = 160000)
result <- predict(relation, salary)
print(result)
plot(x, y, col="blue", main="Income vs Spent", abline(lm(y~x)), cex = 1.3, pch = 16, xlab="Income", ylab="Total spent")

# Random Forest
split <- sample.split(Customer_df, SplitRatio = 0.7)
split
train <- subset(Customer_df, split == "TRUE")
test <- subset(Customer_df, split == "FALSE")
set.seed(120)
classifier_RF = randomForest(x = train[2:6],
                             y = train$Total_Purchases,
                             ntree = 500)
classifier_RF
y_pred = predict(classifier_RF, newdata = test[2:6])
confusion_mtx = table(test[, 32], y_pred)
confusion_mtx
plot(classifier_RF)
importance(classifier_RF)
varImpPlot(classifier_RF)

# Naive Bayes
set.seed(120)
classifier_cl <- naiveBayes(Customer_df$Total_Offers~Customer_df$Customer_Education_Level+Customer_df$Customer_Marital_Status, data = train)
classifier_cl
y_pred <- predict(classifier_cl, newdata = test)
cm <- table(test$Total_Offers, y_pred)
cm
confusionMatrix(cm)
