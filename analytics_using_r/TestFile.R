install.packages(c("tidyverse","rpart","rpart.plot","caret","caTools","ROCR"))

install.packages("ggplot2")

install.packages("fastDummies")

library(tidyverse) # Data Manipulation and Visualization 
library(caTools)# Data Splitting 
library(caret) # Prediction: Classification and Regression 
library(ROCR) # Prediction: ROC Curve 
library(rpart) # Prediction: Decision Tree 
library(rpart.plot)

library(ggplot2)

library(fastDummies)

getwd()
setwd('/Users/sunilnair/Documents/ByteSize Trainings/curriculum/analytics_using_r')

df = read_csv('bank_data_for_r.csv')

summary(is.na(df))

ncol(df)
nrow(df)

sample(df)

str(df)

summary(df)

table(df$'Home Ownership')

filter(df, df$'Home Ownership' == "Rent")

select(Home Ownership)

df %>% select('Home Ownership','Loan Status')

df %>% distinct('Home Ownership')

df[,c('Home Ownership','Loan Status')]

colnames(df %>% select_if(is.numeric))

help(filter)

colors = c('Red','Green','Blue') 
png(file = "barchart_stacked.png")
barplot(df$'Annual Income', 
        main = "Home Ownership", 
        xlab = "Ownership", 
        ylab = "count", 
        col = colors)



# Using dplyr package
  
df <- df %>% 
  mutate(`Term` = ifelse(`Term` == 'Short Term',0,1))
df

df$`Home Ownership`[df$`Home Ownership` == 'Home Mortgage'] = 'Have Mortgage'

table(df$Term)

dummy <- dummyVars(" ~ `Home Ownership`", data = df)
dummy_df <- data.frame(predict(dummy, newdata = df))

new = cbind(df,dummy_df)

df <- fastDummies::dummy_columns(df, select_columns = c("Home Ownership","Purpose"))

names(df)


# ggplot(df, aes(x=df$`Credit Score`, y=df$`Current Loan Amount`,col="Term"))+geom_point()
# cmd+shift+c to comment a line

ggplot(df, aes(x = df$`Credit Score`,colour='smooth')) +
  geom_histogram()

ggplot(df, aes(x = df$`Loan Status`)) +
  geom_bar(color='red',fill='orange')

str(df)

# boxplot(df[,c("Current Loan Amount","Annual Income","Credit Score")])

ggplot(df, aes(y="Current Loan Amount")) + geom_boxplot()

ggplot(df, aes(x="", y=table(df$`Home Ownership`), fill=df$`Home Ownership`)) 
+ geom_bar(stat="identity", width=1) 
+ coord_polar("y", start=0)

colors = c("red", "yellow", "green", "violet", 
             +   "orange", "blue", "pink", "cyan")

pie(table(df$Term),border="white",col=colors)

# Drop Variables
df <- df %>% select(-c('Loan ID','Customer ID'))

df

unique(df$'Home Ownership')

summary(df)

table(is.na(df))

sum(is.na(df))

sapply(df, function(x) sum(is.na(x)))

df %>% replace(.=="NULL", NA)

median(df$`Credit Score`,na.rm=TRUE)

getmode(df$`Home Ownership`)


df <- df %>% 
  mutate_at('Credit Score', ~replace_na(.,mean(df$`Credit Score`,na.rm=TRUE)))




df <- df %>% mutate_at('Credit Score', ~replace_na(.,median(df$`Credit Score`,na.rm=TRUE)))


df <- df %>% select(-c('Loan Status','Loan ID','Customer ID'))

df$`Home Ownership`[df$`Home Ownership` == 'HaveMortgage'] = 'Home Mortgage'


df <- df %>% 
  mutate(`Term` = ifelse(`Term` == 'Short Term',0,1))


df <- fastDummies::dummy_columns(df, select_columns = c("Home Ownership","Purpose"))

df <- df %>% select(-c('Home Ownership','Purpose'))


set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample.split(df$Defaulter, SplitRatio = 0.7)
train  <- subset(df, sample == TRUE)
test   <- subset(df, sample == FALSE)

head(train)

dim(train)
dim(test)


classifier = glm(Defaulter ~ ., 
                 family = binomial, data = train)

classifier = step(classifier)



plot(classifier)

summary(classifier)


prob_pred <- predict(classifier, type = 'response', newdata = test)

prob_pred


y_pred <- ifelse(prob_pred > 0.8, 1, 0)

error <- mean(test$Defaulter != y_pred)

error

class(y_pred)

class(test$Defaulter)

print(paste('Accuracy',1-error))


table(test$Defaulter, y_pred)

cm <- confusionMatrix(data=factor(y_pred), reference = factor(test$Defaulter))
cm
