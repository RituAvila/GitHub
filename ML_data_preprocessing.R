getwd()
setwd("C:/Users/Ritu/Desktop/ML")
readLines(con = "C:/Users/Ritu/Desktop/ML/German-Credit_1.csv", n = 5)
df1 <- read.table(file = "C:/Users/Ritu/Desktop/ML/German-Credit_1.csv", header = T, sep = "," )
View(df1)

df1 <- read.csv(file = "C:/Users/Ritu/Desktop/ML/German-Credit_1.csv", header = T)
head(df1)
colnames(df1)
df2 <- read.csv(file = "C:/Users/Ritu/Desktop/ML/German-Credit_2.csv", header = TRUE)
head(df2)

df3 <- merge(x = df1, y = df2, by = "OBS", all = T)

head(df3)
head(df1)
tail(df1)
summary(df1) # Helps to identify the NA values
str(df3) # To identify the type of variable

# Dividing df3 into num and categorical using setdiff()
num_Attr <- c("DURATION","AMOUNT","INSTALL_RATE","AGE","NUM_CREDITS","NUM_DEPENDENTS")
cat_Attr <- setdiff(x = colnames(df3), y = num_Attr) 
df3$OBS
df3$OBS <- as.character(df3$OBS)
str(df3$OBS)

test_vec <- c(0,1,1,1,1,0,0,0,1,0,1,0,1,0)
test_vec
fac_vec <- as.factor(test_vec)
fac_vec
# When we reconvert fcator into original the values change
reconverted_vec <- as.numeric(fac_vec)
reconverted_vec


df3$RESPONSE <- as.factor(df3$RESPONSE)
df3$RESPONSE
#df_cat <- subset(df3,select = cat_Attr)
#df3[,cat_Attr] <- data.frame(apply(df_cat, 2, function(x) as.factor(x)))
#str(df3)
library(magrittr)
library(dplyr)
df3 %<>% mutate_each_(funs(factor(.)),cat_Attr)
str(df3)

colSums(is.na(x = df3))
sum(is.na(df3))
df4 <- na.omit(df3)
dim(df3)
dim(df4)
sum(is.na(df4))

library(DMwR)
# Imputing/Subsetting missing values
manyNAs(df3, 0.1)
# centralImputation - at the centre we are trying to reveal the NA values (will give vague NA values)
# knnImputation - Finding out the nearest neighbours null values
df3_imputed <- centralImputation(data = df3)
sum(is.na(df3_imputed))

df3_imputed1 <- knnImputation(data = df3, k=5)
sum(is.na(df3_imputed1))

# Binning(Bucketing) and discreting the variable
library(infotheo)
x <- c(5,6,7,8,8,8,8,8,11,20,21,22)
length(x)
x0 <- discretize(x, disc = "equalfreq", nbins = 4)
table(x0)
x1 <- discretize(x, disc = "equalwidth", nbins = 4)
table(x1)

AmtBin <- discretize(df3_imputed$AMOUNT, disc = "equalfreq", nbins = 4)
table(AmtBin)
AmtBin <- discretize(df3_imputed$AMOUNT, disc = "equalwidth", nbins = 4)
table(AmtBin)

# Dummy Variables
# It is created for categorical variable. Dummifying the rows into equala no. of columns
library(dummies)
df_ex <- datasets::warpbreaks
table(df_ex$tension)
dummy_ex <- dummy(df_ex$tension)
head(dummy_ex)

df_cat <- subset(df3_imputed, select = cat_Attr)
df_cat_dummies <- data.frame(apply(df_cat, 2, function(x) dummy(x)))
dim(df_cat_dummies)

# Standardizing the data
library(vegan)
# to reduce the variable into extreme values
df_num <- df3_imputed[, num_Attr]
df_num2 <- decostand(x = df_num, method = "range")
summary(df_num2)

df_num3 <- decostand(x = df_num, method = "standardize")
summary(df_num3)

df_final <- cbind(df_num3,df_cat)
head(df_final)

# Train Test Split
rows <- seq(1,1000,1)
set.seed(123)
trainRows <- sample(rows,600)

train_data <- df_final[trainRows,]
test_data <- df_final[-c(trainRows),]
dim(train_data)
dim(test_data)

#Build model
lm_model <- lm(AMOUNT~DURATION, data = train_data)
summary(lm_model)

# Basic Data Visualizations
df <- df_final

# Histogram
hist(df$AGE)
hist(df$AGE, col = "pink")

# Boxplot
boxplot(df$AGE, horizontal = TRUE)
boxplot(AMOUNT~RESPONSE, data = df, xlab = "TARGET", ylab = "AMOUNT", main = "Continuous v/s Categorical")

# Bar plot
barplot(table(df$RESPONSE))
barplot(table(df$RESPONSE), col = "orange")

# Scatter plot
plot(x=df$AGE, y=df$AMOUNT, xlab = "DURATION", ylab = "AMOUNT", main = "Continuous v/s Continuous")
