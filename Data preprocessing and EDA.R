library(readxl)
data <- read_excel("~/Desktop/APU/AML/ASSIGNMENT/AML Assignment/winequality-red.xlsx")
head(data)
dim(data)
str(data)

library(psych)
describe(data)

#EDA
#Transform quality into binary 0 and 1
library(ggplot2)
data$quality <- as.factor(ifelse(data$quality>6, 1, 0))
summary(data$quality)
str(data)

quality.wine <- ggplot(data)+
  geom_bar(aes(quality, fill = "quality"))+
  labs(title = "Distribution of Quality of Wine", xlab = "Rating"); quality.wine

# Fixed.acidity
fixed.acidity <- ggplot(raw, aes(fixed.acidity)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(fixed.acidity)),
             color="blue", linetype="dashed", size=1); fixed.acidity

# Volatile.acidity
volatile.acidity <- ggplot(raw, aes(volatile.acidity)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(volatile.acidity)),
             color="blue", linetype="dashed", size=1); volatile.acidity

# Citric.acid
citric.acid <- ggplot(raw, aes(citric.acid)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(citric.acid)),
             color="blue", linetype="dashed", size=1); citric.acid

# Residual.sugar
residual.sugar <- ggplot(raw, aes(residual.sugar)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(residual.sugar)),
             color="blue", linetype="dashed", size=1); residual.sugar

# Chlorides
chlorides <- ggplot(raw, aes(chlorides)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(chlorides)),
             color="blue", linetype="dashed", size=1); chlorides

# Free.sulfur.dioxide
free.sulfur.dioxide <- ggplot(raw, aes(free.sulfur.dioxide)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide)),
             color="blue", linetype="dashed", size=1); free.sulfur.dioxide

# Total.sulfur.dioxide
total.sulfur.dioxide <- ggplot(raw, aes(total.sulfur.dioxide)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide)),
             color="blue", linetype="dashed", size=1); total.sulfur.dioxide

# Density
density <- ggplot(raw, aes(density)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(density)),
             color="blue", linetype="dashed", size=1); density

# pH
pH <- ggplot(raw, aes(pH)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(pH)),
             color="blue", linetype="dashed", size=1); pH

# Sulphates
sulphates <- ggplot(raw, aes(sulphates)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(sulphates)),
             color="blue", linetype="dashed", size=1); sulphates

#alcohol
alcohol <- ggplot(raw, aes(alcohol)) + 
  geom_density(fill = "lightblue")+
  geom_vline(aes(xintercept=mean(alcohol)),
             color="blue", linetype="dashed", size=1); alcohol

first_row = plot_grid(fixed.acidity,volatile.acidity,nrow =1)
second_row = plot_grid(citric.acid,residual.sugar,chlorides, nrow = 1) 
third_row = plot_grid(free.sulfur.dioxide,total.sulfur.dioxide,density, nrow = 1) 
forth_row = plot_grid(pH,sulphates,alcohol, nrow = 1) 
plot_grid(first_row,second_row,third_row,forth_row,nrow = 4,ncol =1)

g1 <- ggplot(raw,aes(factor(quality), fixed.acidity, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Fixed Acidity", title = "Boxplot of Quality vs Fixed Acidity")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g2 <- ggplot(raw,aes(factor(quality), volatile.acidity, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Volatile Acidity", title = "Boxplot of Quality vs Volatile Acidity")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g3 <- ggplot(raw,aes(factor(quality), citric.acid, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Citric Acid", title = "Boxplot of Quality vs Citric Acid")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g4 <- ggplot(raw,aes(factor(quality), residual.sugar, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Residual Sugar", title = "Boxplot of Quality vs Residual Sugar")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g5 <- ggplot(raw,aes(factor(quality), chlorides, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Chloride", title = "Boxplot of Quality vs Chloride")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g6 <- ggplot(raw,aes(factor(quality), free.sulfur.dioxide, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Free Sulfur Dioxide", title = "Boxplot of Quality vs Free Sulfur Dioxide")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g7 <- ggplot(raw,aes(factor(quality), total.sulfur.dioxide, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Total Sulfur Dioxide", title = "Boxplot of Quality vs Total Sulfur Dioxide")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g8 <- ggplot(raw,aes(factor(quality), density, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Density", title = "Boxplot of Quality vs Density")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g9 <- ggplot(raw,aes(factor(quality), pH, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "pH", title = "Boxplot of Quality vs pH")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g10 <- ggplot(raw,aes(factor(quality), sulphates, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Sulphates", title = "Boxplot of Quality vs Sulphates")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

g11 <- ggplot(raw,aes(factor(quality), alcohol, fill = factor(quality)))+
  geom_boxplot()+
  labs(x = "quality", y = "Alcohol", title = "Boxplot of Quality vs Alcohol")+
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust = 0.5))

first_row2 = plot_grid(g1,g2,nrow =1)
second_row2 = plot_grid(g3,g4, nrow = 1) 
third_row2 = plot_grid(g5,g6, nrow = 1) 
forth_row2 = plot_grid(g7,g8, nrow = 1) 
fifth_row = plot_grid(g9,g10, nrow = 1)
sixth_row = plot_grid(g11, nrow = 1)
plot_grid(first_row2,second_row2,third_row2, nrow = 3,ncol =1)
plot_grid(forth_row2,fifth_row,sixth_row,nrow = 3,ncol =1)

#Data pre-processing
library(missForest)
library(DataExplorer)
library(ggplot2)
library(inspectdf)
library(GGally)
set.seed(1111)
data <- prodNA(data, noNA = 0.05)
plot_missing (data)
colSums(sapply(data,is.na)) 

#missing data percentage
percent <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,percent)

#Impute
library(mice)
impute <-mice(data,m=3,seed =123)
print(impute)

#complete data
data_impute <-complete(impute,3)

# normalize function - each variable can be seen with different ranges
normalize <- function(x){
  return ( 
    (x - min(x))/(max(x) - min(x)) 
  )}

# normalize our data
data_normalized <- data_impute
data_normalized[,-12] <- sapply(data_normalized[,-12], normalize)
summary(data_normalized)

# Down-sampling
library(caret)
set.seed(123)
data_downsampled <- data_normalized
data_downsampled <- downSample(x = data_downsampled[,-12], y = data_downsampled$quality, list = F, yname = "quality")
inspect1<-inspect_cat(data_downsampled)
show_plot(inspect1)
prop.table(table(data_downsampled$quality))

# Up-sampling
set.seed(1234)
data_upsampled <- data_normalized
data_upsampled <- upSample(x = data_upsampled[,-12], y = data_upsampled$quality, list = F, yname = "quality")
inspect1<-inspect_cat(data_upsampled)
show_plot(inspect1)
prop.table(table(data_upsampled$quality))

# Data Partition - Downsample
set.seed(111)
ind_downsample <- sample(2,nrow(data_downsampled), replace = T, prob = c(0.7,0.3)) 
train_downsample <- data_downsampled[ind_downsample==1,] 
test_downsample <- data_downsampled[ind_downsample==2,] 

# Data Partition - Upsample
set.seed(112)
ind_upsample <- sample(2,nrow(data_upsampled), replace = T, prob = c(0.7,0.3)) 
train_upsample <- data_upsampled[ind_upsample==1,] 
test_upsample <- data_upsampled[ind_upsample==2,] 

# Data Partition - No resampling
set.seed(113)
ind <- sample(2,nrow(data_normalized), replace = T, prob = c(0.7,0.3)) 
train <- data_normalized[ind==1,] 
test <- data_normalized[ind==2,] 

#Cross-validation
library(e1071)
control <- trainControl(
  method = "cv", 
  number = 10)


