
#set my working directory 
setwd("C:/Users/bukharis/Desktop/My_data files")
getwd()
#loading my datasets 
diabetes_datasets <- read.csv("diabetes_Datasets.csv")
str(diabetes_Datasets)
summary(diabetes_datasets)

# Unpaired T-test
 out_put_ttest_unpaired <- t.test(Glucose ~ Outcome, data = diabetes_datasets, paired = FALSE)
 print(out_put_ttest_unpaired)




#Create a box plot
 boxplot(Glucose ~ Outcome, data = diabetes_datasets, 
         xlab = "Outcome", ylab = "Glucose Levels",
         main = "Glucose Levels by Diabetes Outcome")

 # Add colors and legend
 boxplot(Glucose ~ Outcome, data = diabetes_datasets, 
         xlab = "Outcome", ylab = "Glucose Levels",
         main = "Glucose Levels by Diabetes Outcome",
         col = c("goldenrod", "darkgreen"), 
        names = c("Non-diabetic", "Diabetic"))
 legend("topright", legend = c("Non-diabetic", "Diabetic"),
        fill = c("goldenrod", "darkgreen"))

# Logistic regression 

diabetes_logistic_fit <- glm(Outcome ~ Glucose + BMI + Age + BloodPressure, data = diabetes_datasets, family = binomial)
summary(diabetes_logistic_fit)







To do pca#



install.packages("ade4")
library(ade4)
library(ggplot2)

library(stats)
 data <- read.csv("C:/Users/bukharis/Desktop/My_data files/diabetes_datasets.csv")
 features <- data[, -which(names(data) == "Outcome")]
 pca_result <- prcomp(features, scale = TRUE)
 summary(pca_result)
 plot(pca_result, type = "l", main = "Scree Plot for PCA")
 cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
plot(cumulative_variance, type = "b", xlab = "Number of Components", ylab = "Cumulative Proportion of Variance Explained", main = "Cumulative Proportion of Variance Explained")
 threshold <- 0.95 
  num_components <- which(cumulative_variance >= threshold)[1]
selected_components <- pca_result$x[, 1:num_components]
loadings <- pca_result$rotation[, 1:num_components]
print(loadings)


plot(pca_result, type = "l", main = "Scree Plot for PCA")



for log regression based on pca:

install.packages("ggplot2")
library(ggplot2)
library(caret)
pca_model <- preProcess(features, method = "pca", pcaCalcInclude = c("eigen"))
 pca_data <- predict(pca_model, features)
 pca_data <- pca_data[, 1:4]
 
 # Split dataset into training and testing sets
 set.seed(123)
 train_indices <- createDataPartition(target, p = 0.8, list = FALSE)
 train_data <- pca_data[train_indices, ]
 train_target <- target[train_indices]
 test_data <- pca_data[-train_indices, ]
 test_target <- target[-train_indices]
 
 # Train logistic regression model
 logistic_model <- glm(train_target ~ ., data = data.frame(train_data), family = "binomial")
 
 # Make predictions on test data
 test_probs <- predict(logistic_model, newdata = test_data, type = "response")
 test_preds <- ifelse(test_probs > 0.5, 1, 0)
 test_preds_factor <- factor(test_preds, levels = c(0, 1))
 test_target_factor <- factor(test_target, levels = c(0, 1))
 
 # Calculate confusion matrix
 confusion_matrix <- confusionMatrix(data = test_preds_factor, reference = test_target_factor)
 print(confusion_matrix)

 auc_roc <- roc(test_target_factor, test_probs)
 print(auc_roc)


# plot the confusion matrix
 ggplot(data = as.data.frame(confusion_matrix$table), aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
   geom_text(aes(label = Freq), vjust = 1) +
   scale_fill_gradient(low = "lightblue", high = "darkblue") +
   labs(title = "Confusion Matrix",
        x = "Predicted",
       y = "Actual") +
   theme_minimal()





 #correlation coefficient #

 diabetes_data <- read.csv("C:/Users/bukharis/Desktop/My_data files/diabetes_datasets.csv")
 selected_variables <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", 
                         "Insulin", "BMI", "DiabetesPedigreeFunction", "Age", "Outcome")
 correlation_matrix <- cor(diabetes_data[selected_variables])
 print(correlation_matrix)
function (path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, 
    recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, 
    no.. = FALSE) .Internal(list.files(path, pattern, all.files, full.names, recursive, 
    ignore.case, include.dirs, no..))
bytecode: 0x000001c1279d5008>
environment: namespace:base>





