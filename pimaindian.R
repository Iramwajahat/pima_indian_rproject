
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

Advanced pharmacology Project.

 setwd("C:/Users/bukharis/Desktop/My_data files")
 getwd()
 
diabetes_datasets <- read.csv("diabetes_Datasets.csv")
summary(diabetes_datasets)

setwd("C:/Users/bukharis/Desktop/My_data files")
 getwd()
diabetes_datasets <- read.csv("diabetes_Datasets.csv")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages("pROC")
library(ggplot2)
library(dplyr)
library(caret)


 # Check for missing values


     # Convert 'Outcome' (target variable) to a factor
colSums(is.na(diabetes_datasets))

diabetes_datasets$Outcome <- as.factor(diabetes_datasets$Outcome)
set.seed(123)
 train_index <- createDataPartition(diabetes_datasets$Outcome, p = 0.8, list = FALSE)
 train_data <- diabetes_datasets[train_index, ]
 test_data <- diabetes_datasets[-train_index, ]
 logistic_model <- glm(Outcome ~ Glucose + BMI + Age + BloodPressure, 
                       data = train_data, family = binomial)
 summary(logistic_model)

 #

 test_data$predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")
 test_data$predicted_outcome <- ifelse(test_data$predicted_prob >= 0.5, 1, 0)
 test_data$predicted_outcome <- as.factor(test_data$predicted_outcome)
 conf_matrix <- confusionMatrix(test_data$predicted_outcome, test_data$Outcome)
 print(conf_matrix)

 

ggplot(test_data, aes(x = Glucose, y = predicted_prob, color = Outcome)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Glucose", y = "Predicted Probability") +
   theme_minimal()

 # Analyze predictions based on glucose levels
 cutoff <- 0.5
high_risk <- test_data %>% filter(predicted_prob >= cutoff)

 # Display high-risk individuals
 head(high_risk)

 # Save the high-risk individuals' data to a CSV file
 write.csv(high_risk, "F:/High_Risk_Individuals.csv", row.names = FALSE)

  install.packages("clusterProfiler")
install.packages("org.Hs.eg.db")

install.packages("pathview")

 # Install clusterProfiler if not already installed
if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
  install.packages("BiocManager")  # Install BiocManager first
   BiocManager::install("clusterProfiler")  # Install clusterProfiler}

   # Load clusterProfiler
   library(clusterProfiler)

# Install org.Hs.eg.db for human gene annotations (needed for pathway analysis)
if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
BiocManager::install("org.Hs.eg.db")}

library(org.Hs.eg.db)


# Install clusterProfiler if not already installed
 if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
   install.packages("BiocManager")  # Install BiocManager first
   BiocManager::install("clusterProfiler")  # Install clusterProfiler
 }

 
 # Load clusterProfiler
 library(clusterProfiler)
# Install org.Hs.eg.db for human gene annotations (needed for pathway analysis)
 if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
  BiocManager::install("org.Hs.eg.db")}

  # Important

 significant_genes <- unique(c(glucose_genes, bmi_genes, bp_genes))
 print(significant_genes)

install.packages("org.Hs.eg.db")

library(org.Hs.eg.db)

gene_symbols <- c("INS", "GCK", "SLC2A4")
 entrez_ids <- as.character(mapIds(org.Hs.eg.db, keys = gene_symbols, column = "ENTREZID", keytype = "SYMBOL"))

 print(entrez_ids)

  # Run KEGG enrichment with Entrez IDs


glucose_pathways <- enrichKEGG(gene = entrez_ids, organism = "hsa")

 # View results
 summary(glucose_pathways)ges


 





