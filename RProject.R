data<-read.csv("C:/Users/fatma/OneDrive/Desktop/HREA.csv", header=TRUE, stringsAsFactors=TRUE)
str(data)
var_names <- c(
  "Attrition", "BusinessTravel", "DistanceFromHome", "JobInvolvement", 
  "JobRole", "JobSatisfaction", "MonthlyIncome", "NumCompaniesWorked", 
  "OverTime", "StockOptionLevel", 
  "YearsSinceLastPromotion", "YearsWithCurrManager"
)
#Creat definition table
definitions <- c(
  "Employee attrition status (Yes = employee left, No = stayed)",
  "Frequency of business travel",
  "Distance between home and work (in km or miles)",
  "Level of job involvement (1 = Low, 4 = Very High)",
  "Employee's job role",
  "Level of job satisfaction (1 = Low, 4 = Very High)",
  "Monthly income in USD",
  "Number of companies worked at",
  "Whether the employee works overtime (Yes/No)",
   "Stock option level (0 to 3)",
  "Years since last promotion",
  "Years with current manager"
)

for (var in var_names) {
  if (is.numeric(data)) {
    data[[var]][is.na(data[[var]])] <- mean(data[[var]], na.rm = TRUE)
  }
}

summary_table <- data.frame(
  Variable = character(),
  Definition = character(),
  Mean = numeric(),
  SD = numeric(),
  Min = numeric(),
  Max = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(var_names)) {
  var <- var_names[i]
  if (is.numeric(data[[var]])) {
    summary_table <- rbind(summary_table, data.frame(
      Variable = var,
      Definition = definitions[i],
      Mean = round(mean(data[[var]], na.rm = TRUE), 2),
      SD = round(sd(data[[var]], na.rm = TRUE), 2),
      Min = min(data[[var]], na.rm = TRUE),
      Max = max(data[[var]], na.rm = TRUE)
    ))
  } else {
    summary_table <- rbind(summary_table, data.frame(
      Variable = var,
      Definition = definitions[i],
      Mean = NA, SD = NA, Min = NA, Max = NA
    ))
  }
}
View(summary_table)

#jobrole table and attrition 
table_role_attrition <- table(data$JobRole, data$Attrition)
job_roles <- rownames(table_role_attrition)
attrition_yes <- table_role_attrition[, "Yes"]
attrition_total <- rowSums(table_role_attrition)
attrition_rate <- round((attrition_yes / attrition_total) * 100, 1)

role_table <- data.frame(
  JobRole = job_roles,
  AttritionYes = attrition_yes,
  Total = attrition_total,
  AttritionRate = attrition_rate
)

View(role_table)

#barplot role~attrition
install.packages("ggplot2")
library(ggplot2)
barplot(attrition_rate,
        names.arg = job_roles,
        col = "salmon",
        las = 2,
        ylim = c(0, max(attrition_rate) + 10),
        main = "Attrition Rate by Job Role (%)",
        ylab = "Attrition Rate (%)")

#box plot income~Attrition
boxplot(MonthlyIncome ~ Attrition,
        data = data,
        main = "Monthly Income by Attrition Status",
        xlab = "Attrition",
        ylab = "Monthly Income",
        col = c("lightgreen", "salmon"))

#boxplot
ggplot(data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill", color = "white") +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(
    title = "Attrition Proportion by OverTime",
    subtitle = "Shows the proportion of employees who left vs stayed by overtime status",
    x = "OverTime",
    y = "Proportion",
    fill = "Attrition Status"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 15, vjust = 0.7)
  )

#scatter income vs current manager by attrition
library(ggplot2)
ggplot(data, aes(x = YearsWithCurrManager, y = MonthlyIncome, color = Attrition)) +
  geom_jitter(alpha = 0.5, size = 1.5, width = 0.3) +
  scale_color_manual(values = c("No" = "forestgreen", "Yes" = "red")) +
  labs(title = "Income vs. Years with Current Manager by Attrition",
       x = "Years with Current Manager",
       y = "Monthly Income") +
  theme_minimal()

#decision tree
library(rpart)
library(rpart.plot)

set.seed(123)

# 70% / 30% split
sample_index <- sample(1:nrow(data), size = 0.7 * nrow(data))

train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

cat("Train size:", nrow(train_data), "\n")
cat("Test size:", nrow(test_data), "\n")


fit_tree <- rpart(
  Attrition ~ OverTime + MonthlyIncome + StockOptionLevel +
    YearsWithCurrManager + JobSatisfaction,
  data = train_data,
  method = "class"
)

rpart.plot(fit_tree,
           type = 3,
           extra = 101,
           fallen.leaves = TRUE,
           main = "Decision Tree : Predicting Employee Attrition")

train_pred <- predict(fit_tree, train_data, type = "class")
conf_matrix_train <- table(Predicted = train_pred, Actual = train_data$Attrition)
print(conf_matrix_train)
prop_table_train <- prop.table(conf_matrix_train)
accuracy_train <- sum(diag(prop_table_train))
print(paste("Training Accuracy :", round(accuracy_train, 4)))

test_pred <- predict(fit_tree, test_data, type = "class")
conf_matrix_test <- table(Predicted = test_pred, Actual = test_data$Attrition)
print(conf_matrix_test)
prop_table_test <- prop.table(conf_matrix_test)
accuracy_test <- sum(diag(prop_table_test))
print(paste("Testing Accuracy :", round(accuracy_test, 4)))

#logistic
# Convert attrition to binary
data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

# Convert OverTime and StockOptionLevel to factors if needed
data$OverTime <- as.factor(data$OverTime)
data$StockOptionLevel <- as.factor(data$StockOptionLevel)

# Train-test split
set.seed(345)
train_index <- sample(1:nrow(data), size = nrow(data) * (2/3))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Run logistic regression
logit_model <- glm(Attrition ~ MonthlyIncome + OverTime + YearsWithCurrManager +
                     StockOptionLevel + JobSatisfaction,
                   data = train_data, family = "binomial")

# Summary
summary(logit_model)

# Predict on test set
test_probs <- predict(logit_model, test_data, type = "response")
test_pred <- ifelse(test_probs > 0.5, 1, 0)

# Confusion matrix & accuracy
conf_matrix <- table(Predicted = test_pred, Actual = test_data$Attrition)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Test Accuracy:", round(accuracy, 4)))

#clustering
selected_vars <- c("BusinessTravel", "DistanceFromHome", "JobInvolvement",
                   "NumCompaniesWorked", "YearsSinceLastPromotion")

clust_data <- data[, selected_vars]
clust_data$BusinessTravel <- as.numeric(as.factor(clust_data$BusinessTravel))
clust_data <- na.omit(clust_data)
clust_data_scaled <- scale(clust_data)

set.seed(123) 
kmeans_result <- kmeans(clust_data_scaled, centers = 4, nstart = 25)
data$Cluster <- as.factor(kmeans_result$cluster)
table(data$Cluster, data$Attrition)
prop.table(table(data$Cluster, data$Attrition), margin = 1) * 100  # درصدها

#
library(ggplot2)
pca_result <- prcomp(clust_data_scaled)
pca_data <- as.data.frame(pca_result$x[,1:2])
pca_data$Cluster <- data$Cluster
pca_data$Attrition <- data$Attrition

ggplot(pca_data, aes(x=PC1, y=PC2, color=Cluster, shape=Attrition)) +
  geom_point(size=3, alpha=0.8) +
  labs(title = "KMeans Clustering with PCA",
       subtitle = "Colored by Cluster, Shape shows Attrition",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()
# creat table for no used attributes
table(data$Cluster, data$Attrition)
prop.table(table(data$Cluster, data$Attrition), margin = 1) * 100  # درصدی

library(dplyr)
data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Distance = mean(DistanceFromHome, na.rm = TRUE),
    Avg_Involvement = mean(JobInvolvement, na.rm = TRUE),
    Avg_NumCompanies = mean(NumCompaniesWorked, na.rm = TRUE),
    Avg_YearsSincePromo = mean(YearsSinceLastPromotion, na.rm = TRUE)
  )


