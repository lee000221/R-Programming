
setwd("D:/Master/S3/7004/Group Assignment")
source("function.R")
source("visualization.R")
df <- read_csv("raw_rental mala.csv")

################################################################################
df
#data preprocessing
df <- df[!duplicated(df), ]
sum(duplicated(df))
# Removing 'RM' from monthly rent
df <- df %>%
  mutate(monthly_rent = gsub("RM|per month|,", "", monthly_rent),
         monthly_rent = gsub("\\s+", "", monthly_rent),
         monthly_rent = as.numeric(monthly_rent))

colnames(df)[colnames(df) == "monthly_rent"] <- "monthly_rent_rm"

# Dropping 'sq.ft' from size
df$size <- as.numeric(str_extract(df$size, "(.*?)(?= sq)"))
# rename column name as size_sqft
colnames(df)[colnames(df) == "size"] <- "size_sqft"

# Dropping 'city name' from location
df$location <- str_extract(df$location, "\\w+$")

# count the no of facilities 
df <- df %>%
  mutate(fac_count = str_count(facilities, ",") + 1,
         add_fac_count = str_count(additional_facilities, ",") + 1)
# generate a new column named nearby_ktm_lrt from aditional_facilities 
df$nearby_ktm_lrt <- sapply(df$additional_facilities, function(x) extract_near_ktm_lrt(x))
write.csv(df, file = "Testing.csv", row.names = FALSE)

# drop the 4 type of meanless columns 'ads_id', 'prop_name', 'facilities', 'additional_facilities'
df <- df[, !(names(df) %in% c('ads_id', 'prop_name', 'facilities', 'additional_facilities'))]
# collect categorical data from df
is_categorical <- df[,sapply(df, function(x) is.factor(x) || is.character(x))]
# see unique value for category data
sapply(is_categorical, function(x) length(unique(x)))
#write.csv(df, file = "Testing.csv", row.names = FALSE)
################################################################################
# Visualization
# bar chart
#bar_all <- do.call(grid.arrange, c(bar_charts, ncol = 2))

# histogram
#his_interval <- grid.arrange(his_completion_year, his_monthly_rm, his_parking, his_size, 
#          his_rooms,his_bathroom,his_fac_count,his_add_fac_count,nrow = 4,ncol = 2)

# boxplot
#box_nominal <- grid.arrange(box_uncropped_size,box_cropped_size,
#            box_uncropped_bathrooms,box_cropped_bathrooms,
#             box_uncropped_parking,box_cropped_parking,
#            box_uncropped_rent,box_cropped_rent,
             #            box_uncropped_rooms,box_cropped_rooms,nrow = 5, ncol = 2)

# pie chart
#pie_furnished 
#pie_property_type 
#pie_region 
#pie_nearby_ktm_lrt 
#pie_location

# line chart 
#line_corr <- grid.arrange(line_size_VS_rent, line_year_VS_rent, line_Parking_VS_rent,  
#             line_add_fac_count_VS_rent,line_fac_count_VS_rent,line_rooms_VS_rent,line_bathroom_VS_rent,nrow = 4,ncol = 2)

#box_corr <- grid.arrange(box_lrt_VS_rent,
 #            box_furnished_VS_rent,
 #            box_property_VS_rent,
  #           box_region_VS_rent, nrow = 4, ncol = 1)

################################################################################
# data cleaning
# data filter processing
df1 <- df
df1 <- df1 %>% filter(monthly_rent_rm > 100 & monthly_rent_rm < 4000)
df1 <- df1 %>% filter(size_sqft > 100 & size_sqft < 2000)
df1 <- df1 %>% filter(rooms > 0 & rooms < 4)
df1 <- df1 %>% filter(parking > 0 & parking < 4)
df1 <- df1 %>% filter(bathroom > 0 & bathroom < 4)
df1 <- df1[df1$property_type %in% c("Apartment", "Condominium", "Service Residence"), ]

# count the number of rows for each unique value
location_counts <- df1 %>%
  group_by(location) %>%
  summarise(n = n())
# get the unique value count greater than 100
selected_locations <- location_counts %>%
  filter(n > 100) %>%
  pull(location)
# keep the unique value count greater than 100
df1 <- df1 %>%
  filter(location %in% selected_locations)

df1$furnished <- round(as.numeric(factor(df1$furnished, levels = c("Not Furnished", "Partially Furnished", "Fully Furnished"), labels = c(1, 2, 3))),0)
df1$region <- round(as.numeric(factor(df1$region, levels = c("Kuala Lumpur", "Selangor"), labels = c(1, 2))),0)
df1$nearby_ktm_lrt <- round(as.numeric(factor(df1$nearby_ktm_lrt, levels = c("yes", "no"), labels = c(2, 1))),0)
df1$property_type <- round(as.numeric(factor(df1$property_type, levels = c("Condominium", "Apartment", "Service Residence"), labels = c(1, 2, 3 ))),0)

#NA processing
colSums(is.na(df1))
X_cat <- df1[,sapply(df1, function(x) is.factor(x) || is.character(x))]
X_cat

# transform unique value of in category of X to new column
X_cat_ohe <- dummyVars(~ . , data = X_cat)
X_cat_ohe <- predict(X_cat_ohe, newdata = X_cat)
X_cat_ohe
# Apply numerical imputation
num_name <- colnames(df1)[sapply(df1, function(x) is.numeric(x) || is.integer(x))]
X_num <- df1[, num_name]
X_num_imputed <- numericalImputation(X_num, strategy = "mode")

df1 <- cbind(X_num_imputed, X_cat_ohe)
colSums(is.na(df1))

################################################################################
# Call the function and assign the results to X and y
result <- extractInputOutput(data = df1, output_column_name = "monthly_rent_rm")
X1 <- result$input_data
y1 <- result$output_data
nrow(X1)
length(y1)
X_clean1 <- standardizerData(X1)
################################################################################
# Train test spilt
# Set seed for reproducibility
set.seed(123)
# Train-test split
# Create the train and test indices
train_indices1 <- createDataPartition(y1, p = 0.8, list = FALSE)

# Split the data into training and testing sets for regression model
X_train1 <- X_clean1[train_indices1, ]
y_train1 <- y1[train_indices1]
X_test1 <- X_clean1[-train_indices1, ]
y_test1 <- y1[-train_indices1]
round(nrow(X_test1) / nrow(X_clean1), 2)

################################################################################
### linear regression model
y_baseline <- rep(mean(y_train1), length(y_train1))
# Train the linear regression model
model_lr <- lm(y_train1 ~ ., data = X_train1)
# Predict using the train data
y_pred_lr <- round(predict(model_lr, X_train1),0)
# Calculate metrics
metrics <- calculate_metrics(y_train1, y_pred_lr)
cat("R2-score:", metrics$R2_score, "and MAE score:", metrics$MAE_score, "\n")
plot(y_train1, y_pred_lr, type = "p",
     xlab = "Actual Rent",
     ylab = "Predicted Rent",
     main = "Scatter plot of Actual Rent vs Predicted Rent")
lines(smooth.spline(y_train1, y_pred_lr), col = "red")

# Build Gradient Boosting Regression model
model_gbr <- gbm(y_train1 ~ ., data = X_train1, n.trees = 100, 
                 distribution = "gaussian", interaction.depth = 4, shrinkage = 0.1, 
                 n.minobsinnode = 10, bag.fraction = 0.5, train.fraction = 0.8, 
                 cv.folds = 5, verbose = TRUE)
# Predict on the training data
y_pred_gbr <- predict(model_gbr, newdata = X_train1, n.trees = 100, type = "response")
# Calculate mean absolute error
mae_gbr <- mean(abs(y_train1 - y_pred_gbr))
# Calculate R-squared
r2_gbr <- 1 - sum((y_train1 - y_pred_gbr)^2) / sum((y_train1 - mean(y_train1))^2)
# Print R-squared and MAE scores
cat("R2-score:", round(r2_gbr, 4), "and MAE score:", round(mae_gbr, 4))
plot(y_train1, y_pred_gbr, type = "p",
     xlab = "Actual Rent",
     ylab = "Predicted Rent",
     main = "Scatter plot of Actual Rent vs Predicted Rent")
lines(smooth.spline(y_train1, y_pred_gbr), col = "red")

# randomForest
# Build random forest
colnames(X_train1) <- gsub(" ", "_", colnames(X_train1))
model_rf <- randomForest(y_train1 ~ ., data = X_train1)
# Predict
y_pred_rf <- predict(model_rf, newdata = X_train1)
# Calculate mean absolute error
mae_rf <- mean(abs(y_train1 - y_pred_rf))
# Calculate R-squared
r2_rf <- cor(y_pred_rf, y_train1)^2
cat("R2-score:", format(r2_rf, digits = 4), "and MAE score:", format(mae_rf, digits = 4))

# Create the summary DataFrame
summary_df <- data.frame(
  Model = c("linear regression", "Gradient Boosting Regressor", "random forest"),  # Update with model names
  MAE_Train = c(metrics$MAE_score, mae_gbr, mae_rf),  # Update with MAE scores
  R2_Score = c(metrics$R2_score, r2_gbr, r2_rf) # Update with R2 scores
)
# Sort the DataFrame by R2-Score in descending order
regression_results <- summary_df[order(summary_df$R2_Score, decreasing = TRUE), ]
### for regression model, the performance of random forest is the best, 
# however, it takes longest time to train the model.
# linear regression spend the least time to train the model.

## classification model
# Calculate IQR and median using quantile
q25 <- quantile(df1$monthly_rent_rm/ df1$rooms, probs = 0.25)
q75 <- quantile(df1$monthly_rent_rm/ df1$rooms, probs = 0.75)
median <- quantile(df1$monthly_rent_rm/ df1$rooms, probs = 0.5)

# Print the results
print(q25)
print(q75)
print(median)
df1$renovation_level <- ifelse(df1$monthly_rent_rm / df1$rooms < 400, "standard",
                              ifelse(df1$monthly_rent_rm / df1$rooms >= 400 & 
                                       df1$monthly_rent_rm / df1$rooms <= 800,
                                     "exquisite","luxury"))
df1$renovation_level <- round(as.numeric(factor(df1$renovation_level, levels = c("luxury", "exquisite", "standard"), labels = c(3, 2, 1))),0)

df2 <- df1
#df2 <- subset(df, select = -renovation_level)
result2 <- extractInputOutput(data = df2, output_column_name = "renovation_level")
X2 <- result2$input_data
y2 <- result2$output_data
X_clean2 <- standardizerData(X2)

################################################################################
# Train-test split
# Create the train and test indices
train_indices2 <- createDataPartition(y2, p = 0.8, list = FALSE)

# Split the data into training and testing sets
X_train2 <- X_clean2[train_indices2, ]
y_train2 <- y2[train_indices2]
X_test2 <- X_clean2[-train_indices2, ]
y_test2 <- y2[-train_indices2]
round(nrow(X_test2) / nrow(X_clean2), 2)
################################################################################
# classification model prediction
table(y_test2)
colnames(df2)
corr <- c("monthly_rent_rm","completion_year", "rooms", "parking", "bathroom", "size_sqft", 
          "fac_count", "add_fac_count","region", "nearby_ktm_lrt","property_type","renovation_level")
heat <- df2[,corr]
heat
options(repr.plot.width=8, repr.plot.height=6)

qplot(x=Var1, y=Var2, data=melt(cor(heat, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(low = "green", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  coord_fixed()+
  ggtitle("Figure 7 Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.4))
################################################################################
# Create the decision tree model
X_train2 <- X_train2[, !(names(X_train2) %in% c("monthly_rent_rm"))]
X_test2 <- X_test2[, !(names(X_test2) %in% c("monthly_rent_rm"))]
ctrl=rpart.control(cp=0.001)
model_dt <- rpart(y_train2 ~ ., data = X_train2, method = 'class', control=ctrl)
rpart.plot(model_dt, extra = 106)
# Predict using the tuned decision tree model
y_pred_dt <- predict(model_dt, X_test2, type = 'class')
# Get the predicted labels based on the highest column index
pre_mat_dt <- table(y_test2, y_pred_dt)
# print accuracy based on the confusion matrix prediction
acc_test_dt <- sum(diag(pre_mat_dt)) / sum(pre_mat_dt)
print(paste('Accuracy for Decision Tree test:', round(acc_test_dt, 4)))

## Naive Bayes using in-built package e1071
NBclassfier <- naiveBayes(y_train2 ~ ., data = X_train2)
y_pred_NB <- predict(NBclassfier, newdata = X_test2)  
pre_mat_NB <- table(y_test2, y_pred_NB)
acc_test_NB <- sum(diag(pre_mat_NB)) / sum(pre_mat_NB)
print(paste('Accuracy for Naive Bayes test:', round(acc_test_NB, 4)))

## Build random forest for classification 
rf_classifer <- randomForest(y_train2 ~ ., data = X_train2, importance =TRUE,ntree=500,nodesize=7, na.action=na.roughfix)
# Predict
y_pred_rf2 <- round(predict(rf_classifer, newdata = X_test2))
pre_mat_rf2 <- table(y_test2, y_pred_rf2)
acc_test_rf <- sum(diag(pre_mat_rf2)) / sum(pre_mat_rf2)
print(paste('Accuracy for random forest test:', round(acc_test_rf, 4)))
#——————————————————————————————————————————————————————————————————————————————#
classification_results <- list(pre_mat_dt, pre_mat_NB, pre_mat_rf2)
# Calculate metrics
classification_results <- classification_results_metrics(classification_results)
###############################################################################

regression_results
classification_results







