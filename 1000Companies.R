library(faraway)
library(readr)
library(mlbench)
library(caret)
library(corrplot)

rack <- read_csv("1000_Companies.csv")
spec(rack)

rack <- rack[,-4]
head(rack, n = 20)

validationIndex <- createDataPartition(rack$Profit, p = .8, list = FALSE)
validation <- rack[-validationIndex,]
rax <- rack[validationIndex,]

dim(rax)
sapply(rax, class)

#Histograms of Attributes
par(mfrow = c(1,4))
hist(rax$`R&D Spend`, main = "R&D Spend")
hist(rax$Administration, main = "Administration")
hist(rax$`Marketing Spend`, main = "Marketing Spend")
hist(rax$Profit, main = "Profit")

#Density Plots of Attributes
par(mfrow = c(1,4))
plot(density(rax$`R&D Spend`), main = "R&D Spend")
plot(density(rax$Administration), main = "Administration")
plot(density(rax$`Marketing Spend`), main = "Marketing Spend")
plot(density(rax$Profit), main = "Profit")

#Boxplots of Attributes
par(mfrow = c(1,4))
boxplot(rax$`R&D Spend`, main = "R&D Spend")
boxplot(rax$Administration, main = "Administration")
boxplot(rax$`Marketing Spend`, main = "Marketing Spend")
boxplot(rax$Profit, main = "Profit")

#Scatter Plot Matrix
pairs(rax[,1:4])

#Jittered Scatter Plot Matrix
raxjitter <- sapply(rax[,1:4], jitter)
pairs(raxjitter, names(rax[,1:4]), col = rax$Profit)

#Correlation Plot
correlations <- cor(rax[,1:4])
corrplot(correlations, method = "circle")

#Run the algorithms using 10-fold cross-validation#
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"

#Linear Model#
set.seed(7)
fit.lm <- train(Profit ~ ., data = rax, method = "lm", metric = metric, preProc = c("BoxCox"), trControl = trainControl)

#Generalized Linear Model#
set.seed(7)
fit.glm <- train(Profit ~ ., data = rax, method = "glm", metric = metric, preProc = c("BoxCox"), trControl = trainControl)

#Cubist#
set.seed(7)
fit.cubist <- train(Profit ~ ., data = rax, method = "cubist", metric = metric, preProc = c("BoxCox"), trControl = trainControl)

#Partial Least Squares#
set.seed(7)
fit.pls <- train(Profit ~ ., data = rax, method = "pls", metric = metric, preProc = c("BoxCox"), trControl = trainControl)

#Random Forest#
set.seed(7)
fit.rf <- train(Profit ~ ., data = rax, method = "rf", metric = metric, preProc = c("BoxCox"), trControl = trainControl)

outcome <- resamples(list(LM = fit.lm, GLM = fit.glm, CUBIST = fit.cubist, PLS = fit.pls, RF = fit.rf))
summary(outcome)
dotplot(outcome)

#Random Forest Wins

#Test with Random Forest#
library(randomForest)

onek <- randomForest(Profit ~ `R&D Spend` + `Administration` + `Marketing Spend`, data = rack)
summary(onek)

prediction <- predict(onek, rack, type = "response")
model_output <- cbind(rack, prediction)

model_output$log_prediction <- log(model_output$prediction)
model_output$log_Profit <- log(model_output$Profit)


#Model Output Graphs
par(mfrow = c(1,2))
plot(model_output$log_prediction)
plot(model_output$log_Profit)

rmse <- sqrt(mean((model_output$log_prediction - model_output$log_Profit)^2))
r2 <- cor(model_output$log_Profit, model_output3$log_prediction)^2
print(rmse) 
print(r2) 

#Random Forest Fails

#Try Cubist
library(Cubist)

par(mfrow = c(1,1))
print(fit.cubist)
plot(fit.cubist)

#Tune the Cubist algorithm#
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric <- "RMSE"
set.seed(7)
grid <- expand.grid(committees = seq(15, 25, by = 1), neighbors = c(3, 5, 7))
tune.cubist <- train(Id ~ ., data = liquor, method = "cubist", metric = metric, preProc = c("BoxCox"), tuneGrid = grid, trControl = trainControl)
print(tune.cubist)
plot(tune.cubist)

#Prepare the data transform using training data
set.seed(7)
tX <- sample(1:nrow(rax), floor(.8*nrow(rax)))
p <- c("R&D Spend", "Administration", "Marketing Spend")
tXp <- rax[tX, p]
tXr <- rax$Profit[tX]
fM <- cubist(x = tXp, y = tXr, commitees = 25, neighbors = 5)
summary(fM)
predictions <- predict(fM, tXp)

#Compute the RMSE & R^2#
rmse <- sqrt(mean((predictions - tXr)^2))
r2 <- cor(predictions, tXr)^2
print(rmse) #1675.588
print(r2) #.9985427


