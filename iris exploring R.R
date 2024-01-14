data("iris")
View(iris)
ir <- iris
ir
# explore the data
head(ir)
tail(ir)
summary(ir)
# check for missing values
sum(is.na(ir))
# advanced explor
skim(ir)
# grouping the data and use skim
ir %>%
  group_by(Species) %>%
  skim()
# explore the data with basic gragh
plot(ir,col = "red")
plot(ir$Sepal.Length,ir$Sepal.Width ,col = "blue",
     xlab = "sepal_length",ylab= "sepal_width")
hist(ir$Sepal.Length , col = "green")
# set seed
set.seed(100)
#split the data
data_index <- createDataPartition(ir$Species , p = 0.8 , list = F)
training_data <- ir[data_index, ]
testing_data <- ir[-data_index, ]
# ploting the splitting data
ggplot(training_data,aes(x= Sepal.Length , y=Sepal.Width, color = Species))+geom_point()
plot(training_data , col = "blue")
ggplot(testing_data,aes(x= Sepal.Length , y=Sepal.Width, color = Species))+geom_point()
plot(testing_data)
# building the modol
model <- train(Species ~. ,data = training_data ,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale","center"),
              trConrol = trainControl(method = "none") ,
              tuneGrid = data.frame(degree = 1 ,scale =1, C =1))

# building crossV modol
cvmodel <- train(Species ~. ,data = training_data ,
                 method = "svmPoly",
                 na.action = na.omit,
                 preProcess = c("scale","center"),
                 trConrol = trainControl(method = "cv", number = 10) ,
                 tuneGrid = data.frame(degree = 1 ,scale =1, C =1))

# apply a modol for prediction 
model_training <- predict(model , training_data) # apply the model on TR data
model_testing <- predict(model , testing_data) # apply the model on TS data
crossv_model <- predict(cvmodel , training_data) # apply cross validation model on TR 
# model performance 
pr_TRmodel <- confusionMatrix(model_training , training_data$Species)
pr_TSmodel <- confusionMatrix(model_testing , testing_data$Species)
pr_CVmodel <- confusionMatrix(crossv_model , training_data$Species)

print(pr_TRmodel)
print(pr_TSmodel)
print(pr_CVmodel)
# importance of the varibels
imp <- varImp(model)
plot(imp)
