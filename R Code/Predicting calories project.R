library(car)

calories<-read.csv("C:/Users/Mohsin Asif/Box Sync/MS IS/Spring Semester/Flex 1/DAM/Final Project/Final Project Calories/calories.csv")

str(calories)

names(calories)

names(calories)=c(
  "Fast Food Restaurant",
  "Type",
  "ServingSize",
  "Calories",
  "TotalFat",
  "SaturatedFat",
  "TransFat",
  "Sodium",
  "Carbs",
  "Sugars",
  "Protein"
)

str(calories)

calories<-calories[,3:11]

str(calories)

any(is.na(calories))==TRUE

which(is.na(calories))
is.na(calories)

calories$TransFat[which(is.na(calories$TransFat))]<-mean(calories$TransFat,na.rm = TRUE)

any(is.na(calories$TransFat))==TRUE

any(is.na(calories))==TRUE

nrow(calories)

head(calories, 5)

summary(calories)

calories$Sodiumg=calories$Sodium/1000

head(calories)


pairs(calories)


model1<-lm(Calories~ServingSize+TotalFat+SaturatedFat+TransFat+Sodiumg+Carbs+Sugars+Protein, data=calories)

summary(model1)

vif(model1)

model2<-lm(Calories~TotalFat+SaturatedFat+TransFat+Sodiumg+Carbs+Sugars+Protein, data=calories)

summary(model2)

vif(model2)


model3<-lm(Calories~TotalFat+TransFat+Carbs+Protein, data=calories)

summary(model3)

vif(model3)

PRESS_res=model3$residuals/(1 - lm.influence(model3)$hat)

PRESS_res

plot(model3)

calories
calories1<-calories[-c(77,81,85),]

model5<-lm(Calories~TotalFat+TransFat+Carbs+Protein, data=calories1)

calories1<-calories1[-c(90),]

plot(model5)


