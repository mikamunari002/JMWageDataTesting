##Running Hypthesis Test to see if gender, age, or gender*age interaction are
##signicant predictors of wage 
library(psych)
wageData <- as.data.frame(Rdataset)
options(scipen= 999)
View(wageData)
#make "Years of Employement "YoP"
wageData$YoP<- wageData$'Years of Employment'
##Create factor variable Gender into a Dummy Variable
wageData$genderDummy <- ifelse(wageData$Gender == 'Male', 1, 0)

##Summary Stats for dataset grouped by gender
describeBy(x= wageData, group = wageData$Gender)

##Summary Stats for dataset grouped by age
describeBy(x= wageData, group = wageData$Age)

##Summary Stats for dataset grouped by Years of employment
describeBy(x= wageData, group = wageData$YoP)

##Looking at mean wage of gender and age combos **This is probably better than describeBy
aggregate(Wage~Age+Gender,  wageData,  mean)

##Does a low correlation coeffiecient tell us that something else effects wage?
cor(x= wageData$Age, y = wageData$Wage)
cor(x= wageData$Age, y = wageData$YoP)



wageSimp.lm<- lm(wageData$Wage~wageData$Age, data= wageData)
wageSimp2.lm<- lm(wageData$Wage~wageData$YoP, data= wageData)
plot(wageData$Wage~wageData$Age, data= wageData)
abline(wageSimp.lm, col = "blue")
plot(wageData$Wage~wageData$YoP, data= wageData)
abline(wageSimp2.lm, col = "red")



wageMultVar.lm<- lm(Wage~ Age+YoP+ genderDummy , data= wageData)
summary(wageMultVar.lm)


##Need to still see if linear model is the best way to interpret
##tgis dataset


