#2
table(STB_survey$Gender)

#3
prop.table(table(STB_survey$Gender))

#4
table(STB_survey$Gender,STB_survey$Grade)

#5
barplot(table(STB_survey$Nationality),main = "Nationality Resarch",col = c(1,2,3),xlab = "nationality")

#6
barplot(table(STB_survey$`residential area`),horiz = TRUE,col = c(4,5,6))

#7
barplot(table(STB_survey$Gender,STB_survey$Grade),col = c(2,5))

#8
pie(table(STB_survey$Grade),col = c(1,2,3))

#9
hist(table(STB_survey$Age),main = "AGE Frequency",xlab = "age",col = rainbow(10))

#10

boxplot(STB_survey$Age~STB_survey$Grade,col = rainbow(3))

a<-data.frame(STB_survey$Age,STB_survey$Grade)
a
b<-data(a$STB_survey.Grade=='2','AGE')

#11
plot(x=STB_survey$Grade,y=STB_survey$Age)
