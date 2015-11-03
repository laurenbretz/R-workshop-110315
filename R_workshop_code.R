#Spartan Hackers R Workshop 11-3-15

#display working directory
getwd()
#change path to reflect the location of the CSV data file on your machine
setwd("/Users/laurenbretz/Desktop")

#read in data
mydata = read.csv("R_workshop_data.csv")
#shorten variable names
names(mydata) = c("gender", "study", "class", "gpa", "act", "animal", "pizza", "sport", "worldend", "pres")
#display data
mydata

#transform "gender" variable into matrix of three dummy variables
tempmatrix = model.matrix(~gender -1, data=mydata)
#display matrix of gender dummy variables
tempmatrix

#iterates through each column in tempmatrix
for (i in 1:ncol(tempmatrix)){
	#adds column i from tempmatrix to mydata
	mydata[ncol(mydata)+1] = tempmatrix[,i]
	#adds column names to gender dummy variables in mydata
	colnames(mydata)[ncol(mydata)] = colnames(tempmatrix)[i]
}

#function that automates lines 15-26
#accepts two arguments: the variable that you want to tranform and the data frame that contains it
dummies=function(var,dataframe){
	tempmatrix <- model.matrix(~var -1, data=dataframe)
	for (i in 1:ncol(tempmatrix)){
		mydata[ncol(mydata)+1]<-tempmatrix[,i]
		colnames(mydata)[ncol(mydata)]<-colnames(tempmatrix)[i]
	}
	#returns a new dataset augmented with the desired dummy variables
	return(mydata)
}

#calls the function to transform each non-numerical variable
#the $ symbol all you to refer to a specific variable from a specific dataset
mydata = dummies(mydata$study,mydata)
mydata = dummies(mydata$class,mydata)
mydata = dummies(mydata$animal,mydata)
mydata = dummies(mydata$pizza,mydata)
mydata = dummies(mydata$sport,mydata)
mydata = dummies(mydata$worldend,mydata)
mydata = dummies(mydata$pres,mydata)

#shorten new variable names
names(mydata) = c("gender", "study", "class", "gpa", "act", "animal", "pizza", "sport", "worldend","pres","genderF","genderM","genderNB","studyArts","studyBiz","studyComm","studyEdu","studyMed","studyNatsci","studySocsci","class1","class2","class3","class4","class5","classGrad","animalCats","animalDogs","pizzaCheese","pizzaHI","pizzaMeat","pizzaPepp","pizzaVeggie","sportBaseball","sportBasketball","sportBoard","sportEsports","sportFootball","sportSoccer","worldendWarm","worldendNukes","worldendSun","worldendRapture","worldendZombie","presSanders","presTrump","presClinton","presBush")
#display data
mydata

#summarize numerical data
summary(mydata$gpa)
summary(mydata$act)
#summarize non-numerical data
summary(mydata$pres)
#summarize with dummy variables
summary(mydata$presSanders)
summary(mydata$presTrump)
summary(mydata$presClinton)
summary(mydata$presBush)

#plain histogram of ACT scores
hist(mydata$act)
#pretty histogram of ACT scores
hist(mydata$act, #tell R which variable to plot
col=c("green","blue"), #alternates as many colors as you want
main="ACT Scores of Spartan Hackers", #title of histogram
xlab="ACT") #label on x-axis

#average GPA of people who would vote for a Republican
mean(subset(mydata,pres=="Donald Trump" | pres=="Jeb Bush")$gpa)
#average GPA of people who would vote for a Democrat
mean(subset(mydata,pres=="Hillary Clinton" | pres=="Bernie Sanders")$gpa)

#create variables for the two values we want to compare
mean1 = mean(subset(mydata,pres=="Donald Trump" | pres=="Jeb Bush")$gpa)
mean2 = mean(subset(mydata,pres=="Hillary Clinton" | pres=="Bernie Sanders")$gpa)
#n is the sample size, or number of observations (rows) in mydata
n = nrow(mydata) 
#s is the sample standard deviation, or measure of the data’s spread
s = sd(mydata$gpa)
#a score for how different mean1 and mean2 are
teststat = (mean1 - mean2)/(s/sqrt(n))
#display the value of our test statistic
teststat
#The amount of error we are willing to accept (in this case, 5%)
alpha = 0.05 
#critical value for our indicated alpha and n
t_half_alpha = qt(1-alpha/2, df=n-1) #if teststat is outside of this range, mean1 & mean2 are different
c(-t_half_alpha,t_half_alpha)
