options(max.print=10000)
dataset<-read.csv("G:/Projects/DSR Project/oasis_longitudinal.csv")
dataset
View(dataset)
dataset<-na.omit(dataset)

#density plots for variables

plot(density(dataset$EDUC), main="Kernel Density of Education")
polygon(density(dataset$EDUC), col="darkblue", border="red")

plot(density(dataset$SES), main="Kernel Density of SES")
polygon(density(dataset$SES), col="darkblue", border="red")

plot(density(dataset$MMSE), main="Kernel Density of MMSE")
polygon(density(dataset$MMSE), col="darkblue", border="red")

plot(density(dataset$eTIV), main="Kernel Density of eTIV")
polygon(density(dataset$eTIV), col="darkblue", border="red")

plot(density(dataset$nWBV), main="Kernel Density of nWBV")
polygon(density(dataset$nWBV), col="darkblue", border="red")

#scatterplot

pairs(~CDR+nWBV+eTIV+MMSE,data = dataset,main="Scatterplot Matrix",
      col=dataset$Group,upper.panel=NULL)

#corrgram
install.packages("corrgram")
install.packages("dplyr")
library(corrgram)
library(dplyr)
library(rattle)
attach(dataset)
gsdd <-group_by(tbl_df(dataset)) 

corrgram(gsdd, lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlation of interesting variables")

#heat tree

install.packages("rpart")
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
reg_tree_1<-rpart(data=gsdd,  CDR~Age+MMSE+eTIV+nWBV)
heat.tree <- function(reg_tree_1, low.is.green=FALSE, ...) { # dots args passed to prp
  y <- reg_tree_1$frame$yval
  if(low.is.green)
    y <- -y
  max <- max(y)
  min <- min(y)
  cols <- rainbow(99, end=.36)[
    ifelse(y > y[1], (y-y[1]) * (99-50) / (max-y[1]) + 50,
           (y-min) * (50-1) / (y[1]-min) + 1)]
  prp(reg_tree_1, branch.col=cols, box.col=cols, ...)
}

heat.tree(reg_tree_1, type=4,cex=0.5, varlen=0, faclen=0, fallen.leaves=TRUE, main="Heat Tree")

#prediction of dementia

fitTree<-rpart(Group~MMSE+CDR+eTIV+nWBV,dataset)
rpart.plot(fitTree,type=4,extra=2,clip.right.labs=FALSE,varlen=0,faclen=0)
newdata<-data.frame(MMSE=c(5,15,23,30),
                    CDR=c(0,0.5,1,2.0),
                    eTIV=c(1200,1500,1700,2000),
                    nWBV=c(0.6,0.7,0.75,0.8))
newdata
predict(fitTree,newdata,type="class")

#male-female percentage
install.packages("plotrix")
library(plotrix)

demented_data<-subset(dataset,Group=="Demented")
View(demented_data)
count_demented<-sum(complete.cases(demented_data))
count_demented

male_demented_data<-subset(demented_data,M.F=="M")
View(male_demented_data)
count_demented_male<-sum(complete.cases(male_demented_data))
count_demented_male

female_demented_data<-subset(demented_data,M.F=="F")
View(female_demented_data)
count_demented_female<-sum(complete.cases(female_demented_data))
count_demented_female

slices<-c(count_demented_male,count_demented_female)
percent<-round(slices/sum(slices)*100)
lbls<-c("Male","Female")
lbls<-paste(lbls, percent) 
lbls<-paste(lbls,"%",sep="") 
pie3D(slices,labels=lbls,explode=0.1,main="Pie Chart of Demented Individuals ")

#count and mean of demented individuals

interval <- seq(0, 100, by = 10) 

sample<-cut(demented_data$Age,interval)

tapply(demented_data$Age, sample, sum)
count<-table(cut(demented_data$Age,interval)) 
count
average<-tapply(demented_data$Age, sample, mean)
average

bargraph<-plot(sample,main="Mean and Count of Demented Individuals",xlab="Age",ylab="Number of People",ylim=c(0,100),col="cyan")
points(x = bargraph, y = average,col="red",pch=19)
lines(x = bargraph, y = average,col="red")

#alzheimer's percentage

my.data.frame <- subset(demented_data, Age > 60 & MMSE < 27 & eTIV < 1600)
my.data.frame
count1<-sum(complete.cases(my.data.frame))
count1
count2<-sum(complete.cases(dataset))
count2
totalcount<-(count1/count2)*100
sprintf("The percentage of people suffering from Alzheimer's is %f percent.",totalcount)


----