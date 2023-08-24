library(ggplot2)
library(gridExtra)
library(RColorBrewer)

rm(list = ls()) #remove all data stored previously

# reading our CSV files
corona<-read.csv("owid-covid-data.csv")
corona2<-read.csv("COVID19_line_list_data.csv")



#variables declaration
corona_china = subset(corona2, country == "China")
corona_china2 = subset(corona, location == "China")
corona_egy = subset(corona, location == "Egypt")


#Egypt plot (date & new cases)
date_egy<-corona_egy$date
n_cases_egy<-corona_egy$new_cases
date_egy<-as.factor(corona_egy$date)#convert from char to factor
plot(date_egy,n_cases_egy,xlab="Date",ylab="New Cases",main = "Number Of Cases Everyday IN Egypt",col.main="red",font.main=5)

#china plot (date & new cases)
date_china<-corona_china2$date
n_cases_china<-corona_china2$new_cases
date_china<-as.factor(corona_china2$date)#convert from char to factor
plot(date_china,n_cases_china,xlab="Date",ylab="New Cases",main = "Number Of Cases Everyday IN China",col.main="red",font.main=5)

#comparison between Egypt and china depends on new cases & date

pt1 <- ggplot(data = corona_egy) +
  geom_point(aes(date, new_cases))

pt2 <- ggplot(data = corona_china2) +
  geom_point(aes(date, new_cases))

grid.arrange(pt1, pt2, ncol = 2)


#Egypt plot (date & total deaths)
num_of_month <- c(2,3,4,5,6,7,8,9,10,11,12) 
num_death <-corona_egy[c(15,46,76,107,137,168,199,229,260,290,309),"total_deaths"]
egy_confirm<- data.frame(num_of_month,num_death)
coul <- brewer.pal(11, "Set2")
barplot(num_death,main="Total Death in Egypt", xlab="date",  
        ylab="total death", names.arg=num_of_month, 
        border="white",col=coul)



#China plot (date & total deaths)
num_of_month <- c(2,3,4,5,6,7,8,9,10,11,12) 
num_death_china <-corona_china2[c(15,46,76,107,137,168,199,229,260,290,309),"total_deaths"]
china_confirm<- data.frame(num_of_month,num_death_china)
coul1 <- brewer.pal(12, "Set2")
barplot(num_death_china,main="Total Death in China", xlab="Date",  
        ylab="Total Death", names.arg=num_of_month, 
        border="white",col=coul1)


#comparison between Egypt and china depends on total deaths & date

pt3 <- ggplot(data = corona_egy) +
  geom_point(aes(date, total_deaths))

pt4 <- ggplot(data = corona_china2) +
  geom_point(aes(date, total_deaths))

grid.arrange(pt3, pt4, ncol = 2)



#linear regression_1 predict number confirm for Egypt depending on number of month  
num_of_month <- c(2,3,4,5,6,7,8,9,10,11,12) 
num_confirm <-corona_egy[c(15,46,76,107,137,168,199,229,260,290,309),"total_cases"]
#making data frame
egy_confirm<- data.frame(num_of_month,num_confirm)
#making correlation 
cor(egy_confirm$num_of_month,egy_confirm$num_confirm)
#making linear regression 
ln1 <- lm(egy_confirm$num_confirm~egy_confirm$num_of_month,data=egy_confirm)

abline(ln1)
summary(ln1)
plot(num_of_month,num_confirm,col = "blue",main = "Month Number & Confirm Cases Regression",
     abline(lm(num_confirm~num_of_month)),cex = 1.3,pch = 16,xlab = "months",ylab = "confirm cases")

#linear regression_2 to predict number of deaths for Egypt depending on number of cases 
total_cases<-corona_egy$total_cases
total_death<-corona_egy$total_deaths
#making data frame
total<- data.frame(total_cases,total_death)
#making correlation 
cor(total$total_cases,total$total_death)
#making linear regression 
ln2<-lm(total_death~total_cases)

abline(ln2)
summary(ln2)
plot(total_death,total_cases,col = "blue",main = "Total Cases & Total Deaths Regression",
     abline(lm(total_cases~total_death)),cex = 1.3,pch = 16,xlab = "total_cases",ylab = "total_deaths")


#cleaning date values from china death column
corona_china$death_dummy <- as.integer(corona_china$death != 0)

#calculating death rate for china 
sum(corona_china$death_dummy) / nrow(corona_china)

#checking if people with higher age are more significant to death
death = subset(corona_china, death_dummy == 1)
life = subset(corona_china, death_dummy == 0)
mean(death$age, na.rm = TRUE)
mean(life$age, na.rm = TRUE)
#Checking if this is statistically significant
t.test(life$age, death$age, alternative="two.sided", conf.level = 0.99)
#people with higher age are more expected to die
# p-value~0, so this is statistically significant

#checking if people of different gender are more significant to death
men = subset(corona_china, gender == "male")
women = subset(corona_china, gender == "female")
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE) 
#Checking if this is statistically significant
t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)
# Men has higher death chance than women
# p-value = 0.002 < 0.05, so this is statistically significant

#One Way Anova

#Subsetting From corona which stores The [Owid-coivid-data] datasets The Egyptian patients
E<- subset(corona, location == "Egypt")
#Subsetting From corona which stores The [Owid-coivid-data] datasets The Chinese patients
C<- subset(corona, location == "China")
#Subsetting From corona which stores The [Owid-coivid-data] datasets The Italian patients
I<- subset(corona, location == "Italy")

#Subsetting From The Egyptian patients Those Who died and limiting it to 310 records only so it Could be compared to the Chinese & Italian Patients Who died
TDE <- E[c(1:310),c(8)]
#Subsetting From The Chinese patients Those Who died and limiting it to 310 records only so it Could be compared to the Egyptian & Italian Patients Who died
TDC <- C[c(1:310),c(8)]
#Subsetting From The Italian patients Those Who died and limiting it to 310 records only so it Could be compared to the Chinese & Egyptian Patients Who died
TDI <- I[c(1:310),c(8)]

#Combing All The Dead Patients In a Data Frame 
TotalDeathOWA <- data.frame(cbind(TDE,TDC,TDI))

#Stacking The Previously combined Data Frames Referencing(TotalDeathOWA)
TotalDeatStack <- stack(TotalDeathOWA)

#Using The aov Function To calculate the One-way Analysis Of The Variance
DEATHONEWAYANOVA <- aov(values ~ind,data=TotalDeatStack)

#Summarizing The One-way Analysis Of The Variance
summary(DEATHONEWAYANOVA)

#plotting
SB1 <- TotalDeatStack[,1]
SB2 <- TotalDeatStack[,2]
plot(SB1~SB2,data=TotalDeatStack)