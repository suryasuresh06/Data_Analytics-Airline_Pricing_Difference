# Analysis of Airline Ticket Pricing
# NAME: Surya Suresh
# EMAIL: suryaallstars06@gmail.com
# COLLEGE / COMPANY:IIT Madras


# Attaching libraries
library(ggplot2)
library(psych)
library(lattice)
library(car)

# Reading and Attaching dataset
airlines <-read.csv(paste("C://Users//surya suresh//Downloads//SixAirlines (1).csv"))
attach(airlines)

#Initial Understanding
str(airlines)
describe(airlines)

#Understanding the distribution between domestic and international flights
table(airlines$INTERNATIONAL)

#Separating the international data points
international<-airlines[which(airlines$INTERNATIONAL==1),]
domestic<-airlines[which(airlines$INTERNATIONAL==0),]

#Histogram of Important parameters
histogram(FLIGHT_DURATION,international, ylab="count", col="blue",breaks=20,type=c("count"))
histogram(MONTH,international,ylab="count",type=c("count"))
histogram(N,international,xlab="Number of Seats",ylab="count",col="green",type=c("count"))

#Understanding the Pricing Dynamics
par(mfrow=c(1,2))
hist(international$PRICE_ECONOMY,main = "Histogram of EClass Price",ylab = "count",xlab = "Economy Class Price",col = "red")
hist(international$PRICE_PREMIUM,main= "Histogram of PClass Price", ylab = "count",xlab = "Premium Class Price",col = "blue")

#New column to store the price difference
international$diff <- international$PRICE_PREMIUM - international$PRICE_ECONOMY

#Plotting the price difference vs the Flight Duration
g<-ggplot(international,aes(x=FLIGHT_DURATION,y=diff)) + geom_point()+geom_smooth(method = "lm")
plot(g)
g+xlim(c(0,15)) + ylim(c(0,2000))

# Understanding the relationship between price difference and quality
international.df<-as.data.frame(international)
mean.diff<-aggregate(diff~QUALITY,data=international.df,mean)
mean.diff

#Tabulating realtionship between quality and price difference for each aircraft.
aggregate(cbind("Quality"=international$QUALITY,"Price Difference"=international$diff),by=list("Airline"=international$AIRLINE),mean)

#Finding the impact of the percentage of premium seats
international$percent_Premium <- (international$SEATS_PREMIUM/international$N)*100
aggregate(international$percent_Premium,by=list("Airplane"=international$AIRLINE),mean)

#Plotting the above
l<-ggplot(international,aes(x=percent_Premium,y=diff)) + geom_point()+geom_smooth(method = "lm")
l+xlim(c(0,35)) + ylim(c(0,2000))

#Finding the difference in pitch and width between economy and premium class
international$Wdiff <- international$WIDTH_PREMIUM - international$WIDTH_ECONOMY
international$Pdiff <- international$PITCH_PREMIUM - international$PITCH_ECONOMY

#Tabulating the impact of width and pitch difference on prices
aggregate(international$diff,by=list("Width Difference"=international$Wdiff),mean)
aggregate(international$diff,by=list("Pitch Difference"=international$Pdiff),mean)

#Consolidated Table to identify skew in the data
table(international$AIRLINE)
aggregate(cbind("Width Difference"=international$Wdiff,"Pitch Difference"=international$Pdiff,"Price Difference"=international$diff),by=list("Airline"=international$AIRLINE),mean)

#Factor Column for Airlines
international$airline_factor<-factor(international$AIRLINE,levels = c("AirFrance","British","Delta","Jet","Singapore","Virgin"),labels=c(1,2,3,4,5,6))

#Isolating Jet Airways
international_NoJet <- international[which(international$airline_factor!=4),]

#Studying the relationship of Width,Pitch and Quality without Jet
aggregate(international_NoJet$diff,by=list("Width Difference"=international_NoJet$Wdiff),mean)
aggregate(international_NoJet$diff,by=list("Pitch Difference"=international_NoJet$Pdiff),mean)
aggregate(international_NoJet$diff,by=list("Quality"=international_NoJet$QUALITY),mean)

#Linear model with 5 parameters including Jet
model<-lm(diff~FLIGHT_DURATION+Wdiff+Pdiff+QUALITY+percent_Premium,data=international)
summary(model)

#Find out which parameters are perfectly correlated
cor(international$QUALITY,international$Pdiff)

#Corrected Linear Model including Jet
model1<-lm(diff~Wdiff+FLIGHT_DURATION+percent_Premium,data=international)
summary(model1)


international_NoJet$percent_Premium<-(international_NoJet$SEATS_PREMIUM/international_NoJet$N)*100

#Linear Model with 5 parameters without Jet
model_nojet<-lm(diff~FLIGHT_DURATION+Wdiff+Pdiff+QUALITY+percent_Premium,data=international_NoJet)
summary(model_nojet)

#Creating extra columns in domestic airlines data set
domestic<-airlines[which(airlines$INTERNATIONAL==0),]
domestic$diff<-domestic$PRICE_PREMIUM-domestic$PRICE_ECONOMY
domestic$Wdiff<-domestic$WIDTH_PREMIUM-domestic$WIDTH_ECONOMY
domestic$Pdiff<-domestic$PITCH_PREMIUM-domestic$PITCH_ECONOMY
domestic$percent_Premium <- (domestic$SEATS_PREMIUM/domestic$N)*100

#Histogram to understand pricing dynamics
histogram(domestic$diff,xlab="Price Difference",type="count")

#Tabulating important parameters
aggregate(cbind("Width Difference"=domestic$Wdiff,"Pitch Difference"=domestic$Pdiff,"Price Difference"=domestic$diff),by=list("Airline"=domestic$AIRLINE),mean)

#Linear model for domestic airlines
model_domestic<-lm(diff~FLIGHT_DURATION+Pdiff+percent_Premium,data=domestic)
summary(model_domestic)


