# FOR PREDICTING CONTACTS USING TOP DOWN APPROACH

# Using pre contacts and multiple seasonality

library(forecast)

#Read data
preCont <- read.csv("Contacts_Pre_2017.csv", header = T, sep = ",", 
                    na.strings = c("", "NA"), stringsAsFactors = T)

# set Date format
preCont$Date <- as.Date(preCont$START.DATE, format="%Y-%m-%d")

#Aggreagte by day and set column names
precont_byDay <- aggregate(preCont$Contacts, by=list(preCont$Date), FUN=sum)

colnames(precont_byDay) <- c("Date", "Contacts")

#Aggreagte by day and type
precont_byDayType <- aggregate(preCont$Contacts, by=list(preCont$Date, preCont$CONTACT.TYPE),
                               FUN=sum)
colnames(precont_byDayType) <- c("Date", "Type","Contacts")
precont_byDayType$Date <- as.Date(precont_byDayType$Date, format="%Y-%m-%d")

#Create a multiple seasonality data
y <- msts(precont_byDay$Contacts, seasonal.periods=c(6,7,60,90, 365.25))

#Using TBATS (this model used)
fit_tbats <- tbats(y,seasonal.periods=c(6,7,90, 365.25))
fc2 <- forecast(fit_tbats, 74)

#Calculate group contributions
precont_byDayType.2016 <- precont_byDayType[precont_byDayType$Date > "2015-12-31",]

#aggregate(precont_byDayType.2016$Contacts, by=list(precont_byDayType.2016$Type, 
#                                              precont_byDayType.2016$weekend),
#          FUN=mean)

#Generate output dataset

library(lubridate)
library(plyr)
library(reshape2)

yy <- seq(ymd('2017-01-01'),ymd('2017-03-15'), by = '1 day')
xx <- as.numeric(fc2$mean)
zz <- data.frame(as.Date(yy, format="%Y-%m-%d"),xx)

colnames(zz) <- c("Date", "Prediction")

zz$weekend <- ifelse(weekdays(zz$Date) %in% 
                       c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'),
                     "N", "Y" )

#Apply the % distribution for weekday and weekend separtly for all Types
zz$Call.Input <- ifelse(zz$weekend == "Y", round(zz$Prediction*.365), round(zz$Prediction*.64105))

zz$Fax.Input <- ifelse(zz$weekend == "Y", round(zz$Prediction*.0055), round(zz$Prediction*0.0008))

zz$FaxAcknowledgement.Input <- ifelse(zz$weekend == "Y", round(zz$Prediction*.004), round(zz$Prediction*0.00055))

zz$InstallationReport.Input <- ifelse(zz$weekend == "Y", round(zz$Prediction*.0025), round(zz$Prediction*0.00025))

zz$Internal.Management <- ifelse(zz$weekend == "Y", round(zz$Prediction*.014), round(zz$Prediction*0.0725))

zz$Mail.Input <- ifelse(zz$weekend == "Y", round(zz$Prediction*.117), round(zz$Prediction*0.0006))

zz$Mail.Recieved <- ifelse(zz$weekend == "Y", 0, round(zz$Prediction*0.042))

zz$Tweet.Input <- ifelse(zz$weekend == "Y", 0, round(zz$Prediction*0.00025))

zz$Visit <- ifelse(zz$weekend == "Y", 0, round(zz$Prediction*0.13))

zz$Web.Input <- ifelse(zz$weekend == "Y", round(zz$Prediction*0.492),round(zz$Prediction*0.112))

zz1 <- zz[,-c(2,3)]

zz1 <- melt(zz1,id="Date")
colnames(zz1) <- c("Date", "CONTACT.TYPE", "Contacts")

zz1$CONTACT.TYPE <- as.character(zz1$CONTACT.TYPE)

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Call.Input")] <- "Call - Input"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Fax.Input")] <- "Fax - Input"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="FaxAcknowledgement.Input")] <- "Fax Acknowledgement - Input"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="InstallationReport.Input")] <- "Installation Report - Input"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Internal.Management")] <- "Internal Management"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Mail.Input")] <- "Mail - Input"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Mail.Recieved")] <- "Mail - Recieved"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Tweet.Input")] <- "Tweet - Input"

zz1$CONTACT.TYPE[which(zz1$CONTACT.TYPE=="Web.Input")] <- "Web - Input"

#Change date format according to contact,test file
a11 <- strptime(as.character(zz1$Date), "%Y-%m-%d")
zz1$Date <- format(a11, "%d-%m-%y")

contact.test <- read.csv("Contacts2017_test.csv", header = T)

final.ContactOut <- join(contact.test, zz1, type="inner") 

final.ContactOut <- final.ContactOut[,-c(1,2)]

#Write to Output for submission
write.csv(final.ContactOut,"Contacts.csv", row.names = F)

