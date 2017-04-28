# MODELLING of Resolution using optimal approach (by Robert Hyndman)

#This is a hirarchial time series problem
library(hts)
# Read data
pre.resol <- read.csv("Resolution_Pre_2017.csv", header = T, 
                      na.strings = c("", "NA"), stringsAsFactors = T)

#Aggregate by Date, Category and Subject
pre.resol$Date <- as.Date(pre.resol$Date, format = "%Y-%m-%d")

pre.resol1 <- pre.resol[pre.resol$Date > "2013-12-31",]

pre.resol.byDayCatSubj <- aggregate(pre.resol1$Resolution, 
                                by=list(pre.resol1$Date, pre.resol1$Category, pre.resol1$Subject),
                                FUN=sum)

colnames(pre.resol.byDayCatSubj) <- c("Date","category", "Subject" ,"Resolutions")

pre.resol.byDayCatSubj$Subject <- as.character(pre.resol.byDayCatSubj$Subject)

pre.resol.byDayCatSubj$Subject[pre.resol.byDayCatSubj$Subject=="Others" & 
                                 pre.resol.byDayCatSubj$category=="Request"] <- "ROthers"

pre.resol.byDayCatSubj$Subject[pre.resol.byDayCatSubj$Subject=="Others" & 
                                 pre.resol.byDayCatSubj$category=="Tecnical Claim"] <-  "TOthers"


pre.resol.byDayCatSubj$Date <- as.Date(pre.resol.byDayCatSubj$Date, format="%Y-%m-%d")

pre.resol.byDayCatSubj <- pre.resol.byDayCatSubj[with(pre.resol.byDayCatSubj, 
                                                      order(Date, category, Subject)),]
# drop category column and dcast as required by hts()

pre.resol.byDaySubj <- pre.resol.byDayCatSubj[,-2]

nn1 <- c("Customer Data Modification",
         "Modifications Payments / Collections",
         "Supplies",
         "Duplicate Documents",
         "Invoice Modifications",
         "Offer acceptance",
         "ROthers",
         "Facilities",
         "Infrastructures",
         "Mod. Commercial Data",
         "Data Protection and Comunic.Publi.",
         "Appointment",
         "Smart Metering",
         "Customer Care",
         "Technical management",
         "Billing cycle",
         "Business Cycle",
         "-",
         "Invoice charges",
          "Invoiced consumption",
         "Quality service",
         "Contractual conditions",
         "Damage",
         "Official Complaint",
         "Complain",
         "Payment",
         "Charter Commitments",
         "Service point trading",
         "GDPR",
          "Escape",
          "Water is missing",
          "Lack of pressure",
          "Closing Application",
          "Defective installation",
          "Paving",
          "TOthers",
          "Water quality",
          "Sewerage")
       
#Convert to factor to preserve column order
pre.resol.byDaySubj$Subject <- factor(pre.resol.byDaySubj$Subject, nn1)

pre.resol.byDaySubj.Dcast<- dcast(pre.resol.byDaySubj,Date~Subject,fun.aggregate=sum)

# Segregate nodes for each subject
nodes <- list(5, c(13,4,1,11,9))

# Convert to matrix
pre.resol.byDaySubj.DcastFin <- (data.matrix(pre.resol.byDaySubj.Dcast[,-1]))
#                                     seasonal.periods=c(6,7,60,90, 365.25))

hts_fit <- hts(pre.resol.byDaySubj.DcastFin, nodes = nodes)

fc_hts <- forecast(hts_fit, 74)

plot(fc_hts, include = 100 )

#Fit tbats() with multiple seasonality on all sub groups

allts <- aggts(hts_fit)
allf <- matrix( , nrow=74, ncol=ncol(allts))

# Users can select their preferred time-series forecasting method
# for each time series
    for(i in 1:ncol(allts))
  allf[,i] <- forecast(tbats(allts[,i],seasonal.periods=c(6,7,90, 365.25)), 
                       h=74)$mean

#Conbine using conbinef function
allf <- ts(allf, start = 2556)
y.f <- combinef(allf, hts_fit$nodes, weights = NULL, keep = "bottom", algorithms = "lu")
plot(y.f)

#Convert to DF and populate column names
outDF <- data.frame(y.f)
colnames(outDF) <- nn1

#Calculate weekend mean for all columns in the most recennt year

pre.resol.byDaySubj.Dcast$weekday <- weekdays(pre.resol.byDaySubj.Dcast$Date)

pre.resol.byDaySubj.Dcast$weekend <- ifelse(pre.resol.byDaySubj.Dcast$weekday %in% 
                                        c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'), "N", "Y" )

aba <- colMeans(pre.resol.byDaySubj.Dcast[pre.resol.byDaySubj.Dcast$weekend=="Y" &
                                            pre.resol.byDaySubj.Dcast$Date > "2015-12-31"
                                            ,-c(1,40,41)])

outDF1 <- outDF

# Iterate and update negative values with mean
for (i in 1:length(aba))
{
  outDF1[which(outDF1[,i]<0),i] <- aba[i]
  
}

# Combine with date and melt
outDF1.withDate <- data.frame(cbind(yy,outDF1))

outDF1.melted <- melt(outDF1.withDate,id="yy")

colnames(outDF1.melted) <- c("Date", "Subject", "Resolution")

#Correct Subject Names (could have been coded better by gsub)
outDF1.melted$Subject <- as.character(outDF1.melted$Subject)

outDF1.melted$Subject[outDF1.melted$Subject=="Customer.Data.Modification"] <- "Customer Data Modification"
outDF1.melted$Subject[outDF1.melted$Subject=="Modifications.Payments...Collections"] <- "Modifications Payments / Collections"

outDF1.melted$Subject[outDF1.melted$Subject=="Duplicate.Documents"] <- "Duplicate Documents"

outDF1.melted$Subject[outDF1.melted$Subject=="Invoice.Modifications"] <- "Invoice Modifications"

outDF1.melted$Subject[outDF1.melted$Subject=="Offer.acceptance"] <- "Offer acceptance"

outDF1.melted$Subject[outDF1.melted$Subject=="Mod..Commercial.Data"] <- "Mod. Commercial Data"

outDF1.melted$Subject[outDF1.melted$Subject=="Data.Protection.and.Comunic.Publi."] <- "Data Protection and Comunic.Publi."

outDF1.melted$Subject[outDF1.melted$Subject=="Smart.Metering"] <- "Smart Metering"

outDF1.melted$Subject[outDF1.melted$Subject=="Customer.Care"] <- "Customer Care"

outDF1.melted$Subject[outDF1.melted$Subject=="Technical.management"] <- "Technical management"

outDF1.melted$Subject[outDF1.melted$Subject=="Billing.cycle"] <- "Billing cycle"

outDF1.melted$Subject[outDF1.melted$Subject=="Business.Cycle"] <- "Business Cycle"

outDF1.melted$Subject[outDF1.melted$Subject=="X."] <- "-"

outDF1.melted$Subject[outDF1.melted$Subject=="Invoice.charges"] <- "Invoice charges"

outDF1.melted$Subject[outDF1.melted$Subject=="Invoiced.consumption"] <- "Invoiced consumption"

outDF1.melted$Subject[outDF1.melted$Subject=="Quality.service"] <- "Quality service"

outDF1.melted$Subject[outDF1.melted$Subject=="Contractual.conditions"] <- "Contractual conditions"

outDF1.melted$Subject[outDF1.melted$Subject=="Official.Complaint"] <- "Official Complaint"

outDF1.melted$Subject[outDF1.melted$Subject=="Charter.Commitments"] <- "Charter Commitments"

outDF1.melted$Subject[outDF1.melted$Subject=="Service.point.trading"] <- "Service point trading"

outDF1.melted$Subject[outDF1.melted$Subject=="Water.is.missing"] <- "Water is missing"

outDF1.melted$Subject[outDF1.melted$Subject=="Lack.of.pressure"] <- "Lack of pressure"

outDF1.melted$Subject[outDF1.melted$Subject=="Closing.Application"] <- "Closing Application"

outDF1.melted$Subject[outDF1.melted$Subject=="Defective.installation"] <- "Defective installation"

outDF1.melted$Subject[outDF1.melted$Subject=="Water.quality"] <- "Water quality"

#Populate category column
outDF1.melted$Category <- "Request"

outDF1.melted$Category <- ifelse(outDF1.melted$Subject %in% 
                                   c(          "Escape",
                                               "Water is missing",
                                               "Lack of pressure",
                                               "Closing Application",
                                               "Defective installation",
                                               "Paving",
                                               "TOthers",
                                               "Water quality",
                                               "Sewerage"),
                                 "Tecnical Claim" ,outDF1.melted$Category)

outDF1.melted$Category <- ifelse(outDF1.melted$Subject %in% 
                                   c(          "-"),
                                 "Non Compliance" ,outDF1.melted$Category)

outDF1.melted$Category <- ifelse(outDF1.melted$Subject %in% 
                                   c(          "Invoice charges",
                                          "Invoiced consumption",
                                               "Quality service",
                                               "Contractual conditions",
                                               "Damage",
                                               "Official Complaint",
                                               "Complain",
                                               "Payment",
                                               "Charter Commitments",
                                               "Service point trading",
                                               "GDPR"),
                                 "Commercial Claim" ,outDF1.melted$Category)

outDF1.melted$Category <- ifelse(outDF1.melted$Subject %in% 
                                   c(          "Customer Care",
                                               "Technical management",
                                               "Billing cycle",
                                               "Business Cycle"),
                                 "Consultation" ,outDF1.melted$Category)

outDF1.melted$Subject[outDF1.melted$Subject == "ROthers" |
                           outDF1.melted$Subject== "TOthers" ] <- "Others"

#Read file and match records to create output file
Res.test <- read.csv("Resolution2017_test.csv", header = T)
Res.test <- Res.test[,-4]

final.ResOut <- join(Res.test, outDF1.melted, type="left") 

final.ResOut <- final.ResOut[,-c(1,2,3)]

#Replace nulls with zero (as these indicate subject category combination where there is no data in training set)
final.ResOut$Resolution[is.na(final.ResOut$Resolution)] <- 0

#Write to Output for submission
write.csv(final.ResOut,"Resolution.csv", row.names = F)

