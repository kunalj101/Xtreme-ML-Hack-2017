### Contest: Xtreme ML Hack on Analytics Vidhya
### Author: Rohan Rao
### Date: 2017-04-24

## loading libraries
library(data.table)
library(Metrics)

## loading data
train_contacts <- fread("Contacts_Pre_2017.csv")
test_contacts <- fread("Contacts2017.csv")

## feature engineering
setnames(train_contacts, "START.DATE", "Date")
train_contacts <- train_contacts[, .(Contacts = sum(Contacts)), .(Date, CONTACT.TYPE)]

# throwing away data prior to 2015
train_contacts <- train_contacts[Date >= "2015-01-01"]

# replacing easter days (outliers) with previous and next week values in train data
train_mar_24 <- train_contacts[Date %in% c("2016-03-17", "2016-03-31"), .(Contacts = mean(Contacts)), .(CONTACT.TYPE)]
train_mar_25 <- train_contacts[Date %in% c("2016-03-18", "2016-04-10"), .(Contacts = mean(Contacts)), .(CONTACT.TYPE)]
train_mar_28 <- train_contacts[Date %in% c("2016-03-21", "2016-04-13"), .(Contacts = mean(Contacts)), .(CONTACT.TYPE)]

train_mar_24[, Date := "2016-03-24"]
train_mar_25[, Date := "2016-03-25"]
train_mar_28[, Date := "2016-03-28"]

train_contacts <- rbind(train_contacts[!Date %in% c("2016-03-24", "2016-03-25", "2016-03-28")], train_mar_24, train_mar_25, train_mar_28)

# creating cross matrix with all combinations to include 0s
panel_contacts <- CJ(c(unique(train_contacts$Date), unique(test_contacts$Date)),
                     unique(train_contacts$CONTACT.TYPE))

setnames(panel_contacts, c("Date", "CONTACT.TYPE"))

X_panel <- merge(panel_contacts, train_contacts, all.x = T, by = c("Date", "CONTACT.TYPE"))
X_panel[is.na(X_panel)] <- 0

library(lubridate)

# date features
X_panel[, ":="(Year = year(as.Date(Date)),
               Month = month(as.Date(Date)),
               Day = as.numeric(format(as.Date(Date), "%d")),
               Weekyear = as.numeric(format(as.Date(Date), "%W")),
               Weekday = wday(as.Date(Date)))]

# lag variable: previous year, same week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "CONTACT.TYPE", "Contacts"), with = F]
X_prev[, Year := Year + 1]
setnames(X_prev, "Contacts", "prev_year_1_type")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "CONTACT.TYPE"))

# lag variable: previous year, previous week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "CONTACT.TYPE", "Contacts"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear + 1]
setnames(X_prev, "Contacts", "prev_year_1_prev_week_1_type")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "CONTACT.TYPE"))

# lag variable: previous year, next week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "CONTACT.TYPE", "Contacts"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear - 1]
setnames(X_prev, "Contacts", "prev_year_1_next_week_1_type")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "CONTACT.TYPE"))

# lag variable: previous year, second previous week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "CONTACT.TYPE", "Contacts"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear + 2]
setnames(X_prev, "Contacts", "prev_year_1_prev_week_2_type")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "CONTACT.TYPE"))

# lag variable: previous year, second next week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "CONTACT.TYPE", "Contacts"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear - 2]
setnames(X_prev, "Contacts", "prev_year_1_next_week_2_type")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "CONTACT.TYPE"))

# imputing missing values with previous year
X_panel$prev_year_1_prev_week_1_type[is.na(X_panel$prev_year_1_prev_week_1_type)] <- X_panel$prev_year_1_type[is.na(X_panel$prev_year_1_prev_week_1_type)]
X_panel$prev_year_1_prev_week_2_type[is.na(X_panel$prev_year_1_prev_week_2_type)] <- X_panel$prev_year_1_type[is.na(X_panel$prev_year_1_prev_week_2_type)]

# prediction is weighted average of historic averages
X_panel[, pred := 0.50 * prev_year_1_type
        + 0.20 * prev_year_1_prev_week_1_type
        + 0.20 * prev_year_1_next_week_1_type
        + 0.05 * prev_year_1_prev_week_2_type
        + 0.05 * prev_year_1_next_week_2_type]

# adjustment for internal management
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Internal Management" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 350

# adjustment for other types based on 2016 / 2015 trend ratios
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Call - Input" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 0.95 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Call - Input" & X_panel$Weekday %in% c(2,3,4,5,6)]
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Fax - Input" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 0.75 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Fax - Input" & X_panel$Weekday %in% c(2,3,4,5,6)]
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Fax Acknowledgement - Input" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 0.75 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Fax Acknowledgement - Input" & X_panel$Weekday %in% c(2,3,4,5,6)]
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Mail - Input" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 0.6 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Mail - Input" & X_panel$Weekday %in% c(2,3,4,5,6)]
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Mail - Recieved" & X_panel$Weekday %in% c(1,2,3,4,5,6,7)] <- 1.05 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Mail - Recieved" & X_panel$Weekday %in% c(1,2,3,4,5,6,7)]
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Visit" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 1.2 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Visit" & X_panel$Weekday %in% c(2,3,4,5,6)]
X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Web - Input" & X_panel$Weekday %in% c(2,3,4,5,6)] <- 0.95 * X_panel$pred[X_panel$Year == 2017 & X_panel$CONTACT.TYPE == "Web - Input" & X_panel$Weekday %in% c(2,3,4,5,6)]

# validation scores
val_actual <- X_panel$Contacts[X_panel$Date >= "2016-01-21" & X_panel$Date <= "2016-03-15"]
val_pred <- X_panel$pred[X_panel$Date >= "2016-01-21" & X_panel$Date <= "2016-03-15"]

print(rmse(val_actual, val_pred))
# Val RMSE: 124.10

## submission
X_submit <- merge(X_panel[Year == 2017, c("Date", "CONTACT.TYPE", "pred"), with = F], test_contacts[, c("Date", "CONTACT.TYPE", "ID"), with = F], by = c("Date", "CONTACT.TYPE"))
setnames(X_submit, "pred", "Contacts")

fwrite(X_submit[, c("ID", "Contacts"), with = F], "Contacts1.csv")
