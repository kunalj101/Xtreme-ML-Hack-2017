### Contest: Xtreme ML Hack on Analytics Vidhya
### Author: Rohan Rao
### Date: 2017-04-24

## loading libraries
library(data.table)
library(Metrics)

## loading data
train_resolutions <- fread("Resolution_Pre_2017.csv")
test_resolutions <- fread("Resolution2017.csv")

## feature engineering
train_resolutions <- train_resolutions[, .(Resolution = sum(Resolution)), .(Date, Category, Subject)]

# throwing away data prior to 2015
train_resolutions <- train_resolutions[Date >= "2015-01-01"]

# replacing easter days (outliers) with previous and next week values in train data
train_mar_24 <- train_resolutions[Date %in% c("2016-03-17", "2016-03-31"), .(Resolution = mean(Resolution)), .(Category, Subject)]
train_mar_25 <- train_resolutions[Date %in% c("2016-03-18", "2016-04-10"), .(Resolution = mean(Resolution)), .(Category, Subject)]
train_mar_28 <- train_resolutions[Date %in% c("2016-03-21", "2016-04-13"), .(Resolution = mean(Resolution)), .(Category, Subject)]

train_mar_24[, Date := "2016-03-24"]
train_mar_25[, Date := "2016-03-25"]
train_mar_28[, Date := "2016-03-28"]

train_resolutions <- rbind(train_resolutions[!Date %in% c("2016-03-24", "2016-03-25", "2016-03-28")], train_mar_24, train_mar_25, train_mar_28)

# creating cross matrix with all combinations to include 0s
panel_resolutions <- CJ(c(unique(train_resolutions$Date), unique(test_resolutions$Date)),
                        unique(train_resolutions$Category), unique(train_resolutions$Subject))

setnames(panel_resolutions, c("Date", "Category", "Subject"))

X_panel <- merge(panel_resolutions, train_resolutions, all.x = T, by.x = c("Date", "Category", "Subject"), by.y = c("Date", "Category", "Subject"))
X_panel[is.na(X_panel)] <- 0

# date features
X_panel[, ":="(Year = year(as.Date(Date)),
               Month = month(as.Date(Date)),
               Day = as.numeric(format(as.Date(Date), "%d")),
               Weekyear = as.numeric(format(as.Date(Date), "%W")),
               Weekday = wday(as.Date(Date)))]

# lag variable: previous year, same week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "Category", "Subject", "Resolution"), with = F]
X_prev[, Year := Year + 1]
setnames(X_prev, "Resolution", "prev_year_1_cat_sub")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "Category", "Subject"))

# lag variable: previous year, previous week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "Category", "Subject", "Resolution"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear + 1]
setnames(X_prev, "Resolution", "prev_year_1_prev_week_1_cat_sub")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "Category", "Subject"))

# lag variable: previous year, next week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "Category", "Subject", "Resolution"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear - 1]
setnames(X_prev, "Resolution", "prev_year_1_next_week_1_cat_sub")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "Category", "Subject"))

# lag variable: previous year, second previous week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "Category", "Subject", "Resolution"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear + 2]
setnames(X_prev, "Resolution", "prev_year_1_prev_week_2_cat_sub")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "Category", "Subject"))

# lag variable: previous year, second next week, same weekday
X_prev <- X_panel[, c("Year", "Weekyear", "Weekday", "Category", "Subject", "Resolution"), with = F]
X_prev[, Year := Year + 1]
X_prev[, Weekyear := Weekyear - 2]
setnames(X_prev, "Resolution", "prev_year_1_next_week_2_cat_sub")
X_panel <- merge(X_panel, X_prev, all.x = T, by = c("Year", "Weekyear", "Weekday", "Category", "Subject"))

# imputing missing values with previous year
X_panel$prev_year_1_prev_week_1_cat_sub[is.na(X_panel$prev_year_1_prev_week_1_cat_sub)] <- X_panel$prev_year_1_cat_sub[is.na(X_panel$prev_year_1_prev_week_1_cat_sub)]
X_panel$prev_year_1_prev_week_2_cat_sub[is.na(X_panel$prev_year_1_prev_week_2_cat_sub)] <- X_panel$prev_year_1_cat_sub[is.na(X_panel$prev_year_1_prev_week_2_cat_sub)]

# prediction is weighted average of historic averages
X_panel[, pred := 0.5 * prev_year_1_cat_sub
        + 0.20 * prev_year_1_prev_week_1_cat_sub
        + 0.20 * prev_year_1_next_week_1_cat_sub
        + 0.05 * prev_year_1_prev_week_2_cat_sub
        + 0.05 * prev_year_1_next_week_2_cat_sub]

# adjustment for duplicate documents
X_panel$pred[X_panel$Year == 2017 & X_panel$Category == "Request" & X_panel$Subject == "Duplicate Documents"] <- 1.6 * X_panel$pred[X_panel$Year == 2017 & X_panel$Category == "Request" & X_panel$Subject == "Duplicate Documents"]

# validation scores
val_actual <- X_panel$Resolution[X_panel$Date >= "2016-01-21" & X_panel$Date <= "2016-03-15"]
val_pred <- X_panel$pred[X_panel$Date >= "2016-01-21" & X_panel$Date <= "2016-03-15"]

print(rmse(val_actual, val_pred))
# Val RMSE: 21.03

## submission
X_submit <- merge(X_panel[Year == 2017, c("Date", "Category", "Subject", "pred"), with = F], test_resolutions[, c("Date", "Subject", "Category", "ID"), with = F], by = c("Date", "Subject", "Category"))
setnames(X_submit, "pred", "Resolution")

fwrite(X_submit[, c("ID", "Resolution"), with = F], "Resolution1.csv")
