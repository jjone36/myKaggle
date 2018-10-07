library(tidyverse)
library(data.table)
library(jsonlite)
library(ggthemes)
library(magrittr)
library(lubridate)
library(rebus)
library(gridExtra)
library(caret)
library(glmnet)
library(gbm)
library(xgboost)

# This is the full code for the competition 'Google Analytics Customer Revenue Prediction' held in Kaggle.
# This competition is to predict revenue per customer of a Google Merchandise Store 
# You can find the full dataset here: https://www.kaggle.com/c/ga-customer-revenue-prediction

## 1. Reading the dataset ####
train = read_csv('train.csv') 
test = read_csv('test.csv') 

full = rbind(train, test)
str(full)

## 2. Preprocessing columns one by one ####
# date
full$date = ymd(full$date)

# visitStartTime
full$visitStartTime = as.POSIXct(full$visitStartTime, tz = 'UTC', origin = '1970-01-01')

# browser
table(full$browser)
full$browser[str_length(full$browser) > 25] = 0

# device 
device = paste("[", paste(full$device, collapse = ","), "]") %>% fromJSON(flatten = T)
str(device)
names(device)

device = device[, names(device) %in% c('browser', 'operatingSystem', 'isMobile', 'deviceCategory')]

full$device = NULL
full = cbind(full, device)

# geoNetwork
geoNetwork = paste("[", paste(full$geoNetwork, collapse = ", "), "]") %>% fromJSON(flatten = T)
names(geoNetwork)

geoNetwork = geoNetwork[!names(geoNetwork) %in% c('cityId', 'latitude', 'longitude', 'networkLocation')]

full$geoNetwork = NULL
full = cbind(full, geoNetwork)

# socialEngagementType 
table(full$socialEngagementType)  # also dropping this column due to lack of information
full$socialEngagementType = NULL

# totals 
totals = paste("[", paste(full$totals, collapse = ", "), "]") %>% fromJSON(flatten = T)

table(totals$visits)  
totals$visits = NULL

table(totals$hits)
summary(totals$hits)
totals$hits = as.numeric(totals$hits)

table(totals$pageviews)
totals$pageviews = as.numeric(totals$pageviews)
totals$pageviews[is.na(totals$pageviews)] = 0
summary(totals$pageviews)

table(totals$bounces)
which(is.na(totals$bounces))
totals$bounces = as.numeric(totals$bounces)
totals$bounces[is.na(totals$bounces)] = -1

table(totals$newVisits)
totals$newVisits = as.numeric(totals$newVisits)
totals$newVisits[is.na(totals$newVisits)] = -1

range(totals$transactionRevenue, na.rm = T)   # The target variable
totals$transactionRevenue = as.numeric(totals$transactionRevenue)

full = cbind(full, totals)
full$totals = NULL

# trafficSource 
str(full$trafficSource)
trafficSource = paste("[", paste(full$trafficSource, collapse = ", "), "]") %>% fromJSON()
str(trafficSource)

df = trafficSource$adwordsClickInfo
str(df)
df$targetingCriteria = NULL

table(df$criteriaParameters) 
df$criteriaParameters = NULL

table(df$page)
df$page[is.na(df$page)] = -1

table(df$slot)
sum(is.na(df$slot)) 
df$slot[is.na(df$slot)] = -1

table(df$gclId)
sum(is.na(df$gclId))
df$gclId[is.na(df$gclId)] = -1

table(df$adNetworkType)
sum(is.na(df$adNetworkType))
df$adNetworkType[is.na(df$adNetworkType)] = -1

table(df$isVideoAd)
df$isVideoAd = ifelse(is.na(df$isVideoAd), -1, 1)

trafficSource = cbind(trafficSource, df)
trafficSource$adwordsClickInfo = NULL

table(trafficSource$isTrueDirect)
trafficSource$isTrueDirect = ifelse(is.na(trafficSource$isTrueDirect), -1, 1)

sum(is.na(trafficSource$referralPath))
trafficSource$referralPath = ifelse(is.na(trafficSource$referralPath), -1, 1)
table(trafficSource$referralPath)

table(trafficSource$adContent)
sum(is.na(trafficSource$adContent))
trafficSource$adContent[is.na(trafficSource$adContent)] = -1

table(trafficSource$campaignCode)
trafficSource$campaignCode = NULL

table(trafficSource$campaign)

table(trafficSource$source)
trafficSource$source = gsub(pattern = '.com', replacement = '', x = trafficSource$source)
trafficSource$source = gsub(pattern = 'm.', replacement = '', x = trafficSource$source)
trafficSource$source[grepl(pattern = 'google', x = trafficSource$source)] = 'google'
trafficSource$source[grepl(pattern = 'yahoo', x = trafficSource$source)] = 'yahoo'
trafficSource$source[grepl(pattern = 'youtube', x = trafficSource$source)] = 'youtube'
trafficSource$source[grepl(pattern = 'facebook', x = trafficSource$source)] = 'facebook'
trafficSource$source[grepl(pattern = 'bing', x = trafficSource$source)] = 'bing'
trafficSource$source[grepl(pattern = 'reddit', x = trafficSource$source)] = 'reddit'
trafficSource$source[grepl(pattern = 'github', x = trafficSource$source)] = 'github'
trafficSource$source[grepl(pattern = 'blackboard', x = trafficSource$source)] = 'blackboard'
trafficSource$source[grepl(pattern = 'search', x = trafficSource$source)] = 'search'
table(trafficSource$source)

table(trafficSource$medium)

table(trafficSource$keyword)
trafficSource$keyword[is.na(trafficSource$keyword)] = -1

full = cbind(full, trafficSource)
full$trafficSource = NULL

part = 1:nrow(train)
train = full[part, ]
test = full[-part, ]


## 3. Exploratory data analysis ####
summary(train$visitNumber)

train %>%
  count(visitNumber) %>%
  filter(visitNumber > 100) %>%
  ggplot(aes(x = visitNumber, y = n)) + geom_col()

summary(train$transactionRevenue, na.rm = T)
sum(!is.na(train$transactionRevenue))

train$logRevenue = log(train$transactionRevenue)
train$logRevenue[is.na(train$logRevenue)] = 0
summary(train$logRevenue>0)
train = train[, c(24, 1:23)]

train %>%
  filter(logRevenue > 0) %>% 
  ggplot(aes(x = logRevenue)) + 
  geom_histogram(fill = 'steelblue') +
  theme_bw()


# visitNumber/Revenue ~ channelGrouping 
table(train$channelGrouping)

myTheme = theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                panel.background = element_rect(fill = 'white', color = 'grey50'),
                panel.grid.major = element_line(color = 'grey90'))

cg_p1 = train %>%
  group_by(channelGrouping) %>%
  filter(channelGrouping != '(Other)') %>%
  count() %>%
  ggplot(aes(x = reorder(channelGrouping, -n), y = n)) +
  geom_col(fill = 'steelblue') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = 'Channel Grouping', y = 'visit Numbers') + 
  myTheme

cg_p2 = train %>%
  group_by(channelGrouping) %>%
  filter(channelGrouping != '(Other)') %>%
  summarise(total_revenue = sum(logRevenue, na.rm = T)) %>%
  arrange(desc(total_revenue)) %>%
  ggplot(aes(x = reorder(channelGrouping, -total_revenue), y = total_revenue)) +
  geom_col(fill = 'steelblue') +
  labs(x = 'Channel Grouping', y = 'Total Revenue (log scaled)') +
  myTheme

grid.arrange(cg_p1, cg_p2, ncol = 2)

train %>%
  filter(channelGrouping != '(Other)') %>%
  ggplot(aes(x = channelGrouping, y = logRevenue, fill = channelGrouping)) +
  geom_boxplot(show.legend = F) +
  labs(x = 'Channel Grouping', y = 'Revenue (log-scaled)') +
  theme_bw()


# visitNumber/Revenue ~ deviceCategory
train %>%
  group_by(deviceCategory) %>%
  count(visitNumber) %>%
  arrange(desc(n)) %>%
  top_n(n = 15, wt = n) %>%
  ggplot(aes(x = visitNumber, y = n, fill = deviceCategory)) + 
  geom_col() +
  labs(x = NULL, y = 'visit Numbers') + 
  theme_bw() 

ggplot(train, aes(x = deviceCategory, y = logRevenue, fill = deviceCategory)) + 
  geom_boxplot(show.legend = F) +
  labs(x = 'Device Category', y = 'Revenue (log-scaled)') + 
  theme_bw()


# visitNumber/Revenue ~ continent
table(train$continent)

train %>%
  group_by(continent) %>%
  filter(continent != '(not set)') %>%
  count() %>%
  ggplot(aes(x = reorder(continent, -n), y = n)) + 
  geom_col(fill = 'steelblue') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = 'visit Numbers') + 
  theme_bw()

ggplot(train, aes(x = continent, y = logRevenue, fill = continent)) + 
  geom_boxplot(show.legend = F) +
  labs(x = 'Continent', y = 'Revenue', title = 'Revenue per Continent (log-scaled)') + 
  theme_bw()

train %>%
  filter(continent != '(not set)') %>%
  filter(!is.na(logRevenue)) %>%
  ggplot(aes(x = date, y = logRevenue, color = continent)) + 
  geom_line(show.legend = F, size = .5) +
  facet_wrap(continent~.) +
  labs(x = NULL, y = 'Revenue', title = 'Revenue changes over time') + 
  theme_bw()

train %>%
  filter(continent == 'Americas') %>%
  filter(!is.na(logRevenue)) %>%
  ggplot(aes(x = date, y = logRevenue, col = subContinent)) + 
  geom_line() + 
  geom_smooth(color = 'black', se = F, size = .7) +
  facet_wrap(.~subContinent, nrow = 2) + 
  theme_bw()


# time ~ visitNumber/revenue by each device (+ peak season)
train %>%
  group_by(date, deviceCategory) %>%
  count() %>% 
  ggplot(aes(x = date, y = n, col = deviceCategory)) + 
  geom_line(show.legend = F) +
  facet_grid(deviceCategory~ ., space = 'free_y') +
  geom_vline(xintercept = as.Date(c('2016-10-4', '2016-11-28')), 
             color = 'Orange', size = 4, alpha = .3) +
  labs(x = NULL, y = 'Visit Number') +
  theme_bw()

train %>%
  group_by(year(date)) %>%
  ggplot(aes(x = logRevenue, fill = factor(year(date)))) + 
  geom_density(alpha = .3) +
  labs(x = "Revenue", y = NULL, title = "Revenue Distribution per Year (log-scaled)") + 
  theme_bw()

train %>%
  group_by(year(date)) %>%
  summarise(median = median(transactionRevenue, na.rm = T),
            mean = mean(transactionRevenue, na.rm = T)) 

train %>%
  group_by(date, deviceCategory) %>%
  summarise(total_revenue = sum(logRevenue, na.rm = T)) %>%
  ggplot(aes(x = date, y = total_revenue, col = deviceCategory)) + 
  geom_line(show.legend = F) +
  facet_grid(deviceCategory ~., space = 'free_y') +
  geom_vline(xintercept = as.Date('2016-12-07'), 
             color = 'Orange', size = 13, alpha = .3) +
  geom_vline(xintercept = as.Date('2017-05-01'), 
             color = 'Orange', size = 6, alpha = .3) +
  labs(x = NULL, y = 'Total Revenue') +
  theme_bw()

train %>%
  filter(deviceCategory == 'desktop') %>%
  group_by(date) %>%
  summarise(total_revenue = sum(logRevenue, na.rm = T)) %>%
  arrange(desc(total_revenue)) 


# weekly by each device / continent 
weekly_p1 = train %>%
  group_by(weekday = wday(date, label = T)) %>%
  count() %>%
  ggplot(aes(x = weekday, y = n)) + 
  geom_col(fill = 'steelblue') + 
  labs(x = NULL, y = NULL, title = 'Weekly Visit Numbers') + 
  theme_bw()

weekly_p2 = train %>%
  group_by(weekday = wday(date, label = T)) %>%
  summarise(total_revenue = sum(logRevenue, na.rm = T)) %>%
  ggplot(aes(x = weekday, y = total_revenue)) + 
  geom_col(fill = 'coral1') + 
  labs(x = NULL, y = NULL, title = 'Weekly Total Revenue') +
  theme_bw()

grid.arrange(weekly_p1, weekly_p2, nrow = 2)

# browser ~ visit Number / Revenue
browser_n = train %>%
  filter(!is.na(browser)) %>%
  group_by(browser) %>%
  count() %>%
  arrange(desc(n))

browser_p1 = browser_n[1:10, ] %>%
  ggplot(aes(x = reorder(browser, n), y = n)) + 
  geom_col(fill = 'steelblue') +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = 'Visit Numbers', x = NULL, title = 'Visit Numbers per Browser') +
  theme_bw()

browser_p2 = train %>%
  group_by(browser) %>%
  summarise(total_revenue = sum(logRevenue, na.rm = T)) %>%
  right_join(browser_n) %>%
  mutate(prop_revenue = total_revenue / n) %>%
  arrange(desc(prop_revenue)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(browser, prop_revenue), y = prop_revenue)) + 
  geom_col(fill = 'coral1') + 
  coord_flip() +
  labs(y = 'Total Revenue / Visit Numbers', x = NULL, title = 'Proportional Revenue per Browser') +
  theme_bw()

grid.arrange(browser_p1, browser_p2, ncol = 2)


# Operation System ~ visit Number
os_n = train %>%
  filter(operatingSystem != '(not set)') %>%
  group_by(operatingSystem) %>%
  count() %>%
  arrange(desc(n))

os_p1 = os_n[1:10, ] %>%
  ggplot(aes(x = reorder(operatingSystem, -n), y = n)) +
  geom_col(fill = 'steelblue') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL, title = 'Visit Number per OS') + 
  myTheme

os_p2 = train %>%
  group_by(operatingSystem) %>%
  summarise(total_revenue = sum(logRevenue, na.rm = T)) %>%
  right_join(os_n) %>%
  mutate(prop_revenue = total_revenue / n) %>%
  arrange(desc(prop_revenue)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(operatingSystem, -prop_revenue), y = prop_revenue)) + 
  geom_col(fill = 'coral1') +
  labs(x = NULL, y = NULL, title = 'Proportional Revenue per OS (Total Revenue / Visit Number)') + 
  myTheme

grid.arrange(os_p1, os_p2, nrow = 2)


# Source. Medium. Keyword
train %>%   
  group_by(source) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(source, -n), y = n)) +
  geom_col(fill = 'lightseagreen') +
  labs(x = NULL, y = NULL, title = 'Visit Number per Source') + 
  myTheme

train %>%
  group_by(medium) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(medium, -n), y = n)) +
  geom_col(fill = 'lightseagreen') +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL, title = 'Visit Number per Medium') +
  theme_bw()

train %>%
  filter(!is.na(keyword) & keyword != '(not provided)') %>%
  group_by(keyword) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20)


# visitNumber ~ Revenue
ggplot(train, aes(x = visitNumber, y = logRevenue)) + 
  geom_jitter(color = 'steelblue') +
  geom_smooth(color = 'Orange') +
  scale_x_continuous(limits = c(0, 50)) +
  labs(x = 'Visit Number', y = 'Revenue (log-sclaed)') +
  theme_bw()



## 4. Feature Enginerring ####
str(full)

# splitting date & time into each column
full$year = year(full$date) %>% as.factor()
full$month = month(full$date) %>% as.factor() 
full$day = day(full$date) %>% as.factor()
full$wday = wday(full$date) %>% as.factor()
full$week = week(full$date) %>% as.factor()

full$day2 = day(full$visitStartTime) - day(full$date)
full$hour = hour(full$visitStartTime) %>% as.factor()
full$minute = minute(full$visitStartTime) %>% as.factor()

# transforming scales 
full$logvisitNumber = log(full$visitNumber)
full$loghits = log(full$hits)
full$logpageviews = log(full$pageviews)
full$isMobile = ifelse(full$isMobile == TRUE, 1, 0) %>% as.factor()

# group features
fn <- funs(mean, median, var, min, max, sum, n_distinct)

sum_by_month <- full %>%
  select(month, hits, pageviews) %>% 
  group_by(month) %>% 
  summarise_all(fn) 

sum_by_day <- full %>%
  select(day, hits, pageviews) %>% 
  group_by(day) %>% 
  summarise_all(fn) 

sum_by_country <- full %>%
  select(country, hits, pageviews) %>% 
  group_by(country) %>% 
  summarise_all(fn) 

sum_by_city <- full %>%
  select(city, hits, pageviews) %>% 
  group_by(city) %>% 
  summarise_all(fn) 

sum_by_op <- full %>%
  select(operatingSystem, hits, pageviews) %>% 
  group_by(operatingSystem) %>% 
  summarise_all(fn) 

sum_by_dc <- full %>%
  select(deviceCategory, hits, pageviews) %>% 
  group_by(deviceCategory) %>% 
  summarise_all(fn) 

sum_by_source <- full %>%
  select(source, hits, pageviews) %>% 
  group_by(source) %>% 
  summarise_all(fn) 

sum_by_medium <- full %>%
  select(medium, hits, pageviews) %>% 
  group_by(medium) %>% 
  summarise_all(fn) 

sum_by_nd <- full %>%
  select(networkDomain, hits, pageviews) %>% 
  group_by(networkDomain) %>% 
  summarise_all(fn) 

full = full %>%
  left_join(sum_by_city, by = 'city', suffix = c('', '_city')) %>%
  left_join(sum_by_country, by = 'country', suffix = c('', '_country')) %>%
  left_join(sum_by_day, by = 'day', suffix = c('', '_day')) %>%
  left_join(sum_by_dc, by = 'deviceCategory', suffix = c('', '_dc')) %>%
  left_join(sum_by_medium, by = 'medium', suffix = c('', '_medium')) %>%
  left_join(sum_by_month, by = 'month', suffix = c('', '_month')) %>%
  left_join(sum_by_nd, by = 'networkDomain', suffix = c('', '_nd')) %>%
  left_join(sum_by_op, by = 'operatingSystem', suffix = c('', '_op')) %>%
  left_join(sum_by_source, by = 'source', suffix = c('', '_source')) 

# selecting subset of variables which will be used for fitting the models 
full$logRevenue = log(full$transactionRevenue)

full_df = full[, !names(full) %in% c('date', 'fullVisitorId', 'sessionId', 'visitId', 'visitNumber', 'visitStartTime',
                                     'networkDomain', 'hits', 'pageviews', 'transactionRevenue')]

full_df = full_df %>%
  mutate_if(is.character, factor)

# splitting data into trainset, validset, and testset
part = 1:903653
tr = full_df[part, ]
tr$logRevenue[is.na(tr$logRevenue)] = 0

ind = sample(x = 2, size = nrow(tr), replace = T, prob = c(.7, .3))
valid = tr[ind == 2, ]
tr = tr[ind == 1, ]

te = full_df[-part, ]

rm(a, device, df, fn, geoNetwork, sum_by_city, sum_by_country, sum_by_day, sum_by_dc, sum_by_medium, 
   sum_by_month, sum_by_nd, sum_by_op, sum_by_source, test, totals, trafficSource, train)
gc()

## 5. Fitting the models ####
## 5-1. GBM 
model_gbm = gbm(formula = logRevenue ~., data = tr,
                distribution = 'gaussian', 
                n.trees = 1000, 
                n.minobsinnode = 100, 
                shrinkage = .01,
                cv.folds = 10)

(opt_ntree_oob = gbm.perf(object = model_gbm, method = 'OOB'))

pred_oob = predict(object = model_gbm, newdata = valid, n.trees = opt_ntree_oob, type = 'response')
pred_gbm = ifelse(pred_oob < 0, 0, pred_oob)


## 5-2. Xgboost 
full_df2 = full_df %>%
  mutate_if(is.factor, as.integer)
str(full_df2)

memory.limit(56000)
part = 1:903653
tr_xgb = full_df2[part, ]
te_xgb = full_df2[-part, ]

tr_xgb$logRevenue[is.na(tr_xgb$logRevenue)] = 0
val_xgb = tr_xgb[ind == 2, ]
tr_xgb = tr_xgb[ind == 1, ]

dtr = xgb.DMatrix(data = data.matrix(tr_xgb[, -ncol(tr_xgb)]), label = tr_xgb$logRevenue)
dval = xgb.DMatrix(data = data.matrix(val_xgb[, -ncol(val_xgb)]), label = val_xgb$logRevenue)
dte = xgb.DMatrix(data = data.matrix(te_xgb[, -ncol(te_xgb)]), label = te_xgb$logRevenue)

# training a xgb model
myParam = list(objective = 'reg:linear', 
               eval_metric = 'rmse',
               eta = .05,
               max_depth = 8, 
               min_child_weight = 10,
               subsample = .7,
               colsample_bytree = .7)

cv = xgb.cv(data = dtr, 
            params = myParam, 
            nrounds = 3000,
            nfold = 5, 
            early_stopping_rounds = 200, 
            maximize = F, 
            print_every_n = 100)

a = cv$evaluation_log$test_mae_mean %>% which.min()
cv$evaluation_log[a]
cv$best_iteration

model_xgb = xgb.train(data = dtr,
                      params = myParam,
                      nrounds = cv$best_iteration,
                      watchlist = list(val = dval),
                      print_every_n = 50,
                      early_stopping_rounds = 100)

pred_xgb = predict(model_xgb, dval)

pred_xgb = ifelse(pred_xgb < 0, 0, pred_xgb)
hist(pred_xgb)

xgb.importance(feature_names = names(tr_xgb), model = model_xgb) %>% xgb.plot.importance(top_n = 15)

## 5-3. GLMNET 
memory.limit(56000)

full_x = full_df[, -1] %>%
  model.matrix(~.-1, data = .)

full_y = full_df$logRevenue

# splitting trainset and valiedset
tr_glm_x = full_x[part, ]
te_glm_x = full_x[-part, ]
tr_glm_y = full_y[part]

# Splitting trainset into train and validation 
tr_glm_y = ifelse(is.na(tr_glm_y), 0, tr_glm_y)
valid_glm_x = tr_glm_x[ind == 2, ]

tr_glm_x = tr_glm_x[ind == 1, ]
tr_glm_y = tr_glm_y[ind == 1]

# fitting the glmnet model
model_glmnet = cv.glmnet(x = tr_glm_x, y = tr_glm_y, family = 'gaussian', 
                         nfolds = 5, alpha = 0, type.measure = 'mse')
model_glmnet$lambda.min

pred_glmnet = predict(model_glmnet, newx = valid_glm_x, s = 'lambda.min')
pred_glmnet = ifelse(pred_glmnet < 0, 0, pred_glmnet)


## comparison model
hist(valid$logRevenue)
hist(pred_gbm)
hist(pred_xgb)
hist(pred_glmnet)


## 6. selecting the xgb model and submit ####
test = fread('test.csv')
pred = predict(model_xgb, dte)
pred = ifelse(pred < 0, 0, pred)
test$pred = pred

mysub = test %>%
  select(fullVisitorId, pred) %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = sum(pred))

mysub$PredictedLogRevenue = round(mysub$PredictedLogRevenue, digits = 5)

write.csv(mysub, file = 'sub.csv', row.names = F)





