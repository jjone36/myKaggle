library(tidyverse)
library(jsonlite)
library(ggthemes)
library(magrittr)
library(lubridate)
library(rebus)
library(gridExtra)

train = read.csv('train.csv', stringsAsFactors = T) 
test = read.csv('test.csv', stringsAsFactors = T) 

full = rbind(train, test)

#### Preprocessing ####
str(full)

## channelGrouping
table(full$channelGrouping)

## date
full$date = ymd(full$date)

# device ####
str(full$device)
device = paste("[", paste(full$device, collapse = ","), "]") %>% fromJSON()
str(device)
device = as.tibble(device)
summary(device)

table(device$browser)
device$browser = ifelse(nchar(device$browser) >= 20, NA, device$browser)
device$isMobile = factor(device$isMobile, labels = c(0, 1))

device = device[, names(device) %in% c('browser', 'operatingSystem', 'isMobile', 'deviceCategory')]

full$device = NULL
full = cbind(full, device)

# geoNetwork ####
str(full$geoNetwork)
geoNetwork = paste("[", paste(full$geoNetwork, collapse = ", "), "]") %>% fromJSON()

head(geoNetwork)
table(geoNetwork$networkDomain)
geo_sub = geoNetwork[names(geoNetwork) %in% c('continent', 'subContinent', 'country', 'networkDomain')]
full = cbind(full, geo_sub)
full$geoNetwork = NULL

# socialEngagementType ####
table(full$socialEngagementType)
full$socialEngagementType = NULL

# totals ####
str(full$totals)
totals = paste("[", paste(full$totals, collapse = ", "), "]") %>% fromJSON()
head(totals)
str(totals)

# visits columne
table(totals$visits)  
which(is.na(totals$visits))
totals$visits = NULL

# hits columne
table(totals$hits)
totals$hits = as.numeric(totals$hits)
summary(totals$hits)
hist(totals$hits, xlim = c(0, 100))

# pageview columne
table(totals$pageviews)
sum(is.na(totals$pageviews))
totals$pageviews = as.numeric(totals$pageviews)

# bounces columne
table(totals$bounces)
which(is.na(totals$bounces))
totals$bounces[is.na(totals$bounces)] = 0
totals$bounces = as.factor(totals$bounces)

# newVisits columne
table(totals$newVisits)
totals$newVisits[is.na(totals$newVisits)] = 0
totals$newVisits = as.numeric(totals$newVisits)

# transactionRevenue columne
range(totals$transactionRevenue, na.rm = T)
totals$transactionRevenue = as.numeric(totals$transactionRevenue)

full = cbind(full, totals)
full$totals = NULL

# trafficSource ####
str(full$trafficSource)
trafficSource = paste("[", paste(full$trafficSource, collapse = ", "), "]") %>% fromJSON()
str(trafficSource)
full$trafficSource = NULL

df = trafficSource$adwordsClickInfo
df$targetingCriteria = NULL
trafficSource = cbind(trafficSource, df)

which(!is.na(trafficSource$campaignCode))

trafficSource[, names(trafficSource) %in% c('adwordsClickInfo', 'criteriaParameter',
                                            'campaignCode', 'gclId', 'referralPath')] = NULL

tfs_sub = trafficSource %>%
  select(source, medium, keyword)
head(tfs_sub)

tfs_sub$source[grepl(pattern = 'google', x = tfs_sub$source)] = 'google'
tfs_sub$source[grepl(pattern = 'yahoo', x = tfs_sub$source)] = 'yahoo'
tfs_sub$source[grepl(pattern = 'youtube', x = tfs_sub$source)] = 'youtube'
tfs_sub$source[grepl(pattern = 'reddit', x = tfs_sub$source)] = 'reddit'
tfs_sub$source[grepl(pattern = 'facebook', x = tfs_sub$source)] = 'facebook'
table(tfs_sub$source)

a = tfs_sub %>%
  group_by(source) %>%
  count() %>%
  filter(n < 1000)

tfs_sub$source[tfs_sub$source %in% a$source] = 'other'

full = cbind(full, tfs_sub)

part = 1:nrow(train)
train = full[part, ]
test = full[-part, ]


write.csv(train, 'gstore_tr.csv', row.names = F)
write.csv(test, 'gstore_te.csv', row.names = F)


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


#### EDA ####
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

# visitStartTime 
str(train$visitStartTime)
train[, 3:8] %>% head()

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



#### Estimation ####
library(caret)

names(train)
str(train)

full$year = year(full$date) %>% as.factor()
full$month = month(full$date) %>% as.factor()
full$day = day(full$date) %>% as.factor()
full$week = wday(full$date) %>% as.factor()

full$operatingSystem = as.factor(full$operatingSystem)
full$deviceCategory = as.factor(full$deviceCategory)
full$continent = as.factor(full$continent)
full$subContinent = as.factor(full$subContinent)

b = full %>%
  group_by(browser) %>%
  count() %>%
  filter(n < 5000)
  
full$browser[full$browser %in% b$browser] = 'other'
full$browser[full$browser == 'Safari (in-app)'] = 'Safari'
table(full$browser)

full$browser = as.factor(full$browser)
full$logRevenue = log(full$transactionRevenue)

saveRDS(object = full, file = 'Gstore_full.rds')

full_df = full[, !names(full) %in% c('date', 'fullVisitorId', 'sessionId', 'visitId', 'visitStartTime', 
                                     'source', 'medium', 'country', 'networkDomain', 'transactionRevenue', 'keyword')]

tr_df = full_df[part, ]
te_df = full_df[-part, ]

tr_df$logRevenue[is.na(tr_df$logRevenue)] = 0
tr_df = tr_df[, c(19, 1:18)]
str(tr_df)


## GBM
library(gbm)
model_gbm = gbm(formula = logRevenue ~., distribution = 'gaussian', 
                data = tr_df, n.trees = 3000)

(opt_ntree_oob = gbm.perf(object = model_gbm, method = 'OOB'))
pred_oob = predict(object = model_gbm, newdata = te_df, n.trees = opt_ntree_oob, type = 'response')

hist(pred_oob)


test$PredictedLogRevenue = pred_oob
mysub = test %>%
  select(fullVisitorId, PredictedLogRevenue)
write.csv(mysub, file = 'submission_gbm.csv', row.names = F)

submission = read.csv('sample_submission.csv')
names(submission)

mysub = data.frame(fullVisitorId = test$fullVisitorId, Pred = pred_oob)
head(mysub)

mysub = mysub %>%
  group_by(fullVisitorId) %>%
  summarise(Predicted = sum(Pred))

mysub = submission %>%
  left_join(mysub, by = 'fullVisitorId')
mysub$PredictedLogRevenue = NULL
names(mysub) = names(submission)

write.csv(mysub, file = 'submission_gbm.csv', row.names = F)

submission_gbm = read.csv('submission_gbm.csv')


