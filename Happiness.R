library(tidyverse)
library(magrittr)
library(corrplot)
library(GGally)
library(nnet)

happiness_2015 = read_csv('2015.csv')
happiness_2016 = read_csv('2016.csv')
happiness_2017 = read_csv('2017.csv')

happiness_2017
happiness_2017 = happiness_2017[ , -c(4,5)]
New_Colnames_2017 = c('country', 'rank', 'score', 'GDP', 'family', 'health', 'freedom', 'generosity', 'corruption', 'dystopia')
colnames(happiness_2017) = New_Colnames_2017

happiness_2016
happiness_2016 = happiness_2016[ , -c(5,6)]
New_Colnames = c('country', 'region', 'rank', 'score', 'GDP', 'family', 'health', 'freedom', 'corruption', 'generosity', 'dystopia')
colnames(happiness_2016) = New_Colnames

happiness_2015
happiness_2015 = happiness_2015[, -5]
colnames(happiness_2015) = New_Colnames

## What countries (or regions) rank the highest in overall happiness? + each of the six factors contributing to happiness 
head(happiness_2017)

head(happiness_2017, n = 10) %>%
  ggplot(aes(x = reorder(country, score), y = score)) + 
  geom_bar(stat = 'identity', fill = 'orange') +
  geom_text(aes(x = country, y = .5, label = formatC(score, digits = 3)), fontface = 'bold') + 
  coord_flip() + 
  scale_y_continuous(limits = c(0, 9)) + 
  labs(x = 'Country', y = 'Happiness Score', title = 'Ten Higgest Happiness Country') +
  theme_classic()

tail(happiness_2017, n = 10) %>%
  ggplot(aes(x = reorder(country, score), y = score)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  geom_text(aes(x = country, y = .3, label = formatC(score, digits = 3)), fontface = 'bold') + 
  coord_flip() + 
  scale_y_continuous(limits = c(0, 4)) + 
  labs(x = 'Country', y = 'Happiness Score', title = 'Ten Lowest Happiness Country') +
  theme_classic()

## GDP, Family, Health, Freedom, Governmonet, Generosity, Corruption
ggplot(happiness_2017, aes(x = score)) + geom_histogram(fill = '#377EB8') + theme_bw()

happiness_2017 = happiness_2017 %>%
  mutate(level = ifelse(score <5, 'low', ifelse(score <=6, 'middle', 'high')))
table(happiness_2017$level)

happiness_2017$level = factor(happiness_2017$level, levels = c('low', 'middle', 'high'))

ggplot(happiness_2017, aes(x = GDP, col = level, fill = level)) + 
  geom_density(alpha = .2) +
  labs(x = 'GDP', y = NULL, title = 'GDP Distribution Plot') +
  theme_bw()

ggplot(happiness_2017, aes(x = family, col = level, fill = level)) + 
  geom_density(alpha = .2) +
  labs(x = 'Family', y = NULL, title = 'Family Distribution Plot') +
  theme_bw()

ggplot(happiness_2017, aes(x = health, col = level, fill = level)) + 
  geom_density(alpha = .2) +
  labs(x = 'Health', y = NULL, title = 'Health Distribution Plot') +
  theme_bw()

ggplot(happiness_2017, aes(x = freedom, col = level, fill = level)) + 
  geom_density(alpha = .2) +
  labs(x = 'Freedom', y = NULL, title = 'Freedom Distribution Plot') +
  theme_bw()

ggplot(happiness_2017, aes(x = generosity, col = level, fill = level)) + 
  geom_density(alpha = .2) +
  labs(x = 'Generosity', y = NULL, title = 'Generosity Distribution Plot') +
  theme_bw()

ggplot(happiness_2017, aes(x = corruption, col = level, fill = level)) + 
  geom_density(alpha = .2) +
  labs(x = 'Corruption', y = NULL, title = 'Corruption Distribution Plot') +
  theme_bw()


gdp_low_high = happiness_2017 %>% 
  filter(level == 'low' & GDP > 1 | level == 'high' & GDP < 1.2)

avg_low_high = gdp_low_high %>%
  group_by(level) %>%
  summarise(family = mean(family),
            health = mean(health),
            freedom = mean(freedom), 
            generosity = mean(generosity),
            corruption = mean(corruption))

gdp_low_high %>%
  ggplot(aes(x = reorder(country, rank), y = corruption, fill = level)) + 
  geom_col() + 
  coord_flip() + 
  theme_bw()


num_df = happiness_2017[, 5:10]
cor_df = cor(num_df)
corrplot(cor_df, method = 'number')   # family&health, corruption&freedom

happiness_2017$year = 2017
happiness_2016$year = 2016
happiness_2015$year = 2015

happiness_2017 = happiness_2017 %>%
  left_join(happiness_2016[1:2], by = 'country')
happiness_2017 = happiness_2017[, c(1, 13, 2:7, 9, 8, 10, 12)]

happy_all = rbind(happiness_2015, happiness_2016, happiness_2017)

happy_all
happy_all$region[grep(pattern = 'Asia', x = happy_all$region)] = 'Asia'
happy_all$region[grep(pattern = 'Africa', x = happy_all$region)] = 'Africa'
happy_all$region[grep(pattern = 'Europe', x = happy_all$region)] = 'Europe'
happy_all$region[happy_all$region == 'Latin America and Caribbean'] = 'South America'
happy_all$region[happy_all$region == 'Australia and New Zealand'] = 'Australia'

table(happy_all$region)
which(is.na(happy_all$region))

happy_all$region[c(348, 386)] = 'Asia'
happy_all$region[c(428, 454, 470)] = 'Africa'

happy_all %>%
  ggplot(aes(x = region, y = score, fill = region)) + 
  geom_boxplot() +
  theme_bw()

## How did country ranks or scores change from 2015 to 2017? 
## Did any country experience a significant increase or decrease in happiness?
happy_all %>%
  filter(rank <= 15) %>%
  select(country, rank, score, year) %>%
  ggplot(aes(x = year, y = score, col = country)) + 
  geom_line(size = 1) +
  theme_bw()

happy_all %>%
  filter(rank > 145) %>%
  select(country, rank, score, year) %>%
  ggplot(aes(x = year, y = score, col = country)) + 
  geom_line(size = 1) + 
  theme_bw()

## The End.
write.csv(x = happy_all, file = 'happy_all.csv', row.names = F)
