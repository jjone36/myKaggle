library(tidyverse)
library(magrittr)
library(tidytext)
library(qdap)
library(tm)
library(rebus)
library(wordcloud)
library(caret)
library(e1071)
library(glmnet)
library(xgboost)

# This is the full code for the competition 'Toxic Comment Classification Challenge' held in Kaggle.
# The project is to build a model that's capable of deecting differnet types of toxicity. 
# You can find more details and the dataset here : https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge

# 1. Reading the dataset ####
train = read_csv('train.csv')
test = read_csv('test.csv')

head(train)

# target variable
(target = names(train[, 3:8]))
summary(train[, target])  # -> binary values, not indicating amount or levels.

test$toxic = NA
test$severe_toxic = NA
test$obscene = NA
test$threat = NA
test$insult = NA
test$identity_hate = NA

part = 1:nrow(train)
full = rbind(train, test)

# 2. Exploratory data analysis ####
# 2-1. the distribution of word length
full$len = str_length(full$comment_text)

ggplot(full, aes(x = len)) + 
  geom_density(fill = 'coral2') + 
  labs(x = NULL, y = NULL, title = 'the distribution of comment length') +
  theme_bw()

summary(full$len)

full = full %>%
  mutate(nword = str_count(comment_text, pattern = one_or_more(WRD)),
         num = str_count(comment_text, pattern = DGT),
         nexcl = str_count(comment_text, '!'),
         nques = str_count(comment_text, '\\?'),
         nsymb = str_count(comment_text, '[[:punct:]]'))


# 2-2. Tokenize the comment text column 
data("stop_words")
head(stop_words)

tr_token = train %>%
  unnest_tokens(output = word, input = comment_text) %>%
  anti_join(stop_words, by = 'word')

# Which words are the most frequently used?
freq_n = tr_token %>%
  group_by(word) %>%
  count() %>%
  arrange(-n)

freq_n[1:20, ] %>%
  ggplot(aes(x = reorder(word, n), y = n)) + 
  geom_col(fill = 'lightseagreen') +
  geom_text(aes(x = word, y = 1, label = paste0('(', n, ')')), 
            hjust = 0, vjust = .3, fontface = 'bold', size = 4) +
  coord_flip() + 
  labs(x = NULL, y = NULL, title = 'Top 20 most Frequent Words') +
  theme_bw()
  
# 2-3. Tf-Idf
tr_token_n = tr_token %>%
  group_by(toxic, severe_toxic, obscene, threat, insult, identity_hate) %>%
  count()   # -> 41 different combinations of 'toxic comment' documents

tr_token_n$document = 1:41
tr_tfidf = tr_token %>% 
  left_join(tr_token_n) %>%
  bind_tf_idf(term = word, document = document, n = n) %>%
  arrange(desc(tf_idf))

p = geom_col(fill = 'coral2') +
  coord_flip() +
  theme_bw()

ggplot(tr_tfidf[1:20, ], aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(fill = 'coral2') +
  coord_flip() +
  theme_bw() + 
  labs(x = NULL, y = 'TF_IDF', title = 'Top 20 Important Words')  

# toxic important word
tr_tfidf %>%
  filter(toxic == 1) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(fill = 'coral2') +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = 'TF_IDF', title = 'Top 20 Important \"Toxic\" Words')

# severe_toxic important word
tr_tfidf %>%
  filter(severe_toxic == 1) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col(fill = 'coral2') +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = 'TF_IDF', title = 'Top 20 Important \"Severe Toxic\" Words')

# 2-4. Word Cloud 
wordcloud(words = tr_tfidf$word, freq = tr_tfidf$tf_idf, max.words = 50, colors = brewer.pal(8, 'Dark2'))

a = c()
plot_word = function(a){
  tr_tfidf[a == 1,] %$%
    wordcloud(words = word, freq = tf_idf, scale = c(3, 1), max.words = 30, colors = brewer.pal(5, 'Dark2'))
}

plot_word(tr_tfidf$toxic)
plot_word(tr_tfidf$severe_toxic)
plot_word(tr_tfidf$obscene)
plot_word(tr_tfidf$threat)
plot_word(tr_tfidf$insult)
plot_word(tr_tfidf$identity_hate)


# 3. Transforming into corpus and document-term matrix ####
head(full)

target = full[, 3:8]
full[, 9:14] = scale(full[, 9:14])

sum(is.na(full$comment_text))

corpus = full$comment_text %>%
  str_trim() %>%
  tolower() %>%
  VectorSource() %>%
  VCorpus()

corpus = tm_map(corpus, removePunctuation)   
corpus = tm_map(corpus, stripWhitespace)   
corpus = tm_map(corpus, removeNumbers)  
corpus = tm_map(corpus, removeWords, stopwords('en'))    
corpus = tm_map(corpus, stemDocument)   

term_dtm = DocumentTermMatrix(corpus)

term_m = removeSparseTerms(term_dtm, sparse = 0.97) %>% as.matrix()
full_df = as.data.frame(term_m)

full_df = cbind(full_df, full[, 9:14])

# 4. Fitting the models ####
# 4-1. GLMNET
tr_x = full_df[part, ] %>% as.matrix()
tr_y = target[part, ]

te_x = full_df[-part, ] %>% as.matrix()
te_y = target[-part, ]

pred_glmnet = te_y
for(x in names(target)){
  y = tr_y[[x]]
  model_glmnet = cv.glmnet(x = tr_x, y = factor(y), family = 'binomial', 
                           type.measure = 'auc', nfolds = 5, alpha = 0)
  
  pred = predict(model_glmnet, newx = te_x, s = 'lambda.min', type = 'response')
  pred = ifelse(pred < .5, 0, 1)
  
  pred_glmnet[, x] = pred
}
head(pred_glmnet)


# 4-2. Xgboost
myParam = list(objective = 'binary:logistic',
               booster = 'gbtree',
               eval_metric = 'auc',
               eta = .1,
               max_depth = 5, 
               min_child_weight = 5,
               subsample = .7,
               colsample_bytree = .5)

pred_xgb = te_y
for(x in names(target)){
  y = tr_y[[x]]
  model_xgb = xgboost(tr_x, y, param = myParam, 
                      nrounds = 1000, print_every_n = 100, early_stopping_rounds = 100)
  pred = predict(model_xgb, te_x, type = 'prob')
  pred = ifelse(pred < .5, 0, 1)
  
  pred_xgb[, x] = pred
}
head(pred_xgb)


# 4-3. Variance Importance
varImp = getModelInfo('glmnet')$glmnet$varImp
varImp(object = model_glmnet, lambda = model_glmnet$lambda.min) %>% 
  rownames_to_column(var = 'word.Imp') %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word.Imp, Overall), y = Overall)) + 
  geom_col(fill = 'grey80') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 20 Important Variables of the Glmnet Model: Identity hate') + 
  theme_classic()

featureName = model_xgb$feature_names
xgb.importance(feature_names = names(full_df), model = model_xgb) %>% 
  xgb.plot.importance(top_n = 20, rel_to_first = T, main = 'Top 20 Importance Variables of the Xgb Model: Identity hate') 

