library(tidyverse)
library(stringr)
library(lubridate)
library(ggthemes)
library(plotly)
library(lmtest)
library(sandwich)
library(stargazer)

rm(list=ls(all=TRUE))
setwd("~/Documents/GitHub/covid-news-political/topic_model")

mallet_comp <- read.table('covid_news_NLP-Mallet_Output_Composition.tsv', sep = '\t')

## clean the source and date from the name column
mallet_clean <- mallet_comp %>%
  mutate(source = str_match(filepath, "/[A-Za-z%20]+_\\d{4}\\.\\d{2}.*")) %>%
  separate(source,into = c("source_name", "source_time"),sep = "_") %>%
  separate(source_time, into = c("year", "month", "day", "hour", "minute", "sec"), extra = 'drop') %>%
  unite(col = "date", year:day, sep = "-") %>%
  mutate(date = as_date(date)) %>%
  mutate(source_name = str_replace(source_name, "/", "")) %>%
  mutate(source_name = str_replace_all(source_name, "%20", "_")) %>%
  filter(source_name != "nan")



#write.csv(mallet_clean, "mallet_clean.csv") ## write out new vers
mallet_clean <- read.csv('mallet_clean.csv')
mallet_clean = subset(mallet_clean, select = -c(X))
mallet_clean$source_name[mallet_clean$source_name == "cnn"] <- "CNN"
mallet_clean <- mallet_clean %>%
  group_by(source_name) %>%
  filter(n() > 100) %>%
  ungroup()

mallet_transformed <- mallet_clean %>%
  select(2:22, date, source_name) %>%
  gather(topic, value, 2:21) %>%
  left_join(biases, by = 'source_name')

write.csv(mallet_transformed, "mallet_transformed.csv")

### DO BIAS IMPUTATION IF NEEDED BEFORE MOVING ON
# biases
biases <- read.csv("topic_averages_w_biases_new.csv")

shapeset_bias =  c('not_imputed'=16,'imputed'=4)
# check spread of lean
ggplot(biases, aes(x = bias, y = reliability))+
  geom_point(size = 2.5, aes(color = local_national, shape = imputed)) +
  scale_shape_manual(values = shapeset_bias) +
  geom_vline(xintercept = -9.5, color = "blue") +
  geom_vline(xintercept = 10, color = 'red') +
  theme_bw() +
  xlab("Bias (Imputed Values Included)") +
  ylab("Reliability Scores") +
  ggtitle("Bias vs. Reliability") +
  theme( axis.text.x = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1),
         axis.text.y = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1))

# make buckets
biases$lean_buckets <- ifelse(biases$bias < -9.5, "left_leaning",
                              ifelse(biases$bias > 10, "right_leaning",
                                     "center"))

# rewrite if needed
# write.csv(biases, 'topic_averages_w_biases.csv')

init_plot_df <- mallet_clean %>%
  summarize_if(is.numeric, mean) %>%
  t()

init_plot_df <- as.data.frame(init_plot_df)
init_plot_df$topics <- rownames(init_plot_df)

# overall plot of topics
init_plot_df %>%
  filter(topics != 'Index' & topics != 'X' & topics != "hour" & topics != "minute" & topics != "sec") %>%
  ggplot(aes(x = reorder(topics, -V1), y = V1)) +
  geom_col(fill = "blue") +
  theme_bw() +
  xlab("topics") +
  ylab("mean occurrence") +
  ggtitle("Mean Occurrences of Topics in Corpus (291810 Obs)") +
  theme( axis.text.x = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1),
         axis.text.y = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1))

# science_testing change by sources
mallet_clean %>%
  group_by(source_name) %>% 
  filter(source_name != "nan") %>%
  summarize(mean_science_testing = mean(pol_covid_testing)) %>%
  ggplot(aes(reorder(source_name, -mean_science_testing), mean_science_testing)) +
  geom_col(fill = "blue") +
  theme_clean() +
  xlab("topics") +
  ylab("mean occurrence") +
  ggtitle("Mean Occurrences of Testing as Topic in Corpus by Source (291810 Obs)") +
  theme( axis.text.x = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1),
         axis.text.y = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1))
# econ_stocks change by sources
mallet_clean %>%
  group_by(source_name) %>% 
  filter(source_name != "nan") %>%
  summarize(mean_stocks = mean(pol_econ_stocks)) %>%
  ggplot(aes(reorder(source_name, -mean_stocks), mean_stocks)) +
  geom_col(fill = "blue") +
  theme_clean() +
  xlab("topics") +
  ylab("mean occurrence") +
  ggtitle("Mean Occurrences of Econ_Stocks as Topic in Corpus by Source (291810 Obs)") +
  theme( axis.text.x = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1),
         axis.text.y = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1))

# econ_workers change by sources
mallet_clean %>%
  group_by(source_name) %>% 
  filter(source_name != "nan") %>%
  summarize(mean_workers = mean(pol_econ_workers)) %>%
  ggplot(aes(reorder(source_name, -mean_workers), mean_workers)) +
  geom_col(fill = "blue") +
  theme_bw() +
  xlab("topics") +
  ylab("mean occurrence") +
  ggtitle("Mean Occurrences of Econ_Stocks as Topic in Corpus by Source (291810 Obs)") +
  theme( axis.text.x = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1),
         axis.text.y = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1))

# econ_stocks change by sources
mallet_clean %>%
  group_by(source_name) %>% 
  filter(source_name != "nan") %>%
  summarize(mean_stocks = mean(pol_econ_stocks)) %>%
  ggplot(aes(reorder(source_name, -mean_stocks), mean_stocks)) +
  geom_col(fill = "blue") +
  theme_clean() +
  xlab("topics") +
  ylab("mean occurrence") +
  ggtitle("Mean Occurrences of Econ_Stocks as Topic in Corpus by Source (291810 Obs)") +
  theme( axis.text.x = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1),
         axis.text.y = element_text(face = "bold", color = "black", 
                                    size = 10, angle = 45, hjust = 1))

# over time plot

# fix some date issues
colorset = c('right_leaning'='red','center'='purple','left_leaning'='blue')
mallet_transformed$date <- lubridate::as_date(mallet_transformed$date)
mallet_transformed$month <- floor_date(mallet_transformed$date, "month")

# covid testing topic
mallet_transformed %>%
  filter(topic == "pol_state_level" & lean_buckets != "nan" & value > 0) %>%
ggplot(aes(x = month, y = value, color = lean_buckets))+
  geom_point(alpha = .3) +
  scale_color_manual(values=colorset) +
  geom_smooth(method = 'loess', color = 'black', se = FALSE) +
  facet_wrap(~lean_buckets) +
  ggtitle("State Level Topic Proportions over time") 

# trump pol
mallet_transformed %>%
  filter(topic == "pol_trump" & lean_buckets != "nan" & value > 0) %>%
  ggplot(aes(x = month, y = value, color = lean_buckets))+
  geom_point(alpha = .3) +
  scale_color_manual(values=colorset) +
  geom_smooth(method = 'loess', color = 'black', se = FALSE) +
  facet_wrap(~lean_buckets) +
  ggtitle("Trump Topic Proportions over time") 

mallet_transformed %>%
  filter(topic == "pol_vaccine" & lean_buckets != "nan" & value > 0) %>%
  ggplot(aes(x = month, y = value, color = lean_buckets))+
  geom_point(alpha = .3) +
  scale_color_manual(values=colorset) +
  geom_smooth(method = 'loess', color = 'black', se = FALSE) +
  facet_wrap(~lean_buckets) +
  ggtitle("Vaccine Topic Proportions over time") 

mallet_transformed %>%
  filter(topic == "pol_econ_workers" & lean_buckets != "nan" & value > 0) %>%
  ggplot(aes(x = date, y = value, color = lean_buckets))+
  geom_point(alpha = .3) +
  scale_color_manual(values=colorset) +
  geom_smooth(method = 'loess', color = 'black', se = FALSE) +
  facet_wrap(~lean_buckets) +
  ggtitle("Econ Workers Topic Proportions over time") 

# calc normalization of each topic
pmi_for_topic <- function(mallet_df, topic){
  source_prob <- mallet_df %>%
    group_by(source_name) %>%
    summarize(prob = n()/nrow(mallet_df))
  
  topic_condo_prob <- mallet_df %>%
    group_by(source_name) %>%
    summarize(prob = mean(!!rlang::ensym(topic)))
  
  topic_overall_prob <- mallet_df %>%
    summarize(prob = mean(!!rlang::ensym(topic)))
  
  pmi <- t(rbind(source_prob$source_name, log(topic_condo_prob$prob/source_prob$prob/topic_overall_prob$prob)))
  return(pmi)
  
  
}

mallet_clean %>%
  group_by(source_name) %>%
  summarize(prob= mean(econ_jobs))

test <- pmi_for_topic(mallet_clean, econ_jobs)


#############################
# regressions on the topics #
#############################

# covid testing topic ####
lm_pol_covid_testing <- lm(pol_covid_testing~ bias + reliability + local_national, data = biases)
summary(lm_pol_covid_testing)
stargazer(coeftest(lm_pol_covid_testing, vcov = vcovHC(lm_pol_covid_testing, type = "HC1")))

lm_pol_covid_testing_bkt <- lm(pol_covid_testing~ lean_buckets + reliability + local_national, data = biases)
summary(lm_pol_covid_testing_bkt)
stargazer(coeftest(lm_pol_covid_testing_bkt, vcov = vcovHC(lm_pol_covid_testing_bkt, type = "HC1")))

# covid cases topic ####
lm_pol_covid_cases <- lm(pol_covid_cases ~ bias+ reliability + local_national, data = biases)
summary(lm_pol_covid_cases)
stargazer(coeftest(lm_pol_covid_cases, vcov = vcovHC(lm_pol_covid_cases, type = "HC1")))

lm_pol_covid_cases_bkt <- lm(pol_covid_cases ~ lean_buckets+ reliability + local_national, data = biases)
summary(lm_pol_covid_cases_bkt)
stargazer(coeftest(lm_pol_covid_cases_bkt, vcov = vcovHC(lm_pol_covid_cases_bkt, type = "HC1")))

# vaccine topic ####
lm_pol_vaccine <- lm(pol_vaccine ~ bias+ reliability  + local_national, data = biases)
summary(lm_pol_vaccine)
stargazer(coeftest(lm_pol_vaccine, vcov = vcovHC(lm_pol_vaccine, type = "HC1")))

lm_pol_vaccine_bkt <- lm(pol_vaccine~ lean_buckets + reliability  + local_national, data = biases)
summary(lm_pol_vaccine_bkt)
stargazer(coeftest(lm_pol_vaccine_bkt, vcov = vcovHC(lm_pol_vaccine_bkt, type = "HC1")))

# congress topic ####
lm_pol_congress <- lm(pol_congress ~ bias + reliability + local_national, data = biases)
summary(lm_pol_congress)
stargazer(coeftest(lm_pol_congress, vcov = vcovHC(lm_pol_congress, type = "HC1")))

lm_pol_congress_bkt <- lm(pol_congress ~ lean_buckets + reliability + local_national, data = biases)
summary(lm_pol_congress_bkt)
stargazer(coeftest(lm_pol_congress_bkt, vcov = vcovHC(lm_pol_congress_bkt, type = "HC1")))

# state topic ####
lm_pol_state <- lm(pol_state_level ~ bias + reliability  + local_national, data = biases)
summary(lm_pol_state)
stargazer(coeftest(lm_pol_state, vcov = vcovHC(lm_pol_state, type = "HC1")))

lm_pol_state_bkt <- lm(pol_state_level ~ lean_buckets + reliability  + local_national, data = biases)
summary(lm_pol_state_bkt)
stargazer(coeftest(lm_pol_state_bkt, vcov = vcovHC(lm_pol_state_bkt, type = "HC1")))

# trump topic ####
lm_pol_trump <- lm(pol_trump ~ bias + reliability  + local_national, data = biases)
summary(lm_pol_trump)
stargazer(coeftest(lm_pol_trump, vcov = vcovHC(lm_pol_trump, type = "HC1")))

lm_pol_trump_bkt <- lm(pol_trump ~ lean_buckets + reliability  + local_national, data = biases)
summary(lm_pol_trump_bkt)
stargazer(coeftest(lm_pol_trump_bkt, vcov = vcovHC(lm_pol_trump_bkt, type = "HC1")))

# econ_workers ####
lm_econ_workers <- lm(pol_econ_workers ~ bias + reliability  + local_national, data = biases)
summary(lm_econ_workers)
stargazer(coeftest(lm_econ_workers, vcov = vcovHC(lm_econ_workers, type = "HC1")))

lm_econ_workers_bkt <- lm(pol_econ_workers ~ lean_buckets + reliability  + local_national, data = biases)
summary(lm_econ_workers_bkt)
stargazer(coeftest(lm_econ_workers_bkt, vcov = vcovHC(lm_econ_workers_bkt, type = "HC1")))

# econ_stocks ####
lm_econ_stocks <- lm(pol_econ_stocks ~ bias + reliability + local_national, data = biases)
summary(lm_econ_stocks)
stargazer(coeftest(lm_econ_stocks, vcov = vcovHC(lm_econ_stocks, type = "HC1")))

lm_econ_stocks_bkt <- lm(pol_econ_stocks ~ lean_buckets + reliability  + local_national, data = biases)
summary(lm_econ_stocks_bkt)
stargazer(coeftest(lm_econ_stocks_bkt, vcov = vcovHC(lm_econ_stocks_bkt, type = "HC1")))



