library(tidyverse)
library(missForest)

rm(list=ls(all=TRUE))
setwd("~/Documents/GitHub/covid-news-political/topic_model")

# read in source bias and mallet files
source_bias <- read.csv("source_bias.csv") %>%
  select(source_name, bias, reliability) %>%
  mutate(imputed = ifelse(is.na(bias), "imputed", "not_imputed"))
  
mallet_clean <- read.csv('mallet_clean.csv') %>%
  select(-c(X))

mallet_clean$source_name[mallet_clean$source_name == "cnn"] <- "CNN"
thing <- mallet_clean %>%
  group_by(source_name) %>%
  summarize(n())

# process mallet to grouped vers
mallet_clean <- mallet_clean %>%
  group_by(source_name) %>%
  filter(n() > 100)

grouped_mallet <- mallet_clean %>%
  summarize_if(is.numeric, mean) %>%
  left_join(source_bias, on = source_name) %>%
  select(-c(hour, minute, sec, Index, source_name, imputed))
  

imputed_data = missForest(xmis = as.matrix(grouped_mallet), ntree = 5000, verbose = TRUE, maxiter = 10)
imputed_data$OOBerror

imputed_data$ximp

full_set <- as.data.frame(cbind(imputed_data$ximp, source_name = grouped_mallet$source_name, imputed = grouped_mallet$imputed))
write.csv(full_set, "topic_averages_w_biases_new.csv")
