# LDA analysis for Multicultural Group Therapy review
# Andrew Gerlach
# 9/2/22

# USAGE INSTRUCTIONS
#    Set the path in R to the directory containing this file
#    Download and install tree-tagger
#    Change <PATH TO TREE TAGGER> to the location where tree-tagger is installed
#    Ensure all required libraries are installed

# Load libraries
library(stringr)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(topicdoc)
library(wordcloud2)
source('helper_functions.R')

# Flag to start from scratch or load abstract_word_list
import_and_clean_data = FALSE
# Path to tree-tagger executable
tt.ex.path = '<PATH TO TREE TAGGER>/tree-tagger-MacOSX-Intel-3.2.3/bin/tree-tagger'
# Path to tree-tagger parameter file
tt.par.path = '<PATH TO TREE TAGGER>/tree-tagger-MacOSX-Intel-3.2.3/english.par'

if(import_and_clean_data) {

    # Data file was renamed, reduced to a single sheet, and converted to .csv format
    fn = 'article_data.csv'
    data = read.csv(fn)

    # Clean abstract data
    abstract_word_list = lapply(data$Abstract, clean_text)

    # Write to human readable file
    write_word_list(abstract_word_list, 'abstract_word_list.xlsx')

    # Convert to tidy format
    abstract_word_list = convert_to_tidy(abstract_word_list)

    # Write to R data storage
    saveRDS(abstract_word_list, 'abstract_word_list.rds')

} else {

    abstract_word_list = readRDS('abstract_word_list.rds') }

# Create document-term matrix
dtm = abstract_word_list %>% cast_dtm(abstract, word, n)

# number of topics to fit models for
topics = 6:12

# Perform LDA analysis
models = list()
for(i in topics) {

    print_msg(paste('Fitting model with', i, 'topics'))
    models[[i - min(topics) + 1]] = topicmodels::CTM(dtm, i)

}

# Load final model for generating figures and tables (Model cannot be reproduced
# exactly from the above code since the LDA package initializes with a randrom
# seed, unfortunately)
final_model = readRDS('final_model_10topics.rds')

# Generate word clouds

for(topic in 1:10) {
     create_wordcloud(final_model, topic, 0.002) }

# calculate prevalence and average year of publication for each topic
topic_summary = topic_summary(final_model, data$year)


