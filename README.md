
# README

[![DOI](https://zenodo.org/badge/318579178.svg)](https://zenodo.org/badge/latestdoi/318579178)

This repository includes supplementary material to accompany Skripnikov
et al. red tide Twitter analysis. Content includes anonymized tweet data
(`Secure_Tweets_data.csv`), vocabularies for sentiment analysis, and
code to run sentiment analysis.

The file structure in this repository is as follows. The tweet data are
in the root directory and the sentiment analysis code and vocabularies
are in the `Sentiment Analysis` folder.

    .
    +-- DESCRIPTION
    +-- LICENSE
    +-- LICENSE.md
    +-- README.md
    +-- README.Rmd
    +-- red-tide-twitter.Rproj
    +-- Secure_Tweets_Data.csv
    \-- Sentiment Analysis
        +-- Code
        |   +-- Cleaning_For_Sentiment_Analysis_for_Secure_Data.R
        |   \-- SentimentR_Analysis_for_Secure_Data.R
        \-- Vocabularies
            +-- Hashtag_Break_Apart_Words.csv
            +-- Red_Tide_Polarized_Sentiment_Words_Vocabulary.csv
            \-- Red_Tide_Valence_Shifters_Vocabulary.csv
