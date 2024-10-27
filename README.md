## Description
This repository contains R code for extracting a variety of text features, including character-based, token-based, sentiment, readability, lexical diversity, and more. These functions leverage libraries like NLP, openNLP, tm, and SnowballC to preprocess text and generate insightful features for further analysis.

## Functions
1. text_to_character_features: Extracts various character-based features from input text, such as the number of whitespaces, text length (excluding whitespaces), numeric, alphabetic, and non-alphanumeric characters.
2. text_to_token_features: Extracts various token-based features from input text, including counts of different types of tokens such as alphabetic, numeric, non-alphanumeric, and tokens of various lengths and cases.
3. text_to_differential_features: Generates differential features from input text by calculating character and token-based features, and then comparing them with features from the concatenated text to identify differences.
4. text_to_transformed_text: Compresses input text by removing non-alphanumeric characters, then chunks the compressed text based on dictionary matches to create a transformed text version.
5. text_to_indicator_features: Determines the presence of numeric, alphabetic, punctuation, non-ASCII, and non-alphanumeric ASCII characters in input text, generating corresponding indicator features.
6. text_to_sentiment_features: Extracts various sentiment features from input text using multiple sentiment analysis methods, including NRC, Bing, AFINN, emotion classification, and polarity classification.
7. text_to_chunked_features: Extracts various features from transformed text, including character-based, token-based, sentiment, and indicator features, and labels them as chunked.
8. text_to_readability_features: Computes various readability features for input text using different readability indices and lexical distributions, ensuring text is encoded in UTF-8 format.
9. text_to_lexical_diversity_features: Computes lexical diversity features for input text using various indices, including TTR, MSTTR, MATTR, Maas, and MTLD.
10. text_to_ngrams_list: Generates a list of the most frequent n-grams (up to 1000) from the input text using a specified n-gram length.
11. text_to_ngram_features_scoring: Generates a matrix of n-gram features for input text, scoring each text based on the presence of specific n-grams from a predefined list.
12. text_to_ngram_features: Generates n-gram features for input text, creating a matrix that scores each text based on the presence and frequency of specific n-grams up to a length of 1000.
13. text_to_pos_features: Extracts Part-of-Speech (POS) features from input text using Penn Treebank tags and the OpenNLP library, creating a matrix of POS tag frequencies.
14. text_to_entity_features: Identifies and extracts named entities from input text using OpenNLP models, generating a matrix with counts for entities like dates, locations, money, organizations, people, and percentages.
