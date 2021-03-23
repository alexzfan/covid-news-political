# covid-news-political

## Data Collection
The data used was queried from NewsAPI (https://newsapi.org/). The api_query notebook assumes a business API access token, which allows for queries up to 2 years in their index. That notebook outputs various pandas data frames of the features given by the api. The pd_to_gcp notebook in the same directory processes those data frames and uploads them to GCP.

## Topic Model
This section requires some back-and-forth between notebooks and r scripts. Running the gensim topic model notebook returns a pyLDAvis html, which is simply for visualization purposes. The main chunk of this section is with the mallet_topic_model, which should produce both mallet keys and mallet composition files. Using keys, the composition file must be given headers manually based on what the keys likely represent. 

Using the mallet composition file, the mallet composition analysis r script does some simple processing, with the main output being a cleaned version with time and source metadata separated out into feature columns. The source bias imputation script uses the cleaned mallet file in conjunction with the source bias csv (which contains missing values) to impute. 

Back in the mallet composition script, lean buckets can be created, and local/national features were manually recorded (this in the future needs to be improved). Additional topic vis and regressions can be computed through various commands in the script.

## Word Embedding Models
This section just uses the one notebook to create CBOW word2vec embeddings. The only quirk is that it also re-copies files to different splits so that mini-models can be created. Time-based mini-models only require the filename metadata, but the source-based mini-models uses the source biases, which requires running topic models before. 

## Classifier
This section uses the classifier notebook to test various text classification models. There is a neural network section, using LSTM RNNs to classify as a test. This was not run locally; instead, it was run on gradient papersource notebooks.

The sample was derived from the sampler notebook, which is just a random sampling (this could be improved to perhaps oversample some particular classes).

## Misc
This section contains various notebooks that aren't tied the other sections explicitly. The main notebook here is the tfidf/word count visualizations notebook.