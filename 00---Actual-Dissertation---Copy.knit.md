---
title: "Dissertation"
author: "Ryan Pollard"
date: "03/08/2021"
bibliography: bib/references.bib  
output:
  pdf_document: default
  bookdown::pdf_document2:
    latex_engine: lualatex
  tufte::tufte_handout:
    latex_engine: xelatex
header-includes: \usepackage{amsmath}
---


<!--- For HTML Only --->







\tableofcontents
\pagebreak

# Abstract



# Structure of the document

The sections are denoted by the following numbers and are summaries below: 

1. Gives a background into the reasons text-based analysis have developed, it's needs and it's uses.
2. Explains what this project aims to achieve using LDA, a text-analysis algorithm.
3. Explains the branch of text-analysis known as "topic modeling". It gives different statistical techniques in this branch and explores their advantages and disadvantages.
4. Goes deep into the theory of the LDA algorithm, which is the core of this project.
5. Outlines how the data is taken in its raw form and converted into a form that can be analysised with the final step running the LDA Algorithm.
6. Talks about the theory about the results of the LDA Topic Model.

# 1 Text Analysis and NLP: A Background

Data is becoming increasingly more important to both businesses and individuals.  Businesses are generating vast quantities of data, consequently there is a desire to investigate a utility for this data. For example, a business might sell their goods on Amazon and receive thousands of customer reviews a day. Data is traditionally thought of as being spreadsheets with cells being populated by numbers, however, the textual-review data, as used as an example above, also has it's uses. In the case of reviews, it represents a sign of the quality of that product, this "sign" is in essence a piece of data. In the same way, social media represents a vessel of data that represents the thoughts and feelings of it's users; that is, there are reams of textual data that indicate a feeling and opinion about a given topic. From casually browsing Twitter for example,  a few Tweets can be read regarding the latest news event and after reading five to ten Tweets it could be conclude how the users of Twitter are feeling about this event.


Twitter, a microblogging site, is one of the most popular social media platforms and plays a huge role in influencing and expressing public opinion. About 500 million Tweets are published everyday - where each Tweet is a message limited to 288 characters [@twit2]. It's an important tool to understand the public's opinion and emotio -  having three hundred and thirty million users ech month [@twituse]. this is done via "Sentiment analysis" of the Tweets.



That act of browsing and reading of Tweets is effectively a data gathering exercise to discern the opinion of the people. Since language is the way humans interface and naturally share data, it's completely natural to humans to read these words on a screen, coming from multiple Tweets, and form a single opinion about how people must be feeling about that topic.  For example, looking at Tweets that speak about tax increases, ten Tweets may be read, seven of which may criticise this change in policy, two of which may praise it, and one that is in between those two feelings. From this a reader of these Tweets can conclude that the attitude to increasing taxes is largely negative, with seven of the ten Tweets carrying negative connotations. 

In addition to Tweets being useful in gauging opinion of it's users, as time goes on more people are using Twitter - meaning there can be varied opinions on a particular topic. With the advent of smartphones across the globe and the ease of access of internet, the volume of users voluntarily inputting text data has exploded; adding to the importance of being able to analyse this data in a statistically structured way. 


In terms of how this data is analysed, the problem with text data is that it is unstructured -  unlike numbers that represent financials or demographics with set numerical rules; and therefore it is inherently difficult to work with. Text is often hard to interpret to a machine, with it's grammar, syntax rules and complex patterns and as a result it is not straight forward to carry out statistical techniques on text data.

There are however rigorous statistical techniques that have been developed that allows text data to be processed statistically. Under the umbrella known as "NLP" - Natural Language Processing, these techniques aim to understand text-based data. NLP, where a Natural Language is any language that evolved with humans to communicate, uses the linguistic rules that every language has in order to extract information from pieces of text. It enables computers to process and understand human language. Using these techniques it's possible to gauge the opinion of Twitter's userbase using Sentiment Analysis [@twitsent1]. 

## 1.2 Sentiment Analysis

Sentiment analysis is a way to evaluate emotional states and subjective information from text data. It tries to discern the opinion of the author of piece of text, where a piece of text may be a produce review or a tweet, as discussed previously, and in turn is able to draw conclusions of the sentiment of groups of texts. For example, sentiment analysis can be used to online movie reviews to give a rating to a movie [@sa1].  It can also be used to classify product reviews depending on their sentiment [@sa2]. These methods classify texts into positive, negative and neutral sentiments in various different ways [@sa3]. 

Different machine learning methods are used in order to classify texts into sentiments using classifiers such as Naive Bayes (NB) [@nb1] , Maximum Entropy [@me1] and Support Vector Machines (SVM) [@svm1]. There are four basic sentiment approaches [@sa3]:

* Supervised Machine-Learning based
* Ensemble Methods
* Lexicon-based
* Hybrid


Supervised Machine-Learning methods take in labeled data;  texts are have been labelled positive, negative and neutral.  The classifier is trained on this data and a test dataset is used to gauge its performance. @slm1 developed a model that obtained sentiment from twitter data, comparing SVM and NB classifiers, with SVM performing better.  Ensemble methods combine multiple classifiers in order to get more accurate results [@ens1]. Using a dictionary of words that have attached a sentiment rating to each word, the Lexicon-based methods attempts to match up words in pieces of texts to the lexicon to obtain the sentiment of the text [@lex1].  This is an unsupervised approach that doesn't require unlabeled data. @unsuper1 stated that it is an easy task to collect huge amounts of unlabeled data from social networks, however for model  training purposes, manually labeling these data is very costly. For that reason supervised sentiment analysis methods are extremely important - especially since the volume of unlabeled data continues to increase. The Hybrid system uses a combination uses various lexicons and classifiers; @hybrid1 combined machine learning, ruled based and lexicon based methods to obtain results, achieving good outcome measures.

Another way to perform sentiment analysis is by using  Topic Modeling. This aims to classify unlabeled data into topics. For example, when analysing a random sample of one hundred tweets, ten percent of those tweets may belong to the topic of politics, fifty percent may belong to sport, and forty percent may belong to music. This assignment to topic process is done via statistical methods which are explored further into this report. Topic modeling has been successfully used in analysing online reviews for airlines [@topmod2] and found to improve performance of sentiment classification [@topmod3] . The benefit of using Topic Modeling to describe the sentiment of texts is it has the ability to classify texts into finer detail than the polarity analysis described previously, where texts are classified as positive, neural or negative - Topic Modeling can split a corpus of text into any number of topics. It also has the potential to be more accurate in sentiment assignments across different domains. For example, the word "unpredictable" in the phrase "unpredictable steering" would be perceived as the lexicon algorithm described previously as negative, but in the domain of movies, the same word in the phrase "unpredictable plot twists" would denote something positive [@topmod1].  Positively and negativity is contextual - a tweet that says "Bitcoin is going to the moon, buy bitcoin" may be classes as neutral with lexicon methods, however this is clearly a positive tweet in the domain of Bitcoin.  This results in topic modeling being an unsupervised method, that doesn't require labeled data, that is highly portable to other domains [@topmod1; @topmod2]. Topic Modeling has been found to be a successful method in undertaking sentiment analysis of Twitter data [@toptweet1; @toptweet2; @toptweet3]. There are also many other machine-learning methods that analyse the sentiment of tweets [@twitsent1] with a high degree of success.  


## 1.3 Supervised and Unsupervised Machine Learning with NLP

Supervised machine learning is taking labeled data and using it to express the relationship between the labels and the independent variables in the data. In the running example, this would mean taking Twitter data that express an opinion about Covid-19 and manually labeling them "misinformation" if it contains misinformation and "information" is it contains real information. The relationship could then be explored between the text and the labels and this relationship can be used to predict if a new, unlabeled and unread Tweet as having misinformation to a certain percent of accuracy.

However, since there is exponentially more text information available, it is extremely inconvenient for these texts to be labeled since it requires a human to read each text. Therefore it is common to apply unsupervised (unlabeled) statistical techniques to this type of text data. There are a  family of machine learning algorithms that try to discover latent hidden structures and patterns in unlabeled text data from their various attributes and features. Several unsupervised learning algorithms are also used to reduce the feature space, which is often reduced data from a higher dimension to one with a lower dimension. This dimensionality reduction can be seen as taking a large number of documents and assigning them into a relatively small amount of groups, where each group has its own topic which describes broadly the type of Tweets the group contains. The statistical techniques that carry out this process are known as "Topic Models". 



## 1.4 Twitter's impact on the stock market

Traditionally, predicting the stock market was done by using random walk theory and Efficient Market Hypothesis (EMH) which claims that prices are driven by new information - i.e news [@stock1]. News in unpredictable and follows a random walk and cannot be predicted with more than fifty percent accuracy [@stock2]. This notion has been contradicted by a body of research and there are papers that suggest that the stock market doesn't follow a random walk and can partly be predicted [@stock2].  There have been published papers that have investigated whether public mood with respect to a particular company or asset, garnered from twitter, was correlated with the stock market and found that this was indeed the case [@stock3; @stock4] and some papers have found this link using the sentiment anlaysis described above [@stock5; @stock6]. 



## 1.5 How computers use text data

A "Document" is a single separate piece of text. In the context of Tweets, a document would be one Tweet. If news articles were to be analysed, a document would be one article. Documents comprise a "Corpus", which is all the documents combined into one dataset

Machine learning techniques traditionally need numerical data in order to perform an analysis. A logistic regression is a simple machine learning tool that can express the segmentation of a dependent variable with two groups as a function of its independent variables. It's possible to have exactly the same framework using text data.

To give a toy example, suppose there are ten documents with each document being a single Tweet, five of which give misinformation about Covid-19, and five of which give true information about Covid-19.  The aim is to build a logistic regression such that the relationship between the text in each Tweet and its label of misinformation/information is obtained. However, a logistic regression cannot do this because it can only interpret numerical data. The logical step then is to convert these documents (Tweets, in the example) into numbers. This is done via "Vectorisation" of each document. The corpus of documents are represented as a matrix where each row is document with its vector representing the words in the document. The columns are all the words in the corpus. The numbers in the matrix represent the amount of times a word (the columns) appears in a document (which is a row). Each row then represents all the text in one of the documents. This process converts text data into numerical data and as a result machine learning techniques can be used on this matrix.

Creating the matrix above requires processing steps so that it is primed for statistical analysis to be carried out. The process of splitting each document into it's individual words is called "Tokenisation". This is a list of words that appear in each document which is part of the "Vectorisation"*process. For each document, this list of words is vectorised using one of the following techniques:

1. Document-Term Matrix - a matrix where each word is a column, each row is a document (one piece of text data) and the number in each cell is the number of times the column's word appears in the document's (row) text. An example of this matrix is given in Figure \ref{fig:bowmat}

\begin{figure}
\includegraphics[width=1\linewidth]{images/16034397439042_surfin bird bow} \caption{A toy example of a document-term matrix which is part of the pre-processing step in order to perform NLP.}\label{fig:bowmat}
\end{figure}


2. Term Frequency ??? Inverse Document Frequency (TF-IDF). This generates a matrix that assigns a weight to each word which signifies the importance of the word in the document and corpus. This is more sophisticated than the term-document matrix approach. The importance of a word in each document is based on the amount of times it appears in the document and also how rare the word is in the corpus. Rarer words in a document are given a higher weighting as rare words help distinguish what topic the document belongs to. For example, if a document contains a rare word "nuclear", this is likely to be a good indicator that the document is about the topic "energy". If a word appears frequently in a document but infrequently across the whole corpus, it is given a large weight. 

3. word2vec -  uses a pre-trained neural network model to learn word associations from a large corpus of text. Once trained, such a model can detect synonymous words or suggest additional words for a partial sentence. This is even more sophisticated than the above as context of the word is taken into account.

Using one of these methods to convert a corpus of documents into a matrix, machine learning techniques can be used in order to classify documents and other operations.


## 1.6 Applications of NLP

* **Machine Translation** - translating text from one language to another. This is becoming more accurate with the aid of Deep Learning [@mt1].
* **Automatic Speech Recognition (ASR)** -  Recognition of vocal language and converting it to text, aiding human-human and human-computer interactions. As Machine Learning techniques have access to more powerful computers, the amount of errors in ASR are reduced [@asr1].
* **Question Answering Systems** - Aims to provide quick answers to user questions from a collection of documents in a database [@qas1] For example chatbots often used to interface between clients and businesses. Another example is Personal Assistants such as Siri. This has been successfully used in the domain of medical questions [@qas1]. 

* **Text Summarisation** - Taking a large piece of text and condensing it but retaining the original meaning. This has been successfully used to summarise large pieces of legal texts into smaller summaries for lawyers and citizens to do research relates to their case [@sum1]. 

* **Text Categorisation** - Classifying texts. For example, perhaps given a piece of text, a Tweet in this example, analysing the words gives rise to ability to classify it as a Tweet that contains misinformation about Covid-19 to a certain level of accuracy using machine learning techniques. This has been successfully performed using Recurrent Neural Networks as the classifying algorithm [@textclass1].

* **Text Analytics** - Deriving insights from text data. Methods include clustering, summarisation, sentiment analysis. An example application of this is Spam detection. Statistical techniques are used to classify certain emails as spam so they don't reach a user's inbox.


* **Topic Modeling** - Classifying a corpus of documents into topics. This finds hidden structures in large amounts of data and has been applied to information retrieval, social media analysis and text mining [@topmoda1]. Within these methods, topic modeling has also been applied to a large amount of domains, from medical sciences, software engineering, georgraphy and political science [@topmoda1] demonstrating its versatility.




<!-- //from] -->
<!-- wiki 2000s: With the growth of the web, increasing amounts of raw (unannotated) language data has become available since the mid-1990s. Research has thus increasingly focused on unsupervised and semi-supervised learning algorithms. Such algorithms can learn from data that has not been hand-annotated with the desired answers or using a combination of annotated and non-annotated data. Generally, this task is much more difficult than supervised learning, and typically produces less accurate results for a given amount of input data. However, there is an enormous amount of non-annotated data available (including, among other things, the entire content of the World Wide Web), which can often make up for the inferior results if the algorithm used has a low enough time complexity to be practical. -->



<!-- from dataversity -->
<!-- Natural Language Processing (NLP) is an aspect of Artificial Intelligence that helps computers understand, interpret, and utilize human languages. NLP allows computers to communicate with people, using a human language. Natural Language Processing also provides computers with the ability to read text, hear speech, and interpret it. NLP draws from several disciplines, including computational linguistics and computer science, as it attempts to close the gap between human and computer communications. -->

<!-- Generally speaking, NLP breaks down language into shorter, more basic pieces, called tokens (words, periods, etc.), and attempts to understand the relationships of the tokens. This process often uses higher-level NLP features, such as: -->

<!-- Content Categorization: A linguistic document summary that includes content alerts, duplication detection, search, and indexing. -->
<!-- Topic Discovery and Modeling: Captures the themes and meanings of text collections, and applies advanced analytics to the text. -->
<!-- Contextual Extraction: Automatically pulls structured data from text-based sources. -->
<!-- Sentiment Analysis: Identifies the general mood, or subjective opinions, stored in large amounts of text. Useful for opinion mining. -->
<!-- Text-to-Speech and Speech-to-Text Conversion: Transforms voice commands into text, and vice versa. -->
<!-- Document Summarization: Automatically creates a synopsis, condensing large amounts of text. -->
<!-- Machine Translation: Automatically translates the text or speech of one language into another. -->



\newpage




# 2 Aims


## 2.1 Motivation for and summary of the project

In early 2021, the impact of social media on the stock market gained notoriety when the price of GameStop (GME) stock went from hovering around \$20 to reaching the lofty heights of $347 dollars in just one month. This increase was attributed to the collective frenzy that took place on the social media platform of Reddit, specifically the sub-forum known as "wallstreetbets" that encouraged users to buy more stock which in turn elevated the stock to those high prices.

This was a clear demonstration of how public word-of-mouth can impact the stock market, but this kind of thing has happened for time immemorial. Before social media, word-of-mouth opinions about stocks would have also made an impact on the price of stocks. The difference now is that these opinions are much more visible, and importantly for this project, they are recorded. 

This project aims to use Topic Modeling on Tweets that refer to the company Gamestop and the cryptocurrency Bitcoin. Using Topic Modeling,  Tweets are classified into groups depending on the topic discussed in each Tweet. It further looks to investigate how these topics of public opinion influences the price of the assets Gamestop and Bitcoin. The intention is to find a topic that can be classified as positive in regards to the asset (where asset denotes Bitcoin or Gamestop stock). Time Series analysis is then undertaken to explore the impact of the frequency at which this topic is posted and to see if it relates to the price of the asset. Since a constant random sample of tweets are taken daily, the frequency of a positive topic represents how positive the public feel in regards to the asset for that particular day. For example, if thirty percent of the sample of tweets one month fell into the topic "positive attitude" towards the asset, and the next month this proportion was fifty percent, perhaps this change in proportion of positivity would have an impact on the market. 



## 2.2 An overview of the approach to be taken

There are two main steps when it comes to approaching this analysis.

1. Assigning topics to Tweets that represent opinions about stocks and cryptocurrency.
2. Finding the relationship between these opinions and change in asset price.

Reading social media posts gives the reader an understanding of the opinion and feeling of the public for a particular topic. For example,  a social media poster may simply say *"I believe Bitcoin is going to be worth ??1.2 million one day"* and although the poster's believed price may not be remembered by the reader, the sentiment of the post - that bitcoin is going to increase in value - will be.

In this example for simplicity and demonstrative purposes,  consider trying to assign Tweets to just two topics - "positive" or "negative"; the above example Tweet can be classified as a "positive" sentiment for Bitcoin. A post using skeptical language in regards to bitcoin can be classified as "negative". 

Probabilistic Topic Modeling, a type of machine learning algorithm outlined by @blei, can be used to analyse posts/articles for the type of language they use regarding a certain asset at a time point, and the frequency of positive or negative posts can found for a particular asset. This then gives us a "sentiment score" - telling what is the public opinion of the asset, is it mainly positive, negative, or equal? This type of analysis is known as Natural Language Processing (NLP). 


Topic Modeling algorithms analyse the words of the original texts to uncover the underlying topics, where a topic can be interpreted as a sentiment, that run through the Tweets and how the importance of these topics change through time depending on when the Tweet was posted. This project uses uses Latent Dirichlet Allocation (LDA) to allocate Tweets to topics.

Classifying a post/article into positive and negative is a useful way to gauge public opinion, however, positive and negative posts can be classified into further topics. For example, how positive is the post, perhaps the language of the post is hugely positive, so it can be classified as "very positive". Perhaps the language used is positive, but tinged with some negativity of skepticism - and this could be classified as "weakly positive". Further still, within the trading community there are trading behaviours known as "bearish" (many people following the trend of selling) and "bullish" (many people following the trend of buying) and the posts can be further classified into these groups. This report explores how the the impact public sentiment has on the price of an assets Bitcoin and Gamestop.







## 2.3 The Data


Tweets were pulled using the Twitter API. Three hundred Tweets were pulled for each day that contain the terms "BTC" or "Bitcoin" for Bitcoin data, and the terms "GME" or "Gamestop" for the Gamestop data. This data was pulled for Tweets posted from January 1st 2016 until August 31st 2021. The data is extracted as a CSV (comma-separated values) file.

This data will then be run through an LDATopic Modeling pipeline to find the underlying topics in these Tweets. 


\newpage
 
# 3 Topic Models

## 3.1 Introduction


To serve as an introduction to Topic Model a toy example is considered: Suppose there is data comprising twenty thousand Tweets expressing opinions about Covid-19. Using unsupervised machine learning techniques and the vectorisation of text explained above, the data can be mathematically expressed and analysed. These Tweets can be grouped into three groups using machine learning algorithms with the following aim: Tweets in group one contains text that refer to Covid-19 cures, group two contains Tweets about Covid-19 vaccines and group three contain Tweets about Covid-19 prevention. These groups can be interpreted to be topics. To reach this goal, a range of statistical techniques are used called Topic Models.

Topic models extract the distinguishing concepts, or topics, from the the corpus (that is, all the documents). It groups up documents into topics without human intervention and judgment. These topics can include opinions or facts. The models then use statistical techniques to explore the hidden and latent structures in the corpus in order to group up documents, where the hidden structures in the corpus refer to topics.


## 3.2  Methods for topic modeling

There are various algorithms to carry out Topic Modeling.

The following three methods are explored in the following chapters:

* Latent Semantic Indexing (Section 2.3).
* Latent Dirichlet Allocation (Section 2.4).
* Non-negative matrix factorization (Chapter 3).

## 3.3 Latent Semantic Indexing (LSA)

Latent Semantic Analysis takes the matrix that contains the documents and terms and decomposes it into a document-topic matrix and topic-term matrix - this is the TF-IDF matrix explained in Section 1.1. Since the matrix contains all the terms in the corpus and a lot of terms will not be important in defining the topics, this matrix will be sparse. In order to find only a few topics this matrix needs to undergo dimensionality reduction. This reduction can be done using truncated Singular Value Decomposition (SVD); a technique that factorises a matrix into the product of 3 separate matrices, as seen in Figure \ref{fig:lsa_svd} [@albrecht]. $V$ represents the TF-IDF matrix, the matrix  $\Sigma$ represents the singular values of $V$ and to keep the most important topics only the top $t$ largest singular values are kept, which means keeping only the first  $t$ columns of $U$ and $V^*$. In other words, $t$ is a hyperparameter which adjusts the number of topics outputted by the SVD.

$U$ represents the document-topic matrix, $V^*$ represents the term-topic matrix. For both matrices the columns represent topics. For $U$, the rows represent the document vectors expressed in terms of topics. The rows in $V$ represents the term vectors expressed in terms of topics. The numbers in the cells represent how important the topic is for the term or document in $V^*$ and $U$ respectively. In this way, the documents can be assigned a topic by choosing the highest topic importance value [@albrecht].


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/lsa} 

}

\caption{Singular Value Decomposition of the document-term matrix used in Latent Semantic Indexing to perform Topic Modeling.}\label{fig:lsa_svd}
\end{figure}






## 3.4 Non-negative matrix factorisation

One way to find the hidden topics of a corpus is the factorisation of the "document-term matrix" - this is the a matrix with the rows being the documents and the columns being the words in the whole corpus - and if a document contains a particular word in the corpus it gets a value of the amount of times the word appears in the document in that cell where the column is that particular word [@albrecht]. This matrix has only positive-value elements, consequently methods from linear algebra can be used to order to represent the matrix as the product of two other non-negative matrices. The original matrix is called $V$, and the factors are $W$ and $H$:

\begin{equation}
V \approx W \cdot H,
\end{equation}


and can the factorisation is visualised in Figure \ref{fig:nmf} [@albrecht].

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/lsa} 

}

\caption{Non-negative matrix factorisation of the document-term matrix, a technique used to perform Topic Modeling.}\label{fig:nmf}
\end{figure}
In the context of text analytics, both $W$ and $H$ have an interpretation. The matrix $W$ has the same number of rows as the document-term matrix and therefore maps documents to topics (document-topic matrix). $H$ has the same number of columns as features, so it shows how the topics are constituted of features (topic-feature matrix). The number of topics (the columns of $W$ and the rows of $H$) can be chosen arbitrarily. The smaller this number, the less exact the factorization.


The above two methods are computationally much cheaper than LDA but lacks in the ability to find multiple topics in single documents - this ability is gained by adding a Dirichlet prior on top of the data generating process, as discussed below. 




\newpage



# 4 Latent Dirichlet Allocation


## 4.1 LDA Introduction


LDA considers each document as consisting of a mixture of different topics and these topics are made up of a mixture of words. To ensure that the number of topics per document is low and to have only a few important words constituting the topics, LDA uses a Dirichlet prior in its statistical process. This is applied both for assigning topics to documents and for assigning words to the topics. After these initial assignments, a generative process begins. It uses the Dirichlet distributions for topics and words and tries to re-create the words from the original documents with stochastic sampling. This process has to be iterated many times and is therefore is computationally intensive. The results can be used to generate documents for any identified topic. For a given corpus of documents, each document can be represented as a statistical distribution of a fixed set of topics. LDA assumes that each document is generated by a statistical generative process governed by a document's topic distribution, and the topics' word distributions. After this generative process has taken place and when the documents generated that most closely resemble the actual documents in the corpus, the topic distribution of the documents can be observed to assign documents to topics.

## 4.2 LDA In-Depth

Topic models are based on the idea that each document, that is each piece of text, are a mixture of topics. For example, perhaps there is a news article about electric vehicles and how it impact the economy. In this example it can be said this document (i.e the article) is a combination different topics, not just one - it could well be 50% "transport", 30% "technology" and 20% "economy". This distribution is labeled as $\theta$. Figure \ref{fig:plot1} shows an example for this distribution.



\begin{figure}

{\centering \includegraphics[width=0.8\linewidth]{00---Actual-Dissertation---Copy_files/figure-latex/plot1-1} 

}

\caption{Example of a multinomial distribution of the Topics in LDA for a single document.}\label{fig:plot1}
\end{figure}

Documents have a probability distribution over topics, as described in the example. A document chosen at random may have any percent of $n$ different topics. Equally, it can be said a topic is a distribution over words. For example, the topic transport can be defined by a list of words that appear in that topic with varying frequencies. For example consider that the topic "transport", for simplicity, comprises three words - car, bus, roads - "car" appears 70% of the time for that topic, "bus" 10% and "roads" 20%. Figure \ref{fig:plot2} shows a toy example of a word distribution for the topic "transport" in from Figure \ref{fig:plot1}. The other two topics in Figure \ref{fig:plot1} would have their own word distributions.

\begin{figure}

{\centering \includegraphics[width=0.8\linewidth]{00---Actual-Dissertation---Copy_files/figure-latex/plot2-1} 

}

\caption{Example of a multinomial distribution of the words in a Topic in LDA.}\label{fig:plot2}
\end{figure}



This is analogous to the reality where, if you were to read an article you might discern it's topic by considering the words used in the article  and also the frequency of the words used in the article to make the judgment. For example the word "car" would appear more often than petrol station across all "transport" documents - leading to the conclusion that the main topic would be indeed "transport" and not "energy" which might be the case if the phrase "petrol Station" were to be more prevalent. 


A topic model is a generative process that describes a statistical procedure through which the observed documents could be produced with the highest likelihood.  A generative process for documents describes how words in documents might be generated based on variables that are not seen - latent variables. It aims to find the best set of latent variables that explains the documents in the corpus. These latent variables dictate the probability distributions described above in the toy examples. The aim of LDA is define these latent variables such that the generated documents resemble the observes documents as closely as possible.

## 4.2.1  The Generative Process of LDA

A document in the corpus can be assumed to be created by first choosing a distribution over topics  - just like how an author might decide they are going to write about two topics with proportions 60% "transport" and 40% "technology". These two topics are then the distribution of topics per document.  After, for each place where a word appears in the document, a word is chosen from a distribution of words from the selected topic; where the selected topic is sampled from the distribution of topics per document. For example, the topic "transport" is sampled for the first word, and within topic "transport" there is a distribution of words taking the form of 60% "car" and 40% "bus". A word is chosen from this distribution and since "car" is most likely, for this first word of the document "car" is chosen. The role of LDA is inferring these topic and word distributions through which the documents most likely have been generated.

In practice when using LDA, each document is a distribution over many topics, but typically each document is dominated by one or two topics - just as in real life where an article typically only has one or two topics. Each of those topics have also a distribution of many words. For example a topic may have thousands of words within its word distribution. Roughly speaking,  the Dirichlet distribution is used to constrain the amount of topics that define a document, and also the amount of words that define a topic. This yields results which are easier to interpret. 

The generative process which gives topic and word distributions is useful -  by seeing the distribution of words in a topic the topic can be given a name. For example if the words "Fight" and "Glove" were grouped together in a distribution with 80% and 20% probability respectively, it can easily say that this topic is about boxing.

### 4.2.1 Notation

The distribution over words within the document specified by the models given by:
\begin{equation}
P(w_i) = \sum_{j=1}^{T}P(w_i|z_j =j)P(z_i=j),
\end{equation}
where $P(z_i=j)$ is the probability that the *j*th topic was chosen for the *i*th word,  $P(w_i|z_j =j)$ is the probability of the word $w_i$ under topic *j*
and $T$, the number of topics.








## 4.3 The Dirichlet Distribution




The Dirichlet distribution $Dir(\alpha)$ is a family of continuous multivariate probability distributions parameterised by a vector $\alpha$ of positive reals. It is a multivariate generalisation of the beta distribution. The Dirichlet distribution is a conjugate prior to many important probability distributions and specifically a conjugate prior to the multinomial distribution - the distribution LDA uses for sampling topics and words. The Dirichlet distribution is used in LDA to sample a multinomial distribution of topics and words. It is useful because it allows for topics and words to have varying probabilities of being sampled, allowing for documents to be confined to one to two topics which is a more natural way to interpret a document.


Multinomial distributions can be drawn from a Dirichlet distribution which is needed to sample $\phi$ and $\theta$, where ${\phi}^{(j)} = P(z=j)$ is a multinomial distribution over words for topic *j* and ${\theta}^{(d)} = P(z)$ is a multinomial distribution over topics for document *d*. These are the latent variables which ultimately LDA aims to approximate. The parameter $\theta$ indicates what topics are important for a particular document and $\phi$ indicates what words are important in each topic. 


### 4.3.1 Toy Example for the Dirichlet Distribution


In this example inspired by @diriex, suppose a six-sided dice is to be manufactured with the outcomes of the dice being only the numbers one, two and three. If this is a fair die then the three outcomes with have the same probability of $\frac{1}{3}$. The probabilities of the outcomes can be represented by the vector $\boldsymbol{\mathbf{\theta}} = (\theta_{1}, \theta_{2}, \theta_{3})$. Since the sum of these probabilities must be equal to one and none of the probabilities can be negative, rolling the dice can be described by a multinomial distribution. Since each $\theta_{i} \in [0,1]$ this means the set of allowable values for $\boldsymbol{\mathbf{\theta}}$ is confined to a triangle. The Dirichlet distribution is used to define the probability density at each point on this triangle. 


The Dirichlet distribution defines a probability density for the vector $\boldsymbol{\mathbf{\theta}}$  and is given by 
\begin{equation}
Dir(\boldsymbol{\mathbf{\theta}}| \boldsymbol{\alpha})= \frac{1}{beta(\boldsymbol{\alpha})}\prod_{i=1}^{K}\theta_i^{\alpha_i -1}
\end{equation}

where $K$ is the number of variables in the vector $\boldsymbol{\mathbf{\theta}}$. 

The Dirichlet distribution is parameterised by the vector $\boldsymbol{\alpha}$ which has the same number of elements as $\boldsymbol{\mathbf{\theta}}$, that is $K$. $P(\boldsymbol{\mathbf{\theta}}|\boldsymbol{\alpha})$ can be interpreted as giving the probability density associated with the multinomial distribution $\boldsymbol{\mathbf{\theta}}$ given the Dirichlet parameter of $\boldsymbol{\alpha}$.

The Dirichlet distribution is a multivariate generalization of the beta distribution. The beta distribution is defined in the interval [0,1] and has two parameters, $\alpha$ and $\beta$. This distribution is a conjugate prior for the binomial distribution, hence the multivariate form of the beta - that is the Dirichlet distribution is a prior for the multivariate form of the binomial distribution, the multinomial distribution. Figure \ref{fig:betadist} [@diriex] shows how the distribution profile is changed depending on the values of $\alpha$ and $\beta$. When these parameters are less than one, it can be seen that the edges of the distribution are regions with high probability density. In the context of topic modeling, this can be useful because the samples from this distribution will be generally from one topic since the extremes of the distribution can be linked to a topic. 



\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/Beta distribution} 

}

\caption{The beta distribution with different parameters, which is the univariate version of the Dirichlet distriubtion used in the LDA Topic Modeling algorithm.}\label{fig:betadist}
\end{figure}


The Dirichlet distribution can also be visualised where $K=3$; in other words this serves as a toy example where the number of possible topics for the corpus is 3. This means visualising the distribution on a triangle simplex for differing values of $\alpha$ as to investigate how changing $\alpha$ changes the probability distribution. 


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/dirichlet2} 

}

\caption{How the probabiliy distribution for the Dirichlet changes with varying values of alpha - one of the hyperparameters of the LDA algorithm. This hyperparamer influences the distribution of the  multinomial distrubtion of topics for the documents, and the multinomial distribution of the words in each topic.}\label{fig:dirivis2}
\end{figure}

Figure \ref{fig:dirivis2} shows that when $\alpha_i < 1$, or  that is to say where $\boldsymbol{\alpha} < K$, higher distribution areas are in the corners of the triangle [@diriex]. the corners of the triangle can be thought of as a topic, since when sampling from the Dirichlet a topic is being sampled. As stated above, this is useful because a document is typically made up of one or two topics in the real, not a mixture of all possible the topics as would be the assumption if the Dirichlet distribution with all $\alpha_i = 1$ would be used, as seen in the second triangle in Figure \ref{fig:dirivis2}. With the SVD Matrix Factorisation approach described previously in section 2.3, each document has a value for each topic, so a document is assumed to be made up of all the topics in the analysis at varying proportions. LDA has the ability to assume a document is made up of a small number of topics using the Dirichlet distribution, which is a natural assumption as often pieces of texts only deal with one or two topics, for example, "technology" and "science".









<!-- The Figure \ref{fig:diripic} below it can be seen that changing the parameter $\alpha$, a parameter of the Dirichlet distribution, varies the probabilities in the multinomial distributions . In the equation in \ref{fig:eq1}below, $m_k$ represents the mean and is analogous to centre of the Dirichlet distribution. When $\alpha * m = 1$ the right hand side of the equation becomes 1 (as the exponent is 0) (every multinomial vector is raised to 0) so all of the vectors $p_k$ are equiprobable and that means all the topics are equally likely to sampled. As $\alpha$ gets larger, the probability distribution concentrates around the mean. -->






 <!-- ```{r diripic, echo=FALSE, fig.cap = "Examples of the Dirichlet distribution with varying parameters", out.width='100%', fig.asp=0.4, fig.align="center"} -->
 <!-- knitr::include_graphics("images/dirichlet2.jpg") -->
 <!-- ``` -->

<!-- Using the Dirichlet prior is useful because it allows the model to estimate that the documents are made up of a few topics with one or two topics being stronger than the others. This is intuitive to how real life documents are made and aids interpetability.  -->

### 4.3.2 Dirichlet in detail

With LDA, there is a  Dirichlet prior on ${\theta}^{(d)} = P(z)$, a multinomial distribution over topics for document *d*. This is a conjugate prior for the multinomial. The probability density of a $T$, the number of topics, dimensional Dirichlet distribution over the multinomial distribution $p=(p_1,...,p_T)$ and is given by:

\begin{equation}
Dir(\alpha_1,...,\alpha_T) =  \frac{\Gamma(\sum_{j}^{}\alpha_j)}{\prod_{j}^{}\Gamma(\alpha_j)}\prod_{j=1}^{T}p_j^{\alpha_j -1}
\end{equation}

The parameters of this distribution are specified by $\alpha_1,..\alpha_T$. Each hyperparameter $\alpha_j$ can be interpreted as a prior observation count for the number of times topic $j$ is sampled in a document before having observed any actual words from that document. The Dirichlet prior on the topic distribution $\theta$ results in a smoothed topic distribution depending on the $\alpha$ parameter. This parameter controls the number of topics for each document or, in other words ,it controls the probability distribution of topics for each document. Sampling from the Dirichlet distribution means getting a multinomial distribution for the topics for that document. The $\alpha$ parameter is a hyperparameter, that is a parameter of parameters, that dictates what the multinomial distribution of topics looks like for each document. The benefit of the Dirichlet distribution is that for $\alpha<1$ the multinomial distribution is often represented by a few topics. If $\alpha>1$ the distribution is quite even - that is the distribution is represented by many topics. It is more natural to assume a document only contains a few topics, just like a document in real life. For that reason an $\alpha<1$ is a useful parameter in topic modeling. 

A Dirichlet distribution prior is also placed on $\phi$, the multinomial probability distribution of words of each of the topics, using the parameter $\beta$. It can be interpreted as the prior observation count on the number of times words are sampled from a topic before any word from the corpus is observed. Just like the $\alpha$ parameter, it controls the word distribution for each topic, either making the probability distribution uniform or having only a few words have high probabilities for each topic. Again, this is important because for some topics not all words are equally as important. For example, in the topic of "Finance", the word "money" defines the topic much more than "bank", since "Finance" can be described without mentioning banks, and also banks may refer to a river bank. 

In summary, there are two Dirichlet distribution used in LDA, and each distribution comes with its own hyperparameter, defined as:

- $\alpha$ - controls how different the probabilities will be for words in the topics.
- $\beta$ - controls how different the probabilities will be for topics in the document.

## 4.4 Plate notation


Plate notation is a way to represent the LDA algorithm [@steyvers], shown in Figure \ref{fig:boxdia} [@steyvers]. Shaded and non-shaded areas indicate observed and latent variables respectively. $\phi$, the distribution of words in the topics, $\theta$, the distribution of topics in each document and $z$, the assignment of word tokens to topics are what three latent variables aim to infer. $\alpha$ and $\beta$ are the hyperparameters that are treated as constants in the model. Arrows represent conditional dependencies between the variables and the plates, the boxes in the figure, refer to repetitions of sampling steps with the variable in the right corner referring to the number of samples. For example, the inner plate that has the bubbles $z$ and $w$ illustrated the repeated sampling of topics and words until $N_d$, the number of words in each document *d*, words have been generated for document $d$. The plate surrounding $\theta^{(d)}$ shows the repeated sampling of a distribution of topics for each document $d$ for a total of $D$ documents. The plate surrounding $\phi^{(z)}$ shows the  repeated sampling of word distributions for each topic $z$ until $T$ documents have been generated.



\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/Box diagram} 

}

\caption{Box diagram describing the generative process of LDA, showing the relationships between the latent variables, hyperparameters and distributions.}\label{fig:boxdia}
\end{figure}




Here is a labeled version of Figure \ref{fig:boxdia}

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/plate dia lab} 

}

\caption{Box diagram with additional notes giving more detail on how the LDA algorithm works.}\label{fig:platedialab}
\end{figure}




The goal of LDA is to estimate $\theta$ and $\phi$ -  latent variables that aren't observed which the model attempts to fit. These latent variables are determined by using the words observed in the corpus, so the document document making process is reverse engineered to get the parameters that make those observed words. LDA aims to find the parameters that maximise the likelihood of the words observed in the corpus. 

Unlike statistical models like OLS, there is no analytic solution with LDA. The best solution can be approximated in an iterative way, using steps and getting closer to the best solution. One way to do this is with Gibbs Sampling. 

## 4.5 Gibbs sampling

The intuition behind Gibbs sampling is that the topics of all  words are assumed to be known except for the word token of interest. For this word token of interest, a topic must be chosen from the topic distribution for the document. The model prefers to choose a topic that is already present in the document. At the same time, the model also wants to pick a topic that the word already occurs in. So there are 2 probability distributions: one that favors the topics already present in the document and one that favors the topics that this word is already present in.

Taking these two probability distributions together, a joint probability distribution is gained and will give you the chance of picking a topic for this one missing word.

Then, using the iterative Gibbs Sampling in this way:

1. Start with random sampling -  assigning every word in every document to one topic.
2. Then iterate over all the words and all the documents. For each word in each document,  the proportion of topics is computed in that document (disregarding the word itself). 
3. Compute the proportion of topics for that word (disregarding the word itself). I.e what are the topics that the word occurs in and at what frequency?
4. Multiply these two distributions to get a joint probability distribution.
5. Choose a new topic from that probability distribution and then update the proportions.
6. Repeat this step, going to the next word, and eventually to the next document, until every word is updated in every document.
7. Initially there were completely random distributions, but  after these steps there are slightly less random distribution because the this method, for the words, tends to choose topics that are consistent with that document. 
8. Each time these steps are run the solution becomes more stable. These steps are run until it converges to a stable solution.


### 4.5.1 Gibbs Sampling in detail

Gibbs sampling is a form of the Markov Chain Monte Carlo sampling method which samples values from high-dimensional distributions. It simulates a complex distribution by sampling over marginal distributions given a specific value of the other distribution. This is done sequentially until the sampled values approximate the desired distribution.


The Gibbs procedure considers each word token in a document and estimates a probability of assigning the current word token to each topic, conditioned on the topic assignments on the topic assignments to all the other word tokens.  From this conditional distribution, a topic is sampled and then is the new topic assignment for this word token. This conditional distribution can be written as:

\begin{equation}
P(z_i=j \mid z_{-i}, w_i, d_i,)
\end{equation}

$z_i=j$ represents the topic assignment of token $i$ to topic $j$, $z_{-1}$ refers to the topic assignments of all the other word tokens and the dot refers to all other known or observed information as such as all the other word and document indices $w_{-i}$ and $d_{-i}$ and hyperparamters $\alpha$ and $\beta$.

This probability is proportional to:
\begin{equation}
P(z_i = j | z_{-i}, w_i, d_i, \cdot) \propto \frac{C^{WT}_{w_ij} + \beta}{\sum_{w=1}^{W}C^{WT}_{wj} + W\beta}\frac{C^{DT}_{d_ij} + \alpha}{\sum_{t=1}^{T}C^{DT}_{d_ij} + T\alpha},
\end{equation}
where $C^{WT}$ and $C^{DT}$ are matrices of counts, $C^{WT}_{wj}$ contains the number of times word $w$ is assigned to $j$, not including the current iteration of $i$, $C^{DT}_{dj}$ contains the number of times topic $j$ is assigned to some word token in document $d$, not including the current iteration $i$.
<!-- (\#eq:test) -->
<!-- \@ref(eq:test) cant get latex referencing working -->

Note that Equation 6 isn't the probability of assigning a word token to topic $j$ but it is proportional to the right hand side of the equation. The actual probability is produced by dividing the equation by the sum of all topics $T$. The left part of the right hand side of the equation can be seen as the probability of word $w$ under topic $j$. The right part is the probability that topic $j$ has under the current topic distribution for document $d$. 

Because the top of the left part $C^{WT}_{wj}$ contains the number of times word $w$ is assigned to $j$, it means that when many tokens of a word have been assigned to topic $j$ across the documents, it will increase the probability of assigning any particular token of that word to topic $j$.

At the same time, looking at the right part of the right hand side, if topic $j$ has been used multiple times in one document, this means a higher $C^{DT}_{dj}$, the number of times topic $j$ is assigned to some word token in document $d$, it will increase the probability that any word from that document will be assigned to topic $j$.

As a result, words are assigned to topics depending on how likely the word is for the topic and also how dominant that topic is in a document.

1. Assign each word token to a random topic.
2. Decrease the count matrices $C^{WT}$ and $C^{DT}$ by one for the entries that correspond to the current topic assignment.
3. A new topic is sampled from the distribution in Equation 4 and the count matrices $C^{WT}$ and $C^{DT}$  are incremented with the new topic assignment.
4. Repeat these steps for all N word tokens in the corpus, achieving one Gibbs sample.
5. Repeat steps 1-4 until a stable solution converges - that is the posterior distribution over topic assignments are approximated as best as possible.

### Obtaining the topic and word distributions

Recall that:

- ${\phi}^{(j)} = P(z=j)$, a multinomial distribution over words for topic *j*
- ${\theta}^{(d)} = P(z)$, a multinomial distribution over topics for document *d*

These can be obtained as follows:

\begin{equation}
{\phi}'^{(j)}_i= \frac{C^{WT}_{ij} + \beta}{\sum_{k=1}^{W}C^{WT}_{kj} + W\beta}
\end{equation}
and

\begin{equation}
{\theta}'^{(j)}_j= \frac{C^{DT}_{dj} + \alpha}{\sum_{k=1}^{T}C^{DT}_{dk} + T\alpha}.
\end{equation}


See [@steyvers] for more detail on this process. 

## 4.6 Benefits of LDA


LDA adds a Dirichlet prior on top of the data generating process, meaning NMF qualitatively leads to worse mixtures. It fixes values for the probability vectors of the multinomials, whereas LDA allows the topics and words themselves to vary. Thus, in cases where it is believed that the topic probabilities should remain fixed per document (oftentimes unlikely) ??? or in small data settings in which the additional variability coming from the hyperpriors is too much ??? NMF performs better [@nmf3]. There is support for using NMF over LDA where NMF was found to perform better [@nmf3] however NMF was also found to have the tendency to learn more incoherent topics
than LDA and LSA  [@nmf2]. For applications in which a human end-user interprets and interacts the generated topics, the
flexibility of LDA and the higher coherence scores of LDA means it is a strong algorithm to use for Topic Modeling [@nmf2].

\newpage


\newpage

# 5 Pipeline Architecture

All code was written and executed using Python 3.7.

## 5.1 Pre-processing




Before an LDA model can be built the Tweets must be pre-processed (cleaning of the data). Pre-processing is done to remove noise from the Tweets so that the model can uncover the meaning of the Tweets more effectively. Before the processes undertaken all characters in the corpus are converted to lowercase and all subsequent filters act independently on the character's case. Uppercase is used to describe the filters in this document to fit within the grammatical rules of producing this report only. 


Pre-processing is an important step in sentiment analysis. Various papers have showed an improvement in sentiment analysis comparing undertaking pre-processing steps against leaving the text in it's native form and shown improvements on outcomes on twitter data [@prep2; @prep3]. @prep1 showed improved accuracy of sentiment analysis of twitter data using the pre-processing steps of lemmatisaiton, replacing repetitions of punctuation, replacing contractions (such as "don't") and removing numbers. These steps are taken in the analysis - however numbers that appear in words are kept. It also recommended keeping negations as is - in this report a contraction and a negative such as "don't" has the apostrophe removed and kept as "dont" upon the recommendation of the paper.   Stopwords are a list of words which are considered to be noise in NLP. This is a list of words which is removed from each document. The benefits of removing stopwords has been debated in recent years [@stopwords1] and [@stopwords1] shown that using pre-compiled lists of stopwords negatively impacts the performance of Twitter sentiment classification approaches. Stopwords will not be removed from this analysis. The steps for pre-processing in this analysis are described later on in the chapter. 

Emojis and emoticons are icons which people use to express sentiment. Using emojis and emoticons in sentiment analysis has been shown to improve classification of sentiment [@emoji1] and coincide with the sentiment of a tweet [@emoji2]. In thsis analysis emoticons and emojis are translated to words or phrases. Emojis are translated using the "emoji" package [@emojipack] and emoticons using a list of emoticons and their word meanings from wikipedia [@emoticonwiki]. 

There are two main pre-processing steps that were undertaken on the data.

1. Corpus-wide noise: the removal of Tweets that are not relevant to the analysis. Examining the Tweets uncovered a portion of Tweets that don't express opinions of people and are most likely Tweets by bots. Here is an example Tweet

> "Current price: 433.54$ $BTCUSD $btc #bitcoin 2016-01-02 18:40:05 EST"

This type of Tweet appears multiple time in the dataset and more than likely is produced by a bot that serves to update users of Twitter the price of bitcoin. There are many Tweets such as these that can judged as being produced by bots and removing them from the data is important because they don't represent any sentiment of actual humans. These Tweets are considered noise and are removed.

These Tweets were found by running the LDA algorithm on the corpus and exploring the t-SNE plot which shows individual Tweets in each topic. This plot will be discussed later.

If the Tweet contained one of the phrases from the following list they were removed from the corpus and deemed to be an irrelevant Tweet. This does not capture all irrelevant Tweets and may delete Tweets that were posted by a human (each filter works independent of whether the characters are upper or lower case):

* Current price
* Price
* Price update
* Prices update
* Price action
* Price increase
* Price decrease
* Density
* Last hour
* Latest block info
* Closed sell
* Alert
* Hourly update
* %
* Removal of Tweets that contain ten or more digits. These Tweets were observed to be produced by bots reporting statistical data about bitcoin.


The removal of these Tweets means that the sentiment of the users of Twitter can be more effectively investigated.


2. Removal of within document noise. For example, a word which appears in the data may be presented like this "#hate". This phrase carries the same meaning as the word hate phrased as this "hate". In this example, the hashtag is removed from "#hate" and the word is saved in the Tweet as "hate". As explained previously, Topic Model algorithms depend on the frequency of words in the corpus. It would not be accurate to consider "#hate" and "hate" as two separate frequencies. 

For the statistical analysis, "#hate" and "hate" will be grouped together because they are exactly the same in terms of their sentiment, and this is important because topic modeling's purpose is to group Tweets based on similarities within those Tweets and the way to pick up that similarity is by seeing if Tweets have the same words inside them. The algorithm has no way of knowing if "#hate" and "hate" are the same word. For that characters such as hashtags are removed from the Tweets, amongst other characters that will be explained further.



The following cleaning of characters were undertaken for each Tweet. using regular expressions:

* Conversion of html escapes to characters, for example conversion of "&amp;" to "&". 
* Removal of html tags. For example "\<tab>" is removed.
* Removal of markdown URLs, for example: "\[Some text](https://....)".
* Removal of text or code in square brackets.
* Removal of sequences of special character, for example "&#" is removed.
* Removal of sequences of hyphens such as --- or ==.
* Removal of sequences of white spaces.
* Removal of #.
* Removal of $ (this character is used as a cashtag, similar to a hashtag).

Using the python package "Textacy" normalisation was executed on the corpus. Normalisation is a process that transforms text into its canonical form. For example, the word "b??at" with an accent would be converted to "boat" without an accent. 

* Normalize words in the document that have been split across lines by a hyphen for visual consistency (aka hyphenated) by joining the pieces back together, sans hyphen and whitespace.
* Normalize all ???fancy??? single- and double-quotation marks in text to just the basic ASCII equivalents.
* Normalize unicode characters in the document into canonical forms.
* Remove accents from any accented unicode characters in text, either by replacing them with ASCII equivalents or removing them entirely.

The following words are removed from each document as they are synonyms for the word Bitcoin and don't carry any sentiment. The decision to remove these was made by observing the result of the topic model and observing these words having high importance in topics:

* BTC
* Bitcoin
* Crypto
* Cryptocurrency
* Coin



### 5.1.2 Lemmatisation

In addition to removal of special characters to ensure words with the same meaning are the same, the syntax of a word is cleaned. A short example of this is, the phrase "buy" and "bought" carry the same meaning - they just refer to different moments of time, the present and past respectively. For that reason these words should appear the same for the algorithm as they carry exactly the same sentiment and meaning; they will be both classed as the present "buy". This helps the algorithm give the same meaning to Tweets that use words that only differ by their grammar (such as past, present, future, gerund, past participle). It seeks to make each word be independent of its grammatical version. 

### 5.1.3 Tokenisation

This process is the separation of the Tweet into single words, and these words are known as "tokens", so the LDA algorithm can determine where each word starts and ends. This is necessary for the algorithm to analyse the data. This was done using the package [@nltk].



### 5.1.4 n-grams: Including phrases

This joins two or three (bigram or trigram, respectively) tokens together to form a two or three token phrase. If a two or three word phrase appears  often in the Tweets, specifically if they appear more than 30 times, they are included in the dictionary which is used in the LDA algorithm. This is important because some words when joined together contain a very different meaning. For example, the phrase "money-hungry" gives a different meaning to the document than if the words "money" and "hungry" were to be used separately. This also preserves negative sentiments such as "don't buy" by joining common two words sentences - it is not recommended to replace negative words such as "don't" when doing sentiment analysis on twitter data [@prep1].


### 5.1.5 Filtering tokens that exhibit extreme frequencies

If a word appears only five times in the whole corpus, it is removed from the dictionary. If a word appears in eighty percent or more of the Tweets, it is also removed from the dictionary. Since these words are removed, they are then not used in the LDA algorithm.

Words that are very frequent or not very frequent are not relevant to the analysis because if they appear very frequently, then they're not a good way to distinguish between topics. If they don't appear frequently documents won't have these words in common and simply adds to the noise of the data. 





## 5.2 Running the LDA Algorithm

Before the model can be run, two elements need to be generated as an input into the model:
1. Create a dictionary: This is the list of all the words in the corpus (all the documents).
2. Create Bag of Words for each document: This is  gives a unique number ID to each word in each Tweet, accompanied with the frequency of that word in the document. This is necessary since the algorithm will process the documents using ID number of each words and their frequencies. 



#### 5.2.1 Inputs into the model: Hyperparameters


In Section 3.3.1 hyperparameters and their role in LDA were outlined. In section 5.4 the $\beta$ and $\alpha$ hyperparamters were specified. These hyperparameters can be optimised within the LDA Mallet algorithm. LDA Mallet contains and option called ???optimize-interval???, and this option is described on the LDA Mallet website as:

> ???This option turns on hyperparameter optimization, which allows the model to better fit the data by allowing some topics to be more prominent than others. Optimization every 10 iterations is reasonable.???

For every interval of the generative process these hyperparameters are updated. An article, [@dragonfly], looked into the impact of changing these values on a corpus. Figure \ref{fig:optint} shows how changing the value for the optimisation interval (each line represents one chosen optimisation interval) impacts the multinomial distribution of the topics. Lower values gives high probabilities to only a few topics, and higher values (500 to 2000) approach the uniform distribution. Using this information, and given tat LDA Mallet states 10 iterations is reasonable, an optimisation interval of 50 is chosen. This is a low number that allows only a few topics and words to have high probabilities which is what we want, as discussed previously in the paper, to achieve interpretable results. 

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/optinterval} 

}

\caption{Plot of how different optimisation intervals, which controls the optimisation of the hyperparameters, used in the LDA algorithm impacts the topic probability distribution. Courtesy of Christof Sch??ch (2016).}\label{fig:optint}
\end{figure}


### 5.2.1 Inputs into the model: Evaluating the Best Number of Topics



The LDA model requires that the statistician inputs the number of topics that the corpus should be split into. Since the actual number of topics is hard to discern from such a large corpus of Tweets a way to mathematically ascertain the best number of topics is necessary. Choosing the right number of topics for the model to output is important because if too many are chosen those superfluous topics won't make sense. Equally if too few are chosen the model would miss topics which might be important to the statistician. Topic models make no guarantee on the interpretability of their output nor their topics. 

The overall aim of evaluating a topic model is seeing if the outputted topics tell a good story and if the topics are well-defined. One way of doing that is called a "word intrusion" test. This is a test based on a human observers. For each topic, the list of the most probable words in that topic are presented to an observer, with one important word in that topic substituted with a word that has a high-probability from another topic.

Given is a toy example that describes the "Word Intrusion" test:

*  A topic outputted by the model has the following top five most important words : car, boot, window, drive, road 
* The same topic's five most important words with a word intruder (here, *banana* is the intruder): car, boot, *banana*, drive, road
 
If the human observer can identify the word intruder, then the original list of most important words for that topic makes sense and therefore the topic is well defined.  If the observer can't identify the intruding word then the original topic's list of most important words doesn't make sense because it means the word intruder looks as good as the words in the topic before, which suggests that the topic's important words are defined at random. This suggests that the number of topics the user chose as a parameter should be changed.


This Word-Intrusion test is a measure of topic "Coherence". Coherence in this context is a measure of how much the documents support each other - or in other words how well the topics resemble an actual topic that human labels it as a distinct topic. These human-based measures serve as a gold standard for coherence evaluation. However, they are expensive to produce and therefore a statistical based measure is called for. 

There are several popular coherence measures used in Topic Modeling [@cohermes], with the measure UMass being amongst them. This measure correlates well the the human based word intrusion measure. UMass calculates the correlation of words in a given document based on conditional
probability. It takes the set of $N$ most important words of a topic and sums a *confirmation measure* over all the word pairs in the set. This confirmation measure takes a pair of words or word subsets as well as the corresponding word probabilities to compute how strong one set of words supports the other. This score measures how much, within the words used to describe a topic, a common word is on average a good predictor for a less common word.  The confirmation measure is given by:

\begin{equation}
C_{UMass} = \frac{2}{N\cdot(N-1) } \sum_{i=2}^{N}\sum_{j=1}^{i=1}log\frac{P(w_i,w_j)+\epsilon }{P(w_j)}.
\end{equation}

This sum of this gives the UMass Coherence score [@cohermes]. Models are run for each unique value of number of topics $k\in[5,45]$, where k is the number of topics. It is computationally expensive to run the models therefore the number of topics has been constrained to this set. The model with the lowest Coherence UMass scored is considered as the best model with k being the best number of topics. 







The paper @cohermes compared many different coherence measures by looking at their correlations with human based measures of topic models [@cohermes]. The best performing coherence measure was found to be newly proposed measure. This measure, $C_V$ combines the indirect cosine measure with the NPMI and the boolean sliding window, see @cohermes for more detail.

This score ranges from zero to one with one being being perfect coherence. Like the UMass score, multiple models will be run for each value for the number of topics and the value closest to one will be considered as the best number of topics to use. Although it is also important that the topic makes sense for a human and therefore these topics were analysed such that words in the topics were judged by the human to belong together.

The plot in figure \ref{fig:cohumass} shows how the UMass coherence score changes over different number of topics. It decreases as the number of topics increases - meaning that that more topics means a more coherent model. The $C_V$ coherence metric is shown in figure \ref{fig:cohcv} and shows a peak coherence score at number of topics = 15. For this reason, 15 topics will be used in the analysis of the bitcoin data.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/Best_coherenceU_mass} 

}

\caption{Plot of UMass Coherence score over the number of topics outputted}\label{fig:cohumass}
\end{figure}


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/Best_coherence_cv} 

}

\caption{Plot of UMass Coherence score over the number of topics outputted}\label{fig:cohcv}
\end{figure}





### 5.2.3 Running the Model

Using two packages known as Gensim [@rehurek2011gensim] and LDA Mallet [@mallet], the model is run through the LDA algorithm using the pre-processed data and using a Dirichlet $\beta$ hyperparameter of 0.01 and a Dirichlet $\alpha$ parameter of $\frac{5}{T}$ where T denotes the number of topics specified in the algorithm [@mallet]. See Section 3.3.1 for the impact of these parameters on the resultant multinomial distributions. Crucially, both of these values are less than one, allowing for the topic and word distributions to not be uniform, with only a few topics and words having a high probability. Gensim offers an LDA function however, it does not Gibbs sampling in its algorithm therefore LDA Mallet is used which is a wrapper that sits ontop of Gensim's LDA algorithm. The Gensim package was selected due to its ability to process large corpora by optimising ram management [@rehurek2011gensim].




\newpage



# 6 Visualisation 

## 6.1 LDAvis

Visualizing the result of a topic model is difficult the fitted model has many dimensions - the LDA model is applied to thousands of documents, modeled as many topics, and these topics are modeled as distributions over many words. Using a package called pyLDAvis, the result of the topic model can be visualised in a user-friendly way [@ldavis].


Thie visualisation is shown in figure \ref{fig:ldavis1}.  The right hand panel shows a bar chart, showing the most "useful" words for a topic highlighted on the left panel. This intends to show the meaning of the topic. The bars show two overlaid bars per word - the topic-specific frequency of a word (red) and the corpus-wide frequency of the word (blue). Selecting a word on the right changes the size of circles of the topics on the left - with the sizes of the circles representing the conditional distribution over topics of the chosen word.


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/ldavis1} 

}

\caption{Output of LDAvis using 15 topic models for the Bitcoin data}\label{fig:ldavis1}
\end{figure}


### 6.1.1 "Useful" words

In order to observe a topic meaning, a ranked list of the most probable terms in that topic. The issue with investigating topics like this is that common words in the corpus occur in the ranked list of multiple topics which in turn makes it hard to differentiate meanings of these topics. In order to resolve this issue @bischof suggested ranking terms for a given topic by using the frequency of the term in that topic and the exclusivity of the term to the topic. The exclusivity takes into account how many times the term appears in that topic to the exclusion of others. This metric is defined as "usefulness" and pyLDAvis uses a similar measure called "relevance" to rank terms in order of "usefulness". Relevance is defined as:



\begin{equation}
r(w,k|\lambda)=\lambda log(\phi_kw) + (1-\lambda)log(\frac{\phi_kw}{p_w})
\end{equation}

where $\phi_kw$ denotes the probability of term $w\in \left \{  1,...,V \right \}$ for topic $k\in \left \{  1,...,K \right \}$ where $V$ denotes the number of terms in the vocabulary. Also, $p_w$ denotes the marginal probability of term $w$ in the corpus. $\lambda$ determines the weight given to the probability of term $w$ under topic $k$ relative to to its lift. Lift is defined as the ratio of a term's probability within a topic to its marginal probability across the corpus, which can be seen as the subject of the log function. Setting $\lambda =1$ results in ranking the terms in a topic by their topic-specific probability. Setting $\lambda=0$  ranks the terms solely by their lift.

A study was carried out with humans to see what the optimal value of $\lambda$ [@ldavis]. Using documents that have been assigned topics (ground truth), subjects were asked to assign a list of words to a topic - with the list of words coming from topics made from a topic model. The list of words presented to the subjects were made with varying values of $\lambda$ and the relationship between accuracy rate assignment of the list of words to a topic and values of $\lambda$ was explored. The study found that the optimal value of $\lambda$ was 0.6 (no confidence interval given) yielding 70% probability of correct topic identification. The relevance score for the words will be used to determine the definition of each topic of the topic model. Figure \ref{fig:ldavis2} shows the most relevant terms for Topic 7 (LDAvis labels the topics one greater than the actual label, so is labelled as 8) using $\lambda =0.6$.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/ldavis2} 

}

\caption{The word relevance of Topic 7 in the LDAvis and setting lambda = 0.6}\label{fig:ldavis2}
\end{figure}


## 6.2 t-SNE

Another useful visualisation of topic models is known as t-SNE, which is implemented in python using Bokeh [@bokeh]. It stands for "t distributed Stochastic Neighbourhood Embedding" and was developed in 2008. This is an unsupervised non-parametric method of dimensionality reduction, similar to the left hand pane of the LDAvis. It's useful in separating data that cannot be separated by any straight line. "t-SNE" preserves the local structure in the data such that points that are close together in high dimensions will also be close together when the dimensions are reduced by the algorithm to just two dimensions.

It's useful for visualisation since, with Principal Component Analysis (PCA), a popular dimension reducing algorithm, the global structure of the data is preserved. PCA concerns itself with the direction in which the variance is maximises and it doesn't take into account the distance between individual points. t-SNE on the other hand concerns itself with the local structure and preserves the both the local and global structure of the data. This is importance when displaying topic models since it makes sense that  similar documents  be close together.  It's also good art handing non-linear data. PCA is a linear algorithm; creating components which are the linear combination of existing feature. This means it's not able to interpret polynomial relationships between features. t-SNE is capable of working with non-linear data however in t-SNE is computationally complex and expensive. This visualisation will be used to quickly view each document in each topic in order to give a label to the topic.

Figure \ref{fig:tsne3} shows the t-SNE plot for the resulting topic model for the Bitcoin data. Hovering over areas of the plot displays individual tweets - useful to classify the meaning of each topic.
\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/tsne1} 

}

\caption{The  t-SNE plot for the topic model for the Bitcoin data using 15 topics}\label{fig:tsne3}
\end{figure}


### 6.2 Using t-SNE to detect bot-generated Tweets

As discussed previously, Tweets produced by bots do not represent sentiment by people and can be sent automatically at any frequency. For this reason it's important to consider these Tweets as noise which can hinder the ability for the topic model to classify Tweets into topics and therefore these Tweets should be removed from the corpus. There is no catch-all method for this since there are many patterns produced by Tweets and often they mimic the language of humans. One way to detect Tweets generated by bots is by observing the t-SNE plot. Tweets that follow the same form, such as price updates such as:

> "Current price: 433.54$ $BTCUSD $btc #bitcoin 2016-01-02 18:40:05 EST"

will form a cluster in the t-SNE plot. This is because local structure is preserved so these Tweets will be close in two-dimensions. In Figure \ref{fig:tsnebot} the t-SNE plot can be shown for a topic model. The red box highlights a cluster of Tweets isolated from the rest of the other Tweets. Highlighting this cluster shows the content of the Tweets and it can be seen that these Tweets report price updates of Bitcoin and do not contain any sentiment. Using this method, Tweets were removed based off key words in Tweets like this as explained in Section 5.1. In Figure \ref{fig:tsnebot} for example, the Tweets to be removed are Tweets that contain the word "current #bitcoin price". There is a danger of removing Tweets that are posted by human users however, after doing some data checks, the frequency of these Tweets are very low and the advantage of eliminating bot-produced Tweets outweigh removing a small amount of Tweets by humans.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/tsnebots} 

}

\caption{Demonstration of how the t-SNE plot can identify Tweets produced by bots with the red box showing a cluster of similar Tweets. Tweets produced by bots are repeated and have the same form, therefore in the t-SNE plot they appear as their own cluster, since local structure is preserved. Hovering over these points with the mouse shows the content of these tweets.}\label{fig:tsnebot}
\end{figure}
\newpage

# 7 Technical Points

The Bitcoin data contains 510,753 rows. Running this amount of data through the topic model process takes a lot of time depending on the quality of computer on which the topic model is run. Google Colab is a system through which Python scripts can be run online using computers provided by good which can run the model on powerful GPUs. However, there are issues running the LDA model (specifically LDA Mallet) on Google Colab. For this reason, the data was sampled down randomly to 10% of its size, to 50,753 rows. 

# 8 Results

<!-- Read in the data  -->

The topic model was run on the bitcoin data using 15 topics. 

Each topic was labelled using the following process:

1. Observing the most relevant word distribution for each topic. Using this word distribution the topic can be understood. Figure \ref{fig:ldavis3} shows the word relevance distribution for topic 14 (recall that the LDAvis topic number = Topic number + 1). "buy" and the rocket emoji are seen to be very important for this topic - so this topic can be labelld as tweets that talk about buying bitcoin.

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/ldavis3} 

}

\caption{The word relevance of Topic 14 in the LDAvis and setting lamba = 0.6. From this it can be seen the word buy and the emoji rocket are very important terms in this topic. }\label{fig:ldavis3}
\end{figure}

2. Observing tweets each topic using the t-SNE plot. From this, individual tweets from the topic can be easily seen, and also their distance from the other tweets - such that tweets further away can be interpreted as the tweets that define the topic more. In figure \ref{fig:tsne2} tweets for Topic 14 are seen, with the word "buy" highlighted. 


\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/tsne2} 

}

\caption{The t-SNE plot is used to view individual tweets in each topic, and taking their location into account. }\label{fig:tsne2}
\end{figure}



3. Observing the most frequent words in each topic, shown in Table \ref{tab:topictable}.



\begin{table}

\caption{\label{tab:topictable}Topics and their top 10 most frequent words}
\centering
\begin{tabular}[t]{l|l}
\hline
Topic & Top Terms\\
\hline
Topic 0 & year,             buy,          invest,            make,           money,            life,       invest\_in,          change,           youre,            time\\
\hline
Topic 1 & face.with.tears.of.joy,                                         play,                                          win,                                         game,                                     have\_win,                                        0.005,                                   0.005\_play,                                           ll,                           loudly.crying.face,                  rolling.on.the.floor.laughe\\
\hline
Topic 2 & ..., market,   time,   week,   move,  close,   long,   high,  break,   back\\
\hline
Topic 3 & currency,      money,      world,       bank,    digital,    network,       fiat,      asset,     system,     mining\\
\hline
Topic 4 & number,                          rt,                        news,                      escort,                      google,                     website,                       video,                         usa,                        eeuu,                    nowplaye\\
\hline
Topic 5 & currency,       alt,      time,       day,     trade,    follow,     money,  ethereum,     today,   trading\\
\hline
Topic 6 & nt,    people,      dont,      make,     do\_nt,       ...,     thing,      shit,understand,      give\\
\hline
Topic 7 & ...,      good,      talk,      find,       lot,      dont,       eye,    lot\_of,      love,talk\_about\\
\hline
Topic 8 & eth,      xrp,     doge,      ltc, currency,   ripple,     lite, ethereum,  binance,      trx\\
\hline
Topic 9 & market,            stock,         currency,             gold,             real,              cap,          economy,         donation,           dollar,         economic\\
\hline
Topic 10 & blockchain,  ethereum,  currency,   project,       ico,       eth,     token,     great,      team,      good\\
\hline
Topic 11 & wallet,   exchange,       send,        pay,     accept,    account,        ...,       cash,    payment,       card\\
\hline
Topic 12 & term,        long,      future,       short,      marine,       tweet,         etf,   long\_term,        risk,         mtc\\
\hline
Topic 13 & fee,                              rate,                               sit,                             vbyte,                              hour,                             block,                       transaction,                          fee\_rate,                         sit\_vbyte,       smiling.face.with.sunglasse\\
\hline
Topic 14 & buy,       rocket,         sell,          ...,         wait,          dip,         fire,           ..,         dont,          eth\\
\hline
\end{tabular}
\end{table}



## 8.1  Defining the Topics 

Table \ref{tab:topicdeftable} gives the label of each topic - diserned from the 3-step processed described above. 


\begin{table}

\caption{\label{tab:topicdeftable}Topics and their labels- Labelled using the 3-step process described previously.}
\centering
\begin{tabular}[t]{l|l}
\hline
Topic & Topic Definition\\
\hline
Topic 0 & Investing changed life/good decision to invest.\\
\hline
Topic 1 & Marketin bots/ Using btc to refer to something else.\\
\hline
Topic 2 & Market analysis/BTC updates.\\
\hline
Topic 3 & Predicting bitcoin to be a future world currency.\\
\hline
Topic 4 & Marketing/ buying bitcoin practicalities.\\
\hline
Topic 5 & Alt coins and bitcoin news.\\
\hline
Topic 6 & Negative attitude to bitcoin and negative response to bitcoin critics.\\
\hline
Topic 7 & Miscellaneous bitcoin comments.\\
\hline
Topic 8 & Alt coins.\\
\hline
Topic 9 & Bot updates/ marketing and linking stock market with btc market.\\
\hline
Topic 10 & Technicalities of bitcoin purchasing and marketing bots.\\
\hline
Topic 11 & Logistics of purchasing bitcoin.\\
\hline
Topic 12 & Promoting long term holding of bitcoin / bots.\\
\hline
Topic 13 & Broadcasts of bitcoin transactions.\\
\hline
Topic 14 & Promoting buying bitcoin/ selling in the future.\\
\hline
\end{tabular}
\end{table}

## 8.2 Other Visualisations


Figure \ref{fig:proptime} shows how the proportion of topics change through time. Topic 2 - "Market analysis/BTC updates" has increased a lot past after 2017. By the end of 2018 Bitcoin price had increased a large increase in price and this could be a possible reason for this increase. Topic 1 - "	Marketin bots/ Using btc to refer to something else" used the term "btc" to refer to something else other than Bitcoin, and this reference has decreased significantly since the end of 2017 - again, this is when bitcoin was gaining more fame. Topic 14 - "Promoting buying bitcoin/ selling in the future" spiked just before the end of 2018 - coinciding with the large jump in bitcoin price at the end of 2018. Figure \ref{fig:heatmap} shows the co-occurences between words in the documents - that is how often the words occur in the same documents. In the top left it can be seen "alt", "currency" and "blockchain" often appear together and "face" and "money" co-occur a lot.
\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/07 - Topics over time} 

}

\caption{The proportion of topics through time}\label{fig:proptime}
\end{figure}

\begin{figure}

{\centering \includegraphics[width=1\linewidth]{images/09 - heatmap} 

}

\caption{Heatmap of word co-occurences.}\label{fig:heatmap}
\end{figure}



\newpage




## 7.4 Word Clouds

### Topic 0
![](images/03 - Topic_0.png){width=750px} 

### Topic 1
![](images/03 - Topic_1.png){width=750px} 

### Topic 2
![](images/03 - Topic_2.png){width=750px} 

### Topic 3
![](images/03 - Topic_3.png){width=750px} 

### Topic 4
![](images/03 - Topic_4.png){width=750px} 

### Topic 5
![](images/03 - Topic_5.png){width=750px} 

### Topic 6
![](images/03 - Topic_6.png){width=750px} 

### Topic 7
![](images/03 - Topic_7.png){width=750px} 

### Topic 8
![](images/03 - Topic_8.png){width=750px} 

### Topic 9
![](images/03 - Topic_9.png){width=750px} 

### Topic 10
![](images/03 - Topic_10.png){width=750px} 

### Topic 11
![](images/03 - Topic_11.png){width=750px} 

### Topic 12
![](images/03 - Topic_12.png){width=750px} 

### Topic 13
![](images/03 - Topic_13.png){width=750px} 

### Topic 14
![](images/03 - Topic_14.png){width=750px} 

### Topic 15
![](images/03 - Topic_15.png){width=750px} 

### Topic 16
![](images/03 - Topic_16.png){width=750px} 

### Topic 17
![](images/03 - Topic_17.png){width=750px} 







# 8 Time Series Analysis

Three hundred Tweets were extracted daily from Twitter and these Tweets were classified into topics. The frequency of topics over time represent how spoken about that topic is over time. The three hundred Tweets represents a random sample of Tweets. If, for example, a topic of "buy bitcoin" for one day comprises fifty percent of the total Tweets one day, and one month later the topic comprises 20% of the Tweets, then it can be said that this topic is decreasing in popularity. This proportion of topics over time can be seen as a time series. A time series is a set of observations made through time for which the time aspect is potentially important to the analysis. The price of the asset can also be considered a time series.   In order to investigate how and if Tweets impact the price of an asset a Time Series model can be used - specifically an ARIMAX  model. ARIMAX models have been used to forecast traffic flow [@arimax1]. This model explains how a time-series data moves through time with the ability for it to dependent on other time-series data. The aim is to see how the popularity of a topic of Tweets affects the price of an asset. Since the topic frequency on a daily basis represents a proportion of  a random sample of all tweets about assets, the frequency of Tweets from the data in this report can be used an indicator of the total frequency that that particular topic is posted about on Twitter.


It is not enough to explore the correlation between the two native times series. Comparing two trended time series are likely to correlate by virtue of the fact they may be increasing or decreasing over time and not because they are related in any way. The VAR model process takes trends, among other things, into account when exploring the the relationship between time series data.

In order for time series to be analysed they need to be cleaned and put into a stationary form. Stationary is when the properties of the time series stay the same through time. In stationary series one block of successive values and another block from another part of the series should look as though they were generated from the same fixed joint distribution. In practice, a stationary series follows a resembles flat line around the value zero. Taking out trends (where trends are moments in time where the time series increases or decreases successively) and seasonality (that is trends that repeat at the same time over between consecutive fixed time points, for example, how chocolate is sold much more every 12 months at Christmas) should lead to a stationary series. The following steps describe the method through which a stationary series is obtained.


Normalisation:
It's important to normalise both time series since both time series are of different scales. This is done in the following way:

\begin{equation}
n_{t} = \frac{y_t - \overline{y}}{\sigma},
\end{equation}

where $y_t$ is the time series, $t$ is a time point in the time series ($t$ runs from the time point at which the time series starts and the time point at which it ends), $\overline{y}$ is the mean of the time series and $\sigma$ is the standard deviation of the time series. 




In order to perform the ARIMAX model the time series must be detrended (after normalisation). The trend as outlined above can be perceived as distortion in the data that will lead to inaccurate results from the model. Detrending can be done by differencing. To remove a linear trend, first order differencing is applied to the time series. Second order differencing removes a quadratic trend. Differencing is applied successively to the time series until it appear stationary. A stationary time series is one whose properties do not depend on the time at which the series is observed

First order differencing is given by the equation:
\begin{equation}
\nabla y_t = y_{t} - y_{t-1} = (1 -B)y_t,
\end{equation}

which represents taking the value of the time series one time point before away from the time series. $B$ is the backward shift operator given by:

\begin{equation}
By_t = y_{t-1},
\end{equation}

and the general $k$-th order differencing is given by:

\begin{equation}
\nabla^k = (1-B)^k.
\end{equation}


The Dicky-Fully test is used to test for stationarity, where a significant p-value signifies that the time series is stationary.

## 8.1 ARIMA Model 

Autoregressive integrated moving average (ARIMAX) models extend ARIMA models through the inclusion of exogenous variables $X$. The exogenous variables allow for a time series $y_t$ to not only be dependent on it's past values but also other time series $X$. The ARIMAX model is  given by: 
\begin{equation}
\Delta^{D}y_{t} = \sum^{p}_{i=1}\phi_{i}\Delta^{D}y_{t-i} + \sum^{q}_{j=1}\theta_{j}\epsilon_{t-j} + \sum^{M}_{m=1}\beta_{m}X_{m,t} + \epsilon_{t},
\end{equation}

where $y_t$ is the time series of interest, $d$ is the number of times the series is differenced, $p$ is the number of autoregressive lags, $q$ is the number of moving average lags and $\beta_{m}$ is the coefficient for the exogenous variable. More details on ARIMA models can be found in @arimax2.


Using the Box Jenkins method for Identification of the initial values for the ARIMA Model, correlograms are used. 


Can be seen this and that needs to be used. 



## 8.5 VAR Model


The aim of the VAR model is to model each time series as an equation, modeling its evolution over time. The equations involves the lagged values of the time series. So for two variables used in a VAR model, the model explores how the past (lagged) values of the each of the variables impact the current values of each variable. Each variable's current value isn't only impacted by its past values, but also the past values of the other variables. 

2. explain var(1) process

A VAR(1) model assumes that the current value for each time series is dependent on the each time series' value in the one time point in the past (for example, if the time points were measured in days, the current values would depend only on yesterdays values)

This can be formalised into two equations:

\begin{equation}
a_t = c_{11}a_{t-1} + c_{12}b_{t-1} + \varepsilon_{a,t}, 
\end{equation}
and
\begin{equation}
b_t = c_{21}a_{t-1} + c_{22}b_{t-1} + \varepsilon_{b,t},
\end{equation}

where $a_t$ in this context can be the time series representing the price of the asset, $b_t$ as the time series representing the proportion a topic that make up the three hundred daily Tweets, $c_{11}$ as the coefficient that represents how $a_t$ depends on it's previous value, $c_{12}$ as the coefficient representing how $a_t$ depends on the previous value of $b_t$, $c_{21}$ representing how $b_t$ depends on the previous value of $a_t$, $c_{22}$ representing how $b_t$ depends on the previous value of $b_t$, $\varepsilon_{a,t}$ represents the error for $a_t$ and $\varepsilon_{b,t}$ is the error for $b_t$.


This can be written in matrix form:


\begin{equation}
\begin{bmatrix} 
a_t\\ 
b_t
\end{bmatrix} = \begin{bmatrix}
c_{11} & c_{12} \\ 
c_{21} & c_{22}
\end{bmatrix}
\begin{bmatrix} 
a_{t-1}\\ 
b_{t-1}
\end{bmatrix} +
\begin{bmatrix} 
\varepsilon_{a,t}\\ 
\varepsilon_{b,t}
\end{bmatrix}
\end{equation}

which is equal to

\begin{equation}
F_t = CF_{t-1} + \Sigma_t
\end{equation}


This method can be extended to further time points in the past. The VAR model looks to see which lags correlate, using Pearson correlation, with the current value (taking out trends and other noise that may impact this relationship, as described above). Lagged values that have a significant correlation will be assumed to have a relationship with the current value. In this way, it is possible to see if the proportion of a topic on Twitter in the past has an impact on the price of an asset.

3. explain generalised process

https://www.analyticsvidhya.com/blog/2021/08/vector-autoregressive-model-in-python/

use equation here


http://www.ams.sunysb.edu/~zhu/ams586/VAR_Lecture2.pdf














# RUBBISH - NOTES

https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf conducted a user study to determine whether there was an optimal value of lamba from the definition of relevance. r


pyLDAvis







DONT USE
A high UMass measure would indicate that the topics are well defined.

Another coherence measure was proposed  https://svn.aksw.org/papers/2015/WSDM_Topic_Evaluation/public.pdf which is a unifying framework which combines different coherence measures into one score. It works as follows.

1. **S - Segmentation** : Divide a word set (such as the top most probable words in a topic) into smaller pieces, such as a set of pairs. This is a set of different kinds of segmentations
2. **M - Confirmation Measure** : The set of conffirmation measure that scores the agreement of a given pair, for example UMass 
3. **P - Word probabilities** : These can be computed i ndifferent ways so P serves as a set of ways the word probabilities are calculated.
4. **Epsilon - Aggregate**: This is a set of aggregration functions which determine how to aggregate scaalr values.

DONT USE END






There are several methods to use statistical measures to determine the ideal number of topics. One such method is known as the Coherence Score, where a high score represesents.

To find the best Coherence Scrore, a model was built for each n, number of topics. For example, to investigate the best number of topics, a model is first built using the parameter n = 3, 3 topics, and the coherence score is obtained. This process is repeated for n = 4, up until n = 30 and the highest coherence score amongst these models indicates the ideal number of topics. 

Cv has issues,  as stated herehttps://palmetto.demos.dice-research.org/
2 issues
https://github.com/dice-group/Palmetto/issues/13


i think umass doesnt use wikipedia, but hte rest doesnt. so that makes me think dont use the wikipedia one because these topics are so granular
http://qpleple.com/topic-coherence-to-evaluate-topic-models/



https://stats.stackexchange.com/questions/375062/how-does-topic-coherence-score-in-lda-intuitively-makes-sense 
more explanation




1. intro - set scene, big picture, can book appointment with writing advisory service, english language teaching department
2. literature review
3. theoretical - can do it in this section or when introduce these plots
4. EDA 
5. Analysis
6. Results
7. Discussion, what have you done. where does it lie in research area, why is it interesting. what can be done in the future. discursive. 
8. Conclusion -informative.
9. References
10. lay summary





RUBBISH END
	









# References


