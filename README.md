# Data Science Project with Melon Music Chart

This is an archive for Team Project done in Data Science Lecture held in Yonsei. I focus on crawling using Selenium (done in Python), lyric analysis with basic NLP (done in Python), EDA (done in R), linear regression (done in R) and clustering (done in R).

This repository contains 5 code files, 2 in Python and 3 in R. 
I recently added Word2Vec folder to upload code written for Word2Vec with Python.

The analysis was carried out in following timeline.

## 1) Melon Music Chart Crwalind.ipynb

Data was crawled from Korean music streaming website: https://www.melon.com/

I was able to complete the code by help of this posting(http://baekse.me/Melon-Chart-Crawling/). Refer to this original posting of crawling using Selenium as you wish. 

Note that the main difference is `soup = BeautifulSoup(html, "html.parser")` and `soup = BeautifulSoup(html, "lxml")`. I came across the fact that recent websites are written in lxml format and it cause less trouble to use lxml especially accessing lyric information.


## 2) NRC Lexicon Matching on Korean Lyrics.ipynb

Utilizing NRC Lexicon, scores on sentimnets such as anger, anticipation, disgust etc. was calculated. I got the Lexicon dictionary on this website(https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) where they have Lexicon translated in various language(original study was done in English language).


## 3) EDA on Lyric Data.R

Here, I included EDA and visualization on Dataset created at step 2. You can refer to this intermediate processed dataset with `df_input.csv` and I have also included the ouput as `df.csv` on my repository. Extra csv file called `df_rank.csv` is used while EDA was carried out. I have created several new variables in EDA process.

This blog (https://junhewk.github.io/text/) helped me out alot. 


## 4) Linear Regression Model.R

Taking `df.csv` as an input, I carry out linear regression analysis.


## 5) K-prototype Clustering.R

Taking `df.csv` as an input as well, k-prototpye clustering was done to group similar musics. K-prototype clustering is a combination of traditional K-means and K-mode. It is used to clustering data cases that have both numerical and categorical variables. 

Take a loot at these paper if you're interested in math used in K-prototype algorithm:  
* http://www.cs.ust.hk/~qyang/Teaching/537/Papers/huang98extensions.pdf  
* https://arxiv.org/ftp/cs/papers/0603/0603120.pdf  

## Word2Vec

I'll upload Word2Vec analysis carried out in python to this folder. Word2Vec analysis will take `df.csv` as an input dataframe. (will be updated)
