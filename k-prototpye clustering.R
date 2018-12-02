library(clustMixType)
library(dplyr)

# preparing dataframe
df_eda <- df %>% dplyr::select(genre, label, positive:trust, num_words, lexical_density, year, month, top_artist, feat, onair, season, weather, rank_3, times_appear) %>% dplyr::select(-disgust)

df_eda$label <- as.character(df_eda$label)
df_eda$label[df_eda$label == "开林青 YES"] <- 1
df_eda$label[df_eda$label == "开林青 NO"] <- 0

df_eda <- df_eda %>% mutate(sum = anger + anticipation + fear + joy + sadness + surprise + trust)
df_eda <- df_eda %>% mutate(anger = anger / sum)
df_eda <- df_eda %>% mutate(anticipation = anticipation / sum)
df_eda <- df_eda %>% mutate(fear = fear / sum)
df_eda <- df_eda %>% mutate(joy = joy /sum)
df_eda <- df_eda %>% mutate(sadness = sadness / sum)
df_eda <- df_eda %>% mutate(surprise = surprise / sum)
df_eda <- df_eda %>% mutate(trust = trust / sum)

df_eda <- df_eda %>% mutate(sum = positive + negative)
df_eda <- df_eda %>% mutate(positive = positive / sum)
df_eda <- df_eda %>% mutate(negative = negative / sum)

df_eda$anger[is.na(df_eda$anger)] <- 0
df_eda$anticipation[is.na(df_eda$anticipation)] <- 0
df_eda$fear[is.na(df_eda$fear)] <- 0
df_eda$joy[is.na(df_eda$joy)] <- 0
df_eda$sadness[is.na(df_eda$sadness)] <- 0
df_eda$surprise[is.na(df_eda$surprise)] <- 0
df_eda$trust[is.na(df_eda$trust)] <- 0
df_eda$positive[is.na(df_eda$positive)] <- 0
df_eda$negative[is.na(df_eda$negative)] <- 0

df_eda <- df_eda %>% dplyr::select(-sum)
df_eda <- df_eda %>% filter(!is.na(year))

# apply k-prototyps
kpres <- kproto(df_eda, 4) # 4俺狼 努矾胶磐
clprofiles(kpres, df_eda)

# Check for the optimal number of clusters given the data
wss<-vector()

for (i in 2:15){ wss[i] <- sum(kproto(df_eda, i)$withinss)}
par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters",
     pch=20, cex=2)

# plots between Total and other numerical Attributes with clusters: 
par(mfrow=c(1,2))

for(i in 1: 1:6){
  plot(pokemon[,c(5,5+i)], col=df_eda$cluster, main="K-prototypes")
}

# modeling with optimal number of clusters
lambdaest(df_eda)
res <- kproto(df_eda, 8, lambda = lambdaest(x))
clprofiles(res, df_eda)
df_eda$cluster = res$cluster


summary(res)

predicted.clusters <- predict(res, df_eda)