library(dplyr)


df <- read.csv("df_input.csv")
df <- df[, -1]

# plot of sentiments facet wrap by 역주행 여부
df_tidy <- df %>% dplyr::select(id, anger:trust, label) %>% gather(sentimet, value, anger:trust) %>% group_by(label, sentimet) %>% summarise(label_sum = sum(value)) %>% mutate(per_sent = label_sum / sum(label_sum))
df_tidy$label[df_tidy$label == 1] <- "역주행 YES"
df_tidy$label[df_tidy$label == 0] <- "역주행 NO"

ggplot(data=df_tidy, aes(x=sentimet, y=per_sent, fill=sentimet)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ label, ncol=2) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        plot.title=element_text(size=14)) +
  labs(x="감정", y="빈도(in percentage)",
       title="역주행 감정 히스토그램") +
  theme(text=element_text(family="NanumGothic"))

# plot of sentiments facet wrap by 역주행 여부 (excluding disgust sentiment)
df_tidy <- df %>% dplyr::select(id, anger:trust, label) %>% dplyr::select(-disgust) %>% gather(sentimet, value, anger:trust) %>% group_by(label, sentimet) %>% summarise(label_sum = sum(value)) %>% mutate(per_sent = label_sum / sum(label_sum))
df_tidy$label[df_tidy$label == 1] <- "역주행 YES"
df_tidy$label[df_tidy$label == 0] <- "역주행 NO"

ggplot(data=df_tidy, aes(x=sentimet, y=per_sent, fill=sentimet)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ label, ncol=2) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        plot.title=element_text(size=14)) +
  labs(x="감정", y="빈도(in percent)",
       title="역주행 감정 히스토그램(w/o disgust)") +
  theme(text=element_text(family="NanumGothic"))


# creating new binary variable called "times_appear_binary" based on times_appear
summary(df$times_appear) # take 3rd quantile value which is 9
df <- df %>% mutate(times_appear_binary = ifelse(times_appear > 9, "9번 초과", "9번 이하"))
summary(as.factor(df$times_appear_binary))

# plot of sentiments facet wrap by times_appear_binary
df_tidy <- df %>% dplyr::select(id, anger:trust, times_appear_binary) %>% dplyr::select(-disgust) %>% gather(sentimet, value, anger:trust) %>% group_by(times_appear_binary, sentimet) %>% summarise(sum_sent = sum(value)) %>% mutate(per_sent = sum_sent / sum(sum_sent))

ggplot(data=df_tidy, aes(x=sentimet, y=per_sent, fill=sentimet)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ times_appear_binary, ncol=2) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        plot.title=element_text(size=14)) +
  labs(x="감정", y="빈도(in percent)",
       title="차트 등장 빈도에 따른 감정 히스토그램") +
  theme(text=element_text(family="NanumGothic"))


# creating new variable called year, month using release variable
df$release  <- as.Date(df$release, format = "%Y.%m.%d")
df <- df %>% mutate(year = as.numeric(format(release,'%Y'))) %>% mutate(month = as.numeric(format(release, '%m')))
df$year <- as.factor(df$year)
df$month <- as.factor(df$month)

# plot of sentiments facet wrap by year
df_tidy <- df %>% dplyr::select(id, anger:trust, year) %>% dplyr::select(-disgust) %>% gather(sentimet, value, anger:trust) %>% group_by(year, sentimet) %>% summarise(sum_sent = sum(value)) %>% mutate(per_sent = sum_sent / sum(sum_sent))
df_tidy <- df_tidy %>% filter(year != '1991') %>% filter(!is.na(year))

ggplot(data=df_tidy, aes(x=sentimet, y=per_sent, fill=sentimet)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ year, ncol=6) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        plot.title=element_text(size=14)) +
  labs(x="감정", y="빈도(in percent)",
       title="년도에 따른 감정 히스토그램") +
  theme(text=element_text(family="NanumGothic"))


# creating new variable called decade using release variable
df$decade <- df$year
df$year <- as.character(df$year)
df$decade <- as.character(df$decade)

df$decade[df$year == "1991"] <- "1990"
df$decade[df$year == "1995"] <- "1990"
df$decade[df$year == "1996"] <- "1990"
df$decade[df$year == "1997"] <- "1990"
df$decade[df$year == "1998"] <- "1990"

df$decade[df$year == "2000"] <- "2000"
df$decade[df$year == "2001"] <- "2000"
df$decade[df$year == "2002"] <- "2000"
df$decade[df$year == "2003"] <- "2000"
df$decade[df$year == "2004"] <- "2000"
df$decade[df$year == "2005"] <- "2000"
df$decade[df$year == "2006"] <- "2000"
df$decade[df$year == "2007"] <- "2000"
df$decade[df$year == "2008"] <- "2000"
df$decade[df$year == "2009"] <- "2000"

df$decade[df$year == "2010"] <- "2010"
df$decade[df$year == "2011"] <- "2010"
df$decade[df$year == "2012"] <- "2010"
df$decade[df$year == "2013"] <- "2010"
df$decade[df$year == "2014"] <- "2010"
df$decade[df$year == "2015"] <- "2010"
df$decade[df$year == "2016"] <- "2010"
df$decade[df$year == "2017"] <- "2010"
df$decade[df$year == "2018"] <- "2010"

df$year <- as.factor(df$year)
df$decade <- as.factor(df$decade)

# plot of sentiments facet wrap by decade 
df_tidy <- df %>% dplyr::select(id, anger:trust, decade) %>% dplyr::select(-disgust) %>% gather(sentimet, value, anger:trust) %>% group_by(decade, sentimet) %>% summarise(sum_sent = sum(value)) %>% mutate(per_sent = sum_sent / sum(sum_sent))
df_tidy <- df_tidy %>% filter(!is.na(decade))

ggplot(data=df_tidy, aes(x=sentimet, y=per_sent, fill=sentimet)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette="Spectral") +
  guides(fill=FALSE) +
  facet_wrap(~ decade, ncol=6) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8),
        plot.title=element_text(size=16)) +
  labs(x="감정", y="빈도 (in percent)",
       title="년대에 따른 감정 히스토그램") +
  theme(text=element_text(family="NanumGothic"))


# plot of positive and negative 
df_tidy <- df %>% filter(!is.na(positive)) %>% dplyr::select(id, positive, negative, year) %>% gather(sentimet, value, positive:negative) %>% group_by(year, sentimet) %>% summarise(sum_sent = sum(value)) %>% mutate(per_sent = sum_sent / sum(sum_sent)) %>% filter(!is.na(year))
df_tidy <- df_tidy %>% mutate(per_sent = ifelse(sentimet == "positive", per_sent, (-1) * per_sent)) %>% group_by(year) %>% summarise(pos_neg = sum(per_sent))

ggplot(data=df_tidy) +
  geom_bar(aes(x=year, y=pos_neg, fill=pos_neg), stat="identity", position="identity") +
  ylim(-1, 1) +
  labs(y="평균 감정 점수", x="년도",
       title="NRC 감정 점수 히스토그램") +
  guides(fill=guide_legend(title="NRC 점수")) +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8), text=element_text(family="NanumGothic"))


# cleaning genre variable 
df$genre <- as.character(df$genre)
df$genre[df$genre == "Adult Contemporary"] <- "Others"
df$genre[df$genre == "Ballad,Jazz"] <- "Ballad"
df$genre[df$genre == "Ballad,Rock"] <- "Ballad"
df$genre[df$genre == "Dance,Electronica"] <- "Dance"
df$genre[df$genre == "Drama,Ballad"] <- "Drama"
df$genre[df$genre == "Drama,Folk"] <- "Drama"
df$genre[df$genre == "Drama,R&B / Soul"] <- "Drama"
df$genre[df$genre == "Drama,Rap / Hip-hop"] <- "Drama"
df$genre[df$genre == "Jazz"] <- "Others"
df$genre[df$genre == "Korean Movie"] <- "Others"
df$genre[df$genre == "Musical "] <- "Others"
df$genre[df$genre == "Rap / Hip-hop"] <- "Hiphop"
df$genre[df$genre == "Rap / Hip-hop,Dance"] <- "Hiphop"

df$genre <- as.factor(df$genre)
summary(df$genre)

# plot of genre variable in pie chart (역주행인 곡)
df$label[df$label == "역주행 NO"] <- 0
df$label[df$label == "역주행 YES"] <- 1

df_genre = df %>% dplyr::select(genre, label)

data <- df_genre %>% 
  filter(label == 1) %>%
  group_by(genre) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(genre))

data$label <- scales::percent(data$per)

ggplot(data = data)+
  geom_bar(aes(x = "", y = per, fill = genre), stat = "identity", width = 1) +
  scale_fill_brewer(palette="Spectral") +
  coord_polar("y", start = 0, direction = -1) +
  theme_void()+
  theme_set(theme_gray(base_family = 'NanumGothic')) +
  geom_text(aes(x = 1.2, y = cumsum(per) - per/2, label = label), size = 3) +
  labs(title = "역주행곡 장르 분석")

# plot of genre variable in pie chart (역주행 아닌 곡)
data <- df_genre %>% 
  filter(label == 0) %>%
  group_by(genre) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(genre))

data$label <- scales::percent(data$per)

ggplot(data = data)+
  geom_bar(aes(x = "", y = per, fill = genre), stat = "identity", width = 1) +
  scale_fill_brewer(palette="Spectral") +
  coord_polar("y", start = 0, direction = -1) +
  theme_void()+
  theme_set(theme_gray(base_family = 'NanumGothic')) +
  geom_text(aes(x = 1.2, y = cumsum(per) - per/2, label = label), size = 3) +
  labs(title = "역주행(아닌)곡 장르 분석")


# plot of lexical density by 역주행 여부
df$label <- as.factor(df$label)

df$label[df$label == 0] <- "역주행 NO"
df$label[df$label == 1] <- "역주행 YES"

ggplot(df, aes(x=label, y=lexical_density)) + 
  geom_boxplot(aes(fill = label)) +
  geom_jitter(position=position_jitter(0.2), aes(color = label), alpha = 0.2, size = 0.3) + 
  theme_set(theme_gray(base_family='NanumGothic')) + 
  labs(title = "Lexical Density 비교", x = "역주행곡", y = "Lexical Density")

# plot of lexical density by year
df$year <- as.factor(df$year)

df %>% filter(!is.na(year)) %>% ggplot(aes(x=year, y=lexical_density)) + 
  geom_boxplot(aes(fill = year)) +
  geom_jitter(position=position_jitter(0.2), aes(color = year), alpha = 0.2, size = 0.3) + 
  theme_set(theme_gray(base_family='NanumGothic')) + 
  labs(title = "Lexical Density 비교", x = "year", y = "Lexical Density") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8)) 


# plot of num_words by 역주행 여부
df %>% filter(!is.na(num_words)) %>%
  ggplot(aes(x=label, y=num_words)) + 
  geom_boxplot(aes(fill = label)) +
  geom_jitter(position=position_jitter(0.2), aes(color = label), alpha = 0.2, size = 0.3) + 
  theme_set(theme_gray(base_family='NanumGothic')) + 
  labs(title = "가사의 단어 수 비교", x = "역주행곡", y = "가사의 단어 수")

# plot of num_words by year
df %>% filter(!is.na(num_words)) %>% filter(!is.na(year)) %>%
  ggplot(aes(x=year, y=num_words)) + 
  geom_boxplot(aes(fill = year)) +
  geom_jitter(position=position_jitter(0.2), aes(color = year), alpha = 0.2, size = 0.3) + 
  theme_set(theme_gray(base_family='NanumGothic')) + 
  labs(title = "가사의 단어 수 비교", x = "year", y = "가사의 단어 수") +
  theme(axis.text.x=element_text(angle=45, hjust=1, size=8)) 


# 가수별 역주행 곡 갯수
df$artist <- as.character(df$artist)
df_tidy <- df %>% filter(label == "역주행 YES")

as_tibble(data.frame(table(df_tidy$artist)))  %>% 
  mutate(Artist = as.character(Var1)) %>% 
  dplyr::select(-Var1) %>% 
  arrange(-Freq)  %>% 
  top_n(25, Freq) %>% 
  ggplot(aes(reorder(Artist, Freq), Freq)) +
  geom_bar(stat = "identity", fill = "#1B687E") +
  labs(title = "가수별 역주행 곡 개수", y = NULL, x = NULL) +
  geom_text(aes(label = Freq) , hjust = -0.25) +
  theme(axis.text.y = element_text(family = "Apple SD Gothic Neo")) +
  coord_flip()


# creating new variable top_artist
toplst <- c(unique(df_tidy$artist))
df$top_artist <- 0

for (i in 1:6048){
  if (df$artist[i] %in% toplst){
    df$top_artist[i] <- 1
  }
}

# creating new variable feat
df$title <- as.character(df$title)
df$feat <- df$title
df$feat <- lapply(df$feat, function(x) replace(x, grep("Feat.", x), 1))
df$feat[df$feat != "1"] <- 0

# creating new variable onair (combining new datatable)
df$onair <- 0
lst <- c("나는 가수다", "위대한 탄생", "무한도전", "명불허전", "나는 작사가다", "슈퍼스타", "불후의 명곡", "SBS", "보이스 코리아", "보이스코리아", "MBC", "Voice Korea", "Show Me The Money", "쇼미더머니", "K팝 스타", "OST", "복면가왕", "언니들의 슬램덩크", "언프리티 랩스타", "고등래퍼")

for (i in 1:6048){
  for (item in lst){
    if (grepl(item, df$album[i])){
      df$onair[i] <- 1
    }
  }
}

# creating new variable weather and season
df$season <- 0
lst <- c("봄", "여름", "가을", "겨울", "크리스마스", "christmas", "Christmas", "벚꽃", "Summer", "summer", "Valentine", "valentine", "발렌타인")
for (i in 1:6048){
  for (item in lst){
    if (grepl(item, df$title[i])){
      df$season[i] <- 1
    }
  }
}

df$weather <- 0
lst <- c("비", "Rainy", "rain", "Rain", "rainy", "눈", "소나기")
for (i in 1:6048){
  for (item in lst){
    if (grepl(item, df$title[i])){
      df$weather[i] <- 1
    }
  }
}

# creating new variable top_artist: if ever appeared on top 3 chart
df_final <- read.csv("datatable/df_rank.csv")
df_rank3 <- df_final %>% filter(rank %in% c(1,2,3)) %>% dplyr::select(artist)
df_rank3$artist <- as.character(df_rank3$artist)
df_rank3 <- df_rank3[!duplicated(df_rank3), ]

df$rank_3 <- 0
for (i in 1:6048){
  if (df$artist[i] %in% df_rank3){
    df$rank_3[i] <- 1
  }
}

