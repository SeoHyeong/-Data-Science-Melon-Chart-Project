library(stringr)

# loading
DTM <- read.csv('/Users/jeongseohyeong/Dropbox/Yonsei/Fall_18/Data Science/project/dtm.csv', fileEncoding='utf-8', sep=",", header=F, skip=1)
vec <- read.csv('/Users/jeongseohyeong/Dropbox/Yonsei/Fall_18/Data Science/project/lyric_w2b_result.txt', fileEncoding='utf-8', sep=" ", header=F, skip=1)

# distance matrix
distance <- dist(vec[,-1]) # measure Euclidean distance
distance <- as.matrix(distance) 
colnames(distance) <- vec[,1]
rownames(distance) <- vec[,1]

# weight matrix
func <- c('사랑','겨울','크리스마스','가족')
funclocation <- which(rownames(distance) %in% func)
weight <- distance[funclocation,]
weight <- exp(-weight^2/100)
location <- order(colnames(weight))
weight <- weight[,location]

# weight 행렬의 단어와 DTM 행렬의 단어가 일치해야 내적 의미가 있음, 0이 나와야 함
rownames(DTM) <- DTM[, 1]
DTM <- DTM[, -1]
which(colnames(weight) != rownames(DTM)) 

# inner-product
result <- t(as.matrix(DTM)) %*% t(weight)

# 곡별 가사 길이 체크
doc.length <- colSums(DTM)

# post-processing     <- 여기서부터 다시 보기
for (i in 1:dim(result)[1]) {
  if (is.na(doc.length[i]) == T) {
    doc.length[i] <- 1
  }
  # 단어 개수가 2 이하인 리뷰는 아예 빠지도록 함
  if (doc.length[i] < 3) {
    result[i,] <- 0 
  } 
  # 문장별 스코어를 단어 개수로 나누어 Normalize
  else {
    result[i,] <- result[i,] / doc.length[i]
  }
}

## 가장 높은 스코어로 뽑으면 해당 단어에 대한 노래 추천 가능
lst <- (seq(from = 1, by = 1, length.out = 6038))
rownames(result) <- lst

## for example
result <- as.data.frame(result)
result_겨울 <- result[order(result$겨울,decreasing=T)[10:15],]
rownames(result_겨울)


