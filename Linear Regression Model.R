library(ggplot2)
library(dplyr)
library(data.table)
library(gtools)
library(corrplot)

df_eda <- df %>% dplyr::select(genre, label, positive:trust, num_words, lexical_density, year, month, top_artist, feat, onair, season, weather, rank_3, times_appear) %>% dplyr::select(-disgust)

# mutating count to percent
df_eda$label <- as.character(df_eda$label)
df_eda$label[df_eda$label == "¿ªÁÖÇà YES"] <- 1
df_eda$label[df_eda$label == "¿ªÁÖÇà NO"] <- 0

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

# cramer's V heatmap
cramer <- df_eda[, c(1, 2, 14:21)]
cat_var <- colnames(cramer)

cv.test = function(x,y) {  ## function to compute Cramer's V
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],length(unique(y))[1]) - 1)))
  return(as.numeric(CV))
}

v_cramer_all <- function(cat_var, df){
  cat_var_grid <- data.table(combinations(n = length(cat_var), r = 2, v = cat_var, repeats.allowed = FALSE))
  
  do.call(rbind,
          apply(cat_var_grid, 1, function(x){
            tmp <- as.character(x)
            vec1 <- unlist(df[tmp[1]])
            vec2 <- unlist(df[tmp[2]])
            
            data.table(
              variable_x = tmp[1],
              variable_y = tmp[2],
              chi2 = chisq.test(x = vec1, vec2, correct=FALSE)$p.value,
              v_cramer = cv.test(x = vec1, y = vec2)
            )
          }))
  
}

results <- v_cramer_all(cat_var = cat_var, df = cramer)

ggplot(results, aes(variable_x, variable_y)) +
  geom_tile(aes(fill = v_cramer), colour = "black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_set(theme_gray(base_family = 'NanumGothic')) +
  xlab(NULL) + ylab(NULL) + theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  ggtitle("Cramer's V heatmap")

# correlation between numerical variables
M <- cor(df_eda[, c(3:13, 22)])
corrplot(M, method="circle")

# pair scatter plots between numerical variables
pairs(df_eda[, c(3:13, 22)], pch = 21, cex = 0.1, cex.labels = 1.5)


# regression model
lm <- lm(times_appear ~., data=df_eda)
summary(lm)

# visualization on prediction 1
lm.pred <- predict(lm, df_eda[, -22])
lm.pred <- round(lm.pred)
compare_lm <- cbind(lm.pred, df_eda[, 22])
compare_lm <- as.data.frame(compare_lm)

compare_lm <- compare_lm %>% filter(!is.na(lm.pred))
mean((compare_lm$lm.pred-compare_lm$V2)^2) # 78.69737

ggplot(data = compare_lm) + 
  geom_histogram(aes(x = compare_lm$lm.pred, fill = "predicted value"), ,alpha = 0.5, binwidth = 1) + 
  geom_histogram(aes(x = compare_lm$V2, fill = "true value"), alpha = 0.5, binwidth = 1) + 
  labs(title = "Comparison for linear regression", x = "number of violent crimes per 100k", y = "count") + 
  guides(fill=guide_legend(title="Values"))

# visualization on prediction 2
ggplot(aes(x=actual, y=pred), data=data.frame(actual=compare_lm$V2, pred=compare_lm$lm.pred)) + 
  geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("Linear Regression r^2=", 0.1395, sep=""))


