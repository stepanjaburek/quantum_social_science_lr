###################################
# PCA and cluster analysis
###############################
# setup
#install.packages("factoextra")
#install.packages("cluster")
library(tidyverse)
library(factoextra)
library(cluster)

setwd("C:/Users/stepa/downloads")
df<-read_csv("0_kw_analysis.csv")

###
# define the variables of interest
filenames <- df$filename
features <- df %>% select(-filename)

# run PCA
pca_result <- prcomp(features, scale. = TRUE)
summary(pca_result)
fviz_eig(pca_result, 
  title = "Scree Plot of Principal Components",
  xlab = "Principal Components",
  ylab = "Percentage of explained variances")

# feature importance in components
loadings <- data.frame(pca_result$rotation)
top_loadings <- loadings %>%
  mutate(feature = rownames(loadings)) %>%
  gather(PC, loading, -feature) %>%
  group_by(PC) %>%
  arrange(desc(abs(loading))) %>%
  slice_head(n = 5)

top_loadings

# PC1 and 2
pc12_loadings<-top_loadings %>%
  filter(PC %in% c("PC1", "PC2")) %>%
  arrange(PC, desc(abs(loading)))
pc12_loadings



# PCA biplot
fviz_pca_biplot(pca_result,
  geom.ind = "point",  
  label = "var",      
  title = "PCA of Paper Concepts")

###################################
# cluster analysis
set.seed(456)
n_clusters <- 5  
km_result <- kmeans(pca_result$x[, 1:2], centers = n_clusters)

# visualize the clusters
fviz_cluster(km_result, 
             data = pca_result$x[, 1:2],
             main = "PCA with K-means Clustering")

######################
# Random Forest
install.packages("randomForest")
library(randomForest)

target <- features$`political science`
predictors <- features %>% 
  select(-`political science`)

# enter the (random) forest!
set.seed(123)
rf_model <- randomForest(x = predictors, 
                         y = target,
                         importance = TRUE)

# variable importance
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("feature") %>%
  arrange(desc(`%IncMSE`))

# plot top 20 most important features
ggplot(head(importance_df, 20), 
       aes(x = reorder(feature, `%IncMSE`), y = `%IncMSE`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Features",
       y = "% Increase in MSE",
       title = "Top 20 Concepts Most Predictive of Political Science")

