###################################
# PCA and cluster analysis
###############################
# Setup
########
install.packages("factoextra")
install.packages("cluster")
library(tidyverse)
library(factoextra)
library(readxl)
library(cluster)
library(factoextra)

setwd("C:/Users/stepan.jaburek/Downloads")
#data<-read_csv("0_kw_analysis.csv")
data<-read_excel("pdf_search_results(1).xlsx")
metadata<-read_excel("pdf_list(1).xlsx")
##############################
# Data cleaning and manipulation
###########################
# Lookup if data and metadata have the same papers
missing_file_data <- setdiff(data$filename, metadata$file_name)
missing_file_data
missing_file_metadata <- setdiff( metadata$file_name, data$filename)
missing_file_metadata

# Rename "Broekaert_2018" in metadata
metadata$file_name[252] <- "Broekaert_2018_The Tacit ‘Quantum’ of Meeting the Aesthetic Sign; Contextualize, Entangle,2.pdf"
data$filename[252] <- "Broekaert_2018_The Tacit ‘Quantum’ of Meeting the Aesthetic Sign; Contextualize, Entangle,2.pdf"
# Delete duplicit "Yukalov et al. - 2018 - Information processing by networks of quantum deci.pdf"
data <- data %>% slice(-1170)
# Delete missing "Yilmaz - 2017 - Quantum cognition models of ethical decision-makin.pdf"
data<-data %>% slice(-1166)
# Delete missing "Park_2016_Decision-making &amp quantum mechanical models of cognitive processing.pdf"
metadata<-metadata %>% slice(-892)

# Exclude authors
df<-data %>% select(-2,-3,-5,-17,-31)
# Exclude social science fields
df<-df %>% select(-27:-38)
# Possibly exclude "quantum" and "quantization" for robustness checks
df<-df %>% select(-18,-19)

# Can I see some ID?
df <- df %>%
  mutate(id = row_number()) %>%
  filter(rowSums(across(2:24)) > 0)
metadata<-metadata %>% mutate(id=row_number())

# Features for PCA
features <- df %>% 
  select(-filename, -id)
feature_names <- colnames(features)

# Row normalization - divide each row by its sum
features <- features %>%
  mutate(across(everything(), ~./rowSums(features)))

# Convert back to dataframe with original column names if needed
features <- as.data.frame(features)
colnames(features) <- feature_names
##############################
# PCA
pca_result <- prcomp(features, scale=FALSE)
summary(pca_result)
fviz_eig(pca_result, 
         title = "Scree Plot of Principal Components",
         xlab = "Principal Components",
         ylab = "Percentage of explained variances")

# Feature importance in components
loadings <- data.frame(pca_result$rotation)
top_loadings <- loadings %>%
  mutate(feature = rownames(loadings)) %>%
  gather(PC, loading, -feature) %>%
  group_by(PC) %>%
  arrange(desc(abs(loading))) %>%
  slice_head(n = 5)
top_loadings


#########################
# HCPC
# PCA
res.pca <- PCA(features, 
               scale.unit = FALSE, 
               ncp = 8, # number of dimensions kept in results
               graph = FALSE) 

#HIerachical clusering
hc <- HCPC(res.pca, nb.clust=-1, # number of clusters. -1 find ideal number
           method = "ward")


# 1. Plot just the dendrogram tree with inertia gain barplot
plot(hc, 
     choice = "tree",
     tree.barplot = TRUE,    
     rect = TRUE)            

# 2. Plot 2D factor map with cluster colors
plot(hc,
     choice = "map",         
     draw.tree = TRUE,      
     ind.names = FALSE,      
     title = "Cluster Map with Projected Tree")

# 3. Create 3D visualization
plot(hc,
     choice = "3D.map",      
     angle = 60,             
     centers.plot = TRUE,   
     ind.names = FALSE,      
     title = "3D Cluster Visualization")

# 4. Just the inertia gain barplot
plot(hc,
     choice = "bar",
     title = "Inertia Gains")


plot(hc,
     choice = "map",
     axes = c(1,3),         # Look at dimensions 1 and 3
     draw.tree = TRUE,
     ind.names = FALSE,
     title = "Cluster Map (Dimensions 1 and 3)")




# 5. K-means to finalize
set.seed(42)  
kmeans_result <- kmeans(pca_result$x[, 1:8], centers=3)
final_labels <- kmeans_result$cluster

fviz_cluster(kmeans_result, 
             data = pca_result$x[, 1:2],
             main = "Cluster plot",
             geom = "point",
             ellipse.type = "convex")






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

############
# Cluster analysis
########


# PAM
pam_result <- pam(pca_result$x[, 1:5], k=3)
fviz_cluster(pam_result, 
             data = pca_result$x[, 1:3],
             main = "PCA with PAM Clustering")


# Get the centroid papers (here we keep 10 with ordering by distance)
centroid_coords <- pam_result$medoids

top_centroids <- lapply(1:nrow(centroid_coords), function(i) {
  distances <- apply(pca_result$x[, 1:5], 1, function(x) 
    sum((x - centroid_coords[i,])^2))
  top_indices <- order(distances)[1:10]
  data.frame(
    cluster = i,
    rank = 1:10,
    centroid_id = df$id[top_indices],
    distance = distances[top_indices]
  )
})

centroids <- do.call(rbind, top_centroids)
centroids

# Bind the centroid papers with their respective metadata
centerpapers <- left_join(centroids, metadata, by = c("centroid_id" = "id"))
dataset<-left_join(data,metadata, by=c("filename" = "file_name"))
############################################################################x


## Not run:
iris
data(iris)
# Principal Component Analysis:
res.pca <- PCA(iris, graph=FALSE)
# Clustering, auto nb of clusters:
hc <- HCPC(res.pca, nb.clust=-1)
### Construct a hierarchical tree from a partition (with 10 clusters)
### (useful when the number of individuals is very important)
hc2 <- HCPC(iris[,1:4], kk=10, nb.clust=-1)
## Graphical interface
install.packages("Factoshiny")
require(Factoshiny)
res <- Factoshiny(iris[,1:4])

