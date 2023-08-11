# Author: Sutulova Tatiana, 30806151
# Assignment 3
# The objective of this assignment is to create a corpus of documents and analyse 
# the relationships between them, as well as the relationships between the important words used in these documents.

rm(list = ls())

library(slam)
library(tm)
library(SnowballC)
library(igraph)

cname = file.path(".", "CorpusAbstracts", "txt")
docs = Corpus(DirSource(cname))
summary(docs)

# 1.3 Processing steps to create DTM
# Removing punctuation
docs <- tm_map(docs, removePunctuation)
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, '“')
docs <- tm_map(docs, toSpace, '”')
docs <- tm_map(docs, toSpace, "–")

# Removing numbers
docs <- tm_map(docs, removeNumbers)

# Converting everything to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Removing stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

# Normalizing spaces
docs <- tm_map(docs, stripWhitespace)

# Removing useless words
docs <- tm_map(docs, toSpace, "can")
docs <- tm_map(docs, toSpace, "one")
docs <- tm_map(docs, toSpace, "many")
docs <- tm_map(docs, toSpace, "new")
docs <- tm_map(docs, toSpace, "often")
docs <- tm_map(docs, toSpace, "people")
docs <- tm_map(docs, toSpace, "work")
docs <- tm_map(docs, toSpace, "may")
docs <- tm_map(docs, toSpace, "also")
docs <- tm_map(docs, toSpace, "even")
docs <- tm_map(docs, toSpace, "life")
docs <- tm_map(docs, toSpace, "time")
docs <- tm_map(docs, toSpace, "two")
docs <- tm_map(docs, toSpace, "way")
docs <- tm_map(docs, toSpace, "make")
docs <- tm_map(docs, toSpace, 'first')
docs <- tm_map(docs, toSpace, 'use')
docs <- tm_map(docs, toSpace, 'still')
docs <- tm_map(docs, toSpace, 'take')
docs <- tm_map(docs, toSpace, 'change')
docs <- tm_map(docs, toSpace, 'live')
docs <- tm_map(docs, toSpace, 'made')
docs <- tm_map(docs, toSpace, 'ing')
docs <- tm_map(docs, toSpace, 'remain')
docs <- tm_map(docs, toSpace, 'day')
docs <- tm_map(docs, toSpace, 'however')
docs <- tm_map(docs, toSpace, 'like')
docs <- tm_map(docs, toSpace, 'form')
docs <- tm_map(docs, toSpace, 'much')
docs <- tm_map(docs, toSpace, 'known')
docs <- tm_map(docs, toSpace, 'will')
docs <- tm_map(docs, toSpace, 'see')
docs <- tm_map(docs, toSpace, 'another')
docs <- tm_map(docs, toSpace, 'person')
docs <- tm_map(docs, toSpace, 'without')
docs <- tm_map(docs, toSpace, 'last')
docs <- tm_map(docs, toSpace, 'towards')
docs <- tm_map(docs, toSpace, 'around')
docs <- tm_map(docs, toSpace, 'well')
docs <- tm_map(docs, toSpace, 'took')
docs <- tm_map(docs, toSpace, 'make')
docs <- tm_map(docs, toSpace, 'just')
docs <- tm_map(docs, toSpace, 'sen')

# Stemming
docs <- tm_map(docs, stemDocument, language = "english")
docs <- tm_map(docs, toSpace, 'includ')

# Creating DTM
dtm <- DocumentTermMatrix(docs)

# Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.65)
dim(dtms)
inspect(dtms)

write.csv(as.matrix(dtms), file = "dtms.csv", row.names = TRUE)

################################################################################
# 1.4 Hierarchical clustering
# Conventional clustering using Euclidean distance
distmatrix = dist(scale(as.matrix(dtms)))
fit = hclust(distmatrix, method = "ward.D")
plot(fit)
plot(fit, hang = -1)

# Clustering based on Cosine Distance.
# Converting DTM to a matrix
tf <- as.matrix(dtms)
# Calculating IDF values
idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) 
# Converting IDF values into a diagonal matrix 
idf <- diag(idf)
# Calculating TF-IDF matrix
tf_idf <- crossprod(tf, idf)
# Assigning the row names of tf as the column names of tf_idf
colnames(tf_idf) <- rownames(tf)
# Calculating the cosine distance between each pair of documents in the tf_idf
cosine_dist = 1-crossprod(tf_idf) /(sqrt(colSums(tf_idf^2)%*%t(colSums(tf_idf^2))))
# Converting matrix into a distance object
cosine_dist <- as.dist(cosine_dist)
# Performing hierarchical clustering
cluster1 <- hclust(cosine_dist, method = "ward.D")
plot(cluster1)
plot(cluster1, hang = -1)

################################################################################
# 1.5 Documents single-mode network creation
# Starting with original document-term matrix
dtmsx = as.matrix(dtms)
# Convert to binary matrix
dtmsx = as.matrix((dtmsx>0)+0)
# Multiply binary matrix by its transpose
ByAbsMatrix = dtmsx%*%t(dtmsx)
#Make leading diagonal zero
diag(ByAbsMatrix) = 0
#create graph object (Basic plot)
ByAbs= graph_from_adjacency_matrix(ByAbsMatrix, mode = "undirected", weighted = TRUE)
plot(ByAbs)

# IMPROVING THE PLOT
#identify community groups/clusters using cluster_fast_greedy()
cfg = cluster_fast_greedy(as.undirected(ByAbs))
g_cfg = plot(cfg, as.undirected(ByAbs),vertex.label=V(ByAbs)$role,main="Fast Greedy")
# Set edge width based on weight:
E(ByAbs)$width <- E(ByAbs)$weight/3
# Calculate centrality measures
degree <- degree(ByAbs)
betweenness <- betweenness(ByAbs)
closeness <- closeness(ByAbs)
eigenvector <- eigen_centrality(ByAbs)$vector
# Combine centrality measures into a single score
combined_centrality <- (degree + betweenness + closeness + eigenvector) / 4
# Assign colors based on combined centrality score
node_colors <- heat.colors(max(combined_centrality) + 1)[combined_centrality + 1]
# Plot the graph with assigned colors
plot(ByAbs, vertex.color = node_colors)

################################################################################
# 1.6 Tokens single-mode network creation
# Multiply binary matrix by its transpose
ByTokenMatrix = t(dtmsx)%*%dtmsx
#Make leading diagonal zero
diag(ByTokenMatrix) = 0
#create graph object (Basic plot)
ByToken= graph_from_adjacency_matrix(ByTokenMatrix, mode = "undirected", weighted = TRUE)
plot(ByToken)

# IMPROVING THE PLOT
#identify community groups/clusters using cluster_fast_greedy()
cfg = cluster_fast_greedy(as.undirected(ByToken))
g_cfg = plot(cfg, as.undirected(ByToken),vertex.label=V(ByToken)$role,main="Fast Greedy")
# Set edge width based on weight:
E(ByToken)$width <- E(ByToken)$weight/3
# Calculate centrality measures
degree <- degree(ByToken)
betweenness <- betweenness(ByToken)
closeness <- closeness(ByToken)
eigenvector <- eigen_centrality(ByToken)$vector
# Combine centrality measures into a single score
combined_centrality <- (degree + betweenness + closeness + eigenvector) / 4
# Assign colors based on combined centrality score
node_colors <- heat.colors(max(combined_centrality) + 1)[combined_centrality + 1]
# Plot the graph with assigned colors
plot(ByToken, vertex.color = node_colors)

################################################################################
# 1.7 Bipartite network creation
# Converting dtmsx to a data frame
dtmsa = as.data.frame(as.matrix(dtms)) 
# Create a new column with row names
dtmsa$ABS = rownames(dtmsa) 
# Creating an empty data frame
dtmsb = data.frame()
# Iterating over the rows and columns of dtmsa
for (i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa)-1)){
    # Building the transformed data frame
    touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)], colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb, touse)
  }
}
# Setting the proper column names
colnames(dtmsb)=c("weight", "abs", "token")
# Selecting only the rows where the "weight" column is not zero. 
dtmsc = dtmsb[dtmsb$weight!=0,] #delete weights
# Reorder the columns
dtmsc = dtmsc[,c(2,3,1)]
dtmsc
dtmsc$weight <- as.numeric(dtmsc$weight)
# Create graph object and declare bipartite
g <-graph.data.frame(dtmsc, directed = FALSE)
bipartite.mapping(g) # perform bipartite mapping 
# Assigning attributes
V(g)$type <-bipartite.mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color<-"lightgray"
plot(g)

# IMPROVING THE PLOT
#identify community groups/clusters using cluster_fast_greedy()
cfg = cluster_fast_greedy(as.undirected(g))
g_cfg = plot(cfg, as.undirected(g),vertex.label=V(g)$role, main="Fast Greedy")
# Set edge width based on weight:
E(g)$width <- E(g)$weight/3
# Calculate centrality measures
degree <- degree(g)
betweenness <- betweenness(g)
closeness <- closeness(g)
eigenvector <- eigen_centrality(g)$vector
# Combine centrality measures into a single score
combined_centrality <- (degree + betweenness + closeness + eigenvector) / 4
# Assign colors based on combined centrality score
node_colors <- heat.colors(max(combined_centrality) + 1)[combined_centrality + 1]
# Plot the graph with assigned colors
plot(g, vertex.color = node_colors)




