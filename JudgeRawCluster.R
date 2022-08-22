library(tidyverse)
library(cluster)
library(factoextra)

set.seed(1234)



Judge_Num <- JudgeRawCluster [,c(4:8)] 

#Scale Data 
Judge_Scaled <- scale(Judge_Num)


# Convert to Data.frame 
Judge_df <- data.frame(Judge_Scaled)
Judge_df1 <- na.omit(Judge_df)

# Find Optimal Number of Clusters (Groups)
fviz_nbclust(Judge_df,kmeans, method = "gap_stat")

# Bend is at 4

# Have computer create cluster/ grouops 
km.res_Judge <- kmeans(Judge_df,4, nstart = 25)


# Combine groups with original data
rownames(Judge_df) <- make.names(JudgeRawCluster$Name, unique = TRUE)

# Create a master Table 
Master_Judge <-cbind(as.data.frame(JudgeRawCluster),as.vector(km.res_Judge$cluster))

#Find the Group Our Player is in 

filter(Master_Judge, Name == "Aaron Judge")

# Create a table with comparable players 

Judge_Comps <- filter(JudgeRawCluster, as.vector(km.res_Judge$cluster) ==1)



write.csv(Judge_Comps, "Judge Comps.csv")


Hit_per_season <- rpois(162,.3*3.9)
sum(Hit_per_season) 

