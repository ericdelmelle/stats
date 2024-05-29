# Install the required packages if you haven't already
install.packages("longitudinal")
install.packages("ggplot2")
install.packages("kml")

# Load the packages
library(longitudinal)
library(ggplot2)
library(kml)
library(dplyr)

# Load the artificialLongData dataset
data(artificialLongData)

# Reshape data to long format
longData <- reshape(artificialLongData, varying = list(names(artificialLongData)[3:12]), 
                    v.names = "value", timevar = "time", idvar = "id", direction = "long")

longData <- longData %>%
  mutate(id = as.numeric(gsub("s", "", id)))
# Check the reshaped data
head(longData)


# Create the spaghetti plot
ggplot(longData, aes(x = time, y = value, group = id)) +
  geom_line(alpha = 0.5) +
  labs(title = "Spaghetti Plot of artificialLongData", x = "Time", y = "Value") +
  theme_minimal()


# Convert longData to wide format for cld function
wideData <- reshape(longData, timevar = "time", idvar = "id", direction = "wide")

# Ensure the correct format for the clusterLongData function
wideData <- wideData[order(wideData$id),]

# Create a 'cld' object
cld_data <- clusterLongData(wideData)

nbCluster = 2

# Perform K-means clustering
kml(cld_data, nbCluster) # Let's assume we want 3 clusters


# Get the cluster assignments
clusters <- as.data.frame(getClusters(cld_data, nbCluster))
colnames(clusters)<- c("cluster")

clusters <- clusters %>%
  mutate(id = row_number())

# Merge the cluster assignments with the original data
longData <- merge(longData, clusters, by = "id")

# Visualize the clusters
ggplot(subset(longData, id < 10), aes(x = time, y = value, color = as.factor(cluster), group = id)) +
  geom_line(alpha = 0.5) +
  labs(title = "K-means Clustering of artificialLongData", x = "Time", y = "Value", color = "Cluster") +
  theme_minimal()
