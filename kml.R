# Install the required packages if you haven't already
install.packages("kml")
install.packages("ggplot2")

# Load the packages
library(kml)
library(ggplot2)


# Load the artificialLongData dataset
data(artificialLongData)

# Reshape data to long format
longData <- reshape(artificialLongData, varying = list(names(artificialLongData)[3:12]), 
                    v.names = "value", timevar = "time", idvar = "id", direction = "long")

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

# Perform K-means clustering
kml(cld_data, nbClusters = 3) # Let's assume we want 3 clusters

# Plot the results
plotTrajMeans(cld_data)

# Get the cluster assignments
clusters <- as.data.frame(getClusters(cld_data))
names(clusters) <- c("id", "cluster")
head(clusters)

# Merge the cluster assignments with the original data
longData <- merge(longData, clusters, by = "id")

# Visualize the clusters
ggplot(longData, aes(x = time, y = value, color = as.factor(cluster), group = id)) +
  geom_line(alpha = 0.5) +
  labs(title = "K-means Clustering of artificialLongData", x = "Time", y = "Value", color = "Cluster") +
  theme_minimal()

