HEXAGON GRAVEYARD




# **HEXAGONS!!!**
### Step 1: check for geometry
```{r}
nc_tracts_data <- st_make_valid(nc_tracts_data)  # This requires sf version >= 0.9-0

# Alternatively, try to clean geometries if st_make_valid is not available
nc_tracts_data <- nc_tracts_data %>% 
  mutate(geometry = st_buffer(geometry, dist = 0))

# Check for any remaining invalid geometries
invalid_geoms <- sum(!st_is_valid(nc_tracts_data))
print(paste("Invalid geometries count:", invalid_geoms))
```

### Transform to a suitable projections (like UTM Mercator)
Do this for the input data, and the outline
```{r}
nc_tracts_data <- st_transform(nc_tracts_data, crs = 32617)  
nc_outline <- st_transform(nc_outline, crs = 32617)  
```

### Generate the hexagonal grid, conduct join, aggregate data
```{r}
output_file <- "HexComputation_times.txt"
write("cellSize, HexCreation, Join, Summarize", output_file) # write the header

possibleSizes <-c(5000, 2500, 1000, 500, 250, 100, 50, 30)
for(cellSize in possibleSizes){
  
  print(cellSize)
  
  
  
  start.time <- Sys.time()
  hexgrid <- st_make_grid(nc_tracts_data, cellSize, what = 'polygons', square = FALSE) %>% 
    st_as_sf()
  end.time <- Sys.time()
  time.takenCreate <- round(as.numeric(difftime(end.time, start.time, units = "secs")),digits = 3)
  print(time.takenCreate)
  
  # Create a spatial join to associate hexagons with tracts data
  start.time <- Sys.time()
  hex_tracts <- st_join(hexgrid, nc_tracts_data, join = st_intersects)
  end.time <- Sys.time()
  time.takenJoin <- round(as.numeric(difftime(end.time, start.time, units = "secs")),digits = 3)
  print(time.takenJoin)
  
  
  # Aggregating data
  start.time <- Sys.time()
  hex_data <- hex_tracts %>%
    group_by(x) %>%
    summarise(
      white_prop = mean(B03002_003, na.rm = TRUE),
      med_HH_inc = mean(B19013_001, na.rm = TRUE),
      collegeDegree = mean(B15003_022, na.rm = TRUE),
      .groups = 'drop'
    )
  end.time <- Sys.time()
  time.takenSummarize <- round(as.numeric(difftime(end.time, start.time, units = "secs")),digits = 3)
  print(time.takenSummarize)
  
  
  line_to_write <- paste(cellSize, as.numeric(time.takenCreate), 
                         as.numeric(time.takenJoin), 
                         as.numeric(time.takenSummarize),sep = ", ")
  write(line_to_write, file = output_file, append = TRUE)
}
```


### Visualization of the running time to create hexagon, interpolate and summarize
```{r}

# Import fonts (do this once per session)
font_import()
loadfonts(device = "win")  # Use 'device = "win"' on Windows; omit or adjust on other OS

# List available fonts to find the one you want
fonts()


# Read the data back into R from the text file
computation_times <- read.table("HexComputation_times.txt", header = TRUE, sep = ",", skip = 1)

# Rename columns appropriately
colnames(computation_times) <- c("cellSize", "HexCreation", "Join", "Summarize")

computation_times <- computation_times %>%
  mutate(total_time = HexCreation + Join +Summarize)




# Revised ggplot code with corrected color mapping
ggplot(data = computation_times, aes(x = cellSize)) +
  geom_point(aes(y = log(HexCreation), color = "Hexagon Creation"), size = 2) +
  #geom_point(aes(y = log(HexCreation)), shape = 3, color = "black", size = 2) +
  geom_line(aes(y = log(HexCreation), color = "Hexagon Creation"), linetype = "dashed") +
  geom_point(aes(y = log(Join), color = "Intersection"), size = 2) +
  geom_line(aes(y = log(Join), color = "Intersection"), linetype = "dashed") +
  geom_point(aes(y = log(Summarize), color = "Summarize"), size = 2) +
  geom_line(aes(y = log(Summarize), color = "Summarize"), linetype = "dashed") +
  geom_point(aes(y = log(total_time), color = "Total Time"), size = 2) +  # Ensure this matches the scale_color_manual
  geom_line(aes(y = log(total_time), color = "Total Time")) +            
  geom_hline(yintercept = log(30 * 60), color = "gray15", linetype = "dashed", linewidth = 0.1) +
  annotate("text", x = 2200, y = log(2500), label = "30 min", 
           color = "gray15", hjust = -0.1, size=4) +
  
  geom_hline(yintercept = log(5 * 60), color = "gray15", linetype = "dashed", linewidth = 0.1) +
  annotate("text", x = 2200, y = log(400), label = "5 min", 
           color = "gray15", hjust = -0.1, size=4) +
  
  geom_hline(yintercept = log(1 * 60), color = "gray15", linetype = "dashed", linewidth = 0.1) +
  annotate("text", x = 2200, y = log(80), label = "1 min", 
           family = "sans", color = "gray15", hjust = -0.1, size=4) +
  
  scale_color_manual(
    values = c("Hexagon Creation" = "#66ccff", 
               "Intersection" = "#00802b", 
               "Summarize" = "#aa80ff",
               "Total Time" = "#e60000"                              # Correct color mapping
    ),
    labels = c("Hexagon Creation", "Intersection", "Summarize", "Total Time")
  ) +
  labs(x = "Hexagon Cell Size",
       y = "Time (seconds)",
       color = "Process"
  ) +
  #labs(title = "Normalized Census Data Indicators by Tract in North Carolina") +
  theme(
    panel.background = element_rect(fill = "#e8e3cf"),
    strip.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #axis.ticks = element_blank()
  )
theme_light(base_size = 14)  # Set base font size to 14 for better readability
```

### Only retain the hexagons `inside` outline
```{r}
hex_data <- st_intersection(hex_data, nc_outline)
```

### Visualize hexagons
```{r}
# Extract colors from the "Oranges" palette
palette <- brewer.pal(9, "Greens")

# Use a continuous color scale with reversed shades
ggplot(data = hex_data) +
  geom_sf(aes(fill = white_prop),color = NA) +
  scale_fill_gradient(low = palette[1], high = palette[9]) +  # Light to dark
  geom_sf(data = nc_outline, fill = NA, color = "black", size = 0.5) +  # Add state outline with no fill
  labs(fill = "proportion white",
       x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
```

```{r}
# Creating the interactive map with a different color palette
# Choose a sequential palette, e.g., "Blues"
sequential_palette <- brewer.pal(10, "Blues")

# Create the color palette function for Leaflet
color_palette <- colorFactor(palette = sequential_palette, domain = hex_data$white_prop)

hex_data_LL <- st_transform(hex_data, crs = 4326)

# Creating the interactive map with the sequential color palette
map <- leaflet(hex_data_LL) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~color_palette(white_prop),
    fillOpacity = 0.7,
    color = "#BDBDC3",
    weight = 1,
    popup = ~paste("White Proportion:", white_prop)
  )

# Print the map
map
```

### Visualize multiple datasets
```{r}
# Assuming hex_data is your existing dataset
hex_data_long <- hex_data %>%
  pivot_longer(
    cols = c(white_prop, med_HH_inc, collegeDegree),
    names_to = "variable",
    values_to = "value"
  )

ggplot(data = hex_data_long, aes(fill = value)) +
  geom_sf(color = NA) +  # Remove outline color
  scale_fill_gradientn(colors = brewer.pal(9, "Greens")) +  # Reverse the palette
  facet_wrap(~ variable, nrow = 3) +  # Change to 3 rows
  geom_sf(data = nc_outline, fill = NA, color = "black", size = 0.5) +  # State outline
  labs(fill = "Value",
       x = "", y = "") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
  )
```

## k-means on the hexagons
```{r}
set.seed(123)  # for reproducibility

hex_data <- hex_data %>%
  select(white_prop, med_HH_inc, collegeDegree) %>%  # Select relevant columns
  na.omit()  # Remove rows with any NAs in the selected columns
summary(hex_data)

# Convert sf object to regular dataframe by setting geometry to NULL
hex_df <- st_set_geometry(hex_data, NULL)


kmeans_result <- kmeans(hex_df, centers = 5)


# Add the cluster results to the data frame
hex_data$cluster <- as.factor(kmeans_result$cluster)


ggplot(data = hex_data) +
  geom_sf(aes(fill = factor(cluster)), color = NA) +  # Use factor if 'cluster' is numeric
  scale_fill_brewer(palette = "Dark2") +
  geom_sf(data = nc_outline, fill = NA, color = "black", size = 0.5) +  # State outline
  labs(title = "K-Means Clustering",
       subtitle = "Race, Income, and Education",
       fill = "cluster") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

```


```{r, label classes}
# Define the cluster labels
cluster_labels <- setNames(
  c("Low Income, Low Education", "High Income, High Education", "Diverse Background"),
  c("1", "2", "3")  # Assuming the cluster numbers are 1, 2, and 3
)

# Assuming 'hex_data' is your data frame and 'cluster' is the column with cluster numbers
hex_data$cluster_label <- cluster_labels[as.character(hex_data$cluster)]



# Create the plot using the descriptive cluster labels
ggplot(data = hex_data) +
  geom_sf(aes(fill = cluster_label), color = NA) +  # Use cluster_label for coloring
  scale_fill_brewer(palette = "Set2") +  # Choose an appropriate palette
  labs(
    title = "K-Means Clustering",
    subtitle = "Race, Income, and Education",
    fill = "Cluster Type"  # Update legend title to reflect descriptive labels
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

```