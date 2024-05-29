# faking the data for 20 patients
set.seed(42)
individual <- as.character(rep(1:20,each=5))
timeperiod <- paste0(rep(c(0, 18,36,54,72),20),"_week")
therapy <- factor(sample(c("Etanercept", "Infliximab", "Rituximab",  "Adalimumab","Missing"), 100, replace=T))
d <- data.frame(individual, timeperiod, therapy)
head(d)

# Plotting it
ggplot(d, aes(x = timeperiod, stratum = therapy, alluvium = individual, fill = therapy, label = therapy)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("Treatment across observation period")


# Plotting it
ggplot(rescaled_vars, aes(x = year, stratum = cluster, alluvium = GEOID, fill = cluster,
                          label = cluster)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("Treatment across observation period")