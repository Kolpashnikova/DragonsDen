#### US Map ####

library(usmap)
library(ggplot2)

# get the averages for an activity from ATUS-X
map_df <- tu_maps(data, "Sleep")

# get labelled data default with usmap
states_df <- statepop

#transform into dataframe and name columns + column with state abbreviations
map_df_1 <- as.data.frame(t(as.matrix(map_df)))
colnames(map_df_1) <- "Sleep"
map_df_1$abbr <- toupper(rownames(map_df_1))

# merge two datasets
df_map <- merge(map_df_1, statepop, by="abbr")

# save usmap data as dataset
us <- usmap::us_map()

# create merged with our dataset
mdf <-
  left_join(us, df_map, by = c("full", "fips", "abbr"))

# calculate centroids for labels
us_centroids <-
  mdf %>%
  group_by(full) %>%
  summarise(centroid.x = mean(range(x)),
            centroid.y = mean(range(y)),
            share = unique(Sleep))

# plot the map and add labels in the centroid positions
plot_usmap(data = df_map, values = "Sleep", color = "white") +
  geom_text(data = us_centroids,
            aes(centroid.x, centroid.y, label = share),
            size = 5/14*8) +
  scale_fill_continuous(name = "Mean(Sleep Time)", label = scales::comma)  +
  theme(legend.position = "right")
