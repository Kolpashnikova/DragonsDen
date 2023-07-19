#### set up git with ssh ####

#### git clone git@github.com:Kolpashnikova/VisualizationWorkshopHand+sOn.git ####

#### open the project file ####

#### you can also download your own ATUS-X extract ####

#### Load the data ####
#library(ipumsr)
#ddi <- read_ipums_ddi("data/atus_00031.xml")
#data <- read_ipums_micro(ddi)

library(haven) #if ipumsr not loaded

data_gss_main <- read_dta("data/gss-89M0034-E-2015-c-29-main_F2.dta")
data_gss_episode <- read_dta("data/gss-89M0034-E-2015-c-29-episode_F2.dta")



## load timeuse package
#devtools::install_github("Kolpashnikova/package_R_timeuse")
library(timeuse)

## load packages that we will be working with
library(ggplot2)
library(tidyverse)
library(paletteer)
library(grid)
library(gridExtra)
library(graphics)
library(jpeg)

## load
library(devtools)
library(pak)
library(transformr)
library(gganimate)
library(RColorBrewer)
library(magick)
library(dplyr)
library(ggpubr)
#library(renv)
library(ggimage)

library(geojsonio)
library(rmapshaper)
library(rgdal)
library(sf)
library(ggtext)
library(ggrepel)
library(ggforce)

data_gss_episode <- select(data_gss_episode, -starts_with("WEPI_"))
data_gss_main <- select(data_gss_main, -starts_with("WTBS_"))

## let's find out which color palette we will use
palettes_d_names
palettes_d_names %>% distinct(package)
view(palettes_d_names %>% filter(length>=12))

#### subsetting the data ####

# RETIRED = 1 means retired
# SEX == 1 means men, SEX == 2 means women
#retired_women <- data[data$RETIRED_CPS8 == 1 & data$SEX == 2, ]

retired_women <- data_gss_main[data_gss_main$MRW_05 == 6 & data_gss_main$SEX == 2, ]
women <- data_gss_main[data_gss_main$SEX == 2, ]

#retired_men <- data[data$RETIRED_CPS8 == 1 & data$SEX == 1, ]
retired_men <- data_gss_main[data_gss_main$MRW_05 == 6 & data_gss_main$SEX == 1, ]
men <- data_gss_main[data_gss_main$SEX == 1, ]

# merge data frames by "PUMFID"
women <- merge(data_gss_episode, women, by = "PUMFID", all.x = TRUE)

source("R/gss_longtempo.R")

tem_women <- gss_longtempo(women)

#nonretired_women <- data[data$RETIRED_CPS8 != 1 & data$SEX == 2, ]

#nonretired_men <- data[data$RETIRED_CPS8 != 1 & data$SEX == 1, ]

#### plotting the data ####

## transform data
#tem_women <- tu_longtempo(retired_women)

#tem_men <- tu_longtempo(retired_men)

#tem_nonwomen <- tu_longtempo(nonretired_women)

#tem_nonmen <- tu_longtempo(nonretired_men)

## save as a dataframe
df_women <- as.data.frame(tem_women$values)
names(df_women) <- tem_women$key

df_men <- as.data.frame(tem_men$values)
names(df_men) <- tem_men$key

#df_nonwomen <- as.data.frame(tem_nonwomen$values)
#names(df_nonwomen) <- tem_nonwomen$key

#df_nonmen <- as.data.frame(tem_nonmen$values)
#names(df_nonmen) <- tem_nonmen$key


df_women <- df_women / n_distinct(women$PUMFID) * 100

df_men <- df_men / n_distinct(men$PUMFID) * 100

#df_nonwomen <- df_nonwomen / n_distinct(nonretired_women$PUMFID) * 100

#df_nonmen <- df_nonmen / n_distinct(nonretired_men$PUMFID) * 100

## define time labels
t_intervals_labels <-  seq.POSIXt(as.POSIXct("2022-12-03 04:00:00 GMT"),
                                  as.POSIXct("2022-12-04 03:59:00 GMT"), by = "1 min")

## add positions and time labels
df_women$time <- 1:1440
df_women$t_intervals_labels <- t_intervals_labels

df_men$time <- 1:1440
df_men$t_intervals_labels <- t_intervals_labels


#df_nonwomen$time <- 1:1440
#df_nonwomen$t_intervals_labels <- t_intervals_labels

#df_nonmen$time <- 1:1440
#df_nonmen$t_intervals_labels <- t_intervals_labels

#### animation ####

groups <- c("15 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years",
            "55 to 64 years", "65 to 74 years", "75 years and over")

df <- data.frame(Housework = numeric(),
                 t_interval = as.POSIXct(character()),
                 group = character())

for(gr in groups){
  n <- which(groups == gr)
  women <- data_gss_main[data_gss_main$AGEGR10 == n & data_gss_main$SEX == 2, ]
  episode <- data_gss_episode[data_gss_episode$PUMFID %in% women$PUMFID, ]

  women <- merge(episode, women, by = "PUMFID", all.x = TRUE)
  tem_women <- gss_longtempo(women)

  df_women <- as.data.frame(tem_women$values)
  names(df_women) <- tem_women$key

  df_women <- df_women / n_distinct(women$PUMFID) * 100

  ## add positions and time labels
  df_women$time <- 1:1440
  df_women$t_intervals_labels <- t_intervals_labels

  df_women <- subset(df_women, select = c(Housework,t_intervals_labels))

  df_women$group <- gr

  df <- rbind(df, df_women)

}

df %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`, group = group)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"), position = "top")  +
  scale_y_continuous(limits = c(0, 30)) +
  coord_flip() + # flips x and y axes
  labs(title = "Retired Women: Housework Time",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none") +
  facet_wrap(~ group, nrow = 2, scales = "free_y")

p <- df %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`, group = group)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"), position = "top")  +
  scale_y_continuous(limits = c(0, 35)) +
  coord_flip() + # flips x and y axes
  labs(title = "",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none") +
  # Here comes the gganimate code
  transition_states(
    group
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')+
  labs(title = "Women: {closest_state}", x = "Time of Day", y = "Percent")


a <- animate(p, width = 240, height = 480,
             duration = 10)
f = "output/first.gif"
anim_save(f, animation = a)

#### animation for men ####

df <- data.frame(Housework = numeric(),
                 t_interval = as.POSIXct(character()),
                 group = character())

for(gr in groups){
  n <- which(groups == gr)
  men <- data_gss_main[data_gss_main$AGEGR10 == n & data_gss_main$SEX == 1, ]
  episode <- data_gss_episode[data_gss_episode$PUMFID %in% men$PUMFID, ]

  men <- merge(episode, men, by = "PUMFID", all.x = TRUE)
  tem_men <- gss_longtempo(men)

  df_men <- as.data.frame(tem_men$values)
  names(df_men) <- tem_men$key

  df_men <- df_men / n_distinct(men$PUMFID) * 100

  ## add positions and time labels
  df_men$time <- 1:1440
  df_men$t_intervals_labels <- t_intervals_labels

  df_men <- subset(df_men, select = c(Housework,t_intervals_labels))

  df_men$group <- gr

  df <- rbind(df, df_men)

}

p <- df %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`, group = group)) +
  geom_area(fill = paletteer_d("rcartocolor::Pastel",1)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  coord_flip() +
  scale_y_reverse(limits = c(35, 0)) + #puts y reverse
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 1, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none")+
  # Here comes the gganimate code
  transition_states(
    group
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')+
  labs(title = "{closest_state}: Men", x = "Time of Day", y = "Percent")

b <- animate(p, width = 240, height = 480,
             duration = 10)
f = "output/second.gif"
anim_save(f, animation = b)

#### combine animations ####

new_gif <- image_append(c(b[1], a[1]))
for(i in 2:100){
  combined <- image_append(c(b[i], a[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

anim_save("output/combined.gif", animation = new_gif)

#### by provinces ####

hsw <- data_gss_main %>%
  group_by(SEX, AGEGR10, PRV) %>%
  summarise(mean_HSW = mean(HSWKDUR, na.rm = TRUE))

hsw <- data_gss_main %>%
  filter(AGEGR10 >= 5) %>%
  group_by(SEX, PRV) %>%
  summarise(mean_HSW = mean(HSWKDUR, na.rm = TRUE)) %>%
  filter(!is.na(mean_HSW)) %>%
  group_by(PRV) %>%
  summarise(diff_mean_HSW = diff(mean_HSW))

hsw$PRUID <- hsw$PRV

theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}
theme_set(theme_map())

canada_raw <- readOGR(dsn = "data/gcd_000b11a_e", layer = "gcd_000b11a_e",
                      use_iconv=TRUE, encoding="CP1250")

canada_raw_json <- geojson_json(canada_raw)
canada_raw_sim <- ms_simplify(canada_raw_json)

geojson_write(canada_raw_sim, file = "data/canada_cd_sim.geojson")

canada_cd <- st_read("data/canada_cd_sim.geojson", quiet = TRUE)
canada_cd

canada_cd <- st_transform(canada_cd, crs = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
canada_cd

map_colors <-  RColorBrewer::brewer.pal(8, "Pastel1")
map_colors <- rep(map_colors, 37)

merged_df <- merge(canada_cd, hsw, by = "PRUID", all = TRUE)

df <- data.frame(xmin = numeric(),
                 ymin = numeric(),
                 xmax = numeric(),
                 ymax = numeric(),
                 PRUID = numeric())


for (pruid in unique(merged_df$PRUID)) {
  pruid_df <- subset(merged_df, PRUID == pruid)
  bb <- st_bbox(pruid_df)
  bb$PRUID <- pruid
  # Do something with pruid_df
  df <- rbind(df, bb)
}

df$cx = (df$xmin + df$xmax)/2
df$cy = (df$ymin + df$ymax)/2

merged_df <- merge(merged_df, df, by = "PRUID", all = TRUE)

merged_df$Housework <- round(merged_df$diff_mean_HSW, 2)


p <- ggplot(data = merged_df, mapping = aes(fill = Housework)) +
  scale_fill_gradient(low = "white", high = paletteer_d("rcartocolor::Pastel",1), na.value = "gray80") +
  geom_sf(color = "gray60",
          size = 0.1)+
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14))

p +
  geom_text(aes(x = cx, y = cy, label = Housework), size = 3, color = "black") +
  labs(title = "Gender Gap in Housework Time by Province,\n Adults >= 55 y.o.") +
  geom_rect(aes(xmin = 1392455.8, xmax = 2793734.2, ymin = 7033978, ymax = 8134795),
            fill = NA, color = "black")+
  geom_rect(aes(xmin = -1333618.3, xmax = -565848.3, ymin = 6640698, ymax = 7933952),
            fill = NA, color = "red")


#ggsave("figures/canada.pdf", p_out, height = 12, width = 15)

#### non-animated plots ####
## area plot for one activity flipped
#plot1 <-

bitmap <- image_data(new_gif)

df_women %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"), position = "top")  +
  scale_y_continuous(limits = c(0, 30)) +
  coord_flip() + # flips x and y axes
  labs(title = "Retired Women: Housework Time",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none") +
  geom_image(aes(image= bitmap))

## area plot for one activity flipped other way
plot2 <- df_men %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`)) +
  geom_area(fill = paletteer_d("rcartocolor::Pastel",1)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  coord_flip() +
  scale_y_reverse(limits = c(30, 0)) + #puts y reverse
  labs(title = "Retired Men: Housework Time",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none")

## area plot for one activity flipped
plot3 <- df_nonwomen %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"), position = "top")  +
  scale_y_continuous(limits = c(0, 30)) +
  coord_flip() + # flips x and y axes
  labs(title = "Women: Housework Time",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none")

## area plot for one activity flipped other way
plot4 <- df_nonmen %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`)) +
  geom_area(fill = paletteer_d("rcartocolor::Pastel",1)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  coord_flip() +
  scale_y_reverse(limits = c(30, 0)) + #puts y reverse
  labs(title = "Men: Housework Time",
       x = "Time of Day",
       y = "Percent") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none")


## plot flipped graphs together
grid.arrange(plot2, plot1, plot4, plot3, ncol=2, top = textGrob("Time Use of Retired Adults",gp=gpar(fontsize=14,fontface = "bold")))

#### chords ####

#### Transitions Plot ####

# load packages
library(circlize)
library(viridis)
library(colormap)


## load data

#library(ipumsr)
#ddi <- read_ipums_ddi("data/atus_00003(1).xml")
#data <- read_ipums_micro(ddi)

## get transitions

trans_women <- tu_transitions(retired_women)
trans_men <- tu_transitions(retired_men)

trans_nonwomen <- tu_transitions(nonretired_women)
trans_nonmen <- tu_transitions(nonretired_men)

## rename activity names that are too long
act <- trans$activities
act[2] <- "PC"
act[5] <- "AC"
act[6] <- "Work"
act[7] <- "Shop"

mdat_women <- as.data.frame(trans_women$trate, row.names = act)
colnames(mdat_women) <- act

mdat_men <- as.data.frame(trans_men$trate, row.names = act)
colnames(mdat_men) <- act

mdat_nonwomen <- as.data.frame(trans_nonwomen$trate, row.names = act)
colnames(mdat_nonwomen) <- act

mdat_nonmen <- as.data.frame(trans_nonmen$trate, row.names = act)
colnames(mdat_nonmen) <- act


# transform into the long format
data_long_women <- mdat_women %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

data_long_men <- mdat_men %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

data_long_nonwomen <- mdat_nonwomen %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

data_long_nonmen <- mdat_nonmen %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)


# parameters
circos.clear()
circos.par(start.degree = 90,
           gap.degree = 4, track.margin = c(-0.1, 0.1),
           points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(length(trans_women$activities), alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:length(trans_women$activities))]

# specify where to save the file
jpeg(filename = "examples/Chords_women.jpg", width = 1800, height = 1800, quality = 300)

# Base plot
chordDiagram(
  x = data_long_women,
  grid.col = mycolor,
  transparency = 0.20,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {

    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")

    # Add names to the sector.
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 3
    )
  }
)

dev.off()


# specify where to save the file
jpeg(filename = "examples/Chords_men.jpg", width = 1800, height = 1800, quality = 300)

# Base plot
chordDiagram(
  x = data_long_men,
  grid.col = mycolor,
  transparency = 0.20,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {

    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")

    # Add names to the sector.
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 3
    )
  }
)

dev.off()

# specify where to save the file
jpeg(filename = "examples/Chords_nonwomen.jpg", width = 1800, height = 1800, quality = 300)

# Base plot
chordDiagram(
  x = data_long_nonwomen,
  grid.col = mycolor,
  transparency = 0.20,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {

    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")

    # Add names to the sector.
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 3
    )
  }
)

dev.off()

# specify where to save the file
jpeg(filename = "examples/Chords_nonmen.jpg", width = 1800, height = 1800, quality = 300)

# Base plot
chordDiagram(
  x = data_long_nonmen,
  grid.col = mycolor,
  transparency = 0.20,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {

    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")

    # Add names to the sector.
    circos.text(
      x = mean(xlim),
      y = 3.2,
      labels = sector.index,
      facing = "bending",
      cex = 3
    )
  }
)

dev.off()


# Read the jpeg files
plot1 <- readJPEG("examples/Chords_women.jpg")
plot2 <- readJPEG("examples/Chords_men.jpg")
plot3 <- readJPEG("examples/Chords_nonwomen.jpg")
plot4 <- readJPEG("examples/Chords_nonmen.jpg")

# Plot the graphs
grid.arrange(
  grid.raster(plot2),
  grid.raster(plot1),
  grid.raster(plot4),
  grid.raster(plot3),
  ncol = 2,
  top = textGrob("Time Use of Retired Adults", gp = gpar(fontsize = 14, fontface = "bold"))
)
