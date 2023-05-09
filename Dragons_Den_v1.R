#### set up git with ssh ####

#### git clone git@github.com:Kolpashnikova/VisualizationWorkshopHand+sOn.git ####

#### open the project file ####

#### you can also download your own ATUS-X extract ####

#### Load the data ####
library(ipumsr)
ddi <- read_ipums_ddi("data/atus_00031.xml")
data <- read_ipums_micro(ddi)

## load timeuse package
devtools::install_github("Kolpashnikova/package_R_timeuse")
library(timeuse)

## load packages that we will be working with
library(ggplot2)
library(tidyverse)
library(paletteer)
library(grid)
library(gridExtra)
library(graphics)
library(jpeg)

## let's find out which color palette we will use
palettes_d_names
palettes_d_names %>% distinct(package)
view(palettes_d_names %>% filter(length>=12))

#### subsetting the data ####

# RETIRED = 1 means retired
# SEX == 1 means men, SEX == 2 means women
retired_women <- data[data$RETIRED_CPS8 == 1 & data$SEX == 2, ]

retired_men <- data[data$RETIRED_CPS8 == 1 & data$SEX == 1, ]

nonretired_women <- data[data$RETIRED_CPS8 != 1 & data$SEX == 2, ]

nonretired_men <- data[data$RETIRED_CPS8 != 1 & data$SEX == 1, ]

#### plotting the data ####

## transform data
tem_women <- tu_longtempo(retired_women)

tem_men <- tu_longtempo(retired_men)

tem_nonwomen <- tu_longtempo(nonretired_women)

tem_nonmen <- tu_longtempo(nonretired_men)

## save as a dataframe
df_women <- as.data.frame(tem_women$values)
names(df_women) <- tem_women$key

df_men <- as.data.frame(tem_men$values)
names(df_men) <- tem_men$key

df_nonwomen <- as.data.frame(tem_nonwomen$values)
names(df_nonwomen) <- tem_nonwomen$key

df_nonmen <- as.data.frame(tem_nonmen$values)
names(df_nonmen) <- tem_nonmen$key


df_women <- df_women / n_distinct(retired_women$CASEID) * 100

df_men <- df_men / n_distinct(retired_men$CASEID) * 100

df_nonwomen <- df_nonwomen / n_distinct(nonretired_women$CASEID) * 100

df_nonmen <- df_nonmen / n_distinct(nonretired_men$CASEID) * 100

## define time labels
t_intervals_labels <-  seq.POSIXt(as.POSIXct("2022-12-03 04:00:00 GMT"),
                                  as.POSIXct("2022-12-04 03:59:00 GMT"), by = "1 min")

## add positions and time labels
df_women$time <- 1:1440
df_women$t_intervals_labels <- t_intervals_labels

df_men$time <- 1:1440
df_men$t_intervals_labels <- t_intervals_labels


df_nonwomen$time <- 1:1440
df_nonwomen$t_intervals_labels <- t_intervals_labels

df_nonmen$time <- 1:1440
df_nonmen$t_intervals_labels <- t_intervals_labels


## area plot for one activity flipped
plot1 <- df_women %>%
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
        legend.position = "none")

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
