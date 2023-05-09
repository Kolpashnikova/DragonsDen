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

#trans <- tu_transitions(data)

## rename activity names that are too long
act <- trans$activities
act[2] <- "PC"
act[5] <- "AC"
act[6] <- "Work"
act[7] <- "Shop"

mdat <- as.data.frame(trans$trate, row.names = act)
colnames(mdat) <- act

# transform into the long format
data_long <- mdat %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

# parameters
circos.clear()
circos.par(start.degree = 90,
           gap.degree = 4, track.margin = c(-0.1, 0.1),
           points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(length(trans$activities), alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:length(trans$activities))]

# specify where to save the file
jpeg(filename = "examples/Chords.jpg", width = 1800, height = 1800, quality = 300)

# Base plot
chordDiagram(
  x = data_long,
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



