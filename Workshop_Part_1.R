#### set up git with ssh ####

#### git clone git@github.com:Kolpashnikova/VisualizationWorkshopHand+sOn.git ####

#### open the project file ####

#### you can also download your own ATUS-X extract ####

#### Load the data ####
library(ipumsr)
ddi <- read_ipums_ddi("data/atus_00003(1).xml")
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

## let's find out which color palette we will use
palettes_d_names
palettes_d_names %>% distinct(package)
view(palettes_d_names %>% filter(length>=12))

## transform data
tem <- tu_longtempo(data, w = "WT06")

## save as a dataframe
df <- as.data.frame(tem$values)
names(df) <- tem$key

## define time labels
t_intervals_labels <-  seq.POSIXt(as.POSIXct("2022-12-03 04:00:00 GMT"),
                                  as.POSIXct("2022-12-04 03:59:00 GMT"), by = "1 min")

## add positions and time labels
df$time <- 1:1440
df$t_intervals_labels <- t_intervals_labels

## create a tempogram using ggplot
df %>%
  gather(variable, value, Sleep:Travel) %>%
  ggplot(aes(x = t_intervals_labels, y = value, fill = variable)) +
  geom_area() +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  #scale_fill_manual(values = paletteer_d("RColorBrewer::Set3",11)) +
  scale_fill_manual(name = "Activity", values = paletteer_d("rcartocolor::Pastel",11)) +
  #coord_flip() +
  labs(title = "Time Use Diaries Tempogram",
       x = "Time of Day",
       y = "Number of Obs",
       caption = "Source: ATUS-X") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(angle=90, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "top",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

## area plot for one activity
df %>%
  ggplot(aes(x = t_intervals_labels, y = `Childcare`)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  labs(title = "Time Use: Childcare Time",
       x = "Time of Day",
       y = "Number of Obs",
       caption = "Source: ATUS-X") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey10", linetype = "dotted"),
        plot.title = element_text(hjust = 0.5, face="bold", size = 14),
        axis.text.x = element_text(angle=90, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(hjust = 0, size = 14),
        legend.position = "none")

## area plot for one activity flipped
plot1 <- df %>%
  ggplot(aes(x = t_intervals_labels, y = `Housework`)) +
  geom_area(fill = "#FF6666") +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"), position = "top")  +
  scale_y_continuous(limits = c(0, 37000)) +
  coord_flip() + # flips x and y axes
  labs(title = "Housework Time",
       x = "Time of Day",
       y = "Number of Obs") +
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
plot2 <- df %>%
  ggplot(aes(x = t_intervals_labels, y = `Working and Education`)) +
  geom_area(fill = paletteer_d("rcartocolor::Pastel",1)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz="EST"))  +
  coord_flip() +
  scale_y_reverse(limits = c(37000, 0)) + #puts y reverse
  labs(title = "Work Time",
       x = "Time of Day",
       y = "Number of Obs") +
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
grid.arrange(plot2, plot1, ncol=2, top = textGrob("Time Use",gp=gpar(fontsize=14,fontface = "bold")))
