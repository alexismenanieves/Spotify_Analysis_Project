# ------------------------------------------------------------------------#
# Author: Manuel Mena
# Date: 26/05/2021
# Project: Spotify Analysis
# Scope: Connection and data retrieval functions
# ------------------------------------------------------------------------#

# Step 0.  Load libraries and functions -----------------------------------
library(scico)
library(systemfonts)
library(ggplot2)
library(ggtext)

# Create a global theme
theme_set(theme_minimal(base_size = 12))

# Modify plot elements globally (for all following plots)
theme_update(
  axis.ticks = element_line(color = "grey92"),
  axis.ticks.length = unit(.5, "lines"),
  panel.grid.minor = element_blank(),
  legend.title = element_text(size = 12),
  legend.text = element_text(color = "grey30"),
  plot.title = element_text(size = 14, face = "bold"),
  plot.title.position = "plot",
  axis.text = element_text(size = 7),
  plot.subtitle = element_text(size = 10, color = "grey30"),
  plot.caption = element_text(size = 9, margin = margin(t = 15))
)
