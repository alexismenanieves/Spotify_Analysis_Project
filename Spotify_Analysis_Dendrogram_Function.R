# ------------------------------------------------------------------------#
# Author: Manuel Mena
# Date: 28/05/2021
# Project: Spotify Analysis
# Scope: Dendrogram customization
# ------------------------------------------------------------------------#

# Step 0.  Load libraries and functions -----------------------------------
library(ggplot2)
library(ggdendro)

dendro_data_c <- function(hc, clusters){
  hcdata <- ggdendro::dendro_data(hc,type = "rectangle")
  segments <- hcdata$segments
  lab_clust <- cutree(hc, clusters)[hc$order]
  seg_clust <- rep(0,nrow(segments))
  heights <- sort(hc$height, decreasing = TRUE)
  height <- mean(c(heights[clusters],heights[clusters - 1]), na.rm = TRUE)
  
  for(i in 1:c){
    xi <- hcdata$labels$x[lab_clust == i]
    idx1 <- segments$x >= min(xi) & segments$x <= max(xi)
    idx2 <- segments$xend >= min(xi) & segments$xend <= max(xi)
    idx3 <- segments$yend < height
    idx <- idx1 & idx2 & idx3
    seg_clust[idx] <- 1
  }
  
  idx <- which(seg_clust == 0)
  seg_clust[idx] <- seg_clust[idx + 1]
  hcdata$segments$clust <- seg_clust
  hcdata$segments$line <- as.integer(seg_clust < 1)
  hcdata$labesl$clust <- lab_clust
  
  hcdata
}

set_label_params <- function(nblabels,
                             direction = c("tb","bt","lr","rl"),
                             fan = FALSE){
  if(fan){
    angle <- 360 / nblabels * 1:nblabels + 90
    idx <- angle >= 90 & angle <= 270
    angle[idx] <- angle[idx] + 180
    hjust <- rep(0, nblabels)
    hjust[idx] <- 1
  } else {
    angle <- rep(0, nblabels)
    hjust <- 0
    if(direction %in% c("tb","bt")){
      angle <- angle + 45
    }
    if(direction %in% c("lr","rl")){
      hjust <- 1
    }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(hcdata,
                          direction = c("tb","bt","lr","rl"),
                          fan = false,
                          scale.color = NULL,
                          brach.size = 1,
                          label.size = 3,
                          nudge.label = 0.01,
                          expand.y = 0.1){
  direction <- match.arg(direction)
  ybreaks <- pretty(segment(hcdata)$y, n = 5)
  ymax <- max(segment(hcdata)$y)
  
  p <- ggplot() +
    geom_segment(data = segment(hcdata),
                 aes(x = x, y = y, xend = xend, yend = yend,
                     linetype = factor(line), colour = factor(clust)),
                 lineend = "round",
                 show.legend = FALSE,
                 size = branch.size)
  
  if(fan){
    p <- p + coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(break = NULL) +
      if(direction %in% c("lr","rl")){
        p + coord_flip()
      }
      if(direction %in% c("bt","lr")){
        p <- p + scale_y_reverse(breaks = ybreaks)
      } else {
        p <- p + scale_y_continuous(breaks = ybreaks)
        nudge.label <- -(nudge.label)
      }
  }
  
  labelParams <- set_label_params(nrow(hcdata$labels),
                                  direction,
                                  fan)
  
  hcdata$labels$angle <- labelParams$angle
  
  p <- p + 
    geom_text(data = label(hcdata),
              aes(x = x, y = y, label = label, 
                  colour = factor(clust), angle = angle),
              vjust = labelParams$vjust,
              hjust = labelParams$hjust,
              nudge_y = ymax * nudge.label,
              size = label.size,
              show.legend = FALSE)
  
  if(!is.null(scale.color)){
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p <- p + expand_limits(y = ylim)
  
  p
}
