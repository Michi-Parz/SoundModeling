
yaxlab <- function(lab,isl,eng) {
  onezero <- paste(ifelse(c(lab,isl,eng), 1,0), collapse =  "")

  switch(
    onezero,
    "100" = expression(paste("Schalldämm-Maß ", italic(" R"), " / dB")),
    "101" = expression(paste("Sound reduction index ", italic(" R"), " / dB")),
    "000" = expression(paste("Bau-Schalldämm-Maß ", italic( " R'"), " / dB")),
    "001" = expression(paste("Building sound reduction index ", italic( " R'"), " / dB")),
    "110" = expression(paste("Norm-Trittschallpegel ", italic(" L")["n"], "  / dB")),
    "010" = expression(paste("Norm-Trittschallpegel ", italic(" L'")["n"], "  / dB")),
    "111" = expression(paste("Impact sound level ", italic(" L")["n"], "  / dB")),
    "011" = expression(paste("Impact sound level ", italic(" L'")["n"], "  / dB"))
  )
}

#' Easy frequency depending plots
#'
#' @param data data.frame
#' @param mapping similar to ggplot
#' @param ymin lower ylim value
#' @param ymax upper ylim value
#' @param sound Is the curve a sound reduction index R or an impact sound level curve?
#' @param lab Is it labatory data? (Ignored if sound == FALSE)
#' @param isl Is it impact sound level? (Ignored if sound == FALSE)
#' @param english Should the axis labeling be in English?

gg_basic <- function(data, mapping = aes(),
                     ymin = 10, ymax = 90, sound = T,
                     lab = T , isl = F, english = T
                     ){
  if (sound) {
    ybreak <- seq(ymin, ymax,5)
    ylabels <- ybreak
    ylabels[seq(2,length(ylabels),2)] <- ""
  }


  y_axis_label <- yaxlab(lab, isl, english)

  xlabl <- expression(paste("Frequenz ",
                           italic(" f"), " / Hz"))
  if (english) {
    xlabl <- expression(paste("Frequency ",
                             italic(" f"), " / Hz"))
  }


  ggp <- ggplot(data, mapping = mapping) +
    scale_x_continuous(trans = log2_trans(),
                       minor_breaks = c(freq,6300,8000,10000),
                       breaks = c(63,125,250,500,1000,2000,4000,8000),
                       labels = c(63,125,250,500,1000,2000,4000,8000),
    ) +
    xlab(xlabl)+
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          panel.grid.major = element_line(linewidth = 1),
          axis.text.x = element_text(angle =40, hjust = 1))

  if (sound){
    ggp <- ggp +
      scale_y_continuous(breaks = ybreak,
                         labels = ylabels,
                         expand=c(0,0), limits = c(ymin,ymax),
                         minor_breaks = seq(ymin,ymax,1))+
      ylab(y_axis_label) +
      coord_fixed(ratio = 0.13)

  }

  ggp
}




