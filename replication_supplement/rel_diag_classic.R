
rel.diag.classic <- function(rlz, FC, bins=FALSE, point.x="avg") {
  # Some settings
  plot.theme <- theme_bw()
  plot.col <- "red"
  plot.lwd <- 1
  plot.lty <- 1
  plot.pts.size <- 3

  df <- data.frame(FC,rlz)

  # The case without bins
  if (class(bins)=="logical" && bins==FALSE){
    df <- df %>% mutate(bin.index = dense_rank(FC))
    df$bin.midpoint <- df$FC
    df.bins <- df %>% group_by(bin.index) %>% summarise(bin.freq=mean(rlz), bin.n=length(rlz), bin.midpoint=unique(bin.midpoint))

    # Plot the reliability diagram with histogram embedded
    if (length(unique(df$FC)) <= 5){
      # Use 40 equally spaced bins as histogram breaks if <= 5 unique FCs and reldiags without binning
      p.reldiag <- ggplot(data=df.bins, mapping=aes(x=bin.midpoint, y=bin.freq)) +
        geom_histogram(data=df, mapping=aes(x=FC, y = ..ncount../5), inherit.aes=FALSE, color="black", fill="white", breaks=seq(0,1,length.out=41)) +
        ylab("") + xlab("") + xlim(0,1) + ylim(0,1)
    } else {
      # Use the unique FC values as histogram breaks if > 5 unique FCs and reldiags without binning
      p.reldiag <- ggplot(data=df.bins, mapping=aes(x=bin.midpoint, y=bin.freq)) +
        geom_histogram(data=df, mapping=aes(x=FC, y = ..ncount../5), inherit.aes=FALSE, color="black", fill="white", breaks=unique(df$FC)) +
        ylab("") + xlab("") + xlim(0,1) + ylim(0,1)
    }

    p.reldiag <- p.reldiag + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size=plot.lwd/3, colour="black") +
      theme(legend.position = "none") +
      geom_point(size=plot.pts.size, colour=plot.col) + geom_line(size=plot.lwd, colour=plot.col) +
      plot.theme

  } else {
    # The case with bins
    if (class(bins)=="logical" && bins==TRUE){
      bins <- seq(0,1,length.out=11)
      bin.midpoint <- bins[-length(bins)] + diff(bins)/2
    } else if (class(bins)=="numeric" && length(bins)==1){
      bins <- seq(0,1,length.out=bins + 1)
      bin.midpoint <- bins[-length(bins)] + diff(bins)/2
    } else if (class(bins)=="numeric" && length(bins)>=1){
      bin.midpoint <- bins[-length(bins)] + diff(bins)/2
    } else if (class(bins)=="character" && bins=="quantiles"){
      # split the space [0,1] into up to 10 (unique!!!) bins determined by the quantiles of df$FC
      bins <- unique(c(0,as.numeric(quantile(df$FC,seq(0.1,0.9,length.out=9))),1))
      bin.midpoint <- bins[-length(bins)] + diff(bins)/2
    }

    df <- data.frame(FC,rlz)
    df$bin.index <- cut(df$FC, breaks=bins, include.lowest=TRUE, labels = FALSE)
    df$bin.midpoint <- bin.midpoint[df$bin.index]

    df.bins <- df %>% group_by(bin.index) %>%
      summarise(bin.freq=mean(rlz), bin.n=length(rlz), bin.n.small=(length(rlz)<=5), bin.midpoint=unique(bin.midpoint), bin.avg.x = mean(FC))

    # plot either bin averages or bin midpoints on the x-axsis
    if (point.x=="avg"){
      p.reldiag <- ggplot() + geom_histogram(data=df, mapping=aes(x=FC, y = ..ndensity../5), inherit.aes=FALSE, color="black", fill="white", breaks=bins) +
        ylab("") + xlab("") + xlim(0,1) + ylim(0,1)
    } else{
      p.reldiag <- ggplot() + geom_histogram(data=df, mapping=aes(x=FC, y = ..ndensity../5), inherit.aes=FALSE, color="black", fill="white", breaks=bins) +
        ylab("") + xlab("") + xlim(0,1) + ylim(0,1)
    }

    p.reldiag <- p.reldiag + geom_line(data=df.bins,mapping=aes(x=bin.avg.x, y=bin.freq), size=plot.lwd, colour=plot.col) +
      theme(legend.position = "none") + plot.theme +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size=plot.lwd/3, colour="black") +
      theme(legend.position = "none") +
      geom_point(data=df.bins, mapping=aes(x=bin.avg.x, y=bin.freq), size=plot.pts.size, colour=plot.col, show.legend = F) +
      scale_shape_manual(values=c(19))
  }

  #print(p.reldiag)
  return(list(df=df, df.bins=df.bins, p=p.reldiag))
}



