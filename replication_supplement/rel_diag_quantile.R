
rel.diag.quantile <- function(FC, rlz, binning.method="Q1", m.bins=10, point.x="average") {
  df <- data.frame(FC,rlz)

  if (binning.method == "Q1"){
    bin.breaks <- unique(c(0,quantile(FC, (1:(m.bins-1))/m.bins),1))
    df <- df %>% mutate(bin_id = findInterval(FC, bin.breaks))
  } else if (binning.method == "Q2"){
    df <- df %>% dplyr::arrange(FC) %>% mutate(bin_id = findInterval(row_number(), seq(1,dim(df)[1],length.out=m.bins+1), rightmost.closed=TRUE))
  } else if (binning.method == "Q3"){
    df <- df %>% dplyr::arrange(FC, rlz) %>% mutate(bin_id = findInterval(row_number(), seq(1,dim(df)[1],length.out=m.bins+1), rightmost.closed=TRUE))
  } else if (binning.method == "Q4"){
    df <- df %>% dplyr::arrange(FC, desc(rlz)) %>% mutate(bin_id = findInterval(row_number(), seq(1,dim(df)[1],length.out=m.bins+1), rightmost.closed=TRUE))
  }

  # Compute bin midpoint and average:
  df <- df %>% group_by(bin_id) %>% mutate(bin.midpoint=median(FC), bin.average=mean(FC)) %>% ungroup()

  # Generate df.bins
  df.bins <- df %>% group_by(bin_id) %>%
    summarise(bin.freq=mean(rlz), bin.n=length(rlz), bin.n.small=(length(rlz)<=5), bin.midpoint=unique(bin.midpoint), bin.average=unique(bin.average))

  # Generate a df.breaks with bin breaks for the histogram!
  df.breaks <- df %>% group_by(bin_id) %>% summarize(min.FC = min(FC), max.FC =max(FC))
  bin.breaks <- unique(c(0,(df.breaks$min.FC[-1] + df.breaks$max.FC[-length(df.breaks$max.FC)])/2,1))

  p.reldiag <- ggplot() +
    geom_histogram(data=df, mapping=aes(x=FC, y = ..ndensity../5), inherit.aes=FALSE, color="black", fill="white", breaks=bin.breaks) +
    ylab("") + xlab("") +
    xlim(0,1) + ylim(0,1) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size=0.5, colour="black") +
    theme(legend.position = "none") +
    theme_bw()  +
    scale_shape_manual(values=c(19))

  # plot either bin averages or bin midpoints on the x-axsis
  if (point.x=="average"){
    p.reldiag <- p.reldiag + geom_line(data=df.bins, mapping=aes(x=bin.average, y=bin.freq), size=1, colour="red") +
      geom_point(data=df.bins, mapping=aes(x=bin.average, y=bin.freq), size=3, colour="red", show.legend = F)
  } else {
    p.reldiag <- p.reldiag + geom_line(data=df.bins, mapping=aes(x=bin.midpoint, y=bin.freq), size=1, colour="red") +
      geom_point(data=df.bins, mapping=aes(x=bin.midpoint, y=bin.freq), size=3, colour="red", show.legend = F)
  }

  #print(p.reldiag)
  return(list(df=df, df.bins=df.bins, p=p.reldiag))
}



