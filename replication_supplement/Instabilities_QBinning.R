
library(gridExtra)
library(reliabilitydiag)

source(here("replication_supplement/rel_diag_classic.R"))
source(here("replication_supplement/rel_diag_quantile.R"))
FC.tbl <- readRDS(file = here("replication_supplement/data/DGPs_FCtbl.rds"))


# Layout Matrix
layout_matrix <- rbind(rep(seq(1,4), each=2),
  rep(seq(1,4), each=2),
  rep(seq(5,8), each=2),
  rep(seq(5,8), each=2),
  rep(seq(9,12), each=2),
  rep(seq(9,12), each=2),
  c(NA,NA,NA,13,13,NA,NA,NA),
  c(NA,NA,NA,13,13,NA,NA,NA))

#### Loop over all forecasts
#for (FC.type.choice in unique(FC.tbl$FC.type)){
for (FC.type.choice in c("precip.ENS", "M1.DAFFS", "Recid.COMPAS", "SPF.84.4Quarter")) {
  m.bins.set <- 9:11
  x <- FC.tbl%>%filter(FC.type == FC.type.choice)%>%pull(x)
  y <- FC.tbl%>%filter(FC.type == FC.type.choice)%>%pull(y)

  rel.list <- list()
  pdf(file = NULL)
  for (m.bins in m.bins.set){
    # Equidistant Binning
    rel <- rel.diag.classic(y, x, bins=seq(0,1,length.out=m.bins+1))
    df.scores <- rel$df %>% mutate(o.bar=mean(rlz), n=n()) %>% group_by(bin.index) %>%
      summarize(n.bin=n(), o.bin=mean(rlz), FC.bin=mean(FC), o.bar=mean(o.bar), n=mean(n)) %>%
      summarize(REL=sum(n.bin*(FC.bin - o.bin)^2)/mean(n), RES=sum(n.bin*(o.bar - o.bin)^2)/mean(n), UNC=mean(o.bar*(1-o.bar)) )
    rel.list <- list.append(rel.list, rel$p + theme_bw() + annotate("text", x = 0.125, y = 0.94, label = paste("REL = ",formatC(df.scores$REL, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.88, label = paste("RES = ",formatC(df.scores$RES, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.82, label = paste("UNC = ",formatC(df.scores$UNC, digits=3, format="f"))) +
                              ggtitle(paste0("Equidistant Binning with ", m.bins, " Bins")) )

    # Q Binning
    rel <- rel.diag.quantile(x,y, binning.method="Q1", m.bins=m.bins)
    df.scores <- rel$df %>% mutate(o.bar=mean(rlz), n=n()) %>% group_by(bin_id) %>%
      summarize(n.bin=n(), o.bin=mean(rlz), FC.bin=mean(FC), o.bar=mean(o.bar), n=mean(n)) %>%
      summarize(REL=sum(n.bin*(FC.bin - o.bin)^2)/mean(n), RES=sum(n.bin*(o.bar - o.bin)^2)/mean(n), UNC=mean(o.bar*(1-o.bar)) )
    rel.list <- list.append(rel.list, rel$p + theme_bw() + annotate("text", x = 0.125, y = 0.94, label = paste("REL = ",formatC(df.scores$REL, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.88, label = paste("RES = ",formatC(df.scores$RES, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.82, label = paste("UNC = ",formatC(df.scores$UNC, digits=3, format="f"))) +
                              ggtitle(paste0("Q Binning with ", m.bins, " Bins")))

    # Q+ Binning
    rel <- rel.diag.quantile(x,y, binning.method="Q3", m.bins=m.bins)
    df.scores <- rel$df %>% mutate(o.bar=mean(rlz), n=n()) %>% group_by(bin_id) %>%
      summarize(n.bin=n(), o.bin=mean(rlz), FC.bin=mean(FC), o.bar=mean(o.bar), n=mean(n)) %>%
      summarize(REL=sum(n.bin*(FC.bin - o.bin)^2)/mean(n), RES=sum(n.bin*(o.bar - o.bin)^2)/mean(n), UNC=mean(o.bar*(1-o.bar)) )
    rel.list <- list.append(rel.list, rel$p + theme_bw() + annotate("text", x = 0.125, y = 0.94, label = paste("REL = ",formatC(df.scores$REL, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.88, label = paste("RES = ",formatC(df.scores$RES, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.82, label = paste("UNC = ",formatC(df.scores$UNC, digits=3, format="f"))) +
                              ggtitle(paste0("Q+ Binning with ", m.bins, " Bins")))


    # Q- Binning
    rel <- rel.diag.quantile(x,y, binning.method="Q4", m.bins=m.bins)
    df.scores <- rel$df %>% mutate(o.bar=mean(rlz), n=n()) %>% group_by(bin_id) %>%
      summarize(n.bin=n(), o.bin=mean(rlz), FC.bin=mean(FC), o.bar=mean(o.bar), n=mean(n)) %>%
      summarize(REL=sum(n.bin*(FC.bin - o.bin)^2)/mean(n), RES=sum(n.bin*(o.bar - o.bin)^2)/mean(n), UNC=mean(o.bar*(1-o.bar)) )
    rel.list <- list.append(rel.list, rel$p + theme_bw() + annotate("text", x = 0.125, y = 0.94, label = paste("REL = ",formatC(df.scores$REL, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.88, label = paste("RES = ",formatC(df.scores$RES, digits=3, format="f"))) +
                              annotate("text", x = 0.125, y = 0.82, label = paste("UNC = ",formatC(df.scores$UNC, digits=3, format="f"))) +
                              ggtitle(paste0("Q- Binning with ", m.bins, " Bins")))
  }

  rel <- reliabilitydiag::reliabilitydiag(x, y=y)
  rel.list <- list.append(rel.list, plot(rel) + ggtitle(paste0("CORP Method")) +
                            annotate("text", x = 0.125, y = 0.94, label = paste("MCB = ", formatC(as.numeric(summary(rel)[3]), digits=3, format="f"))) +
                            annotate("text", x = 0.125, y = 0.88, label = paste("DSC = ", formatC(as.numeric(summary(rel)[4]), digits=3, format="f"))) +
                            annotate("text", x = 0.125, y = 0.82, label = paste("UNC = ", formatC(as.numeric(summary(rel)[5]), digits=3, format="f"))))

  dev.off()

  formatC(df.scores$REL, digits=4, format="f")

  p <- grid.arrange(grobs=rel.list,layout_matrix=layout_matrix)
  ggsave(here(paste0("replication_supplement/plots/Instability_Q_", FC.type.choice,".pdf")), p, height=16, width=16, units="in")
}



