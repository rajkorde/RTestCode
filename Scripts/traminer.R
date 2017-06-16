library(TraMineR)
data(mvad)
seqstatl(mvad[,17:86])
mvad.alphabet <- c("employment", "FE", "HE", "joblessness", "school", 
                   "training")
mvad.labels <- c("employment", "further education", "higher education", 
                 "joblessness", "school", "training")
mvad.scodes <- c("EM", "FE", "HE", "JL", "SC", "TR")
mvad.seq <- seqdef(mvad, 17:86, alphabet = mvad.alphabet, states = mvad.scodes, 
                   labels = mvad.labels, xtstep = 6)

seqiplot(mvad.seq, with.legend = FALSE, border = NA)
seqIplot(mvad.seq, sortv = "from.start", with.legend = FALSE)
seqfplot(mvad.seq, with.legen = FALSE, border = NA)
seqlegend(mvad.seq)

seqdplot(mvad.seq, with.legend = FALSE, border = NA)

submat <- seqsubm(mvad.seq, method = "TRATE")
dist.om1 <- seqdist(mvad.seq, method = "OM", indel = 1, sm = submat)
st <- seqtree(mvad.seq ~ gcse5eq + Grammar + funemp, data = mvad,
              R = 5000, diss = dist.om1, pval = 0.05)
seqtreedisplay(st, type = "d", border = NA)