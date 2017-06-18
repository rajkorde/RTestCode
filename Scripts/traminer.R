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


# http://analyzecore.com/2014/12/27/sequence-carts-in-depth-analysis-with-r-clustering/
library(dplyr)
library(TraMineR)
library(reshape2)
library(googleVis)

# creating an example of shopping carts
set.seed(10)
data <- data.frame(orderId=sample(c(1:1000), 5000, replace=TRUE),
                   product=sample(c('NULL','a','b','c'), 5000, replace=TRUE,
                                  prob=c(0.15, 0.65, 0.3, 0.15)))
order <- data.frame(orderId=c(1:1000),
                    clientId=sample(c(1:300), 1000, replace=TRUE))
sex <- data.frame(clientId=c(1:300),
                  sex=sample(c('male', 'female'), 300, replace=TRUE, prob=c(0.40, 0.60)))
date <- data.frame(orderId=c(1:1000),
                   orderdate=sample((1:90), 1000, replace=TRUE))
orders <- merge(data, order, by='orderId')
orders <- merge(orders, sex, by='clientId')
orders <- merge(orders, date, by='orderId')
orders <- orders[orders$product!='NULL', ]
orders$orderdate <- as.Date(orders$orderdate, origin="2012-01-01")
rm(data, date, order, sex)

# combining products to the cart
df <- orders %>%
  arrange(product) %>%
  select(-orderId) %>%
  unique() %>%
  group_by(clientId, sex, orderdate) %>%
  summarise(cart=paste(product,collapse=";")) %>%
  ungroup()

max.date <- max(df$orderdate) + 1
ids <- unique(df$clientId)
df.new <- data.frame()

for (i in 1:length(ids)) {
  df.cache <- df %>%
    filter(clientId == ids[i])
  ifelse(nrow(df.cache) == 1, av.dur <- 30,
         av.dur <- round(((max(df.cache$orderdate) - min(df.cache$orderdate))/(nrow(df.cache)-1))*1.5, 0))
  df.cache <- rbind(df.cache, data.frame(clientId=df.cache$clientId[nrow(df.cache)],
                                         sex=df.cache$sex[nrow(df.cache)],
                                         orderdate=max(df.cache$orderdate)+av.dur,
                                         cart='nopurch'))
  ifelse(max(df.cache$orderdate) > max.date,
         df.cache$orderdate[which.max(df.cache$orderdate)] <- max.date,
         NA)
  df.cache$to <- c(df.cache$orderdate[2:nrow(df.cache)]-1, max.date)
  df.cache <- df.cache %>%
    mutate(ord = paste('ord', c(1:nrow(df.cache)), sep=''))
  
  df.new <- rbind(df.new, df.cache)
}
df.new <- df.new %>%
  filter(cart!='nopurch' | to != orderdate)
rm(orders, df, df.cache, i, ids, max.date, av.dur)

df.new <- df.new %>%
  filter(ord %in% c('ord1', 'ord2', 'ord3', 'ord4')) %>%
  select(-ord)

min.date <- as.Date(min(df.new$orderdate), format="%Y-%m-%d")
df.new$orderdate <- as.numeric(df.new$orderdate-min.date+1)
df.new$to <- as.numeric(df.new$to-min.date+1)

# convert SPELL to STS
df.form <- seqformat(df.new, id = 'clientId', begin = 'orderdate', end = 'to', 
                     status = 'cart', from='SPELL', to='STS', process=FALSE)
df.seq <- seqdef(df.form, left = "DEL", right = "unknown", xtstep = 10,
                 void = "unknown")
df.feat <- unique(df.new[ , c('clientId', 'sex')])

seqdplot(df.seq, border = NA, withlegend = "right")
seqdplot(df.seq, border=NA, group=df.feat$sex)

seqstatd(df.seq)

df.seq <- seqdef(df.form, left='DEL', right='DEL', xtstep=10)

seqfplot(df.seq, border = NA, withlegend = "right")
seqfplot(df.seq, group=df.feat$sex, border=NA)

seqmtplot(df.seq, main = "mean time", with.legend = "right")
seqmtplot(df.seq, group=df.feat$sex, main='Mean time')

statd <- seqistatd(df.seq) 
apply(statd, 2, mean)

df.ient <- seqient(df.seq)
hist(df.ient, col='cyan', main=NULL, xlab='Entropy')
df.ent <- cbind(df.seq, df.ient)
boxplot(Entropy ~ df.feat$sex, data=df.ent, xlab='Gender', ylab='Sequences entropy', col='cyan')

#http://analyzecore.com/2014/12/27/sequence-carts-in-depth-analysis-with-r-clustering/
library(cluster)
df.om <- seqdist(df.seq, method = "OM", indel = 1, sm = "TRATE", with.missing = TRUE)
clusterward <- agnes(df.om, diss = TRUE, method = "ward")
df.cl4 <- cutree(clusterward, k=4)
cl4.lab <- factor(df.cl4, labels = paste("Cluster", 1:4))


# distribution chart
seqdplot(df.seq, group=cl4.lab, border=NA)
# frequence chart
seqfplot(df.seq, group=cl4.lab, pbarw=T, border=NA)
# mean time plot

seqrplot(df.seq, group = cl4.lab, dist.matrix = df.om, trep = 0.35, border = NA)
seqmtplot(df.seq, group=cl4.lab, border=NA)

# http://analyzecore.com/2015/01/28/sequence-carts-in-depth-analysis-with-r-events/
df.evseq <- seqecreate(df.seq, tevent = "state")

df.subseq <- seqefsub(df.evseq, pmin.support = 0.01)
plot(df.subseq[1:10], col = "cyan", ylab = "Frequency", xlab = "Subsequences", cex = 1.5)

time.constraint <- seqeconstraint(max.gap = 10, window.size = 30)
df.subseq.time.constr <- seqefsub(df.evseq, pmin.support = 0.01, constraint = time.constraint)
plot(df.subseq[1:10], col = "cyan", ylab = "Frequency", xlab = "Subsequences", cex = 1.5)

discrseq <- seqecmpgroup(df.subseq, group = df.feat$sex)
head(discrseq)
plot(discrseq[1:10], cex=1.5)
plot(discrseq[1:10], ptype="resid", cex=1.5)
