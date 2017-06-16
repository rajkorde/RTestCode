# clickclust
set.seed(123)
library(ClickClust)

n.seq <- 50

p <- 5
K <- 2
mix.prop <- c(0.3, 0.7)


TP1 <- matrix(c(0.20, 0.10, 0.15, 0.15, 0.40,
                0.20, 0.20, 0.20, 0.20, 0.20,
                0.15, 0.10, 0.20, 0.20, 0.35,
                0.15, 0.10, 0.20, 0.20, 0.35,
                0.30, 0.30, 0.10, 0.10, 0.20), byrow = TRUE, ncol = p)

TP2 <- matrix(c(0.15, 0.15, 0.20, 0.20, 0.30,
                0.20, 0.10, 0.30, 0.30, 0.10,
                0.25, 0.20, 0.15, 0.15, 0.25,
                0.25, 0.20, 0.15, 0.15, 0.25,
                0.10, 0.30, 0.20, 0.20, 0.20), byrow = TRUE, ncol = p)


TP <- array(rep(NA, p * p * K), c(p, p, K))
TP[,,1] <- TP1
TP[,,2] <- TP2


# DATA SIMULATION

A <- click.sim(n = n.seq, int = c(10, 50), alpha = mix.prop, gamma = TP)
C <- click.read(A$S)


# EM ALGORITHM

cs <- click.EM(X = C$X, K = 2)

# clickstream
library(clickstream)
clickstreams<-c("User1,h,c,c,p,c,h,c,p,p,c,p,p,o",
                "User2,i,c,i,c,c,c,d",
                "User3,h,i,c,i,c,p,c,c,p,c,c,i,d",
                "User4,c,c,p,c,d",
                "User5,h,c,c,p,p,c,p,p,p,i,p,o",
                "User6,i,h,c,c,p,p,c,p,c,d")
csf<-tempfile()
writeLines(clickstreams, csf)
cls<-readClickstreams(csf, header=TRUE)
mc<-fitMarkovChain(cls)
startPattern<-new("Pattern", sequence=c("h", "c"))
predict(mc, startPattern)
plot(mc)