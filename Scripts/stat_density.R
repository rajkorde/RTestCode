library("MASS")
data(geyser, "MASS")

m <- ggplot(geyser, aes(x = duration, y = waiting)) +
  geom_point() + xlim(0.5, 6) + ylim(40, 110)
m + geom_density2d()

dens <- kde2d(geyser$duration, geyser$waiting, n = 50,
              lims = c(0.5, 6, 40, 110))
densdf <- data.frame(expand.grid(duration = dens$x, waiting = dens$y),
                     z = as.vector(dens$z))
m + geom_contour(aes(z=z), data=densdf)

m + stat_density2d(aes(fill = ..level..), geom="polygon")
m + stat_density2d(aes(alpha = ..level..), geom="polygon", fill = "midnightblue")


set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
qplot(x, y, data = dsmall, geom = "density2d", colour = cut)
qplot(carat, price, data = dsmall, geom = "density2d", colour = cut)

d <- ggplot(dsmall, aes(carat, price)) + xlim(1,3)
d + geom_point() + geom_density2d()

d + stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE)

d + stat_density2d(geom="point", aes(size = ..density..), contour = FALSE)
