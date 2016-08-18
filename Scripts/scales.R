library(ggplot2)
library(scales)

#http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations

ToothGrowth$dose = as.factor(ToothGrowth$dose)
head(ToothGrowth)

ggplot(ToothGrowth, aes(x = dose, y = len)) + geom_boxplot()
sp = ggplot(cars, aes(x = speed, y = dist)) + geom_point()

sp + expand_limits(x = c(0, 30), y = c(0, 150))

sp + scale_x_continuous(name = "Speed of cars", 
                        trans = "log2")

sp + scale_x_continuous(name = "Speed of cars", 
                        trans = "log2") +
  scale_y_reverse()

sp + scale_y_continuous(trans = log2_trans(),
                        breaks = trans_breaks("log2", function(x) 2^x),
                        labels = trans_format("log2", math_format(2^.x)))

sp + scale_y_continuous(labels = percent)
sp + scale_y_continuous(labels = percent_format())
sp + scale_y_continuous(labels = dollar_format())
sp + scale_y_continuous(labels = scientific)

df = data.frame(
  date = seq(Sys.Date(), len=100, by="1 day")[sample(100, 50)],
  price = runif(50)
)
df = df[order(df$date), ]
head(df)

dp = ggplot(data=df, aes(x=date, y=price)) + geom_line()

dp + scale_x_date(labels = date_format("%m/%d"))
dp + scale_x_date(labels = date_format("%W")) #Weeks
dp + scale_x_date(date_breaks = "months", labels = date_format("%b"))

head(economics)
dp = ggplot(data=economics, aes(x=date, y=psavert)) + geom_line()
min = as.Date("2002-1-1")
max = max(economics$date)
dp + scale_x_date(limits = c(min, max))
dp + scale_x_date(date_breaks = "5 years", labels = date_format("%Y"))

