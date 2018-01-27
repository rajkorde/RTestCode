library(jsonlite)

d <- readLines("D:/Temp/AFSDump-11142017/11142017/AFSDump_AAD_156044517231.27.json")

library(qdapTools)
x <- fromJSON(d[1])
y <- stack(x)

for line in d {
#  x <- bind_rows(x, fromJSON(line))
}