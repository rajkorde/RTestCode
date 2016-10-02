#interactive maps with rMaps
library(rMaps)

crosslet(
  x = "country", 
  y = c("freedom_openness"),
  data = web_index
)

ichoropleth(Crime ~ State, data = subset(violent_crime, Year == 2010))
ichoropleth(Crime ~ State, data = violent_crime, animate = "Year")
ichoropleth(Crime ~ State, data = violent_crime, animate = "Year", play = TRUE)


map <- Leaflet$new()
map$setView(c(51.505, -0.09), zoom = 13)
map$tileLayer(provider = 'Stamen.Watercolor')
map$marker(
  c(51.5, -0.09),
  bindPopup = 'Hi. I am a popup'
)
map

d <- read_csv(file = "Data/medianincome.csv")
names(d) <- c("State", "Income")
ichoropleth(Income ~ State, data = d)

#ggmap
library(ggmap)

map<-get_map(location='usa', zoom = 2)
ggmap(map)

library(xlsx)
library(zipcode)
urlfile <-'http://www.psc.isr.umich.edu/dis/census/Features/tract2zip/MedianZIP-3.xlsx'
destfile <- "census20062010.xlsx"
download.file(urlfile, destfile, mode="wb")
census <- read.xlsx2(destfile, sheetName = "Median")

census <- census[c('Zip','Median..')]
names(census) <- c('Zip','Median')
census$Median <- as.character(census$Median)
census$Median <- as.numeric(gsub(',','',census$Median))

data(zipcode)
census$Zip <- clean.zipcodes(census$Zip)

census <- merge(census, zipcode, by.x='Zip', by.y='zip')

map<-get_map(location='united states', zoom=4, maptype = "toner-lite",
             source='google',color='color')

ggmap(map) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, colour=Median), 
  data=census, alpha=.5, na.rm = T)  + 
  scale_color_gradient(low="beige", high="blue")

#geom_map
library(ggplot2)
library(maps)
library(maptools)
library(viridis)
library(ggthemes)
library(mapdata)


choro <- data_frame(some_other_name = unique(us$region),
                    some_critical_value=sample(10000, length(some_other_name)))


ggplot() +
  geom_map(data = us, map = us, aes(long, lat, map_id = region),
           color = "#2b2b2b", fill = NA, size = 0.15) +
  coord_map("polyconic") +
  theme_map() +
  theme(plot.margin = margin(20, 20, 20, 20))

county <- map_data("county")

ggplot() + geom_map(data=county, map=county,
                    aes(long, lat, map_id=region),
                    color="#2b2b2b", fill=NA, size=0.15) +
  coord_map("polyconic") +
  theme_map() +
  theme(plot.margin=margin(20,20,20,20))

ggplot() +
  geom_map(data=us, map=us,
           aes(long, lat, map_id=region),
           color="#2b2b2b", fill=NA, size=0.15) +
  geom_map(data=choro, map = us,
           aes(fill = some_critical_value,
               map_id = some_other_name),
           color="white", size=0.15) +
  scale_fill_viridis(name = "Value") +
  coord_map("polyconic") +
  theme_map() +
  theme(plot.margin=margin(20,20,20,20)) +
  theme(legend.position=c(0.85, 0.2))

state <- map_data("state")
county <- map_data("county")
usa <- map_data("usa")

ggplot() + geom_map(data=county, map=county,
                    aes(long, lat, map_id=region),
                    color="#2b2b2b", fill=NA, size=0.15) + 
  geom_map(data=state, map=state,
                    aes(long, lat, map_id=region),
                    color="#2166ac", fill=NA, size=0.5) +
  geom_map(data=usa, map=usa,
                    aes(long, lat, map_id=region),
                    color="#4d9221", fill=NA, size=1) + 
  coord_map("polyconic") + 
  theme_map() + 
  theme(plot.margin=margin(20,20,20,20))

states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault)) +
  coord_map("albers", at0 = 45.5, lat1 = 29.5)

ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault / murder), color = "white") +
  scale_fill_viridis(option = "magma", alpha = 0.5) +
  coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
  theme_map()


#geom_map
ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)
positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
        0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
        2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

ggplot(values) + geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions)

ggplot(values, aes(fill = value)) +
  geom_map(aes(map_id = id), map = positions) +
  expand_limits(positions)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)

statesname <- data.frame(state.center, state.abb, state = tolower(state.name))
crimes <- merge(crimes, statesname, by = "state")
crimes <- crimes[!crimes$state.abb %in% c("AK", "HI"),]

ggplot(crimes, aes(map_id = state)) +
  geom_map(aes(fill = Murder), map = states) +
  expand_limits(x = states$long, y = states$lat) +
  scale_fill_gradient2(low = muted("red"),  
                       mid = "lightgrey", high = muted("blue"),
                       midpoint = 9, limits = c(0, 18)) + 
# coord_map("polyconic") +
  theme_map() +
  theme(plot.margin=margin(20,20,20,20)) +
  theme(legend.position=c(0.85, 0.2)) +
  geom_text(aes(x=x, y=y, label=state.abb, group=NULL), size=2) 


#example


d <- read.csv(file = "Data/medianincome.csv")
d$MedianIncome <- as.numeric(gsub(",", "", d$MedianIncome))
d$state = tolower(d$state)

states <- map_data("state")
statesname <- data.frame(state.center, state.abb, state = tolower(state.name))

d <- merge(d, statesname, by = "state")
d <- d[!d$state.abb %in% c("AK", "HI"),]

ggplot(d, aes(map_id = state)) +
  geom_map(aes(fill = MedianIncome), map = states) +
  expand_limits(x = states$long, y = states$lat) +
  scale_fill_gradient2(low = muted("red"),  
                       mid = "lightgrey", high = muted("blue"),
                       midpoint = mean(d$MedianIncome), 
                       limits = c(min(d$MedianIncome), max(d$MedianIncome))) + 
  # coord_map("polyconic") +
  theme_map() +
  theme(plot.margin=margin(20,20,20,20)) +
  theme(legend.position=c(0.85, 0.2)) +
  geom_text(aes(x=x, y=y, label=state.abb, group=NULL), size=2) 

