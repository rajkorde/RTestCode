library(survey)
library(foreign)

mydata <- 
  read.dta( 
    "http://www.ats.ucla.edu/stat/books/sop/momsag.dta" , 
    convert.factors = FALSE 
  )

mydesign <- svydesign(ids = ~1, data = mydata, weights = ~weight1,
                      fpc = ~birth)

sum(mydata$weight1)
degf(mydesign)

unwtd.count(~momsag, mydesign)
svymean(~momsag, mydesign)
svytotal(~momsag, mydesign)
svyciprop(~momsag, mydesign, method = "logit")

#ex2
province <-
  read.table( text =
                "id cluster ue91 lab91
              1 1 4123 33786
              2 4 760 5919
              3 5 721 4930
              4 15 142 675
              5 18 187 1448
              6 26 331 2543
              7 30 127 1084
              8 31 219 1330" ,
              header = TRUE
  )
province$fpc <- 32
province$weights <- 4

province.design <-
  svydesign(
    ids = ~1,
    data = province,
    fpc = ~fpc,
    weights = ~ weights
  )

svymean(~ue91, province.design)
svytotal(~ue91, province.design)
(myratio <- svyratio(~ue91, ~lab91, province.design))
svyquantile(~ue91, province.design, quantiles = 0.5, ci = TRUE)

#ex1 post-stratification
#http://www.ats.ucla.edu/stat/r/faq/svy_r_post.htm
mydata <- 
  read.dta( 
    "http://www.ats.ucla.edu/stat/data/dogcats.dta" , 
    convert.factors = FALSE 
  )
mydata$fpc <- 1300


preliminary.design <- svydesign(
  id = ~1,
  data = mydata,
  fpc = ~fpc
)

ps.weights <- data.frame(
  type = c(1, 2),
  Freq = c(850, 450)
)

mydesign <- postStratify(preliminary.design, strata = ~type,
                         population = ps.weights)
degf(mydesign)

unwtd.count(~totexp, mydesign)

svymean(~totexp, mydesign)


svyby(~totexp, ~type, mydesign, svymean)

#ex2
province <-
  read.table( text =
                "id str clu wt ue91 lab91 poststr gwt postwt sruv srcvs
              1 1 1 4 4123 33786 1 .5833 2.333 .25 .43
              2 1 4 4 760 5919 1 .5833 2.333 .25 .43
              3 1 5 4 721 4930 1 .5833 2.333 .25 .43
              4 1 15 4 142 675 2 1.2500 5.0000 .25 .20
              5 1 18 4 187 1448 2 1.2500 5.0000 .25 .20
              6 1 26 4 331 2543 2 1.2500 5.0000 .25 .20
              7 1 30 4 127 1084 2 1.2500 5.0000 .25 .20
              8 1 31 4 219 1330 2 1.2500 5.0000 .25 .20" ,
              header = TRUE
  )

province$fpc <- 32

province.prelim.design <- 
  svydesign( 
    id = ~1 , 
    data = province ,
    weights = ~wt ,
    fpc = ~fpc
  )

province.ps.weights <-
  data.frame(
    poststr = c( 1 , 2 ) ,
    Freq = c( 7 , 25 ) 
  )

province.design <- 
  postStratify(
    province.prelim.design ,
    strata = ~poststr ,
    population = province.ps.weights
  )

svymean(~ue91, province.design)
confint(myratio, df = degf(province.design))
svyquantile(~ue91, province.design, quantiles = 0.5, ci = TRUE)

## post-stratification example
#https://www.r-bloggers.com/social-science-goes-r-weighted-survey-data/
load(url("http://knutur.at/wsmt/R/RData/small.RData"))

small.w <- svydesign(ids = ~1, data = small, weights = small$weight)

#https://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
small.svy.unweighted <- svydesign(ids=~1, data=small)

sex.dist <- data.frame(sex = c("M", "F"),
                       Freq = nrow(small) * c(0.45, 0.55))
edu.dist <- data.frame(edu = c("Lo", "Mid", "Hi"),
                       Freq = nrow(small) * c(0.30, 0.50, 0.20))

small.svy.rake <- rake(design = small.svy.unweighted, sample.margins = list(~sex, ~edu),
                       population.margins = list(sex.dist, edu.dist))

summary(weights(small.svy.rake))
#use trimWeights() if the weights are <0.3 or >3
#small.svy.rake.trim <- trimWeights(small.svy.rake, lower=0.3, upper=3, strict=TRUE) 

