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
