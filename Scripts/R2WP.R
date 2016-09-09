library(RWordPress)
library(knitr)


options(WordpressLogin = c(rajkorde = '***'),
        WordpressURL = 'http://significantdigits.org/xmlrpc.php')
#        WordPressURL = 'https://blog_url/xmlrpc.php')

postid <- knit2wp('Project/bayesian_project.Rmd',
                  title = 'How to publish with R Markdown in WordPress',
                  categories=c('R'),
                  mt_keywords = c('knitr', 'wordpress'),
                  publish = FALSE)

