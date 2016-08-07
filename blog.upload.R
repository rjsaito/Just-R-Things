setwd("C:/Users/rjsai/Dropbox/Data Science/Just R Things/")
library(XMLRPC)
library(RWordPress)
library(knitr)
source("knit2wpCrayon.R")

### Make Post ####
thumbnail <- "General/ProgressBar/progbar.thumbnail.png"
rmd <- "General/ProgressBar/ProgressBar.Rmd"
title <- "Create Progress Bars to Monitor `for` Loops"
category <- "General"
##################################################################


options(WordpressLogin = c(justrthings = '560Grove'), WordpressURL = 'https://justrthings.wordpress.com/xmlrpc.php')

# Upload plots: set knitr options
opts_knit$set(upload.fun = function(file){library(RWordPress);uploadFile(file)$url;})


# Upload featured image / post thumbnail: option: wp_post_thumbnail=postThumbnail$id
postThumbnail <- RWordPress::uploadFile(thumbnail,overwrite = TRUE)

# Post new entry to the wordpress blog and store the post id
postid <- knit2wp(rmd, title = title, categories=category,
    mt_keywords = c('knitr', 'wordpress'), action = "newPost",    #"newPost"
    wp_post_thumbnail=postThumbnail$id, publish=TRUE)



