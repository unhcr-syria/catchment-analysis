
############################################
## Library used to get data from kobo

#source("http://news.mrdwab.com/install_github.R")

#library(devtools)
#install_github("mrdwab/koboloadeR")



### Tutorial to use te
## kobo_datasets 	
#Lists the datasets available for a given user. Returns a data.table with the basic metadata about the available datasets.

##kobo_submission_count 	
#Lists the number of submissions for a particular data collection project. A single integer. This function is mostly for use within the kobo_data_downloader function.


##kobo_data_downloader 	
#Downloads a specified dataset via the KoBo API. Returns a data.table of the entire dataset requested.

#host("https://kobocat.unhcr.org/api/v1/")


## ned to install DT
#install.packages('DT') 
#kobo_apps("data_viewer")


## Store my username and password in another file -- not shared on Github --
# Format is "username:password" 

library("koboloadeR")
source("code/usernamepassword.R")


############################################
## Library used for analysis

#install.packages("labelled")

library("labelled")

library("ggplot2")
library("RColorBrewer")
library("directlabels")
library("ggthemes")


library("sqldf")
library("readxl")

library("stringr")
library("stringi")

library("plyr")
library("tidyr")

library(grid)
library(gridExtra)

###########################################
## library for spatial

library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
# install.packages("deldir")
library(deldir) ## To create voronoi
