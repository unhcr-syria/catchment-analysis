## Load data



#kobo_datasetsunhcr <- kobo_datasets (user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/")
#kobo_datasetsocha <- kobo_datasets (user = usernamepasswordocha , api = "https://kc.humanitarianresponse.info/api/v1/")
#kobo_datasetsocha2 <- kobo_datasets (user = usernamepasswordocha2 , api = "https://kc.humanitarianresponse.info/api/v1/")

########################################################################
###### Downloading info
##kobo_data_downloader(formid, user = NULL, api = "kobo", check = TRUE)
########################################################################


## Downloading using the data+downloader -- but does not work because has 2 grousp in it...
#formid <- 636
#kobo_data_downloader(formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/", check = TRUE)
#write.csv(data_636, "data/data_636.csv")
 
## need to get excel output to get repeat table
#Area1 <- read.csv("data/Community_Centers_Catchment_Area1_2016_08_03_19_11_25.csv")
#Area2 <- read.csv("data/Community_Centers_Catchment_Area2_2016_08_03_19_11_36.csv")
#Area3 <- read.csv("data/Community_Centers_Catchment_Area3_2016_08_03_19_11_41.csv")
#Area4 <- read.csv("data/Community_Centers_Catchment_Area4_2016_08_03_19_11_46.csv")
#Area5 <- read.csv("data/Community_Centers_Catchment_Area5_2016_08_03_19_11_53.csv")
#Area6 <- read.csv("data/Community_Centers_Catchment_Area6_2016_08_03_19_11_57.csv")

form_1 <- "data/Community_Centers_Catchment_Area1_2016_08_03_19_48_23.xlsx"
form1loc <- read_excel(form_1, sheet = 1)   
form1area <- read_excel(form_1, sheet = 2) 

form_2 <- "data/Community_Centers_Catchment_Area2_2016_08_03_19_51_49.xlsx"
form2loc <- read_excel(form_2, sheet = 1)   
form2area <- read_excel(form_2, sheet = 2)

form_3 <- "data/Community_Centers_Catchment_Area3_2016_08_03_19_52_43.xlsx"
form3loc <- read_excel(form_3, sheet = 1)   
form3area <- read_excel(form_3, sheet = 2)

form_4 <- "data/Community_Centers_Catchment_Area4_2016_08_03_19_53_17.xlsx"
form4loc <- read_excel(form_4, sheet = 1)   
form4area <- read_excel(form_4, sheet = 2)

form_5 <- "data/Community_Centers_Catchment_Area5_2016_08_03_19_52_59.xlsx"
form5loc <- read_excel(form_5, sheet = 1)   
form5area <- read_excel(form_5, sheet = 2)

form_6 <- "data/Community_Centers_Catchment_Area6_2016_08_03_19_54_47.xlsx"
form6loc <- read_excel(form_6, sheet = 1)   
form6area <- read_excel(form_6, sheet = 2)

rm(form_1,form_2,form_3,form_4,form_5,form_6)



###################################################################
### Bring Center description together
data <- rbind (form1loc,form2loc,form3loc,form4loc,form5loc,form6loc)
rm(form1loc,form2loc,form3loc,form4loc,form5loc,form6loc)




########################################################################
################# Trying ot get the form nicely with labels 
## https://gist.github.com/mrdwab/28c13a0537044aeb5dc0
########################################################################

#source("code/1-kobo_form_downloader.R")
#kobo_form_downloader (formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/")
## Load survey structure in XLS form
form_tmp <- "data/Community_Centers_Catchment_Area1.xls"
survey <- read_excel(form_tmp, sheet = "survey") 
choices <- read_excel(form_tmp, sheet = "choices") 
## Avoid columns without names
survey <- survey[ ,c("type",   "name" ,  "label::English", "label::Arabic" ,"hint::Arabic",               
                     "hint::English", "relevant",  "required", "constraint",   "constraint_message::Arabic", 
                     "constraint_message::English", "default",  "appearance", "calculation",  "read_only"  ,                
                     "repeat_count")]
## need to delete empty rows from the form
survey <- survey[!is.na(survey$type), ] 

survey_temp <- survey %>%
  filter(!type %in% c("begin group", "end group", "note")) %>%
  separate(type, into = c("q_type", "q_group"), sep = " ", fill = "right")
#names(survey_temp)
survey_temp1 <- survey_temp[ ,c("q_type", "q_group" ,  "name" ,   "label::English")]
names(survey_temp1)[4] <- "label"
survey_temp1 <- as.data.frame(survey_temp1)

choices <- read_excel(form_tmp, sheet = "choices") 
choices <- choices[ ,c( "name",   "label::English")]
names(choices)[2] <- "label"

##################################################
## get variable name from data
rm(datalabel)
datalabel <- as.data.frame( names(data))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)
## new variables name without /
datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")

## Extract the variable name as defined in the form
datalabel$length <- str_length(datalabel$nameor)
datalabel$find <- regexpr(".",datalabel$namenew, fixed = TRUE, useBytes = TRUE)
datalabel$nameor2 <- substr(datalabel$namenew,datalabel$find+1, 200)
datalabel$find2 <- regexpr(".",datalabel$nameor2, fixed = TRUE, useBytes = TRUE)
datalabel$name <- substr(datalabel$nameor2,datalabel$find2 +1, 200)
datalabel <- join(x=datalabel, y=survey_temp1, by="name", type = "left")
datalabel <- join(x=datalabel, y=choices, by="name", type = "left")
names(datalabel)[11] <- "label2"
datalabel$label <- with(datalabel,  ifelse(is.na(datalabel$label),  paste0(datalabel$label2), datalabel$label) )

### Clean variable name to avoid special characters
datalabel$name <- str_replace_all(datalabel$name, "_", ".")
datalabel$name <- str_replace_all(datalabel$name, "-", ".")
datalabel$nameor2 <- str_replace_all(datalabel$nameor2, "_", ".")
datalabel$nameor2 <- str_replace_all(datalabel$nameor2, "-", ".")

## let's recode the variable of the dataset  Reinputing the label
# names(datalabel)
attributes(data)$variable.labels <- datalabel[, 10] 
#for (i in 1:46 ) { var_label(data)[i] <- attributes(data)$variable.labels[i]  }

names(data)<- datalabel[, 5] 


## Eliminate record wich are not in Syria bounding box 35.7270, 32.3106, 42.3850, 37.3190
rm(datasp)

#str(data)
data <- as.data.frame(data)

datasp <-data[( data$.store.gps.longitude > 35.7270 &
                  data$.store.gps.longitude < 42.3850 &
                  data$.store.gps.latitude > 32.3106 &
                  data$.store.gps.latitude < 37.3190 ), c(".store.gps.longitude", ".store.gps.latitude", "center",
            "staff", "benef", "hardware", "centerdate", "organisation", "centerstatus", "internet" ,
            "caseload.Refugees" , "caseload.IDPs" ,  "caseload.Host",           
             "service.Remedial.Classes", "service.Child.Friendly.Spaces",
            "service.Legal.Assistance", "service.Awareness.Raising", "service.PSS.Individual" ,      
            "service.Focus.Group.D", "service.Social.and.R",  "service.In.kind.Assistance",
            "service.Case.Management.CP", "service.Case.Management.SGBV", "service.Small.Business.Grant", 
            "service.Productive.Unit", "service.Vocational.and.life",  "service.primary.health.care",  
            "service.other",     
            "counseling", "outdoor", "bathroom",   "visibility1",  "visibility2", "feedback",  
            "system.excel", "system.casemanagement" ,"system.survey",  "system.no",  "system.other", "data.biodata", "data.spneeds",
            "data.educ", "data.occup", "data.origin",  "data.address", "data.refid", "data.natid",  
            "data.case", "data.biometrics", "data.photos",   "data.incident","data.follow.up",  "data.referals.intern",      
            "data.referals.extern", "data.caserefer",  "data.phone", "data.other")]

## Function to keep the Variable Label
copy_labels(data, datasp)

names(datasp)[1] <- "long"
names(datasp)[2]  <- "lat"
datasp <- as.data.frame(datasp)

## Now we can try to restore the Labels

datalabel2 <- as.data.frame( names(datasp))
names(datalabel2)[1]  <- "nameor2"
datalabel2 <- join(x=datalabel2, y=datalabel, by="nameor2", type = "left")

attributes(datasp)$variable.labels <- datalabel2[, 10] 

############
## ### Now we recode all variables through a loop
#str(datasp)
for (i in 10:57 ) {
  datasp[, i] <- as.factor(datasp[, i])
  datasp[, i] <- revalue(datasp[, i], c( "1"= "Yes", "yes"= "Yes",  "0"= "No",  "no"= "No"))
  datasp[, i][is.na(datasp[, i])] <- "No"
}

### Compute Ratio 
datasp$ratio.staff.benef <- datasp$staff/datasp$benef
datasp$ratio.hardware.staff <- datasp$hardware/datasp$staff

str(datasp)

############################################
##############################
##### Catchement area

area <- rbind (form1area,form2area,form3area,form4area,form5area,form6area)
rm(form1area,form2area,form3area,form4area,form5area,form6area)

## get variable name from data
arealabel <- as.data.frame( names(area))
names(arealabel)[1] <- "name"
arealabel$name <- as.character(arealabel$name)

## new variables name without /
arealabel$name <- str_replace_all(arealabel$name, "/", ".")

## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
names(area) <- arealabel[, 1]

names(area)
## need to deparse
split.var <- strsplit(area$catchment.catchmentarea.catch_repeat.community, " ") 


## Extract from the field 
communities <- as.data.frame(unlist(stri_split_regex(str = area$catchment.catchmentarea.catch_repeat.community, pattern = " ")))
communities.unique <- as.data.frame(unique(communities))

subdistrict <- as.data.frame(unlist(stri_split_regex(str = area$catchment.catchmentarea.catch_repeat.subdistrict, pattern = " ")))
subdistrict.unique <- as.data.frame(unique(subdistrict))

areacom <- as.data.frame(unlist(stri_split_regex(str = area$catchment.catchmentarea.catch_repeat.area, pattern = " ")))
areacom.unique <- as.data.frame(unique(areacom))

names(area)

area$id <- paste0("_index", "_parent_index", sep="" )

## now get in a long format
#for (i in 1:nrow(subdistrict.unique)) {
#  for (j in i:nrow(area)) {
#    subdistrict.unique[i+1] <- ""
#    subdistrict.unique[i+1] <- with(area,
#                                       ifelse(grepl(area[1,j], 
#                                                    ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,
#                                                    area$catchment.catchmentarea.catch_repeat.subdistrict),
#                                              paste0("yes"),subdistrict.unique[i+1] )
#    )
#    # area[12]
#    #print(paste(i,j,sep=","))
#  }
#}








