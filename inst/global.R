
mywd=getwd()
# Install and load packages-------------------------- 
list_packages=c("pixmap",
                "rgdal",
                "jpeg",
                "ade4",
                "ff",
                "ffbase",
                "ggplot2",
                "tidyr",
                "dplyr",
                "shinyFiles")
for (i in 1:length(list_packages)){
  mypackage=list_packages[i]
  if(length(find.package(mypackage, quiet=TRUE))==0){
    install.packages(mypackage)
  }
  require(mypackage, character.only=T)
}


# Create interface ---------------------------------
myblue=rgb(200/255,200/255,255/255)
myyellow=rgb(255/255,255/255,160/255)
mygrey=rgb(210/255,210/255,210/255)
mypink=rgb(255/255,160/255,160/255)
mypalette=colorRampPalette(c(mypink, myyellow, myblue),
                           space = "rgb")
palette(mypalette(5))



source("scripts/0_scr_Functions.R")

# ### Data file selection #########################################
# source("scripts/1_int_File.R")
# ### User input ##############################
# source("scripts/2_int_Input.R")
# ### Description of sub-sample characteristics ####################
# source("scripts/3_int_Description.R")
# ### Co-inertia analysis of sub-sample ############################
# source("scripts/4_int_Analysis.R")
# ### Classification of sub-sample into 1000 cluters ###############
# source("scripts/5_int_Classification.R")
# ### Extrapolation into final number of clusters  #################
# source("scripts/6_int_Extrapolation.R")

