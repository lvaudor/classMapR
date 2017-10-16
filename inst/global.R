mywd=getwd()
# Install and load packages--------------------------
list_packages=c("shiny",
                "pixmap",
                "rgdal",
                "jpeg",
                "ade4",
                "ff",
                "ffbase",
                "ggplot2",
                "tidyr",
                "dplyr",
                "shinyFiles",
                "stringr",
                "raster")
for (i in 1:length(list_packages)){
  require(list_packages[i], character.only=T)
}

# Create interface ---------------------------------
myblue=rgb(200/255,200/255,255/255)
myyellow=rgb(255/255,255/255,160/255)
mygrey=rgb(210/255,210/255,210/255)
mypink=rgb(255/255,160/255,160/255)
mypalette=colorRampPalette(c(mypink, myyellow, myblue),
                           space = "rgb")
palette(mypalette(5))
