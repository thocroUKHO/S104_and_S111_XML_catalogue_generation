install.packages(c("tidyverse","sf","sp","ncdf4","raster","raturalearth",
                   "ggrepel","httr","leaflet","GET","versions","BiocManager",
                   "rnaturalearthdata","reshape2","geosphere","circular","metrics",
                   "multimode","ggthemes","rjson","stringr"),dependencies = T)

BiocManager::install("rhdf5")

## Issues encountered are likely to be with the spatial libraries
## normally you can troubleshoot by following and googling the error
## often its related to where the base library paths are on linux
## but shout if these don't work