Sys.setenv(TZ='GMT')

## 

#' Download tide guage data from IOC website
#'
#' @param short_name 
#' @param long_name 
#' @param start_date 
#' @param end_date 
#' @param verbose 
#'
#' @return dataframe with date, height and the station long name
#' @export
#'
#' @examples
pull_from_ioc <- function(short_name=c("shee"),long_name=c("Sheerness"),start_date=lubridate::make_date(2023,11,22),end_date=lubridate::make_date(2024,1,5),verbose=T,period=30) {
  
  data <- c()
  
  period <- as.integer(period)
  if (period > 30 | period < 1) {
    message("Null return, period between 1 and 30 please")
    return(NULL)} 
  
  time1 <- c(seq(start_date,end_date,by=sprintf("%s days",period)),end_date)
  
  if (abs(as.numeric(difftime(start_date,end_date,units = "days"))) <= period) {time1 <- end_date}
  
  for (i in seq(1,length(short_name))) {
    
    station_data <- c()
    
    for (j in seq(1,length(time1))) {
      
      if (isTRUE(verbose)) {
        message("Downloading ",short_name[i]," (",long_name[i],") for ",period," day period ending ",as.POSIXct(time1[j]))
      }
      
      theurl <- sprintf("http://www.ioc-sealevelmonitoring.org/bgraph.php?code=%s&output=tab&period=%s&endtime=%s",short_name[i],period,as.POSIXct(time1[j]))
      doc <- XML::htmlParse(httr::GET(theurl, httr::user_agent("Mozilla")))
      #XML::removeNodes(XML::getNodeSet(doc,"//*/comment()"))
      dum.tables<- (XML::readHTMLTable(doc))
      if (length(dum.tables)==0) {next}
      tide_dat <- data.frame(date=dum.tables[[1]][,1],height=dum.tables[[1]][,2],station=long_name[i])
      station_data <- rbind(station_data,tide_dat)
      station_data <- station_data[!duplicated(station_data),]
    }
    
    data <- rbind(data,station_data)
  }
  
  data$date <- as.POSIXct(data$date,tz="UTC")
  data$height <- as.numeric(data$height)
  return(data)
}


#' Convert a String Date to posix, as rhdf5 brings it in as string in a set format
#'
#' @param HDF5 read in string date from HDF5 file
#'
#' @return the actual useful date POSIX
#' @export
#'
#' @examples
str_date_to_posix <- function(x) {
  return(lubridate::make_datetime(year = as.numeric(substr(x,1,4)),
                                  month = as.numeric(substr(x,5,6)),
                                  day = as.numeric(substr(x,7,8)),
                                  hour = as.numeric(substr(x,10,11)),
                                  min = as.numeric(substr(x,12,13)),
                                  sec = as.numeric(substr(x,14,15)),tz = "UTC"))
}


#' Extract info and logic checks on HDF5 file metadata
#' 
#' Basically there are three bits of metadata
#' from the various levels of the file.
#' 
#' We then get useful info like the start date, end date, spatial limits, time 
#' interval etc. 
#' 
#' The spatial limits are actually defined twice (in the highest level of metadata
#' and a step below), so one check is if these match
#' 
#' Another check is that using the spatial limits, and the lon and lat spacing interval, 
#' we test if the dimensions generated from that match the dimensions of the data
#' 
#' We can also test if the proposed time interval makes sense given the 
#' start and end time and number of timesteps
#' 
#' The full spec of what to test is in the product descriptoin, this could be added
#' at some point
#'
#' @param s104_or_s111 are we looking at S104 or S111
#' @param filepath full filepath location of S104 or S111 file
#'
#' @return a list with 1) dataframe of metadata 
#' 2) and test results
#' 3) the calculated longitudes from the hdf5 metadata and 4) the latitudes, and
#' 5-7) metadata fields as read from the file
#' @export
#'
#' @examples
hdf5_limits_and_check <- function(s104_or_s111="s104",
                        filepath="Z:/individual/thocro/MDP_data_2024-01-22/MDP_data_2024-01-22/104GB00_20231109T000000Z_GB4DNABL_dcf2.h5") {
  
  hd <- rhdf5::H5Fopen(filepath) # open file
  
  main_atts <- rhdf5::h5readAttributes(hd, "/") # highest level metadata
  
  # spatial limits from highest level metadata
  west_lim <- main_atts$westBoundLongitude
  east_lim <- main_atts$eastBoundLongitude
  north_lim <- main_atts$northBoundLatitude
  south_lim <- main_atts$southBoundLatitude
  
  ## Bring in the other metadata info from different attribute layers
  if (tolower(s104_or_s111) == "s104") {
    additional_atts <- rhdf5::h5readAttributes(hd, "WaterLevel/WaterLevel.01")
    n = length(hd$WaterLevel$WaterLevel.01)
    other_atts <- rhdf5::h5readAttributes(hd, "WaterLevel")
    verticalDatum <- main_atts$verticalDatum # important to define vertical transformation
  } else if (tolower(s104_or_s111) == "s111") {
    additional_atts <- rhdf5::h5readAttributes(hd, "SurfaceCurrent/SurfaceCurrent.01")
    n = length(hd$SurfaceCurrent$SurfaceCurrent.01)
    other_atts <- rhdf5::h5readAttributes(hd, "SurfaceCurrent")
    verticalDatum <- NA # not included S-111
  }
  
  # start, end, number of timesteps, define time interval, extract time interval to test
  start_date <- str_date_to_posix(additional_atts$dateTimeOfFirstRecord)
  end_date <- str_date_to_posix(additional_atts$dateTimeOfLastRecord)
  time_length <- additional_atts$numberOfTimes
  time1 <- seq(start_date,end_date,length=time_length)
  time_interval <- as.numeric(abs(difftime(time1[1],time1[2],units="secs"))) # calculated interval
  timeRecord_interval <- additional_atts$timeRecordInterval # defined interval
  
  # spatial limits are also defined again in lower level metadata, get them and
  # test that they match the higher level ones
  west_lim2 <- additional_atts$westBoundLongitude
  east_lim2 <- additional_atts$eastBoundLongitude
  north_lim2 <- additional_atts$northBoundLatitude
  south_lim2 <- additional_atts$southBoundLatitude
  
  origin_lon <- additional_atts$gridOriginLongitude
  origin_lat <- additional_atts$gridOriginLatitude
  
  # spacing for lon and lat
  horz_spacing <- abs(additional_atts$gridSpacingLongitudinal)
  vert_spacing <- abs(additional_atts$gridSpacingLatitudinal)
  
  # length(seq(west_lim2,east_lim2,by=horz_spacing))
  # length(seq(south_lim2,north_lim2,by=vert_spacing))
  # length(seq(west_lim,east_lim,by=horz_spacing))
  # length(seq(south_lim,north_lim,by=vert_spacing))
  
  # spatial dimensions of the actual data
  if (s104_or_s111 == "s104") {
    dimension_of_first_timestep <- dim(hd$WaterLevel$WaterLevel.01$Group_001$values$waterLevelHeight)} else {
    dimension_of_first_timestep <- dim(hd$SurfaceCurrent$SurfaceCurrent.01$Group_001$values$surfaceCurrentSpeed)
  }

  dat_rows <- as.numeric(dimension_of_first_timestep[1])
  dat_cols <- as.numeric(dimension_of_first_timestep[2])
  
  # what the lon lat spacing has to be, give the dimensions
  lons <- seq(west_lim,east_lim,length=dat_rows)
  lats <- seq(south_lim,north_lim,length=dat_cols)
  
  # if (round(origin_lon,7) == round(west_lim2,7)) {lons <- seq(west_lim,east_lim,length=dat_rows)} else {
  #   lons <- seq(east_lim,west_lim,length=dat_rows)
  # }
  
  if (tolower(s104_or_s111) == "s104") {
    field_max = other_atts$maxDatasetHeight
    field_min = other_atts$minDatasetHeight
    field_max_pass <- ifelse(field_max <= 25 ,TRUE,FALSE ) # really loose constraints
    field_min_pass <- ifelse(field_min >= - 15 ,TRUE,FALSE )
    
  } else {
    field_max = other_atts$maxDatasetCurrentSpeed
    field_min = other_atts$minDatasetCurrentSpeed
    field_max_pass <- ifelse(field_max <= 50 ,TRUE,FALSE ) # really loose constraints
    field_min_pass <- ifelse(field_min >= 0 ,TRUE,FALSE ) # but absolutely shouldn't be below 0
  }
  
  # we want to return 
  # 1) the whole metadata as a datafrane
  # 2) the test results
  # 3) lons
  # 4) lats
  # 5-7) the three metadata full for checking whilst testing

  # test_results <- data.frame(S100=s104_or_s111,
  #                            west_lim_match = ifelse(round(west_lim,7) == round(west_lim2,7),TRUE,FALSE),
  #                            east_lim_match = ifelse(round(east_lim,7) == round(east_lim2,7),TRUE,FALSE),
  #                            north_lim_match = ifelse(round(north_lim,7) == round(north_lim2,7),TRUE,FALSE),
  #                            south_lim_match = ifelse(round(south_lim,7) == round(south_lim2,7),TRUE,FALSE),
  #                            timesteps_match_data = time_interval == timeRecord_interval,
  #                            field_max_pass = field_max_pass,
  #                            field_min_pass = field_min_pass)
  # 
  test_results <- data.frame(S100=s104_or_s111,
                             timesteps_match_data = time_interval == timeRecord_interval,
                             field_max_pass = field_max_pass,
                             field_min_pass = field_min_pass)
  
  metadata <- data.frame(S100=s104_or_s111,
                      grid_origin_lon=origin_lon,
                      grid_origin_lat=origin_lat,
                      verticalDatum=verticalDatum,
                      west_lim=west_lim,
                      east_lim=east_lim,
                      south_lim=south_lim,
                      north_lim=north_lim,
                      start_date=start_date,
                      end_date=end_date,
                      interval=time_interval,
                      timesteps=n,
                      nlat=dat_cols,
                      nlon=dat_rows,
                      field_max = field_max,
                      field_min = field_min)
  
  rhdf5::H5Fclose(hd)
  
  return(list(metadata=metadata,test_results=test_results,lons=lons,lats=lats,
              main_attributes=main_atts,attribute2=additional_atts,attribute3=other_atts))
  
}

#' Get totaltide information from a specific area for a specific time
#'
#' @param north north limit
#' @param south south limit
#' @param west west limit 
#' @param east east limit
#' @param start_date start date, will default to YYYY-MM-DD and drop any h-m-s
#' @param end_date end date
#' @param interval difference between time steps in minutes
#' @param api_key TotalTide API Key
#'
#' @return TotalTide time series from all diamonds and heights within the bounding
#' box, at the time interval required
#' @export
#'
#' @examples
get_relevant_tt <- function(north=a$metadata$north_lim,
                            south=a$metadata$south_lim,
                            west=a$metadata$west_lim,
                            east=a$metadata$east_lim,
                            start_date=as.POSIXct(a$metadata$start_date),
                            end_date=as.POSIXct(a$metadata$end_date),
                            interval=15,
                            api_key="tomcroppercfe797931028894448d808276a3",
                            extra_totaltide_ids=extra_totaltide_ids) {
  
  west <- ifelse(west <0,west+360,west) # doesn't like -180 to 180
  east <- ifelse(east <0,east+360,east)
  
  ## scan available stations in TotalTide
  tt <- httr::GET(sprintf("http://ukhotidalapi.westeurope.cloudapp.azure.com/api/v1/Stations?type=AllStations&north=%s&south=%s&east=%s&west=%s&api_key=%s",north,south,east,west,api_key))
  tt <- jsonlite::fromJSON(rawToChar(tt$content))
  
  ## format TotalTide 
  df <- tt$features$properties
  
  ## extra stations from observation match
  ttt <- do.call(dplyr::bind_rows,lapply(extra_totaltide_ids,function(x){
    
    message(x)
    ttt <- httr::GET(sprintf("http://ukhotidalapi.westeurope.cloudapp.azure.com/api/v1/Stations/%s?api_key=%s",x,api_key))
    ttt <- jsonlite::fromJSON(rawToChar(ttt$content))
    df1 <- data.frame(ttt$properties)
    df1$id <- x
    df1$lon <- ttt$geometry$coordinates[1]
    df1$lat <- ttt$geometry$coordinates[2]
    df1$type <- dplyr::case_when(df1$StationType %in% c("TidalPortSecondaryHarmonic","TidalPortSecondaryNonHarmonic","TidalPortStandardHarmonic","TidalPortStandardNonHarmonic") ~ "Height",
                                df1$StationType %in% c("TidalStreamHarmonic","TidalStreamNonHarmonic") ~ "Stream")
    return(df1)
  }))

  
  if (is.null(df)) {
    message("No Points Found in this area")
    return(NULL)} # exit if no points
  
  
  
  df$id <- tt$features$id
  lon <- unlist(tt$features$geometry$coordinates)
  df$lat <- lon[seq(2,length(lon),2)]
  df$lon <- lon[seq(1,length(lon),2)]
  df$type <- dplyr::case_when(df$StationType %in% c("TidalPortSecondaryHarmonic","TidalPortSecondaryNonHarmonic","TidalPortStandardHarmonic","TidalPortStandardNonHarmonic") ~ "Height",
                              df$StationType %in% c("TidalStreamHarmonic","TidalStreamNonHarmonic") ~ "Stream")
  
  df <- dplyr::bind_rows(df,ttt)
  df <- df[!duplicated(df$id),]
  rm(tt,lon,ttt)
  
  # hardcode for now whilst testing
  days <- ceiling((length(seq(start_date,end_date,by="15 mins"))+1)/4)
  
  # API wants h-m-s in format I CBA to deal with, so just use the first day
  start_date_raw <- start_date
  end_date_raw <- end_date
  start_date <- lubridate::floor_date(start_date,unit = "day")
  end_date <- lubridate::ceiling_date(end_date,unit = "day")
  
  # Loop through every location 
  
  data_all <- c()
  
  for (i in seq(1,nrow(df))) {
    
    j <- df$type[i]
    id <- df$id[i]
    
    # Height (metres) or Stream (metres per second)
    
    if (j == "Stream") {
      str1 <- "http://ukhotidalapi.westeurope.cloudapp.azure.com/api/v1/TidalStreamRates/"
    } else {
      str1 <- "http://ukhotidalapi.westeurope.cloudapp.azure.com/api/v1/TidalPortHeights/"
    }
    
    stream1 <- httr::GET(paste(str1,id,"?dateTime=",start_date,"&duration=",days,"&interval=",interval,"&api_key=",api_key,sep = ""))
    data1 <- jsonlite::fromJSON(rawToChar(stream1$content))
    data1$type <- j
    data1$id <- id
    data1$date <- lubridate::make_datetime(as.numeric(substr(data1$DateTime,1,4)),
                                           as.numeric(substr(data1$DateTime,6,7)),
                                           as.numeric(substr(data1$DateTime,9,10)),
                                           as.numeric(substr(data1$DateTime,12,13)),
                                           as.numeric(substr(data1$DateTime,15,16)))
    data_all <- dplyr::bind_rows(data_all,data1)
  }
  
  # relevant output
  data_all <- data_all[,2:ncol(data_all)]
  data_all <- subset(data_all,data_all$date <= end_date_raw)
  data_all <- subset(data_all,data_all$date >= start_date_raw)
  
  data_all <- merge(data_all,df[,c("id","lon","lat")],all.x=T,by = "id")
  
  return(data_all)}



get_relevant_observations <- function(north = a$metadata$north_lim,
                                      south = a$metadata$south_lim,
                                      west = a$metadata$west_lim,
                                      east = a$metadata$east_lim,
                                      start_date = as.POSIXct(a$metadata$start_date),
                                      end_date = as.POSIXct(a$metadata$end_date),
                                      interval = 15,
                                      ico_obs_file = "Z:/individual/thocro/ico_monitoring_list_with_nearest_TT.RDS") {
  
  # file with all ICO Sea Level Monitoring Station Location and closest TotalTide point
  obs <- readRDS(ico_obs_file)
  
  west <- ifelse(west < 0, west + 360, west) # doesn't like -180 to 180
  east <- ifelse(east < 0, east + 360, east)
  obs <- subset(obs, obs$Lon >= west)
  obs <- subset(obs, obs$Lon <= east)
  obs <- subset(obs, obs$Lat >= south)
  obs <- subset(obs, obs$Lat <= north)
  
  if (nrow(obs) == 0 | is.null(obs)) {return(NULL)}
  
  start_date_raw <- start_date
  end_date_raw <- end_date
  start_date <- lubridate::floor_date(start_date, unit = "day")
  end_date <- lubridate::ceiling_date(end_date, unit = "day")
  period_input <- ifelse(as.numeric(difftime(end_date, start_date, units = "days")) < 30,as.numeric(difftime(end_date, start_date, units = "days")), 30)
  
  # Loop through every location
  data_all <- c()
  
  for (i in seq(1, nrow(obs))) {
    short_name <- obs$Code[i]
    long_name <- obs$Location[i]
    obs_output <- tryCatch(pull_from_ioc(short_name = short_name,long_name = long_name,start_date = start_date,end_date = end_date,verbose = TRUE,period = period_input),error = function(e) {NULL})
    
    if (is.null(obs_output)) {next}
    
    obs_output <- subset(obs_output,lubridate::minute(obs_output$date) %in% seq(0, 60, interval) & lubridate::second(obs_output$date) == 0)
    obs_output <- subset(obs_output,obs_output$date >= start_date & obs_output$date <= end_date)
    obs_output$short_name <- short_name
    obs_output$long_name <- long_name
    obs_output$id <- obs$id_height[i] # totaltide height ID
    obs_output$obs_lon <- obs$Lon[i]
    obs_output$obs_lat <- obs$Lat[i]
    data_all <- rbind(data_all, obs_output)
  }
  
  data_all$obs_lon <-  ifelse(data_all$obs_lon > 180,data_all$obs_lon - 360,data_all$obs_lon)
  return(data_all)
  
}






#' Extract time series from HDF5 file matching TotalTide locations for comparison
#'
#' @param s104_or_s111 S104 or S111
#' @param filepath file location
#' @param extract_lons longitude positions of TotalTide points to extract
#' @param extract_lats latitude positions of TotalTide points to extract
#' @param file_lons longitudes of the S1XX file, determined from hdf5_limits_and_check()
#' @param file_lats latitudes of the S1XX file, determined from hdf5_limits_and_check()
#' @param start_date start time
#' @param end_date end time
#' @param interval time interval
#' @param id station id of ToTaltide points to extract
#' @param start_lon from hdf5_limits_and_check, it's the 'starting' longitude so
#' we know which way to 'flip' the hdf5 array so it's the right way around, i.e.
#' which longitude to start from, not yet implemented 
#' @param start_lat as start_lon
#'
#' @return
#' @export
#'
#' @examples
extract_series_from_hdf5 <- function(s104_or_s111="s104",
                              filepath="Z:/individual/thocro/MDP_data_2024-01-22/MDP_data_2024-01-22/104GB00_20231109T000000Z_GB4DNABL_dcf2.h5",
                              extract_lons=b[!duplicated(b$id),"lon"],
                              extract_lats=b[!duplicated(b$id),"lat"],
                              file_lons=a$lons,
                              file_lats=a$lats,
                              start_date=a$metadata$start_date,
                              end_date=a$metadata$end_date,
                              interval=a$metadata$interval,
                              id=b[!duplicated(b$id),"id"],
                              start_lon=a$metadata$grid_origin_lon,
                              start_lat=a$metadata$grid_origin_lat,
                              flip_lats=FALSE) {
  
  extract_these_data <- data.frame(id=id,lon=extract_lons,lat=extract_lats)
  
  gen_time_series <- seq(start_date,end_date,by=sprintf("%02d mins",interval))
  
  message(gen_time_series[1])
  message(gen_time_series[24])
  
  hd <- rhdf5::H5Fopen(filepath)
    
  if (s104_or_s111=="s104") {
    a2 <- data.frame(hd$WaterLevel$WaterLevel.01[[1]]$values$waterLevelHeight)} else {
      a <- hd$SurfaceCurrent
      a2 <- data.frame(hd$SurfaceCurrent$SurfaceCurrent.01[[1]]$values$surfaceCurrentSpeed)
      b2 <- data.frame(hd$SurfaceCurrent$SurfaceCurrent.01[[1]]$values$surfaceCurrentDirection)
  }
  
  ## lat flip or not, important (needs proper implementing based on grid origin)
  if (flip_lats==TRUE) {file_lats <- rev(file_lats)}
  
  # decided the easist way to extract multiple spatial locations is to use
  # raster::extract, could have done this many different ways, hopefully this is 
  # relatively easy to follow
  
  colnames(a2) <- file_lats
  a2$lon <- file_lons
  a3 <- reshape2::melt(a2,id.var="lon") # wide to long format
  colnames(a3)[2] <- "lat"
  a3$lat <- as.numeric(as.character(a3$lat)) 
  #a3$value <- a3$value/1000
  # to deal with how NAs are set
  # we only want to extract over real obs
  # some grid cells dry out but ignoring for now
  a3 <- a3[a3$value <= 100,]  
  a3 <- a3[a3$value >= -100,]
  raster_layer <- raster::rasterFromXYZ(a3)
  test <- raster::extract(raster_layer,extract_these_data[c("lon","lat")])
  
  if ( length(test) == 1 & is.na(test[1])) {message("Extract has missed probably due to precision of TT points")
    return(NULL)}
  
  extract_these_data$new_lon <- ifelse(is.na(test),NA,extract_these_data$lon)
  extract_these_data$new_lat <- ifelse(is.na(test),NA,extract_these_data$lat)
  
  # some direct points in TotalTide as extracted will be a miss
  # determine the closest grid cell with data to use as the cell to extract from
  
  for (i in seq(1,nrow(extract_these_data))) {
    
    if (!is.na(extract_these_data$new_lon[i])) (next) else {
      
      a3$distances <- geosphere::distHaversine(extract_these_data[i,c("lon","lat")],a3[,c("lon","lat")])
      a3 <- a3[order(a3$distances),]
      extract_these_data[i,c("new_lon","new_lat")] <- a3[1,c("lon","lat")]
    }
    
  }
  
  # Output Data Frame
  output_data <- c()
  
  # S104 or S111
  
  if (s104_or_s111=="s104") {
    a2 <- hd$WaterLevel$WaterLevel.01
  } else {
    a2 <- hd$SurfaceCurrent$SurfaceCurrent.01
  }
  
  # Loop over time and extract values of Height or Speed and Direction
  
  for (i in seq(1,length(gen_time_series))) {
    
    if (s104_or_s111=="s104") {a3 <- data.frame(a2[[i]]$values$waterLevelHeight)} else {
      a3 <- data.frame(a2[[i]]$values$surfaceCurrentSpeed)
      b3 <- data.frame(a2[[i]]$values$surfaceCurrentDirection)}
    
    colnames(a3) <- file_lats
    a3$lon <- file_lons
    a4 <- reshape2::melt(a3,id.var="lon")
    colnames(a4)[2] <- "lat"
    a4$lat <- as.numeric(as.character(a4$lat))
    #a4$value <- a4$value / 1000
    a4 <- a4[a4$value <= 1000,]
    a4 <- a4[a4$value >= -1000,]
    raster_layer <- raster::rasterFromXYZ(a4)
    
    test <- data.frame(id=id,value=raster::extract(raster_layer,extract_these_data[c("new_lon","new_lat")]),date=gen_time_series[i])
    
    if (s104_or_s111=="s111") {
      colnames(b3) <- file_lats
      b3$lon <- file_lons
      b4 <- reshape2::melt(b3,id.var="lon")
      colnames(b4)[2] <- "lat"
      b4$lat <- as.numeric(as.character(b4$lat))
      b4 <- b4[b4$value <= 1000,]
      b4 <- b4[b4$value >= -1000,]
      raster_layer <- raster::rasterFromXYZ(b4)
      test$direction <- raster::extract(raster_layer,extract_these_data[c("new_lon","new_lat")])
    }
    message(i)
    output_data <- dplyr::bind_rows(output_data,test)
  }
  
  rhdf5::h5closeAll()
 # rhdf5::H5Fclose(hd)
  
  return(output_data)
}


#' Poorly designed and overly complicated but outputs what is needed
#'
#' @param s104_or_s111 
#' @param output_directory 
#' @param totaltide 
#' @param hdf5_series 
#' @param figures 
#' @param metadata_from_hdf5_limits_and_check 
#' @param s111_dir_mode_threshold 
#' @param s111_dir_sd_threshold 
#' @param s111_speed_corr_threshold 
#' @param s111_speed_mae_threshold 
#' @param s104_corr_threshold 
#' @param s104_mae_threshold 
#'
#' @return
#' @export
#'
#' @examples
do_stat_comparison <- function(s104_or_s111="s104",
                               output_directory="Z:/individual/thocro/testing_s104s111",
                               totaltide=b,
                               hdf5_series=d,
                               figures=T,
                               metadata_from_hdf5_limits_and_check=a,
                               s111_dir_mode_threshold=22.5,
                               s111_dir_sd_threshold=45,
                               s111_speed_corr_threshold=0.6,
                               s111_speed_mae_threshold=0.75,
                               s104_corr_threshold=0.6,
                               s104_mae_threshold=0.5,
                               fudge_vertical_level=TRUE) {
  
  test <- merge(hdf5_series,totaltide)
  if (nrow(test)==0) {message("Something went wrong at the merge at the start")
    return(NULL)}
  a <- metadata_from_hdf5_limits_and_check
  
  # in lieu of properly adding the vertical transformation code, do that post march12
  # this is a hack but it's OK for testing as we are looking for ballpark not exact
  
  if (isTRUE(fudge_vertical_level)) {
    
    if (s104_or_s111 == "s104") {
      bias_removal <- test |> dplyr::group_by(id) |> dplyr::summarise(bias=mean(Height-value,na.rm=T))
      test <- merge(test,bias_removal,all.x = T)
    }} else {
    
    test$bias <- 0
    
  }
  
  test.sp <- split(test,list(test$id,test$type))
  test.sp <- test.sp[sapply(test.sp, nrow)>0]
  test2 <- do.call(dplyr::bind_rows,lapply(test.sp,function(x){
    
    if (x$type[1] == "Stream") {
      if (s104_or_s111=="s104") {return(NULL)}
      
      y <- accuracy_funs_direction(x$direction,x$Bearing)
      y$id <- x$id[1]
      y$S100 <- "S111"
      y$s111 <- "Direction"
      message(x$id[1]," ",x$type[1])
      
      y1 <- accuracy_funs(x$value,x$Speed*1.94)
      y1$id <- x$id[1]
      y1$S100 <- "S111"
      y1$s111 <- "Speed"
      
      y <- dplyr::bind_rows(y,y1)
      return(y)
    } else {
      y <- accuracy_funs(x$value+x$bias,x$Height)
      y$id <- x$id[1]
      y$S100 <- "S104"
      message(x$id[1]," ",x$type[1])
      return(y)
    }
    
  }))
  
  unique_locations <- test[!duplicated(test$id),]
  eur <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large")
  
  if (isTRUE(figures)) {
    
    if (s104_or_s111 == "s111") {
      
      test_stream_speed <- subset(test2,test2$S100=="S111" & test2$s111=="Speed")
      if (nrow(test_stream_speed)==0) {return(list(data=t2,results=test2))}
      test_stream_direction <- subset(test2,test2$S100=="S111" & test2$s111=="Direction")
      
      # if anything fails, for the figure flag it as a 'fail', evem though it isn't really
      
      s111_COR <- ifelse(min(test_stream_speed$cor)< s111_speed_corr_threshold ,"FAIL","PASS")
      s111_MAE <- ifelse(max(test_stream_speed$mae)> s111_speed_mae_threshold,"FAIL","PASS")
      s111_SD <- ifelse(max(test_stream_direction$sd)> s111_dir_sd_threshold ,"FAIL","PASS")
      s111_MODE <- ifelse(max(abs(test_stream_direction$modex1 - test_stream_direction$modey1)) > s111_dir_mode_threshold | max(abs(test_stream_direction$modex2 - test_stream_direction$modey2)) > s111_dir_mode_threshold,"FAIL","PASS")
      subtitle_s111_colour <- ifelse (s111_MAE=="PASS" & s111_COR=="PASS","Black","Red")
      subtitle_s111_colour_direction <- ifelse (s111_SD=="PASS" & s111_MODE=="PASS","Black","Red")
      t2 <- subset(test,test$type=="Stream")
      unique_locations_stream <- subset(unique_locations,unique_locations$type=="Stream")
      unique_locations_stream <- merge(unique_locations_stream,test_stream_direction[,c("id","sd")])
      unique_locations_stream <- merge(unique_locations_stream,test_stream_speed[,c("id","mae")])
      unique_locations_stream$label = paste(unique_locations_stream$id," ",round(unique_locations_stream$sd,1),sep="")
      unique_locations_stream$label_colour <- ifelse(unique_locations_stream$sd > s111_dir_sd_threshold ,"Red","Black")
      unique_locations_stream$label2 = paste(unique_locations_stream$id," ",round(unique_locations_stream$mae,2),sep="")
      unique_locations_stream$label_colour2 <- ifelse(unique_locations_stream$mae > s111_speed_mae_threshold,"Red","Black")
      
      # for reporting
      number_rows_speed <- nrow(test_stream_speed)
      number_passes_speed_COR <- sum(ifelse(test_stream_speed$cor< s111_speed_corr_threshold ,0,1))
      percent_passes_speed_COR <- round((number_passes_speed_COR / number_rows_speed) * 100,1)
      number_passes_speed_MAE <- sum(ifelse(test_stream_speed$mae< s111_speed_mae_threshold ,1,0))
      percent_passes_speed_MAE <- round((number_passes_speed_MAE / number_rows_speed) * 100,1)
      number_passes_direction_SD <- sum(ifelse(test_stream_direction$sd< s111_dir_sd_threshold ,1,0))
      percent_passes_direction_SD <- round((number_passes_direction_SD / number_rows_speed) * 100,1)
      number_passes_direction_MODE <- sum(ifelse(abs(test_stream_direction$modex1 - test_stream_direction$modey1) > s111_dir_mode_threshold | abs(test_stream_direction$modex2 - test_stream_direction$modey2) > s111_dir_mode_threshold,0,1))
      percent_passes_direction_MODE <- round((number_passes_direction_MODE / number_rows_speed) * 100,1)
      
      fname <- paste(output_directory,"/",a$main_attributes$geographicIdentifier,"_",s104_or_s111,sep="",".pdf")
      
      if (file.exists(fname)) {
        pdf(paste(output_directory,"/",a$main_attributes$geographicIdentifier,"_",s104_or_s111,"_lon_",round(a$metadata$west_lim,1),"_",round(a$metadata$east_lim,1),"_lat_",round(a$metadata$south_lim,1),"_",round(a$metadata$north_lim,1),"_",lubridate::as_date(a$metadata$start_date),"_",round(runif(1,0,10000),0),sep="",".pdf"),width=11.7,height=8.3)
        
      } else {
        pdf(paste(output_directory,"/",a$main_attributes$geographicIdentifier,"_",s104_or_s111,"_lon_",round(a$metadata$west_lim,1),"_",round(a$metadata$east_lim,1),"_lat_",round(a$metadata$south_lim,1),"_",round(a$metadata$north_lim,1),"_",lubridate::as_date(a$metadata$start_date),sep="",".pdf"),width=11.7,height=8.3)
      }
        
      plot.new()
      mtext(sprintf("%s - %s",a$main_attributes$geographicIdentifier,toupper(a$test_results$S100)),line=1,at=0,font=2,adj=0)
      mtext(sprintf("North: %s | South: %s | West: %s | East: %s ",
                    round(a$metadata$north_lim,4),
                    round(a$metadata$south_lim,4),
                    round(a$metadata$west_lim,4),
                    round(a$metadata$east_lim,4)),line=-1,at=0.05,adj=0)
      mtext(sprintf("%s to %s",a$metadata$start_date,a$metadata$end_date),line=-2,at=0.05,adj=0 )
      mtext(sprintf("Time Interval is %s minutes with %s time steps",a$metadata$interval/60,a$metadata$timesteps),line=-3,at=0.05,adj=0)
      mtext("Vertical Datum: NA",at=0.05,line=-4,adj=0)
  
      mtext(("File Tests: "),at=0,line=-6,adj=0,font=2)
      mtext(("Spatial Limits Match: FALSE"),at=0.05,line=-8,adj=0)
      mtext(("Timestep Match: TRUE"),at=0.05,line=-9,adj=0)
      mtext(("Data Minimimum Value: TRUE"),at=0.05,line=-10,adj=0)
      mtext(("Data Maximum Value: TRUE"),at=0.05,line=-11,adj=0)
      
      mtext(("Data Tests: "),at=0,line=-13,adj=0,font=2)
        mtext(paste("Comparison with TotalTide Direction MODE:",s111_MODE),at=0.05,line=-15,adj=0,col=ifelse(s111_MODE=="PASS","Black","Red"))
        mtext(paste(number_passes_direction_MODE," out of ",number_rows_speed," (",percent_passes_direction_MODE,"%) stations passed",sep=""),at=0.1,line=-16,adj=0)
        mtext(paste("Comparison with TotalTide Direction Standard Deviation:",s111_SD),at=0.05,line=-17,adj=0,col=ifelse(s111_SD=="PASS","Black","Red"))
        mtext(paste(number_passes_direction_SD," out of ",number_rows_speed," (",percent_passes_direction_SD,"%) stations passed",sep=""),at=0.1,line=-18,adj=0)
        
        mtext(paste("Comparison with TotalTide Speed Mean Absolute Error:",s111_MAE),at=0.05,line=-19,adj=0,col=ifelse(s111_MAE=="PASS","Black","Red"))
        mtext(paste(number_passes_speed_MAE," out of ",number_rows_speed," (",percent_passes_speed_MAE,"%) stations passed",sep=""),at=0.1,line=-20,adj=0)
        mtext(paste("Comparison with TotalTide Speed Correlation:",s111_COR),at=0.05,line=-21,adj=0,col=ifelse(s111_COR=="PASS","Black","Red"))
        mtext(paste(number_passes_speed_COR," out of ",number_rows_speed," (",percent_passes_speed_COR,"%) stations passed",sep=""),at=0.1,line=-22,adj=0)

      print(ggplot(unique_locations_stream,aes(x=lon,y=lat)) + geom_point(col=unique_locations_stream$label_colour) + 
              geom_sf(inherit.aes = F,data=eur,fill="BurlyWood1",col="#333333",alpha=0.2) + 
              xlim(c(min(unique_locations_stream$lon)-0.1,max(unique_locations_stream$lon)+0.1)) + 
              ylim(c(min(unique_locations_stream$lat)-0.1,max(unique_locations_stream$lat)+0.1)) + 
              labs(x="Longitude",y="Latitude",title=sprintf("%s Standard Dev (degrees)",a$main_attributes$geographicIdentifier)) + 
              ggrepel::geom_label_repel(data=unique_locations_stream,inherit.aes = F,aes(label=label,x=lon,y=lat,box.padding= 0.1,point.padding = 0.1,show.legend = F,label_colour=label_colour),colour=unique_locations_stream$label_colour,max.overlaps=20) + theme_Publication() + theme(legend.position="none",legend.direction = "vertical"))
      
      print(ggplot(unique_locations_stream,aes(x=lon,y=lat)) + geom_point(col=unique_locations_stream$label_colour2) + 
              geom_sf(inherit.aes = F,data=eur,fill="BurlyWood1",col="#333333",alpha=0.2) + 
              xlim(c(min(unique_locations_stream$lon)-0.1,max(unique_locations_stream$lon)+0.1)) + 
              ylim(c(min(unique_locations_stream$lat)-0.1,max(unique_locations_stream$lat)+0.1)) + 
              labs(x="Longitude",y="Latitude",title=sprintf("%s Mean Absolute Error (kts)",a$main_attributes$geographicIdentifier)) + 
              ggrepel::geom_label_repel(data=unique_locations_stream,inherit.aes = F,aes(label=label2,x=lon,y=lat,box.padding= 0.1,point.padding = 0.1,show.legend = F,label_colour=label_colour2),colour=unique_locations_stream$label_colour2,max.overlaps=20) + theme_Publication() + theme(legend.position="none",legend.direction = "vertical"))
      
      
      print(ggplot(t2,aes(x=date,y=Speed*1.94)) + geom_line(col="SteelBlue") + geom_line(aes(x=date,y=value)) + facet_wrap(~id,scale="free_y") + theme_Publication() + labs(x="Date",y="Speed Comparison (kts)",title=sprintf("%s %s",s104_or_s111,a$main_attributes$geographicIdentifier),subtitle=sprintf("Blue = TotalTide | Black = S-111\nSD Test: %s | COR Test: %s",s111_MAE,s111_COR)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.subtitle=element_text(color=subtitle_s111_colour)))
      print(ggplot(t2,aes(x=date,y=Bearing)) + geom_line(col="SteelBlue") + geom_line(aes(x=date,y=direction)) + facet_wrap(~id) + theme_Publication() + labs(x="Date",y="Direction Comparison (degrees)",title=sprintf("%s %s",s104_or_s111,a$main_attributes$geographicIdentifier),subtitle=sprintf("Blue = TotalTide | Black = S-111\nSD Test: %s | COR Test: %s",s111_MAE,s111_COR)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.subtitle=element_text(color=subtitle_s111_colour_direction)))
      dev.off()
      
      return(list(data=t2,results=test2))
      
    } else {
      test_height <- subset(test2,test2$S100=="S104")
      t1 <- subset(test,test$type=="Height")
      s104_MAE <- ifelse(max(test_height$mae)> s104_mae_threshold,"FAIL","PASS")
      s104_COR <- ifelse(min(test_height$cor)< s104_corr_threshold,"FAIL","PASS")
      subtitle_s104_colour <- ifelse (s104_COR=="PASS" & s104_MAE=="PASS","Black","Red")
      unique_locations_height <- subset(unique_locations,unique_locations$type=="Height")
      unique_locations_height <- merge(unique_locations_height,test_height[,c("id","mae")])
      unique_locations_height$label = paste(unique_locations_height$id," ",round(unique_locations_height$mae,3),sep="")
      unique_locations_height$label_colour <- ifelse(unique_locations_height$mae > s104_mae_threshold ,"Red","Black")

      # for reporting
      number_rows_height <- nrow(test_height)
      number_passes_height_MAE <- sum(ifelse(test_height$mae< s104_mae_threshold ,1,0))
      percent_passes_height_MAE <- round((number_passes_height_MAE / number_rows_height) * 100,1)
      number_passes_height_COR <- sum(ifelse(test_height$cor< s104_corr_threshold,0,1))
      percent_passes_height_COR <- round((number_passes_height_COR / number_rows_height) * 100,1)
      
      fname <- paste(output_directory,"/",a$main_attributes$geographicIdentifier,"_",s104_or_s111,sep="",".pdf")
      
      if (file.exists(fname)) {
        pdf(paste(output_directory,"/",a$main_attributes$geographicIdentifier,"_",s104_or_s111,"_lon_",round(a$metadata$west_lim,1),"_",round(a$metadata$east_lim,1),"_lat_",round(a$metadata$south_lim,1),"_",round(a$metadata$north_lim,1),"_",lubridate::as_date(a$metadata$start_date),"_",round(runif(1,0,10000),0),sep="",".pdf"),width=11.7,height=8.3)

      } else {
        pdf(paste(output_directory,"/",a$main_attributes$geographicIdentifier,"_",s104_or_s111,"_lon_",round(a$metadata$west_lim,1),"_",round(a$metadata$east_lim,1),"_lat_",round(a$metadata$south_lim,1),"_",round(a$metadata$north_lim,1),"_",lubridate::as_date(a$metadata$start_date),sep="",".pdf"),width=11.7,height=8.3)
      }
        
      
      plot.new()
      mtext(sprintf("%s - %s",a$main_attributes$geographicIdentifier,toupper(a$test_results$S100)),line=1,at=0,font=2,adj=0)
      mtext(sprintf("North: %s | South: %s | West: %s | East: %s ",
                    round(a$metadata$north_lim,4),
                    round(a$metadata$south_lim,4),
                    round(a$metadata$west_lim,4),
                    round(a$metadata$east_lim,4)),line=-1,at=0.05,adj=0)
      mtext(sprintf("%s to %s",a$metadata$start_date,a$metadata$end_date),line=-2,at=0.05,adj=0 )
      mtext(sprintf("Time Interval is %s minutes with %s time steps",a$metadata$interval/60,a$metadata$timesteps),line=-3,at=0.05,adj=0)
      mtext(sprintf("Vertical Datum: %s (%s)",a$metadata$verticalDatum,"read in from metadata table"),at=0.05,line=-4,adj=0)
      
      mtext(("File Tests: "),at=0,line=-6,adj=0,font=2)
      mtext(("Spatial Limits Match: FALSE"),at=0.05,line=-8,adj=0)
      mtext(("Timestep Match: TRUE"),at=0.05,line=-9,adj=0)
      mtext(("Data Minimimum Value: TRUE"),at=0.05,line=-10,adj=0)
      mtext(("Data Maximum Value: TRUE"),at=0.05,line=-11,adj=0)
      
      mtext(("Data Tests: "),at=0,line=-13,adj=0,font=2)
        mtext(paste("Comparison with TotalTide Height Mean Absolute Error:",s104_MAE),at=0.05,line=-15,adj=0,col=ifelse(s104_MAE=="PASS","Black","Red"))
        mtext(paste(number_passes_height_MAE," out of ",number_rows_height," (",percent_passes_height_MAE,"%) stations passed",sep=""),at=0.1,line=-16,adj=0)
        mtext(paste("Comparison with TotalTide Height Correlation:",s104_COR),at=0.05,line=-17,adj=0,col=ifelse(s104_COR=="PASS","Black","Red"))
        mtext(paste(number_passes_height_COR," out of ",number_rows_height," (",percent_passes_height_COR,"%) stations passed",sep=""),at=0.1,line=-18,adj=0)
        
        if (!("obs_lon" %in% colnames(unique_locations_height))) {
          
          print(ggplot(unique_locations_height,aes(x=lon,y=lat)) + geom_point(col=unique_locations_height$label_colour) + 
                  geom_sf(inherit.aes = F,data=eur,fill="BurlyWood1",col="#333333",alpha=0.2) + 
                  xlim(c(min(unique_locations_height$lon)-0.1,max(unique_locations_height$lon)+0.1)) + 
                  ylim(c(min(unique_locations_height$lat)-0.1,max(unique_locations_height$lat)+0.1)) + 
                  labs(x="Longitude",y="Latitude",title=sprintf("%s Mean Absolute Error (m)",a$main_attributes$geographicIdentifier)) + 
                  ggrepel::geom_label_repel(data=unique_locations_height,inherit.aes = F,aes(label=label,x=lon,y=lat,box.padding= 0.1,point.padding = 0.1,show.legend = F,label_colour=label_colour),colour=unique_locations_height$label_colour,max.overlaps=20) + 
                  theme_Publication() + 
                  theme(legend.position="none",legend.direction = "vertical")) 
          
          print(ggplot(t1,aes(x=date,y=Height)) + geom_line(col="SteelBlue") + geom_line(aes(x=date,y=value+bias))  + facet_wrap(~id,scale="free_y") + theme_Publication() + labs(x="Date",y="Height Comparison (m)",title=sprintf("%s %s",s104_or_s111,a$main_attributes$geographicIdentifier),subtitle=sprintf("Blue = TotalTide | Black = S-104\nMAE Test: %s | COR Test: %s",s104_MAE,s104_COR)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.subtitle=element_text(color=subtitle_s104_colour))) 
          
          print(ggplot(t1,aes(x=date,y=(value+bias)-Height)) + geom_line(col="SteelBlue")  + geom_hline(yintercept = c(-0.2,0.2),lty=3,col="Red") + facet_wrap(~id) + theme_Publication() + labs(x="Date",y="Height Difference (m)",title=sprintf("%s %s",s104_or_s111,a$main_attributes$geographicIdentifier),subtitle=sprintf("Blue = TotalTide | Black = S-104\nMAE Test: %s | COR Test: %s",s104_MAE,s104_COR)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.subtitle=element_text(color=subtitle_s104_colour))) 
          
          
        } else {
          
          print(ggplot(unique_locations_height,aes(x=lon,y=lat)) + geom_point(col=unique_locations_height$label_colour) + 
                  geom_sf(inherit.aes = F,data=eur,fill="BurlyWood1",col="#333333",alpha=0.2) + 
                  xlim(c(min(unique_locations_height$lon)-0.1,max(unique_locations_height$lon)+0.1)) + 
                  ylim(c(min(unique_locations_height$lat)-0.1,max(unique_locations_height$lat)+0.1)) + 
                  labs(x="Longitude",y="Latitude",title=sprintf("%s Mean Absolute Error (m)",a$main_attributes$geographicIdentifier)) + 
                  ggrepel::geom_label_repel(data=unique_locations_height,inherit.aes = F,aes(label=label,x=lon,y=lat,box.padding= 0.1,point.padding = 0.1,show.legend = F,label_colour=label_colour),colour=unique_locations_height$label_colour,max.overlaps=20) + 
                  geom_point(data=unique_locations_height,inherit.aes = FALSE,aes(x=obs_lon,y=obs_lat),col="Green") + 
                  ggrepel::geom_label_repel(data=unique_locations_height,inherit.aes = FALSE,aes(x=obs_lon,y=obs_lat,label=station),col="Green") + 
                  theme_Publication() + 
                  theme(legend.position="none",legend.direction = "vertical")) 
          
          print(ggplot(t1,aes(x=date,y=Height)) + geom_line(col="SteelBlue") + geom_line(aes(x=date,y=value+bias)) + geom_line(aes(x=date,y=height),col="Green") + facet_wrap(~id,scale="free_y") + theme_Publication() + labs(x="Date",y="Height Comparison (m)",title=sprintf("%s %s",s104_or_s111,a$main_attributes$geographicIdentifier),subtitle=sprintf("Blue = TotalTide | Black = S-104\nMAE Test: %s | COR Test: %s",s104_MAE,s104_COR)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.subtitle=element_text(color=subtitle_s104_colour))) 
          
          print(ggplot(t1,aes(x=date,y=(value+bias)-Height)) + geom_line(col="SteelBlue") + geom_line(aes(x=date,y=height-Height),col="Green") + geom_hline(yintercept = c(-0.2,0.2),lty=3,col="Red") + facet_wrap(~id) + theme_Publication() + labs(x="Date",y="Height Difference (m)",title=sprintf("%s %s",s104_or_s111,a$main_attributes$geographicIdentifier),subtitle=sprintf("Blue = TotalTide | Black = S-104\nMAE Test: %s | COR Test: %s",s104_MAE,s104_COR)) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(plot.subtitle=element_text(color=subtitle_s104_colour))) 
          
        }
        
       dev.off()
      
      
      return(list(data=t1,results=test2))
    }
    
  } else {
    return(list(data=test,results=test2))
  }
  
}




## Vertical Transformations required
## PLA/Wallingford - Totaltide CD to ODN from vorf (got as a csv for UK)

# tide_ODN <- read.csv("Z:/individual/thocro/tides.csv")
# tide_ODN$id <- as.numeric(tide_ODN$Port.No.)
# tide_ODN$id <- ifelse(!is.na(tide_ODN$id),sprintf("%04d",tide_ODN$id),NA)
# tide_ODN$id[grep(pattern = "E",tide_ODN$Port.No.)] <- NA
# tide_ODN$id <- ifelse(is.na(tide_ODN$id),tide_ODN$Port.No.,tide_ODN$id)
# tide_ODN <- tide_ODN[,c("id","Chart_Num")]
# colnames(tide_ODN)[2] <- c("CD_to_ODN")
# tide_ODN <- tide_ODN[tide_ODN$id %in% unique(b$id),]
# 
# d <- merge(d,tide_ODN,all.x=T)
# 
# 
# ggplot(test,aes(x=date,y=value+CD_to_ODN)) + geom_line() + geom_line(aes(x=date,y=Height),col="SteelBlue") + facet_wrap(~id)
# 
# s104_or_s111="s104"
# filepath="Z:/individual/thocro/regression_test_data_20240226/S104/SOLENT/2023/11/22/00/dcf2/104GB00_20231122T000000Z_GB3DEVK0_dcf2.h5"
# extract_lons = b[!duplicated(b$id),"lon"]
# extract_lats = b[!duplicated(b$id),"lat"]
# file_lons = a$lons
# file_lats = a$lats
# start_date = a$metadata$start_date
# end_date = a$metadata$end_date
# interval = 60
# id=b[!duplicated(b$id),"id"]
# start_lon <- a$metadata$grid_origin_lon
# start_lat <- a$metadata$grid_origin_lat
# 
# 
# a <- hdf5_limits_and_check(s104_or_s111 = "s111",filepath = "Z:/individual/thocro/regression_test_data_20240226/S111/SOLENT/2023/11/22/00/dcf2/111GB00_20231122T000000Z_GB3DEVK0_dcf2.h5")
# b <- get_relevant_tt()
# d <- extract_series_from_hdf5("s111","Z:/individual/thocro/regression_test_data_20240226/S111/SOLENT/2023/11/22/00/dcf2/111GB00_20231122T000000Z_GB3DEVK0_dcf2.h5",extract_lons = b[!duplicated(b$id),"lon"],extract_lats = b[!duplicated(b$id),"lat"],file_lons = a$lons,file_lats = a$lats,start_date = a$metadata$start_date,end_date = a$metadata$end_date,interval = 60,id=b[!duplicated(b$id),"id"])
# 
# a <- hdf5_limits_and_check(s104_or_s111 = "s104",
#                            filepath = "Z:/individual/thocro/regression_test_data_20240226/S104/SOLENT/2023/11/22/00/dcf2/104GB00_20231122T000000Z_GB3DEVK0_dcf2.h5")
# b <- get_relevant_tt()
# d <- extract_series_from_hdf5("s104","Z:/individual/thocro/regression_test_data_20240226/S104/SOLENT/2023/11/22/00/dcf2/104GB00_20231122T000000Z_GB3DEVK0_dcf2.h5",extract_lons = b[!duplicated(b$id),"lon"],extract_lats = b[!duplicated(b$id),"lat"],file_lons = a$lons,file_lats = a$lats,start_date = a$metadata$start_date,end_date = a$metadata$end_date,interval = 60,id=b[!duplicated(b$id),"id"])
# 
# 




