Sys.setenv(TZ='GMT')

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


#' Extract info and do basic logic checks from HDF5 file metadata
#' 
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
                        filepath="104GB00_20231109T000000Z_GB4DNABL_dcf2.h5") {
  
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

