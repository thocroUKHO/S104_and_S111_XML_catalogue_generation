## Latest attempt to write out a Catalogue XML file for S-111
## 2025-01-30
Sys.setenv(TZ='UTC')
library(XML)

################################################################################
## 1. The following things below - you can change
################################################################################

## limit decimal points
decimals <- 3

## set working directory where you have h5 files
# setwd("/path_to_folder")
list_of_h5_files <- list.files(pattern = ".h5")

## choose a name for the S100XC:identifier section near the top of the file
exchange_set_identifier <- "S-111_ROUTE_FEB25"

## downloaded and unzipped from the UKHO file share service
hd_file <- suppressWarnings(hdf5_limits_and_check(s104_or_s111 = "s111",list_of_h5_files[1]))

## set which version of S-104, this could be read from the file dynamically but 
## for now i prefer to hardcode it here
s111_v1 <- "1.2.0"
s111_v1_1 <- as.numeric(substr(s111_v1,1,1))
s111_v1_2 <- as.numeric(substr(s111_v1,3,3))
s111_v1_3 <- as.numeric(substr(s111_v1,5,5))

## Date Information (required for header at top of catalogue file)
the_date <- hd_file$metadata$start_date
y1 <- lubridate::year(the_date)
m1 <- lubridate::month(the_date)
d1 <- lubridate::day(the_date)
h1 <- lubridate::hour(the_date)
mn1 <- lubridate::minute(the_date)
end_date <- hd_file$metadata$end_date
y1end <- lubridate::year(end_date)
m1end <- lubridate::month(end_date)
d1end <- lubridate::day(end_date)
h1end <- lubridate::hour(end_date)
mn1end <- lubridate::minute(end_date)
h2 <- lubridate::hour(Sys.time()) # extra field for dataset_id

# Define namespaces
namespaces <- c(
  S100XC = "http://www.iho.int/s100/xc/5.2",
  gex = "http://standards.iso.org/iso/19115/-3/gex/1.0",
  S100SE = "http://www.iho.int/s100/se/5.2",
  cit = "http://standards.iso.org/iso/19115/-3/cit/2.0",
  gml = "http://www.opengis.net/gml/3.2",
  gco = "http://standards.iso.org/iso/19115/-3/gco/1.0",
  ns7 = "http://standards.iso.org/iso/19115/-3/lan/1.0",
  ns8 = "http://standards.iso.org/iso/19115/-3/cit/1.0",
  ns9 = "http://standards.iso.org/iso/19115/-3/mcc/1.0",
  ns10 = "http://www.w3.org/1999/xlink",
  ns11 = "http://www.isotc211.org/2005/gco",
  ns12 = "http://www.isotc211.org/2005/gmd",
  ns13 = "http://standards.iso.org/iso/19115/-3/mco/1.0",
  ns14 = "http://standards.iso.org/iso/19115/-3/mri/1.0",
  ns15 = "http://standards.iso.org/iso/19115/-3/mmi/1.0",
  ns16 = "http://www.isotc211.org/2005/gts"
)

# Create root node with namespaces
root <- newXMLNode("S100XC:S100_ExchangeCatalogue", namespaceDefinitions = namespaces)

# Add identifier section
identifier <- newXMLNode("S100XC:identifier", parent = root)
newXMLNode("S100XC:identifier", exchange_set_identifier, parent = identifier)
newXMLNode("S100XC:dateTime", sprintf("%04d-%02d-%02dT%02d:%02d:%02d.%03dZ",y1,m1,d1,h1,mn1,0,0), parent = identifier)

# Add contact section
contact <- newXMLNode("S100XC:contact", parent = root)
organization <- newXMLNode("S100XC:organization", parent = contact)
newXMLNode("gco:CharacterString", "United Kingdom Hydrographic Office", parent = organization)

address <- newXMLNode("S100XC:address", parent = contact)
# newXMLNode("cit:deliveryPoint", "", parent = address)
# newXMLNode("cit:city", "Taunton", parent = address)
# newXMLNode("cit:administrativeArea", "Admiralty Way", parent = address)
# newXMLNode("cit:postalCode", "TA12DN", parent = address)
# newXMLNode("cit:country", "", parent = address)
# newXMLNode("cit:electronicMailAddress", "customerservices@ukho.gov.uk", parent = address)

newXMLNode("cit:deliveryPoint", newXMLNode("gco:CharacterString",""), parent = address)
newXMLNode("cit:city", newXMLNode("gco:CharacterString", "Taunton"), parent = address)
newXMLNode("cit:administrativeArea", newXMLNode("gco:CharacterString", "Admiralty Way"), parent = address)
newXMLNode("cit:postalCode", newXMLNode("gco:CharacterString", "TA12DN"), parent = address)
newXMLNode("cit:country", newXMLNode("gco:CharacterString",""), parent = address)
newXMLNode("cit:electronicMailAddress", newXMLNode("gco:CharacterString", "customerservices@ukho.gov.uk"), parent = address)

# Add exchangeCatalogueComment
comment <- newXMLNode("S100XC:exchangeCatalogueComment", parent = root)
newXMLNode("gco:CharacterString", "s111v1", parent = comment) # hardcoded change if neccessary

# Add datasetDiscoveryMetadata section
metadata <- newXMLNode("S100XC:datasetDiscoveryMetadata", parent = root)

################################################################################
## 2. loop through all hdf5 files in directory and add their information to the 
## catalogue file
################################################################################

for (i in seq(1,length(list_of_h5_files))) {
  
  message("processing ",i," of ",length(list_of_h5_files))
  
  # File Name
  file_name <- list_of_h5_files[i]
  
  # Read in File
  hd_file <- suppressWarnings(hdf5_limits_and_check(s104_or_s111 = "s111",file_name))
  
  # Short Name = "the cell" name
  fname <- hd_file$main_attributes$geographicIdentifier
  
  # positional limits
  west <- round(hd_file$metadata$west_lim,decimals)
  east <- round(hd_file$metadata$east_lim,decimals)
  south <- round(hd_file$metadata$south_lim,decimals)
  north <- round(hd_file$metadata$north_lim,decimals)
  
  # UK naming structure will change but is currently like this
  # 104GB00_YYYY_MMDDTHHMMSSZ_areaname_dcf2.hf
  # 104GB00_20241103T001500Z_GB3DEVV0_dcf2.h5
  # so the dataset id becomes:
  # urn:mrn:iho:s104:1:1:0:GB:GB3DEVV0:type2:20241103T160759+0000
  dataset_id <- sprintf("urn:mrn:iho:s111:%01d:%01d:%01d:GB:%s:type2:%s%02d%02dT%02d%02d%02d+0000",1,1,0,fname,y1,m1,d1,h2,mn1,0)
  
  metadata_item <- newXMLNode("S100XC:S100_DatasetDiscoveryMetadata", parent = metadata)
  newXMLNode("S100XC:fileName", sprintf("file:/S-111/DATASET_FILES/%s",file_name), parent = metadata_item)
  newXMLNode("S100XC:datasetID", dataset_id, parent = metadata_item)
  newXMLNode("S100XC:compressionFlag", "false", parent = metadata_item)
  newXMLNode("S100XC:dataProtection", "false", parent = metadata_item)
  
  ## Ignoring Digital Signature Bit for Now but it would go here
  
  newXMLNode("S100XC:copyright", "false", parent = metadata_item)
  newXMLNode("S100XC:purpose", "newDataset", parent = metadata_item)
  newXMLNode("S100XC:notForNavigation", "false", parent = metadata_item)
  newXMLNode("S100XC:editionNumber", "1", parent = metadata_item)
  newXMLNode("S100XC:issueDate", sprintf("%04d-%02d-%02d",y1,m1,d1), parent = metadata_item)
  newXMLNode("S100XC:issueTime", sprintf("%02d:%02d:%02dZ",h1,mn1,0), parent = metadata_item)
  
  # Add boundingBox
  boundingBox <- newXMLNode("S100XC:boundingBox", parent = metadata_item)
  newXMLNode("gex:westBoundLongitude", newXMLNode("gco:Decimal", west), parent = boundingBox)
  newXMLNode("gex:eastBoundLongitude", newXMLNode("gco:Decimal", east), parent = boundingBox)
  newXMLNode("gex:southBoundLatitude", newXMLNode("gco:Decimal", south), parent = boundingBox)
  newXMLNode("gex:northBoundLatitude", newXMLNode("gco:Decimal", north), parent = boundingBox)
  
  # Add temporalExtent
  temporalExtent <- newXMLNode("S100XC:temporalExtent", parent = metadata_item)
  newXMLNode("S100XC:timeInstantBegin", sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",y1,m1,d1,h1,mn1,0), parent = temporalExtent)
  newXMLNode("S100XC:timeInstantEnd", sprintf("%04d-%02d-%02dT%02d:%02d:%02dZ",y1end,m1end,d1end,h1end,mn1end,0), parent = temporalExtent)
  
  # Add productSpecification
  productSpec <- newXMLNode("S100XC:productSpecification", parent = metadata_item)
  newXMLNode("S100XC:name", "Surface Currents Product Specification", parent = productSpec)
  newXMLNode("S100XC:version", sprintf("%01d.%01d.%01d",s111_v1_1,s111_v1_2,s111_v1_3), parent = productSpec)
  newXMLNode("S100XC:productIdentifier", "S-111", parent = productSpec)
  newXMLNode("S100XC:number", "1", parent = productSpec)
  newXMLNode("S100XC:compliancyCategory", "category3", parent = productSpec)
  
  # Add producingAgency
  producingAgency <- newXMLNode("S100XC:producingAgency", parent = metadata_item, attrs = c("gco:isoType" = "gmd:CI_ResponsibleParty"))
  responsibility <- newXMLNode("cit:CI_Responsibility", parent = producingAgency)
  role <- newXMLNode("cit:role", parent = responsibility)
  newXMLNode("cit:CI_RoleCode", "owner", 
             attrs = c(codeList = "http://standards.iso.org/iso/19115/resources/CodeLists/cat/codelists.xml#CI_RoleCode", 
                       codeListValue = "owner"), 
             parent = role)
  party <- newXMLNode("cit:party", parent = responsibility)
  organization <- newXMLNode("cit:CI_Organisation", parent = party)
  newXMLNode("cit:name", newXMLNode("gco:CharacterString", "UKHO"), parent = organization)
  
  ## Hardcoded to GB00
  # newXMLNode("S100XC:producerCode", "GB00", parent = metadata_item)
  ## Dynamic but some countries might not work so please edit
  newXMLNode("S100XC:producerCode", substr(file_name,4,7), parent = metadata_item)
  
  newXMLNode("S100XC:encodingFormat", "HDF5", parent = metadata_item)
  
  # approximate grid density
  lon_distance <- geosphere::distHaversine(c(hd_file$lons[1],hd_file$lats[1]),c(hd_file$lons[2],hd_file$lats[1]))
  lat_distance <- geosphere::distHaversine(c(hd_file$lons[1],hd_file$lats[1]),c(hd_file$lons[1],hd_file$lats[2]))
  approx_resolution <- round((lon_distance + lat_distance)/2,0)
  rm(lon_distance,lat_distance)
  
  # Add dataCoverage
  dataCoverage <- newXMLNode("S100XC:dataCoverage", parent = metadata_item)
  boundingPolygon <- newXMLNode("S100XC:boundingPolygon", parent = dataCoverage)
  polygon <- newXMLNode("gex:polygon", parent = boundingPolygon)
  polygonElement <- newXMLNode("gml:Polygon", parent = polygon)
  exterior <- newXMLNode("gml:exterior", parent = polygonElement)
  linearRing <- newXMLNode("gml:LinearRing", parent = exterior, attrs = c(srsName = "urn:ogc:def:crs:EPSG:4326", srsDimension = "2"))
  newXMLNode("gml:posList", sprintf("%s %s %s %s %s %s %s %s %s %s",south,west,north,west,north,east,south,east,south,west), parent = linearRing)
  newXMLNode("S100XC:approximateGridResolution", as.character(approx_resolution), parent = dataCoverage)
  
  # Add metadataPointOfContact
  metadataPOC <- newXMLNode("S100XC:metadataPointOfContact", parent = metadata_item)
  responsibilityPOC <- newXMLNode("cit:CI_Responsibility", parent = metadataPOC)
  rolePOC <- newXMLNode("cit:role", parent = responsibilityPOC)
  newXMLNode("cit:CI_RoleCode", "publisher", 
             attrs = c(codeList = "https://schemas.isotc211.org/19115/resources/Codelist/cat/codelists.xml#CI_RoleCode", 
                       codeListValue = "Publisher"), 
             parent = rolePOC)
  partyPOC <- newXMLNode("cit:party", parent = responsibilityPOC)
  organizationPOC <- newXMLNode("cit:CI_Organisation", parent = partyPOC)
  newXMLNode("cit:name", newXMLNode("gco:CharacterString", "UKHO"), parent = organizationPOC)
  
  # Add metadataDateStamp
  newXMLNode("S100XC:metadataDateStamp", sprintf("%04d-%02d-%02d",y1,m1,d1), parent = metadata_item)
  
  # Add comment
  commentNode <- newXMLNode("S100XC:comment", parent = metadata_item)
  newXMLNode("gco:CharacterString", "A conformant dataset may contain features associated with water levels. The specific content is defined by the Feature Catalogue and the Application Schema.", parent = commentNode)
  
  rhdf5::h5closeAll()
}

# Save XML to a file
saveXML(root, file = "CATALOG.xml",
        prefix = "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>\n",
        indent = TRUE)



