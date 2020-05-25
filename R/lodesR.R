install.packages("devtools")

### LODES Functions


# 1. Workplace Area Characteristics (WAC) functions -----------------------

## Function 1. Grabbing and cleaning WAC data for one state, one year e.g. get_wac_data("al", "2012")
get_wac_data <- function(state_name, year) {

  ## Stage One - Extracting Data from LODES
  dl_file <- paste0(state_name, "_wac_S000_JT00_", year, ".csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/wac/", dl_file), dl_file)
  R.utils::gunzip(dl_file)
  temp <- data.table::fread(paste0(state_name, "_wac_S000_JT00_", year, ".csv"))
  rm(dl_file)
  file.remove(paste0(state_name, "_wac_S000_JT00_", year, ".csv"))

  ## Stage Two - Processing Data from LODES
  colnames(temp) <- c("Census_Block_Code", "Total_Jobs", "Jobs_Under29", "Jobs_30to54", "Jobs_Over55",
                      "Low_Income_Jobs", "Middle_Income_Jobs","High_Income_Jobs", "Agr_For_Fish_Hunt",
                      "Mine_Quar_Oil_Gas", "Utilities", "Construction", "Manufacturing", "Whole_Trade",
                      "Retail_Trade", "Transport_Warehouse", "Information", "Finance_Insurance", "Real_Estate",
                      "Prof_Scie_Tech", "Management", "Waste_Admin", "Education", "Health_Social_Care", "Arts_Recr_Enter",
                      "Accom_Food", "Other_Services", "Public_Admin","Jobs_White", "Jobs_Black_AfroAmer", "Jobs_Amer_Indi_Native",
                      "Jobs_Asian", "Jobs_Hawaii", "Jobs_2_Race", "Jobs_Not_Hispanic","Jobs_Hispanic", "Jobs_Low_Education",
                      "Jobs_School_Ed", "Jobs_College", "Jobs_Degree", "Male_Jobs", "Female_Jobs", "Firm_Age_0_1","Firm_Age_2_3",
                      "Firm_Age_4_5", "Firm_Age_6_10", "Firm_Age_11_Plus", "Firm_Size_0_19", "Firm_Size_20-49", "Firm_Size_50_249",
                      "Firm_Size_250_499", "Firm_Size_500_Plus", "Data_Date")
  temp[, ] <- lapply(temp[, ], as.character)
  temp$Census_Block_Code <- stringr::str_pad(temp$Census_Block_Code, width = 15, side = 'left', pad = 0) # add leading zero
  temp <- transform(temp, StateID = substr(Census_Block_Code, 1, 2), CountyID = substr(Census_Block_Code, 3, 5),
                    TractID = substr(Census_Block_Code, 6, 11), BlockGroupID = substr(Census_Block_Code, 12, 12),
                    CensusBlockID = substr(Census_Block_Code, 13, 15), Census_Block_Code = Census_Block_Code)
  temp <- temp[, c(1, 54:58, 2:53)]

  ## Stage Three - Joining the lookup
  dl_file_lookup <- paste0(state_name, "_xwalk.csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/", dl_file_lookup), dl_file_lookup)
  R.utils::gunzip(dl_file_lookup)
  temp_lookup <- data.table::fread(paste0(state_name, "_xwalk.csv"))
  file.remove(paste0(state_name, "_xwalk.csv"))


  ## Stage Four - Processing the lookup
  temp_lookup <- temp_lookup[, c("tabblk2010", "st", "stname", "cty", "ctyname", "trct", "trctname", "bgrp","bgrpname")]
  temp_lookup[, ] <- lapply(temp_lookup[, ], as.character)
  temp_lookup$tabblk2010 <- stringr::str_pad(temp_lookup$tabblk2010, width = 15, side = 'left', pad = 0)
  temp_lookup <- transform(temp_lookup, StateID = substr(tabblk2010, 1, 2), CountyID = substr(tabblk2010, 3, 5),
                           TractID = substr(tabblk2010, 6, 11), BlockGroupID = substr(tabblk2010, 12, 12),
                           CensusBlockID = substr(tabblk2010, 13, 15))
  temp_lookup <- temp_lookup[, c("tabblk2010", "StateID", "stname", "CountyID", "ctyname",
                                 "TractID", "trctname", "BlockGroupID", "bgrpname")]
  colnames(temp_lookup) <- c("Census_Block_Code", "StateID", "StateName", "CountyID", "CountyName",
                             "TractID", "TractName", "BlockGroupID", "BlockGroupName")

  ## Stage Five - Joining the lookup to the main WAC dataset
  temp_merge <- merge(temp, temp_lookup, by = "Census_Block_Code", all.x = TRUE)
  temp_merge <- temp_merge[, c(1:2, 60, 3, 62, 4, 64, 5, 66, 6:58)]
  colnames(temp_merge)[2:9] <- c("StateID", "StateName", "CountyID", "CountyName", "TractID", "TractName",
                                 "BlockGroupID", "BlockGroupName")
  temp_merge$StateAbb <- state_name
  temp_merge <- temp_merge[, c(1:3, 63, 4:62)]

  ## Stage Six - Column classes and final tidying
  cols <- temp_merge[, 12:63]
  cols <- dplyr::mutate_all(cols, as.numeric)
  geog <- temp_merge[, 1:11]
  db <- cbind(geog, cols)

  return(db)
}

###################################################################################################################

###################################################################################################################

## Function  1a. Function that allows cleaned WAC data to be subsetted to focus on a specific job sector
get_jobsector_wac <- function(df, job_code, job_proportion = T) {

  ### Stage One - Subsetting to include only job sector of interest
  df_sub <- df[, c("Census_Block_Code", "StateID", "StateName", "StateAbb", "CountyID", "CountyName","TractID", "TractName", "BlockGroupID",
                   "BlockGroupName", "CensusBlockID","Total_Jobs")]
  df_col <- dplyr::select(df, job_code)
  df_full <- cbind(df_sub, df_col)
  df_full[, 12] <- sapply(df_full[, 12], as.numeric)

  ### Stage Two - Calculating job sector proportion
  if(job_proportion == T) {
    colnames(df_full)[13] <- "Col_of_Interest"
    df_full[, 13] <- sapply(df_full[, 13], as.numeric)
    df_full$Job_Code_Proportion <- (df_full$Col_of_Interest/df_full$Total_Jobs)*100
    colnames(df_full)[13] <- paste0(job_code)
    colnames(df_full)[14] <- paste0(job_code, "_Proportion")}
  else {
    df_full <- df_full
  }

  return(df_full)
}

######################################################################################################################

######################################################################################################################

## Function 1b. Function that converts a cleaned WAC dataset into a simple feature (polygons) for easy plotting/spatial manip.
get_wac_spatial <- function(df) {

  ## Stage One - Setting up identifier for TIGRIS
  options(tigris_class = "sf")
  state_of_interest <- unique(df$StateAbb)
  state_of_interest <- stringr::str_to_upper(state_of_interest)

  ## Stage Two - Extracting census block sf/fixing census geographies
  options(tigris_class = "sf")
  blocks <- tigris::blocks(state = state_of_interest)
  blocks <- blocks[, c("GEOID10","STATEFP10", "COUNTYFP10", "TRACTCE10", "BLOCKCE10")]
  colnames(blocks) <- c("Census_Block_Code","StateID", "CountyID", "TractID", "BlockID")
  blocks <- transform(blocks, BlockGroupID = substr(BlockID, 1, 1), CensusBlockID = substr(BlockID, 2, 4))

  ## Stage Three - Joining WAC data to sf
  shp <- merge(blocks, df, by = "Census_Block_Code", all.x = FALSE)
  shp <- subset(shp, select = -c(BlockID, StateID.y, CountyID.y, TractID.y, BlockGroupID.y, CensusBlockID.y))
  colnames(shp)[2:7] <- c("StateID", "CountyID", "TractID", "geometry","BlockGroupID", "CensusBlockID")
  shp <- sf::st_as_sf(shp)
  return(shp)

}
#######################################################################################################################

#######################################################################################################################


# 2. Residence Area Characteristics (RAC) Functions -----------------------

## Function 2. Grabbing and cleaning RAC data for one state, one year - e.g. get_rac_data("de", "2012")
get_rac_data <- function(state_name, year) {

  ## Stage One - Extracting Data from LODES
  dl_file <- paste0(state_name, "_rac_S000_JT00_", year, ".csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/rac/", dl_file), dl_file)
  R.utils::gunzip(dl_file)
  temp <- data.table::fread(paste0(state_name, "_rac_S000_JT00_", year, ".csv"))
  rm(dl_file)
  file.remove(paste0(state_name, "_rac_S000_JT00_", year, ".csv"))

  ## Stage Two - Processing Data from LODES
  temp <- subset(temp, select=-c(CS01, CS02))
  colnames(temp) <- c("Census_Block_Code", "Total_Jobs", "Jobs_Under29", "Jobs_30to54", "Jobs_Over55",
                      "Low_Income_Jobs", "Middle_Income_Jobs","High_Income_Jobs", "Agr_For_Fish_Hunt",
                      "Mine_Quar_Oil_Gas", "Utilities", "Construction", "Manufacturing", "Whole_Trade",
                      "Retail_Trade", "Transport_Warehouse", "Information", "Finance_Insurance", "Real_Estate",
                      "Prof_Scie_Tech", "Management", "Waste_Admin", "Education", "Health_Social_Care", "Arts_Recr_Enter",
                      "Accom_Food", "Other_Services", "Public_Admin","Jobs_White", "Jobs_Black_AfroAmer", "Jobs_Amer_Indi_Native",
                      "Jobs_Asian", "Jobs_Hawaii", "Jobs_2_Race", "Jobs_Not_Hispanic","Jobs_Hispanic", "Jobs_Low_Education",
                      "Jobs_School_Ed", "Jobs_College", "Jobs_Degree", "Data_Date")
  temp[, ] <- lapply(temp[, ], as.character)
  temp$Census_Block_Code <- stringr::str_pad(temp$Census_Block_Code, width = 15, side = 'left', pad = 0) # add leading zero
  temp <- transform(temp, StateID = substr(Census_Block_Code, 1, 2), CountyID = substr(Census_Block_Code, 3, 5),
                    TractID = substr(Census_Block_Code, 6, 11), BlockGroupID = substr(Census_Block_Code, 12, 12),
                    CensusBlockID = substr(Census_Block_Code, 13, 15), Census_Block_Code = Census_Block_Code)
  temp <- temp[, c(1, 42:46, 2:41)]

  ## Stage Three - Joining the lookup
  dl_file_lookup <- paste0(state_name, "_xwalk.csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/", dl_file_lookup), dl_file_lookup)
  R.utils::gunzip(dl_file_lookup)
  temp_lookup <- data.table::fread(paste0(state_name, "_xwalk.csv"))
  file.remove(paste0(state_name, "_xwalk.csv"))

  ## Stage Four - Processing the lookup
  temp_lookup <- temp_lookup[, c("tabblk2010", "st", "stname", "cty", "ctyname", "trct", "trctname", "bgrp","bgrpname")]
  temp_lookup[, ] <- lapply(temp_lookup[, ], as.character)
  temp_lookup$tabblk2010 <- stringr::str_pad(temp_lookup$tabblk2010, width = 15, side = 'left', pad = 0)
  temp_lookup <- transform(temp_lookup, StateID = substr(tabblk2010, 1, 2), CountyID = substr(tabblk2010, 3, 5),
                           TractID = substr(tabblk2010, 6, 11), BlockGroupID = substr(tabblk2010, 12, 12),
                           CensusBlockID = substr(tabblk2010, 13, 15))
  temp_lookup <- temp_lookup[, c("tabblk2010", "StateID", "stname", "CountyID", "ctyname",
                                 "TractID", "trctname", "BlockGroupID", "bgrpname")]
  colnames(temp_lookup) <- c("Census_Block_Code", "StateID", "StateName", "CountyID", "CountyName",
                             "TractID", "TractName", "BlockGroupID", "BlockGroupName")

  ## Stage Five - Joining the lookup to the main RAC dataset
  df <- merge(temp, temp_lookup, by = "Census_Block_Code", all.x = TRUE)
  df <- subset(df, select = -c(StateID.y, CountyID.y, TractID.y, BlockGroupID.y))
  df <- df[, c(1:2, 47, 3, 48, 4, 49, 5, 50, 6:46)]
  colnames(df)[1:10] <- c("Census_Block_Code", "StateID", "StateName", "CountyID", "CountyName", "TractID", "TractName",
                          "BlockGroupID", "BlockGroupName", "CensusBlockID")


  ## Stage Six - Column classes and final tidying
  cols <- df[, 11:50]
  cols <- dplyr::mutate_all(cols, as.numeric)
  geog <- df[, 1:10]
  db <- cbind(geog, cols)
  db$StateAbb <- state_name
  db <- db[, c(1:3, 51, 4:50)]

  return(db)
}


#####################################################################################################

#####################################################################################################

## Function 2a. Function that allows RAC data to be subset to specific job sector of interest
get_jobsector_rac <- function(df, job_code, job_proportion = T) {

  ## Stage One - Extracting Job Sector of Interest
  df_sub <- df[, c("Census_Block_Code", "StateID", "StateName", "StateAbb", "CountyID", "CountyName","TractID", "TractName", "BlockGroupID",
                   "BlockGroupName", "CensusBlockID","Total_Jobs")]
  df_col <- dplyr::select(df, job_code)
  df_full <- cbind(df_sub, df_col)
  df_full[, 12] <- sapply(df_full[, 12], as.numeric)

  ## Stage Two - Calculating job proportion
  if(job_proportion == T) {
    colnames(df_full)[13] <- "Col_of_Interest"
    df_full[, 13] <- sapply(df_full[, 13], as.numeric)
    df_full$Job_Code_Proportion <- (df_full$Col_of_Interest/df_full$Total_Jobs)*100
    colnames(df_full)[13] <- paste0(job_code)
    colnames(df_full)[14] <- paste0(job_code, "_Proportion")}
  else {
    df_full <- df_full
  }

  return(df_full)
}

########################################################################################################

########################################################################################################

## Function 2b. Function that converts RAC data to simple feature (polygon) for easy plotting/spatial manip.
get_rac_spatial <- function(df) {

  ## Stage One - Setting up identifier for TIGRIS
  options(tigris_class = "sf")
  state_of_interest <- unique(df$StateAbb)
  state_of_interest <- stringr::str_to_upper(state_of_interest)

  ## Stage Two - Getting census blocks for state of interest
  blocks <- tigris::blocks(state = state_of_interest)
  blocks <- blocks[, c("GEOID10","STATEFP10", "COUNTYFP10", "TRACTCE10", "BLOCKCE10")]
  colnames(blocks) <- c("Census_Block_Code","StateID", "CountyID", "TractID", "BlockID")
  blocks <- transform(blocks, BlockGroupID = substr(BlockID, 1, 1), CensusBlockID = substr(BlockID, 2, 4))

  ## Stage Three - Joining RAC data with census blocks
  shp <- merge(blocks, df, by = "Census_Block_Code", all.x = F)
  shp <- subset(shp, select = -c(BlockID, StateID.y, CountyID.y, TractID.y, BlockGroupID.y, CensusBlockID.y))
  colnames(shp)[2:7] <- c("StateID", "CountyID", "TractID", "geometry","BlockGroupID", "CensusBlockID")
  shp_sf <- sf::st_as_sf(shp)
  return(shp_sf)

}


##############################################################################################################

##############################################################################################################


# 3. Origin Destination (OD) Functions ------------------------------------

## Function 3. Function for obtaining OD data for one state, one year (e.g. get_od_data("ak", "2013"))
### main = T is used to get intra-state flows, main = F is used for inter-state flows
get_od_data <- function(state_name, year, main = T) {

  # Stage One - Getting OD Data from LODES
  if(main == T) {
    dl_file <- paste0(state_name, "_od_main_JT00_", year, ".csv.gz")
    download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/od/", dl_file), dl_file)
    R.utils::gunzip(dl_file)
    temp <- data.table::fread(paste0(state_name, "_od_main_JT00_", year, ".csv"))
    rm(dl_file)
    file.remove(paste0(state_name, "_od_main_JT00_", year, ".csv"))}
  else{
    dl_file <- paste0(state_name, "_od_aux_JT00_", year, ".csv.gz")
    download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/od/", dl_file), dl_file)
    R.utils::gunzip(dl_file)
    temp <- data.table::fread(paste0(state_name, "_od_aux_JT00_", year, ".csv"))
    rm(dl_file)
    file.remove(paste0(state_name, "_od_aux_JT00_", year, ".csv"))
  }

  # Stage Two - Cleaning the OD data
  temp <- temp[, c("w_geocode", "h_geocode", "S000")]
  temp$w_geocode <- stringr::str_pad(temp$w_geocode, width = 15, side = 'left', pad = 0)
  temp$h_geocode <- stringr::str_pad(temp$h_geocode, width = 15, side = 'left', pad = 0)
  colnames(temp) <- c("Workplace_Census_Block_Code", "Residence_Census_Block_Code", "Total_Job_Flows")
  temp <- transform(temp, W_StateID = substr(Workplace_Census_Block_Code, 1, 2), W_CountyID = substr(Workplace_Census_Block_Code, 3, 5),
                    W_TractID = substr(Workplace_Census_Block_Code, 6, 11), W_BlockGroupID = substr(Workplace_Census_Block_Code, 12, 12),
                    W_CensusBlockID = substr(Workplace_Census_Block_Code, 13, 15), Workplace_Census_Block_Code = Workplace_Census_Block_Code)
  temp <- transform(temp, R_StateID = substr(Residence_Census_Block_Code, 1, 2), R_CountyID = substr(Residence_Census_Block_Code, 3, 5),
                    R_TractID = substr(Residence_Census_Block_Code, 6, 11), R_BlockGroupID = substr(Residence_Census_Block_Code, 12, 12),
                    R_CensusBlockID = substr(Residence_Census_Block_Code, 13, 15), Residence_Census_Block_Code = Residence_Census_Block_Code)
  temp <- temp[, c("Workplace_Census_Block_Code", "W_StateID", "W_CountyID", "W_TractID", "W_BlockGroupID", "W_CensusBlockID", "Total_Job_Flows",
                   "Residence_Census_Block_Code", "R_StateID", "R_CountyID", "R_TractID", "R_BlockGroupID", "R_CensusBlockID")]

  # Stage Three - Downloading the Lookup
  dl_file_lookup <- paste0(state_name, "_xwalk.csv.gz")
  download.file(paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", state_name, "/", dl_file_lookup), dl_file_lookup)
  R.utils::gunzip(dl_file_lookup)
  temp_lookup <- data.table::fread(paste0(state_name, "_xwalk.csv"))
  file.remove(paste0(state_name, "_xwalk.csv"))

  # Stage Four - Cleaning the Lookup
  temp_lookup <- temp_lookup[, c("tabblk2010", "st", "stname", "cty", "ctyname", "trct", "trctname", "bgrp","bgrpname")]
  temp_lookup$tabblk2010 <- stringr::str_pad(temp_lookup$tabblk2010, width = 15, side = 'left', pad = 0)
  temp_lookup <- transform(temp_lookup, StateID = substr(tabblk2010, 1, 2), CountyID = substr(tabblk2010, 3, 5),
                           TractID = substr(tabblk2010, 6, 11), BlockGroupID = substr(tabblk2010, 12, 12),
                           CensusBlockID = substr(tabblk2010, 13, 15))
  temp_lookup <- temp_lookup[, c("tabblk2010", "StateID", "stname", "CountyID", "ctyname",
                                 "TractID", "trctname", "BlockGroupID", "bgrpname")]
  colnames(temp_lookup) <- c("Census_Block_Code", "StateID", "StateName", "CountyID", "CountyName",
                             "TractID", "TractName", "BlockGroupID", "BlockGroupName")

  # Stage Five - Joining the Lookup to the OD Data
  temp[, ] <- lapply(temp[, ], as.character)
  temp_lookup[, ] <- lapply(temp_lookup[, ], as.character)
  merge1 <- merge(temp, temp_lookup, by.x = c("Workplace_Census_Block_Code", "W_StateID", "W_CountyID", "W_TractID", "W_BlockGroupID"),
                  by.y = c("Census_Block_Code", "StateID", "CountyID", "TractID", "BlockGroupID"), all.x = TRUE)
  colnames(merge1)[14:17] <- c("W_StateName", "W_CountyName", "W_TractName", "W_BlockGroupName")
  merge2 <- merge(merge1, temp_lookup, by.x = c("Residence_Census_Block_Code", "R_StateID", "R_CountyID", "R_TractID", "R_BlockGroupID"),
                  by.y = c("Census_Block_Code", "StateID", "CountyID", "TractID", "BlockGroupID"), all.x = TRUE)
  colnames(merge2)[18:21] <- c("R_StateName", "R_CountyName", "R_TractName", "R_BlockGroupName")
  merge2$W_StateAbb <- state_name
  df <- merge2[, c("Workplace_Census_Block_Code", "W_StateID", "W_StateName","W_StateAbb","W_CountyID", "W_CountyName",
                   "W_TractID", "W_TractName", "W_BlockGroupID", "W_BlockGroupName", "W_CensusBlockID",
                   "Residence_Census_Block_Code", "R_StateID", "R_StateName", "R_CountyID", "R_CountyName",
                   "R_TractID", "R_TractName", "R_BlockGroupID", "R_BlockGroupName", "R_CensusBlockID", "Total_Job_Flows")]
  df$Total_Job_Flows <- as.numeric(as.character(df$Total_Job_Flows))

  return(df)

}


######################################################################################################################

######################################################################################################################

## Function 3a. Function that allows subsetting of flows to a certain threshold
get_od_subset <- function(df, flow_threshold) {
  df <- df[df$Total_Job_Flows > flow_threshold, ]

  return(df)
}

######################################################################################################################

#######################################################################################################################
## Function 3b. Function that allows conversion of cleaned OD data to a dataframe with two sets of coordinates (workplace/residence)
### for easy plotting of linestrings between them
get_od_spatial <- function(df) {

  ## Stage One - Extracting and formatting the Census Blocks for the state of interest
  state_of_interest <- unique(df$W_StateAbb)
  state_of_interest <- stringr::str_to_upper(state_of_interest)
  blocks <- tigris::blocks(state = state_of_interest)
  blocks <- blocks[, c("GEOID10","STATEFP10", "COUNTYFP10", "TRACTCE10", "BLOCKCE10")]
  colnames(blocks) <- c("Census_Block_Code","StateID", "CountyID", "TractID", "BlockID", "geometry")
  blocks <- transform(blocks, BlockGroupID = substr(BlockID, 1, 1), CensusBlockID = substr(BlockID, 2, 4))
  blocks <- blocks[, c(1:5, 7:8, 6)]

  ## Stage Two - Convert the census blocks to centroids
  blocks_sf <- sf::st_as_sf(blocks)
  block_centroids <- sf::st_centroid(blocks_sf)

  # Stage Three - Joining centroids coordinates onto the OD data
  df_merge <- merge(df, block_centroids, by.x = c("Workplace_Census_Block_Code", "W_StateID", "W_CountyID", "W_TractID", "W_BlockGroupID", "W_CensusBlockID"),
                    by.y = c("Census_Block_Code", "StateID", "CountyID", "TractID", "BlockGroupID", "CensusBlockID"), all.x = TRUE)
  colnames(df_merge)[24] <- c("W_geometry")
  df_merge_2 <- merge(df_merge, block_centroids, by.x = c("Residence_Census_Block_Code", "R_StateID", "R_CountyID", "R_TractID", "R_BlockGroupID", "R_CensusBlockID"),
                      by.y = c("Census_Block_Code", "StateID", "CountyID", "TractID", "BlockGroupID", "CensusBlockID"), all.x = TRUE)
  colnames(df_merge_2)[26] <- c("R_geometry")
  df_merge_2 <- subset(df_merge_2, select=-c(BlockID.x, BlockID.y))

  return(df_merge_2)
}

