
get_sf_objects<-function(admin_level=NA){
  #establish connection to database ----
  url<-"mongodb://morizonuser:dreifachwumms@82.165.247.81:27017"
  con <- mongo(db="shapefiles_germany",collection="admin_shapes2", url=url)
  
  ##which admin level is needed?
  if(is.na(admin_level)){
   # data<-con$find("{}")
    n<-con$count()
  }else{
    query<-paste0('{"admin_level" : ', admin_level, '}')
    #data<-con$find(query)
    n<-con$count(paste0('{"admin_level" : ', admin_level, '}'))
  }
  
  # it<-con$iterate(fields =paste0('{"geometry.type" : true,"geometry.coordinates":true , "_id": false,
  #                            "admin_level" :',admin_level, '}'))
  
   it<-con$iterate(fields = '{"geometry.type" : true,"geometry.coordinates":true , "_id": false}')
  js<-it$json(size = n)
  js_list<-as.list(js)
  
  sf_list<-lapply(js_list,geojson_sf)
  # Bind data frame rows to each sf object
  geometry<-lapply(sf_list,st_geometry)
  it<-con$iterate(fields = '{"Object_Name" : true,"Object_Key":true ,"admin_level":true , "_id": false}')
  
  data<-it$page(size = n)

  data$geometry<-unlist(geometry,recursive=F)
  
  sf_object<-st_as_sf(data, crs=4326)
  sf_object<-st_make_valid(sf_object)
  sf_object<-sf_object[st_is_valid(sf_object),]
  



  # install.packages("abind")
  # library(abind)
  # abind(data$geometry$coordinates[[1]])
  #
  # coord_list<-data2$geometry$coordinates
  # class(coord_list[[1]])
  # class(coord_list[[1]][[1]])
  #
  # coord_list<-data$geometry$coordinates[[1]][[1]]
#   data2<-data
#   #find potential errorenous shapefiles ----
#   x<-lapply(data2$geometry$coordinates,function(coord_list)tryCatch({
#     max_level<-nest_level(coord_list)
# 
#     if(is.array(coord_list[[1]])){
# 
#       handle_list_of_arrays(coord_list)
#     } else if(max_level<2) {
#       coord_list<- matrix(coord_list,ncol=2)
#       # print(coord_list)
#       # poly_list<-rrapply(coord_list , f = list_to_polygon, how = "list")
#       x<- st_polygon(list(coord_list)) %>% st_sfc()
#     }else{
#       flat_list<-(coord_list)
#       if(max_level>0)
#       {
#         for(i in 1:(max_level-1))
#         {
#           flat_list<-flat_list %>% modify_if(Negate(is.list), list) %>% flatten()
#         }
#         coord_list<-flat_list %>%  as.list() %>% modify_if(Negate(is.matrix),matrix,ncol=2) %>%
#           modify_if(Negate(is.list), list)
#         x<-coord_list %>%
#           st_multipolygon() %>% st_combine() %>% st_sfc()
#       }
#       x<-coord_list %>%
#         st_multipolygon() %>% st_sfc()
#     }
# 
#   },error=identity))
# 
#   index<-which((vapply(x, is, logical(1), "error"))==T)
# 
#   if(!is_empty(index)){
#     data2<-data[-index,]
#   }else{
#     data2<-data
#   }
# 
#   #data2 are now the ones without errors
#   #x = errorenous ones
#   x<-data[index,]
# 
# x<-convert_to_sf(x$geometry$coordinates[[19]])
# x
# x %>% View()
# 
# x$coordinates<-x$geometry$coordinates
# 
# x<-x %>%
#   filter(lengths(coordinates)<=2)
# 
# x %>% View()
# 
# str(x$coordinates[19])
# 
# lapply(x$geometry$coordinates,convert_to_sf)
# 
# coordinates_list<-x$coordinates[19]
# element<-coordinates_list[[1]]
# sub_element<-element[[2]]
# class(sub_element)
# 
#   
#   #y<-lapply(x$geometry$coordinates,convert_to_sfc) 
#   
#   # convert sf objects ----
#   x<-lapply(data2$geometry$coordinates,convert_to_sfc)
#   
#   geometry<-do.call(c,x)
#   data2$geometry<-geometry
#   
#   #geometry<-do.call(c,y)
#   #data[index,]$geometry<-geometry
#   
#   
#   
#   sf_object<-st_as_sf(data2,crs=4326)
#   sf_object<-st_make_valid(sf_object)
#   sf_object<-sf_object[st_is_valid(sf_object),]
  
}


#functions ----
nest_level <- function(lst) {
  if (!is.list(lst)) {
    return(0)
  } else if (length(lst) == 0) {
    return(1)
  } else {
    nested_levels <- lapply(lst, nest_level)
    return(1 + max(unlist(nested_levels)))
  }
}
handle_list_of_arrays<-function(coord_list){
  flat_list <- rrapply(coord_list, f = matrix,ncol=2, how = "list")
  
  max_level<-nest_level(flat_list)
  even_flat_list<-list()
  for(i in 1:max_level)
  {
    even_flat_list<-flat_list %>% modify_if(Negate(is.list), list) %>% flatten() 
  }
  poly_list<-rrapply(even_flat_list , f = list_to_polygon, how = "list")
  x<-st_multipolygon(poly_list) %>% st_combine()
}



convert_to_sfc<-function(coord_list){
  max_level<-nest_level(coord_list) 
  if(is.array(coord_list[[1]])){
    
    handle_list_of_arrays(coord_list)
  }
  else if(max_level<2)
  {  
    coord_list<- matrix(coord_list,ncol=2) 
    # print(coord_list)
    # poly_list<-rrapply(coord_list , f = list_to_polygon, how = "list")
    x<- st_polygon(list(coord_list)) %>% st_sfc()
    
  }
  else
  {
    flat_list<-(coord_list)
    if(max_level>0)
    {
      for(i in 1:(max_level-1))
      {
        flat_list<-flat_list %>% modify_if(Negate(is.list), list) %>% flatten() 
      }
      coord_list<-flat_list %>%  as.list() %>% modify_if(Negate(is.matrix),matrix,ncol=2) %>% 
        modify_if(Negate(is.list), list)
      x<-coord_list %>%   
        st_multipolygon() %>% st_combine() %>% st_sfc()
    }
    x<-coord_list %>%   
      st_multipolygon() %>% st_sfc()
  }
  
}
  
  list_to_polygon<-function(coord_list)
  {
    
    x<-lapply(coord_list,function(coord_list)tryCatch(
      {
        st_polygon(list(coord_list))
      },error=identity))
    
    index<-which((vapply(x, is, logical(1), "error"))==T)
    # create an sf polygon object
    polygon <- st_polygon(list(coord_list))
    
  }  


get_pop_data<-function(roi=150840590590,admin_shapefiles){
  #establish connection to database ----
  url<-"mongodb://morizonuser:dreifachwumms@82.165.247.81:27017"
  con <- mongo(db="population",collection="ortsteile", url=url)
  
  length_object_key<-nchar(roi)
  
  data<-con$find("{}")
  
  data2<-data %>% select(-geometry) %>% 
    filter(JAHR==2020) %>% 
    as.tibble() %>%  
    unnest_wider(data) %>% 
    mutate(Object_Key=substr(Object_Key,1,length_object_key)) %>% 
    group_by(Object_Key) %>% 
    summarise(TOTAL_SUM=sum(TOTAL_SUM))
  
  x<-data2 %>%
    arrange(-M20) %>% 
    tibble::rownames_to_column() 
  
  index<-x %>% 
    filter(Object_Key==roi) %>% 
    pull(as.numeric(rowname)) %>% as.numeric()
  
  x[c(seq(index,index+1),seq(index,index-1)),"Object_Key"]
  
}  



get_list_of_neighbours_keys <- function(object_key,radius_search=T,radius=NA, admin_level=NA){
  
  if(is.na(radius)){
    radius= ((
      admin_shapefiles[admin_shapefiles$Object_Key == object_key, ] %>% 
        st_area() %>% sqrt())/0.7515194)
  }
  if(is.na(admin_level))
  {
    level<-admin_shapefiles[admin_shapefiles$Object_Key == object_key, ]$admin_level
  }
  else{
    level<-admin_level
  }
  admin_shapefiles_same_level<-admin_shapefiles %>% 
    filter(admin_level==level)
  
  centroide<-admin_shapefiles %>%
    filter(Object_Key==object_key) %>% 
    st_centroid()
  
  buffer<-st_buffer(centroide,radius) %>% 
    select(geometry)
  
  #check if shapefiles are valid
  admin_shapefiles_same_level<-admin_shapefiles_same_level[which(st_is_valid(admin_shapefiles_same_level)==T),]
  
  admin_shapefiles_same_level_radius<-admin_shapefiles_same_level %>% 
    filter(st_intersects(admin_shapefiles_same_level, buffer, sparse = FALSE)[,1])
  
  if(radius_search){
    return(admin_shapefiles_same_level_radius %>%   geojsonsf::sf_geojson())
  }
  else{
    index<-st_intersects((admin_shapefiles[admin_shapefiles$Object_Key == object_key, ]),admin_shapefiles_same_level_radius,
                         sparse = FALSE)[1,]
    
    admin_shapefiles_same_intersecting<-admin_shapefiles_same_level_radius %>% 
      .[index,] %>% 
      geojsonsf::sf_geojson()
    return(admin_shapefiles_same_intersecting)
  } 
  
} 



df2sf <- function(df) {
  class(df) <- c("sf", "data.frame")
  attributes(df)$sf_column <- "geometry"
  df
}

########## opt stuff
otp_traveltime <- function(otpcon = NA,
                           pointsetname=pointsetname,
                           fromPlace = NA,
                           toPlace = NA,
                           fromID = NULL,
                           toID = NULL,
                           mode = "CAR",
                           date_time = Sys.time(),
                           arriveBy = FALSE,
                           maxWalkDistance = 1000,
                           numItineraries = 3,
                           routeOptions = NULL,
                           ncores = max(round(parallel::detectCores() * 1.25) - 1,1),
                           timezone = otpcon$timezone,
                           steps=steps) {
  # Check Valid Inputs
  
  # Back compatibility with 0.2.1
  if (is.null(timezone)) {
    warning("otpcon is missing the timezone variaible, assuming local timezone")
    timezone <- Sys.timezone()
  }
  
  checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL))
  
  checkmate::assert_class(otpcon, "otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
                           choices = c(
                             "TRANSIT", "WALK", "BICYCLE",
                             "CAR", "BUS", "RAIL", "SUBWAY",
                             "TRAM", "FERRY"
                           ),
                           empty.ok = FALSE
  )
  #checkmate::assert_character(mode, len = 1)
  #mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
  checkmate::assert_character(fromID, null.ok = FALSE)
  checkmate::assert_character(toID, null.ok = FALSE)
  checkmate::assert_logical(arriveBy)
  # checkmate::assert_numeric(ncores, lower = 1, len = 1, upper = max(c(round(parallel::detectCores() * 1.25 ) - 1,1)))
  #arriveBy <- tolower(arriveBy)
  
  # Check Route Options
  if (!is.null(routeOptions)) {
    routeOptions <- otp_validate_routing_options(routeOptions)
  }
  
  # Special checks for fromPlace and toPlace
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  fromPlace <- fromPlace[,2:1]
  
  if (!is.null(fromID)) {
    if (length(fromID) != nrow(fromPlace)) {
      stop("The length of fromID and fromPlace are not the same")
    }
  }
  
  if (!is.null(toID)) {
    if (length(toID) != nrow(toPlace)) {
      stop("The length of toID and toPlace are not the same")
    }
  }
  
  # Make a pointset for each fromPLACE
  toPlace <- sf::st_sf(data.frame(geometry = sf::st_geometry(toPlace)),crs=4326)
  #pointsetname <- paste(sample(LETTERS, 10, TRUE), collapse = "")
  #otp_pointset(toPlace, pointsetname, path_data)
  
  # Make surfaces
  
  
  step<-steps
  o<-1
  k<-step
  placeholder_list<-list()
 
  for(i in 1:(nrow(fromPlace)%/%steps))
  {
    placeholder<-fromPlace[o:k,]
    placeholder_fromID<-fromID[o:k]  
    surfaces <- otp_make_surface(otpcon = otpcon,
                                 fromPlace =   placeholder,
                                 mode = mode,
                                 date_time = Sys.time(),
                                 arriveBy = arriveBy,
                                 maxWalkDistance = maxWalkDistance,
                                 routeOptions = routeOptions,ncores = ncores)
    
    #surface_results<-list(res=res,index_of_empty_surface=index_of_empty_surface)
    
    surface_results <- otp_surface(otpcon = otpcon,
                                   surface = surfaces,
                                   pointsset = pointsetname,
                                   get_data = FALSE,
                                   ncores = ncores)
    
    times<-surface_results$res
    
    if(length(surface_results$index_of_empty_surface)!=0)
    {
      names(times) <- placeholder_fromID[-surface_results$index_of_empty_surface]
    }
    else
    {
      names(times) <- placeholder_fromID
    }
    
    times <- purrr::map(times, `[[`, "times")
    
    remove_toId_index<- which(lengths(times) > 0 )
    
    times <- times[lengths(times) > 0]
    times <- list2df(times)
    
    #times<-times %>% 
    #  mutate(if_any(everything(), ~ !is.na(.)))  
    #  slice(1:length(toID))
    
    
    if(i==nrow(fromPlace)%/%steps)
    {
      k<-k+ nrow(fromPlace)%%steps
    }
    else
    {
      k<-k+step
    }
    
    o<-o+step
    
    
    placeholder_list[[length(placeholder_list) + 1]]<-times
    
    
    print(sprintf("Run number %d",i))
    print(sprintf("%d von %d bearbeitet",k-step,nrow(fromPlace)))
    
  }
  
  times<-placeholder_list %>% 
    bind_cols()
  dim(placeholder_list[[1]])
  rownames(times) <- as.character(toID)
  
  times[is.na(times)]<-7200
  
  return(times)
}

#'
#'
#'
#'
#' df2sf <- function(df) {
#'   class(df) <- c("sf", "data.frame")
#'   attributes(df)$sf_column <- "geometry"
#'   df
#' }
#' my_otp<-function (otpcon = NA, fromPlace = NA, toPlace = NA, fromID = NULL,
#'                   toID = NULL, mode = "CAR", date_time = Sys.time(), arriveBy = FALSE,
#'                   maxWalkDistance = 1000, numItineraries = 3, routeOptions = NULL,
#'                   full_elevation = FALSE, get_geometry = TRUE, ncores = max(round(parallel::detectCores() *
#'                                                                                     1.25) - 1, 1), timezone = otpcon$timezone, distance_balance = FALSE,
#'                   get_elevation = FALSE)
#' {
#'   if (is.null(timezone)) {
#'     warning("otpcon is missing the timezone variaible, assuming local timezone")
#'     timezone <- Sys.timezone()
#'   }
#'   checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL))
#'   checkmate::assert_class(otpcon, "otpconnect")
#'   mode <- toupper(mode)
#'   checkmate::assert_subset(mode, choices = c("TRANSIT", "WALK",
#'                                              "BICYCLE", "CAR", "BUS", "RAIL", "SUBWAY", "TRAM", "FERRY",
#'                                              "BICYCLE_RENT", "BICYCLE_PARK", "CAR_PARK", "CABLE_CAR",
#'                                              "GONDOLA", "FUNICULAR", "AIRPLANE"), empty.ok = FALSE)
#'   mode <- paste(mode, collapse = ",")
#'   checkmate::assert_posixct(date_time)
#'   date <- format(date_time, "%m-%d-%Y", tz = timezone)
#'   time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
#'   checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
#'   checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
#'   checkmate::assert_numeric(ncores, lower = 1, len = 1, upper = max(c(round(parallel::detectCores() *
#'                                                                               1.25) - 1, 2)))
#'   checkmate::assert_character(fromID, null.ok = TRUE)
#'   checkmate::assert_character(toID, null.ok = TRUE)
#'   checkmate::assert_logical(arriveBy)
#'   arriveBy <- tolower(arriveBy)
#'   checkmate::assert_logical(distance_balance, len = 1, null.ok = FALSE)
#'   checkmate::assert_logical(get_elevation, len = 1, null.ok = FALSE)
#'   if (distance_balance & (ncores > 1)) {
#'     if (is.null(fromID)) {
#'       stop("Distance balancing changes the order of the output, so fromID must not be NULL")
#'     }
#'     if (is.null(toID)) {
#'       stop("Distance balancing changes the order of the output, so toID must not be NULL")
#'     }
#'   }
#'   if (!is.null(routeOptions)) {
#'     routeOptions <- otp_validate_routing_options(routeOptions)
#'   }
#'   fromPlace <- otp_clean_input(fromPlace, "fromPlace")
#'   toPlace <- otp_clean_input(toPlace, "toPlace")
#'   if (!is.null(fromID)) {
#'     if (length(fromID) != nrow(fromPlace)) {
#'       stop("The length of fromID and fromPlace are not the same")
#'     }
#'   }
#'   if (!is.null(toID)) {
#'     if (length(toID) != nrow(toPlace)) {
#'       stop("The length of toID and toPlace are not the same")
#'     }
#'   }
#'   nrfp <- nrow(fromPlace)
#'   nrtp <- nrow(toPlace)
#'   if (nrfp != nrtp) {
#'     if (nrfp > nrtp & nrtp == 1) {
#'       toPlace <- toPlace[rep(1, times = nrfp), ]
#'       if (!is.null(toID)) {
#'         toID <- toID[rep(1, times = nrfp)]
#'       }
#'       message("repeating toPlace to match length of fromPlace")
#'     }
#'     else if (nrtp > nrfp & nrfp == 1) {
#'       fromPlace <- fromPlace[rep(1, times = nrtp), ]
#'       if (!is.null(fromID)) {
#'         fromID <- fromID[rep(1, times = nrtp)]
#'       }
#'       message("repeating fromPlace to match length of toPlace")
#'     }
#'     else {
#'       stop("Number of fromPlaces and toPlaces do not match")
#'     }
#'   }
#'   if (distance_balance & (ncores > 1)) {
#'     dists <- geodist::geodist(fromPlace, toPlace, paired = TRUE)
#'     dists_0 <- dists != 0
#'     fromPlace <- fromPlace[dists_0, ]
#'     toPlace <- toPlace[dists_0, ]
#'     fromID <- fromID[dists_0]
#'     toID <- toID[dists_0]
#'     dists <- dists[dists_0]
#'     dists <- order(dists, decreasing = TRUE)
#'     fromPlace <- fromPlace[dists, ]
#'     toPlace <- toPlace[dists, ]
#'     fromID <- fromID[dists]
#'     toID <- toID[dists]
#'   }
#'   routerUrl <- make_url(otpcon)
#'   routerUrl <- paste0(routerUrl, "/plan")
#'   fromPlace <- format(fromPlace, scientific = FALSE, digits = 9,
#'                       trim = TRUE)
#'   toPlace <- format(toPlace, scientific = FALSE, digits = 9,
#'                     trim = TRUE)
#'   fromPlace <- paste0(fromPlace[, 1], "%2C", fromPlace[, 2])
#'   toPlace <- paste0(toPlace[, 1], "%2C", toPlace[, 2])
#'   query <- list(mode = mode, date = date, time = time, maxWalkDistance = maxWalkDistance,
#'                 arriveBy = arriveBy, numItineraries = numItineraries)
#'   if (otpcon$otp_version >= 2) {
#'     if (mode == "CAR" | grepl("TRANSIT", mode)) {
#'       query$maxWalkDistance <- NULL
#'     }
#'   }
#'   if (!is.null(routeOptions)) {
#'     query <- c(query, routeOptions)
#'   }
#'   urls <- build_urls(routerUrl, fromPlace, toPlace, query)
#'   print(urls)
#'   message(Sys.time(), " sending ", length(urls), " routes requests using ",
#'           ncores, " threads")
#'   progressr::handlers("cli")
#'   results <- progressr::with_progress(otp_async(urls, ncores))
#'   message(Sys.time(), " processing results")
#'   results_routes <- RcppSimdJson::fparse(results, query = "/plan/itineraries",
#'                                          query_error_ok = TRUE, parse_error_ok = TRUE, always_list = TRUE)
#'   if (is.null(fromID)) {
#'     fromID <- unlist(RcppSimdJson::fparse(results, query = "/requestParameters/fromPlace",
#'                                           query_error_ok = TRUE, parse_error_ok = TRUE), use.names = FALSE)
#'   }
#'   if (is.null(toID)) {
#'     toID <- unlist(RcppSimdJson::fparse(results, query = "/requestParameters/fromPlace",
#'                                         parse_error_ok = TRUE, query_error_ok = TRUE), use.names = FALSE)
#'   }
#'   results_errors <- RcppSimdJson::fparse(results, query = "/error",
#'                                          query_error_ok = TRUE, parse_error_ok = TRUE, always_list = TRUE)
#'   results_errors <- results_errors[lengths(results_errors) >
#'                                      0]
#'   if (sum(lengths(results_routes)) == 0) {
#'     stop("No results returned, check your connection")
#'   }
#'   results_routes <- purrr::pmap(.l = list(itineraries = results_routes,
#'                                           fp = fromID, tp = toID), .f = otp_json2sf, full_elevation = full_elevation,
#'                                 get_geometry = get_geometry, timezone = timezone, get_elevation = get_elevation,
#'                                 .progress = TRUE)
#'   results_routes <- data.table::rbindlist(results_routes,
#'                                           fill = TRUE, use.names = TRUE)
#'   origin <- .POSIXct(0, tz = "GMT")
#'   results_routes$startTime <- as.POSIXct.numeric(results_routes$startTime/1000,
#'                                                  origin = origin, tz = timezone)
#'   results_routes$endTime <- as.POSIXct.numeric(results_routes$endTime/1000,
#'                                                origin = origin, tz = timezone)
#'   results_routes$leg_startTime <- as.POSIXct.numeric(results_routes$leg_startTime/1000,
#'                                                      origin = origin, tz = timezone)
#'   results_routes$leg_endTime <- as.POSIXct.numeric(results_routes$leg_endTime/1000,
#'                                                    origin = origin, tz = timezone)
#'   results_routes <- results_routes[seq_len(nrow(results_routes)),
#'   ]
#'   results_routes <- as.data.frame(results_routes)
#'   if (get_geometry) {
#'     results_routes$geometry <- sf::st_as_sfc(results_routes$geometry,
#'                                              crs = 4326)
#'     results_routes <- df2sf(results_routes)
#'     colnms <- names(results_routes)
#'     colnms <- colnms[!colnms %in% c("fromPlace", "toPlace",
#'                                     "geometry")]
#'     results_routes <- results_routes[c("fromPlace", "toPlace",
#'                                        colnms, "geometry")]
#'   }
#'   else {
#'     colnms <- names(results_routes)
#'     colnms <- colnms[!colnms %in% c("fromPlace", "toPlace")]
#'     results_routes <- results_routes[c("fromPlace", "toPlace",
#'                                        colnms)]
#'   }
#'   if (length(results_errors) > 0) {
#'     results_errors = purrr::map(results_errors, otp_parse_errors)
#'     results_errors = data.table::rbindlist(results_errors,
#'                                            use.names = FALSE)
#'     message(nrow(results_errors), " routes returned errors. Unique error messages are:\n")
#'     results_errors = as.data.frame(table(results_errors$msg))
#'     results_errors = results_errors[order(results_errors$Freq,
#'                                           decreasing = TRUE), ]
#'     for (msgs in seq_len(nrow(results_errors))) {
#'       message(results_errors$Freq[msgs], "x messages: \"",
#'               results_errors$Var1[msgs], "\"\n")
#'     }
#'   }
#'   message(Sys.time(), " done")
#'   return(results_routes)
#' }
#'
otp_clean_input<-function (imp, imp_name)
{
  if (all(class(imp) == "numeric")) {
    checkmate::assert_numeric(imp, len = 2)
    imp <- matrix(imp, nrow = 1, byrow = TRUE)
  }
  if ("sf" %in% class(imp)) {
    if (all(sf::st_geometry_type(imp) == "POINT")) {
      imp <- sf::st_coordinates(imp)
      imp[] <- imp[, c(1, 2)]
    }
    else {
      stop(paste0(imp_name, " contains non-POINT geometry"))
    }
  }
  if ("matrix" %in% class(imp)) {
    checkmate::assert_matrix(imp, any.missing = FALSE, min.rows = 1,
                             min.cols = 2, max.cols = 2, null.ok = FALSE)
    checkmate::assert_numeric(imp[, 1], lower = -180, upper = 180,
                              any.missing = FALSE, .var.name = paste0(imp_name,
                                                                      " Longitude"))
    checkmate::assert_numeric(imp[, 2], lower = -90, upper = 90,
                              any.missing = FALSE, .var.name = paste0(imp_name,
                                                                      " Latitude"))
    imp[] <- imp[, 2:1]
    colnames(imp) <- c("lat", "lon")
    return(imp)
  }
  stop(paste0(imp_name, " is not in a valid format ", paste(class(imp),
                                                            collapse = ", ")))
}

make_url<-function (x, type = "routers")
{
  if (!"otpconnect" %in% class(x)) {
    stop("Object is not of class otpconnect, class is ",
         class(x))
  }
  if (type == "routers") {
    if (is.null(x$url)) {
      if (x$ssl) {
        url <- paste0("https://", x$hostname, ":", x$port,
                      "/otp/routers/", x$router)
      }
      else {
        url <- paste0("http://", x$hostname, ":", x$port,
                      "/otp/routers/", x$router)
      }
    }
    else {
      url <- x$url
    }
  }
  else {
    if (x$ssl) {
      url <- paste0("https://", x$hostname, ":", x$port,
                    "/otp/", type)
    }
    else {
      url <- paste0("http://", x$hostname, ":", x$port,
                    "/otp/", type)
    }
  }
  return(url)
}

build_urls<-function (routerUrl, fromPlace, toPlace, query)
{
  secs <- unlist(query, use.names = TRUE)

  if(query$mode=="TRANSIT"){
    secs["mode"]<-"TRANSIT%2CWALK"
  }
  
  secs <- paste0(names(secs), "=", secs)
  secs <- paste(secs, collapse = "&")
  secs <- gsub(",", "%2C", secs)
  if (is.null(toPlace)) {
    secs <- paste0(routerUrl, "?", "fromPlace=", fromPlace,
                   "&", secs)
  }
  else {
    secs <- paste0(routerUrl, "?", "fromPlace=", fromPlace,
                   "&toPlace=", toPlace, "&", secs)
  }
  secs
}

otp_async <- function(urls, ncores, iso_mode = FALSE, post = FALSE){
  t1 <- Sys.time()
  p <- progressr::progressor(length(urls))
  out <- vector('list', length(urls))
  pool <- curl::new_pool(host_con = ncores)
  lapply( seq_along(urls), function(i){
    h <- curl::new_handle()
    if(post){
      curl::handle_setopt(h, post = TRUE)
    }
    if(iso_mode){
      h <- curl::handle_setheaders(h, "Accept" = "application/json")
    }
    success <- function(res){
      p()
      out[[i]] <<- rawToChar(res$content)
    }
    failure <- function(res){
      p()
      cat("Error: ", res, "\n")
      out[[i]] <<- paste0("Error: ", res)
    }
    curl::curl_fetch_multi(urls[i],
                           done = success,
                           fail = failure,
                           pool = pool,
                           handle = h)
  })
  curl::multi_run(timeout = Inf, pool = pool)
  t2 <- Sys.time()
  message("Done in ",round(difftime(t2,t1, units = "mins"),1)," mins")
  return(unlist(out, use.names = FALSE))
}

otp_plan <- function(otpcon = NA,
                     fromPlace = NA,
                     toPlace = NA,
                     fromID = NULL,
                     toID = NULL,
                     mode = "CAR",
                     date_time = Sys.time(),
                     arriveBy = FALSE,
                     maxWalkDistance = 1000,
                     numItineraries = 3,
                     routeOptions = NULL,
                     full_elevation = FALSE,
                     get_geometry = TRUE,
                     ncores = max(round(parallel::detectCores() * 1.25) - 1,1),
                     timezone = otpcon$timezone,
                     distance_balance = FALSE,
                     get_elevation = FALSE) {
  # Check Valid Inputs
  
  # Back compatibility with 0.2.1
  if (is.null(timezone)) {
    warning("otpcon is missing the timezone variaible, assuming local timezone")
    timezone <- Sys.timezone()
  }
  
  checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL))
  
  checkmate::assert_class(otpcon, "otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode,
                           choices = c(
                             "TRANSIT", "WALK", "BICYCLE",
                             "CAR", "BUS", "RAIL", "SUBWAY",
                             "TRAM", "FERRY","BICYCLE_RENT",
                             "BICYCLE_PARK","CAR_PARK","CABLE_CAR",
                             "GONDOLA","FUNICULAR","AIRPLANE"
                           ),
                           empty.ok = FALSE
  )
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(maxWalkDistance, lower = 0, len = 1)
  checkmate::assert_numeric(numItineraries, lower = 1, len = 1)
  checkmate::assert_numeric(ncores, lower = 1, len = 1, upper = max(c(round(parallel::detectCores() * 1.25 ) - 1,2)))
  checkmate::assert_character(fromID, null.ok = TRUE)
  checkmate::assert_character(toID, null.ok = TRUE)
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(arriveBy)
  checkmate::assert_logical(distance_balance, len = 1, null.ok = FALSE)
  checkmate::assert_logical(get_elevation, len = 1, null.ok = FALSE)
  
  if (distance_balance & (ncores > 1)) {
    if (is.null(fromID)) {
      stop("Distance balancing changes the order of the output, so fromID must not be NULL")
    }
    if (is.null(toID)) {
      stop("Distance balancing changes the order of the output, so toID must not be NULL")
    }
  }
  
  
  # Check Route Options
  if (!is.null(routeOptions)) {
    routeOptions <- otp_validate_routing_options(routeOptions)
  }
  
  # Special checks for fromPlace and toPlace
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  toPlace <- otp_clean_input(toPlace, "toPlace")
  
  if (!is.null(fromID)) {
    if (length(fromID) != nrow(fromPlace)) {
      stop("The length of fromID and fromPlace are not the same")
    }
  }
  
  if (!is.null(toID)) {
    if (length(toID) != nrow(toPlace)) {
      stop("The length of toID and toPlace are not the same")
    }
  }
  
  # Make sure number of fromPlace or toPlace match
  nrfp <- nrow(fromPlace)
  nrtp <- nrow(toPlace)
  if (nrfp != nrtp) {
    if (nrfp > nrtp & nrtp == 1) {
      toPlace <- toPlace[rep(1, times = nrfp), ]
      if (!is.null(toID)) {
        toID <- toID[rep(1, times = nrfp)]
      }
      message("repeating toPlace to match length of fromPlace")
    } else if (nrtp > nrfp & nrfp == 1) {
      fromPlace <- fromPlace[rep(1, times = nrtp), ]
      if (!is.null(fromID)) {
        fromID <- fromID[rep(1, times = nrtp)]
      }
      message("repeating fromPlace to match length of toPlace")
    } else {
      stop("Number of fromPlaces and toPlaces do not match")
    }
  }
  
  if (distance_balance & (ncores > 1)) {
    dists <- geodist::geodist(fromPlace, toPlace, paired = TRUE)
    
    # Remove 0m pairs as OTP will fail on them anyway
    dists_0 <- dists != 0
    fromPlace <- fromPlace[dists_0, ]
    toPlace <- toPlace[dists_0, ]
    fromID <- fromID[dists_0]
    toID <- toID[dists_0]
    dists <- dists[dists_0]
    
    dists <- order(dists, decreasing = TRUE)
    fromPlace <- fromPlace[dists, ]
    toPlace <- toPlace[dists, ]
    fromID <- fromID[dists]
    toID <- toID[dists]
  }
  
  # Build URLs
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/plan")
  
  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, trim = TRUE)
  toPlace <- format(toPlace, scientific = FALSE, digits = 9, trim = TRUE)
  
  fromPlace <- paste0(fromPlace[,1],"%2C",fromPlace[,2])
  toPlace <- paste0(toPlace[,1],"%2C",toPlace[,2])
  
  query <- list(
    mode = mode,
    date = date,
    time = time,
    maxWalkDistance = maxWalkDistance,
    arriveBy = arriveBy,
    numItineraries = numItineraries
  )
  
  if (otpcon$otp_version >= 2) {
    # maxWalkDistance causes itinaries to fail
    if (mode == "CAR" | grepl("TRANSIT", mode)) {
      query$maxWalkDistance <- NULL
    }
  }
  
  if (!is.null(routeOptions)) {
    query <- c(query, routeOptions)
  }
  
  # if(!is.null(fromID)){
  #   fromID <- data.table::data.table(fromID = fromID,
  #                                    fromPlace = gsub("%2C",",",fromPlace))
  #   fromID <- unique(fromID)
  #   if(any(duplicated(fromID$fromID))){
  #     stop("Can't have two fromIDs with the same location, coordinates are rounded to 9 dp")
  #   }
  # }
  # if(!is.null(toID)){
  #   toID <- data.table::data.table(toID = toID,
  #                                    toPlace = gsub("%2C",",",toPlace))
  #   toID <- unique(toID)
  #   if(any(duplicated(toID$toID))){
  #     stop("Can't have two toIDs with the same location, coordinates are rounded to 9 dp")
  #   }
  # }
  
  # Send Requests
  urls <- build_urls(routerUrl,fromPlace, toPlace, query)
  message(Sys.time()," sending ",length(urls)," routes requests using ",ncores," threads")
  progressr::handlers("cli")
  results <- progressr::with_progress(otp_async(urls, ncores))
  
  
  message(Sys.time()," processing results")
  results_routes <- RcppSimdJson::fparse(results,
                                         query = "/plan/itineraries",
                                         query_error_ok = TRUE,
                                         parse_error_ok = TRUE,
                                         always_list = TRUE)
  
  
  if(is.null(fromID)){
    fromID <- unlist(RcppSimdJson::fparse(results,
                                          query = "/requestParameters/fromPlace",
                                          query_error_ok = TRUE,
                                          parse_error_ok = TRUE), use.names = FALSE)
  }
  
  if(is.null(toID)){
    toID <- unlist(RcppSimdJson::fparse(results,
                                        query = "/requestParameters/fromPlace",
                                        parse_error_ok = TRUE,
                                        query_error_ok = TRUE), use.names = FALSE)
    
  }
  
  
  
  
  results_errors <- RcppSimdJson::fparse(results, query = "/error",
                                         query_error_ok = TRUE,
                                         parse_error_ok = TRUE,
                                         always_list = TRUE)
  results_errors <- results_errors[lengths(results_errors) > 0]
  
  
  if(sum(lengths(results_routes)) == 0){
    stop("No results returned, check your connection")
  }
  
  results_routes <- purrr::pmap(.l = list(itineraries = results_routes,
                                          fp = fromID,
                                          tp = toID
  ),
  .f = otp_json2sf,
  full_elevation = full_elevation,
  get_geometry = get_geometry,
  timezone = timezone,
  get_elevation = get_elevation)
  
  
  results_routes <- data.table::rbindlist(results_routes, fill = TRUE, use.names=TRUE)
  
  origin <- .POSIXct(0, tz = "GMT")
  results_routes$startTime <- as.POSIXct.numeric(results_routes$startTime / 1000,
                                                 origin = origin, tz = timezone
  )
  
  results_routes$endTime <- as.POSIXct.numeric(results_routes$endTime / 1000,
                                               origin = origin, tz = timezone
  )
  
  results_routes$leg_startTime <- as.POSIXct.numeric(results_routes$leg_startTime / 1000,
                                                     origin = origin, tz = timezone
  )
  
  results_routes$leg_endTime <- as.POSIXct.numeric(results_routes$leg_endTime / 1000,
                                                   origin = origin, tz = timezone
  )
  
  # fix for bbox error from data.table
  results_routes <- results_routes[seq_len(nrow(results_routes)), ]
  results_routes <- as.data.frame(results_routes)
  
  # if(!is.null(fromID)){
  #   results_routes$fromPlace <- fromID$fromID[match(results_routes$fromPlace, fromID$fromPlace)]
  # }
  # if(!is.null(toID)){
  #   results_routes$toPlace <- toID$toID[match(results_routes$toPlace, toID$toPlace)]
  # }
  
  if(get_geometry){
    results_routes$geometry <- sf::st_as_sfc(results_routes$geometry, crs = 4326)
    results_routes <- df2sf(results_routes)
    colnms <- names(results_routes)
    colnms <- colnms[!colnms %in% c("fromPlace", "toPlace", "geometry")]
    results_routes <- results_routes[c("fromPlace", "toPlace", colnms, "geometry")]
    
  } else {
    colnms <- names(results_routes)
    colnms <- colnms[!colnms %in% c("fromPlace", "toPlace")]
    results_routes <- results_routes[c("fromPlace", "toPlace", colnms)]
  }
  
  if(length(results_errors) > 0){
    results_errors = purrr::map(results_errors, otp_parse_errors)
    results_errors = data.table::rbindlist(results_errors, use.names = FALSE)
    message(nrow(results_errors)," routes returned errors. Unique error messages are:\n")
    results_errors = as.data.frame(table(results_errors$msg))
    results_errors = results_errors[order(results_errors$Freq, decreasing = TRUE),]
    for(msgs in seq_len(nrow(results_errors))){
      message(results_errors$Freq[msgs],'x messages: "',results_errors$Var1[msgs],'"\n')
    }
    
  }
  
  message(Sys.time()," done")
  return(results_routes)
}

#' Parse Errors
#' @param x list
#' @family internal
#' @noRd
otp_parse_errors <- function(x){
  
  data.frame(id = x$id,
             msg = x$msg
  )
  
}


#' Parse Missing
#' @param x list
#' @family internal
#' @noRd
otp_parse_missing <- function(x){
  
  data.frame(id = 0,
             from = x$requestParameters$fromPlace,
             to = x$requestParameters$toPlace,
             msg = "No result was returned"
  )
  
}



#' Clean Batch Inputs
#'
#' Clean numeric, SF, or matrix prior to routing
#'
#' @param imp fromPlace or toPlace input
#' @param imp_name name of input
#' @family internal
#' @noRd

otp_clean_input <- function(imp, imp_name) {
  # For single point inputs
  if (all(class(imp) == "numeric")) {
    checkmate::assert_numeric(imp, len = 2)
    imp <- matrix(imp, nrow = 1, byrow = TRUE)
  }
  # For SF inputs
  if ("sf" %in% class(imp)) {
    if (all(sf::st_geometry_type(imp) == "POINT")) {
      imp <- sf::st_coordinates(imp)
      imp[] <- imp[, c(1, 2)]
    } else {
      stop(paste0(imp_name, " contains non-POINT geometry"))
    }
  }
  
  # For matrix inputs
  # if (all(class(imp) == "matrix")) { # to pass CRAN checks
  if ("matrix" %in% class(imp)) {
    checkmate::assert_matrix(imp,
                             any.missing = FALSE,
                             min.rows = 1,
                             min.cols = 2,
                             max.cols = 2,
                             null.ok = FALSE
    )
    checkmate::assert_numeric(imp[, 1],
                              lower = -180, upper = 180,
                              any.missing = FALSE, .var.name = paste0(imp_name, " Longitude")
    )
    checkmate::assert_numeric(imp[, 2],
                              lower = -90, upper = 90,
                              any.missing = FALSE, .var.name = paste0(imp_name, " Latitude")
    )
    imp[] <- imp[, 2:1] # Switch round lng/lat to lat/lng for OTP
    colnames(imp) <- c("lat", "lon")
    return(imp)
  }
  # Otherwise stop as invalid input
  stop(paste0(
    imp_name,
    " is not in a valid format ",
    paste(class(imp), collapse = ", ")
  ))
}


#' Async Send  Requests
#'
#' @param urls vector of URLs for OTP
#' @param ncores Number of requests to send at once
#' @param iso_mode logical, use isochrone mode
#' @param post logical, make a post request
#' @family internal
#' @noRd
otp_async <- function(urls, ncores, iso_mode = FALSE, post = FALSE){
  
  t1 <- Sys.time()
  p <- progressr::progressor(length(urls))
  out <- vector('list', length(urls))
  pool <- curl::new_pool(host_con = ncores)
  lapply( seq_along(urls), function(i){
    h <- curl::new_handle()
    if(post){
      curl::handle_setopt(h, post = TRUE)
    }
    if(iso_mode){
      h <- curl::handle_setheaders(h, "Accept" = "application/json")
    }
    success <- function(res){
      p()
      out[[i]] <<- rawToChar(res$content)
    }
    failure <- function(res){
      p()
      cat("Error: ", res, "\n")
      out[[i]] <<- paste0("Error: ", res)
    }
    curl::curl_fetch_multi(urls[i],
                           done = success,
                           fail = failure,
                           pool = pool,
                           handle = h)
  })
  curl::multi_run(timeout = Inf, pool = pool)
  t2 <- Sys.time()
  message("Done in ",round(difftime(t2,t1, units = "mins"),1)," mins")
  return(unlist(out, use.names = FALSE))
}


otp_json2sf <- function(itineraries, fp, tp,
                        full_elevation = FALSE,
                        get_geometry = TRUE,
                        timezone = "", get_elevation = FALSE,.progress=T) {
  
  if(is.null(itineraries)){
    return(NULL)
  }
  
  # Loop over itineraries
  legs <- purrr::map(itineraries$legs, parse_leg,
                     get_geometry = get_geometry,
                     get_elevation = get_elevation,
                     full_elevation = full_elevation
  )
  
  names(legs) <- seq_len(length(legs))
  legs <- legs[!is.na(legs)]
  legs <- data.table::rbindlist(legs, fill = TRUE, idcol = "route_option", use.names=TRUE)
  names(legs) <- paste0("leg_",names(legs))
  names(legs)[names(legs) == "leg_route_option"] <- "route_option"
  names(legs)[names(legs) == "leg_geometry"] <- "geometry"
  legs$route_option <- as.integer(legs$route_option)
  
  itineraries$legs <- NULL
  
  # Extract Fare Info
  fare <- itineraries$fare
  if (!is.null(fare)) {
    if (length(fare) == nrow(itineraries)) {
      itineraries$fare <- vapply(fare, fare_func, 1)
      itineraries$fare_currency <- vapply(fare,fare_currency_func, "c")
    } else {
      itineraries$fare <- NA
      itineraries$fare_currency <- NA
    }
  } else {
    itineraries$fare <- NA
    itineraries$fare_currency <- NA
  }
  
  itineraries <- itineraries[legs$route_option, ]
  itineraries <- cbind(itineraries, legs)
  itineraries$fromPlace <- fp
  itineraries$toPlace <- tp
  
  return(itineraries)
}

#' fare parse
#' @param x fare
#' @noRd
fare_func <- function(x) {
  x <- x$fare$regular$cents
  if (length(x) == 0) {
    x <- NA_real_
  } else {
    x / 100
  }
}

#' fare currency parse
#' @param x fare
#' @noRd
fare_currency_func <- function(x) {
  x <- x$fare$regular$currency$currency
  if (length(x) == 0) {
    x <- NA_character_
  }
  x
}



#' Correct the elevation distances
#'
#' OTP returns elevation as a distance along the leg,
#' resetting to 0 at each leg but we need the distance
#' along the total route. so calculate this. Sometimes
#' the legs don't reset at 0, so account for this by
#' looking for a drop in length, sometimes small drops
#' within a leg so allows an error factor to ignore
#' small drops.
#'
#' @param dists numeric from the elevation first column
#' @param err a tolerance for errors in otp results
#' @family internal
#' @noRd

correct_distances <- function(dists, err = 1) {
  lth <- length(dists)
  if (lth <= 2) {
    return(dists) # Can't break up 2 points
  }
  brks <- dists[seq(1, lth - 1)] > (dists[seq(2, lth)] + err)
  brks <- seq(1, lth)[brks]
  if (length(brks) == 0) {
    return(dists) # No places the length decreased
  }
  mxs <- c(0, cumsum(dists[brks]))
  reps <- c(0, brks, lth)
  reps <- reps[seq(2, length(reps))] - reps[seq(1, length(reps) - 1)]
  csum <- rep(mxs, times = reps)
  return(dists + csum)
}

#' Convert Google Encoded Polyline and elevation data into sf object
#'
#' OTP returns the 2d route as a polyline bean and the elevation profile as vector of numbers
#' But the number of points for each is not the same, as 2D line only has a point at change of directions
#' While elevation is regally spaced. If elevation is supplied the correct heights are matched
#'
#' @param line character - polyline
#' @param elevation numeric - vector of elevations
#' @family internal
#' @noRd

polyline2linestring <- function(line, elevation = NULL) {
  line <- googlePolylines::decode(line$points)[[1]]
  line <- matrix(c(line$lon, line$lat), ncol = 2, dimnames = list(NULL, c("lon", "lat")))
  if (!is.null(elevation)) {
    # Some modes don't have elevation e.g TRANSIT, check for this
    if (all(is.na(elevation))) {
      ele <- rep(0, nrow(line))
    } else {
      elevation$first <- NULL
      elevation <- elevation[order(elevation$distance, method = "radix"), ]
      # Calculate the length of each segment
      dist <- geodist::geodist(line, sequential = TRUE, measure = "cheap")
      dist <- cumsum(dist)
      vals <- findInterval(dist, elevation$distance)
      vals[vals == 0] <- 1L
      ele <- elevation$second[c(1, vals)]
    }
    return(sfheaders::sfg_linestring(cbind(line, ele)))
  } else {
    return(sfheaders::sfg_linestring(line))
  }
}

parse_leg <- function(leg,
                      get_geometry = TRUE,
                      get_elevation = TRUE,
                      full_elevation = FALSE) {
  # split into parts
  leg$from <- NULL
  leg$to <- NULL
  
  if (get_elevation | full_elevation) {
    elevation <- purrr::map(leg$steps, parse_elevation)
  } else {
    elevation <- list(NULL)
  }
  
  leg$steps <- NULL
  leg$legElevation <- NULL #2.2 Only
  
  if (full_elevation) {
    leg$elevation <- elevation
  }
  
  if (get_geometry) {
    # Extract geometry
    legGeometry <- purrr::map2(.x = leg$legGeometry,
                               .y = elevation,
                               .f = polyline2linestring)
    leg$geometry <- legGeometry
    leg$legGeometry <- NULL
  } else {
    leg$legGeometry <- NULL
  }
  
  return(leg)
}

#' Parse elevation data
#'
#'
#' @param stp list - a step
#' @family internal
#' @noRd
parse_elevation <- function(stp) {
  if (is.null(stp)) {
    return(NA)
  }
  # Check for OTP1 or OTP2
  if(inherits(stp$elevation, "character")){
    elev <- strsplit(stp$elevation,",")
    elev <- purrr::map(elev, as.numeric)
    elev <- purrr::map(elev, split_alternating)
    elev <- data.table::rbindlist(elev, idcol = "step")
  } else {
    elev <- data.table::rbindlist(stp$elevation, idcol = "step")
  }
  elev$distance <- correct_distances(elev$first)
  return(as.data.frame(elev))
}

#' Split vector into two vectors by alternating values
#' @param x vector
#' @family internal
#' @noRd
split_alternating <- function(x){
  odd <- rep(c(TRUE,FALSE), length(x)/2)
  return(data.frame(first = x[odd],
                    second = x[!odd],
                    check.names = FALSE,
                    check.rows = FALSE))
}

otp_make_surface<-function (otpcon = NULL, fromPlace = c(-1.17502, 50.6459), mode = "CAR",
                            date_time = Sys.time(), maxWalkDistance = 1000, arriveBy = FALSE,
                            routeOptions = NULL, timezone = otpcon$timezone, ncores =  max(round(parallel::detectCores() * 1.25) - 1,1))
{
  # if (!is.null(otpcon$otp_version)) {
  #   if (otpcon$otp_version >= 2) {
  #     stop("Surface is not supported by OTP v2.X")
  #   }
  # }
  if (is.null(timezone)) {
    warning("otpcon is missing the timezone variaible, assuming local timezone")
    timezone <- Sys.timezone()
  }
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_class(otpcon, "otpconnect")
  mode <- toupper(mode)
  checkmate::assert_subset(mode, choices = c("TRANSIT", "WALK",
                                             "BICYCLE", "CAR", "BUS", "RAIL", "SUBWAY", "TRAM", "FERRY",
                                             "BICYCLE_RENT", "BICYCLE_PARK", "CAR_PARK", "CABLE_CAR",
                                             "GONDOLA", "FUNICULAR", "AIRPLANE"), empty.ok = FALSE)
  mode <- paste(mode, collapse = ",")
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(arriveBy)
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9,
                      trim = TRUE)
  fromPlace <- paste0(fromPlace[, 1], "%2C", fromPlace[, 2])
  surfaceUrl <- make_url(otpcon, type = "surfaces")
  querylist <- list(batch = "true", date = date, time = time,
                    mode = mode, maxWalkDistance = maxWalkDistance, arriveBy = arriveBy)
  if (!is.null(routeOptions)) {
    routeOptions <- otp_validate_routing_options(routeOptions)
  }
  if (!is.null(routeOptions)) {
    querylist <- c(querylist, routeOptions)
  }
  urls <- build_urls(surfaceUrl, fromPlace, toPlace = NULL,
                     querylist)
  message(Sys.time(), " making ", length(urls), " surfaces using ",
          ncores, " threads")
  results <- progressr::with_progress(otp_async(urls, ncores,
                                                post = TRUE))
  asjson <- RcppSimdJson::fparse(unlist(results, use.names = FALSE),
                                 parse_error_ok = TRUE)
  return(asjson)
}

otp_surface<-function (otpcon = NULL, surface = NULL, pointsset = NULL, get_data = TRUE,
                       ncores =  max(round(parallel::detectCores() * 1.25) - 1,1))
{
  # if (!is.null(otpcon$otp_version)) {
  #   if (otpcon$otp_version >= 2) {
  #     stop("Surface is not supported by OTP v2.X")
  #   }
  # }
  surfaceUrl <- make_url(otpcon, type = "surfaces")
  #surfaceids <- surface[[1]]$id
  index_of_empty_surface<-which(lengths(surface)==0)
  surface<-surface[lapply(surface,length)>0]
  surfaceids <- purrr::map_int(surface, `[[`, "id")
  
  surfaceUrl <- paste0(surfaceUrl, "/", surfaceids, "/indicator?targets=",
                       pointsset, "&detail=true")
  message(Sys.time(), " evaluating ", length(surfaceUrl),
          " surfaces using ", ncores, " threads")
  
  results <- progressr::with_progress(otp_async(surfaceUrl,
                                                ncores))
  
  asjson <- RcppSimdJson::fparse(unlist(results, use.names = FALSE),
                                 parse_error_ok = TRUE)
  
  res <- purrr::map(asjson, parse_surface, get_data = get_data)   
  
  return(list(res=res,index_of_empty_surface=index_of_empty_surface))
  
}
#
# otp_connect<-function (hostname = "localhost", router = "default", url = NULL,
#                        port = 8080, ssl = FALSE, check = TRUE, timezone = Sys.timezone(),
#                        otp_version = 1.5)
# {
#   coll <- checkmate::makeAssertCollection()
#   checkmate::assert_string(hostname, add = coll)
#   checkmate::assert_string(router, add = coll)
#   checkmate::assert_string(url, add = coll, null.ok = TRUE)
#   checkmate::assert_string(timezone, add = coll)
#   checkmate::assert_int(port, lower = 1, add = coll)
#   checkmate::assert_logical(ssl, add = coll)
#   checkmate::assert_logical(check, add = coll)
#   checkmate::reportAssertions(coll)
#   checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL),
#                            add = coll)
#   otpcon <- list(hostname = hostname, router = router, url = url,
#                  port = port, ssl = ssl, timezone = timezone, otp_version = otp_version)
#   class(otpcon) <- c("list", "otpconnect")
#   if (isTRUE(check)) {
#     chk <-  check_routers(otpcon)
#     if (!isTRUE(chk)) {
#       stop(chk)
#     }
#     otpcon$otp_version <- otp_check_version(otpcon, warn = FALSE)
#   }
#   return(otpcon)
# }




check_routers<-function (otpcon)
{
  if (!"otpconnect" %in% class(otpcon)) {
    stop("Object is not of class otpconnect, class is ",
         class(otpcon))
  }
  if (is.null(otpcon$url)) {
    url <- paste0(ifelse(isTRUE(otpcon$ssl), "https://",
                         "http://"), otpcon$hostname, ":", otpcon$port, "/otp/routers")
  }
  else {
    warning("this is not supported yet")
  }
  check <- try(curl::curl_fetch_memory(url), silent = TRUE)
  if (inherits(check, "try-error")) {
    return(paste0("Router ", make_url(otpcon), " does not exist"))
  }
  check <- rawToChar(check$content)
  check <- rjson::fromJSON(check)
  check$routerInfo[[1]]$routerId<-"default"
  check <- unlist(lapply(check$routerInfo, function(x) {
    x$routerId
  }))
  if (otpcon$router %in% check) {
    message("Router ", make_url(otpcon), " exists")
    return(TRUE)
  }
  else {
    return(paste0("Router ", make_url(otpcon), " does not exist. Valid routers are: ",
                  paste(check, collapse = ", ")))
  }
  return(FALSE)
}

otp_connect<-function (hostname = "localhost", router = "default", url = NULL,
                       port = 8080, ssl = FALSE, check = TRUE, timezone = Sys.timezone(),
                       otp_version = 1.5)
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_string(hostname, add = coll)
  checkmate::assert_string(router, add = coll)
  checkmate::assert_string(url, add = coll, null.ok = TRUE)
  checkmate::assert_string(timezone, add = coll)
  checkmate::assert_int(port, lower = 1, add = coll)
  checkmate::assert_logical(ssl, add = coll)
  checkmate::assert_logical(check, add = coll)
  checkmate::reportAssertions(coll)
  checkmate::assert_subset(timezone, choices = OlsonNames(tzdir = NULL),
                           add = coll)
  otpcon <- list(hostname = hostname, router = router, url = url,
                 port = port, ssl = ssl, timezone = timezone, otp_version = otp_version)
  class(otpcon) <- c("list", "otpconnect")
  if (isTRUE(check)) {
    chk <- check_routers(otpcon)
    if (!isTRUE(chk)) {
      stop(chk)
    }
    #  otpcon$otp_version <- otp_check_version(otpcon, warn = FALSE)
  }
  return(otpcon)
}

otp_get_isochrone<-function (otpcon, location, fromLocation = TRUE, format = "JSON",
                             mode = "TRANSIT", date = format(Sys.Date(), "%m-%d-%Y"),
                             time = format(Sys.time(), "%H:%M:%S"), cutoffs, batch = TRUE,
                             arriveBy = FALSE, maxWalkDistance = NULL, walkReluctance = 2,
                             waitReluctance = 1, transferPenalty = 0, minTransferTime = 0,
                             extra.params = list())
{
  call <- sys.call()
  call[[1]] <- as.name("list")
  params <- eval.parent(call)
  params <- params[names(params) %in% c("mode", "location",
                                        "fromLocation", "format", "extra.params") == FALSE]
  if (otpcon$version != 1) {
    stop("OTP server is running OTPv", otpcon$version, ". otp_get_isochrone() is only supported in OTPv1")
  }
  if (missing(otpcon)) {
    stop("otpcon argument is required")
  }
  else if (missing(location)) {
    stop("location argument is required")
  }
  else if (missing(cutoffs)) {
    stop("cutoffs argument is required")
  }
  format <- toupper(format)
  mode <- toupper(mode)
  args.coll <- checkmate::makeAssertCollection()
  checkmate::assert_list(extra.params)
  checkmate::assert_logical(fromLocation, add = args.coll)
  checkmate::assert_logical(batch, add = args.coll)
  checkmate::assert_numeric(location, lower = -180, upper = 180,
                            len = 2, add = args.coll)
  checkmate::assert_choice(mode, choices = c("WALK", "BUS",
                                             "RAIL", "TRAM", "SUBWAY", "TRANSIT","CAR"), null.ok = F, add = args.coll)
  checkmate::assert_choice(format, choices = c("JSON", "SF"),
                           null.ok = FALSE, add = args.coll)
  checkmate::reportAssertions(args.coll)
  if (identical(mode, "TRANSIT") | identical(mode, "BUS") |
      identical(mode, "SUBWAY") | identical(mode, "TRAM") |
      identical(mode, "RAIL")) {
    mode <- append(mode, "WALK")
  }
  mode <- paste(mode, collapse = ",")
  #do.call(otp_check_params, params)
  routerUrl <- paste0(make_url(otpcon), "/isochrone")
  query <- list(fromPlace = paste(location, collapse = ","),
                mode = mode, batch = batch, date = date, time = time,
                maxWalkDistance = maxWalkDistance, walkReluctance = walkReluctance,
                waitReluctance = waitReluctance, arriveBy = arriveBy,
                transferPenalty = transferPenalty, minTransferTime = minTransferTime)
  if (length(extra.params) > 0) {
    msg <- paste("Unknown parameters were passed to the OTP API without checks:",
                 paste(sapply(names(extra.params), paste), collapse = ", "))
    warning(paste(msg), call. = FALSE)
    query <- append(query, extra.params)
  }
  cutoffs <- as.list(cutoffs)
  names(cutoffs) <- rep("cutoffSec", length(cutoffs))
  query <- append(query, cutoffs)
  if (isTRUE(fromLocation)) {
    req <- httr::GET(routerUrl, query = query)
  }
  else {
    req <- httr::GET(routerUrl, query = append(query, list(toPlace = paste(location,
                                                                           collapse = ","))))
  }
  url <- urltools::url_decode(req$url)
  text <- httr::content(req, as = "text", encoding = "UTF-8")
  if (grepl("\"type\":\"FeatureCollection\"", text)) {
    errorId <- "OK"
    isochrone <- text
    if (format == "SF") {
      isochrone <- geojsonsf::geojson_sf(isochrone)
      isochrone <- sf::st_make_valid(isochrone)
    }
  }
  else {
    errorId <- "ERROR"
  }
  response <- list(errorId = errorId, response = isochrone,
                   query = url)
  return(response)
}


otp_checks <- function(otp = NULL,
                       dir = NULL,
                       router = NULL,
                       graph = FALSE,
                       otp_version = NULL) {
  # Checks
  checkmate::assertDirectoryExists(dir)
  checkmate::assertDirectoryExists(paste0(dir, "/graphs/", router))
  checkmate::assertFileExists(otp, extension = "jar")
  
  
  if (graph) {
    # Check that the graph exists, and is over 5KB
    chkG <- file.exists(paste0(dir, "/graphs/", router, "/Graph.obj"))
    chkg <- file.exists(paste0(dir, "/graphs/", router, "/graph.obj"))
    
    if(!(chkG | chkg)){
      stop("File does not exist")
    }
    
    if(chkG){
      size <- file.info(paste0(dir, "/graphs/", router, "/Graph.obj"))
      size <- size$size
      if (size < 5000) {
        warning("Graph.obj exists but is very small, the build process may have failed")
        return(FALSE)
      }
    }
    
    if(chkg){
      size <- file.info(paste0(dir, "/graphs/", router, "/graph.obj"))
      size <- size$size
      if (size < 5000) {
        warning("graph.obj exists but is very small, the build process may have failed")
        return(FALSE)
      }
    }
    
  } else {
    # Check the data to build a graph exists
    fls <- list.files(file.path(dir, "/graphs/", router))
    if(length(fls) == 0){
      warning(paste0("There are no files in ",file.path(dir, "/graphs/", router)))
      return(FALSE)
    }
    fls <- fls[grepl(".osm.pbf", fls)]
    if(length(fls) == 0){
      warning(paste0("There are no osm.pbf files in ",file.path(dir, "/graphs/", router)))
      return(FALSE)
    }
  }
  
  if (otp_check_java(otp_version = otp_version)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



get_einrichtungen_and_isochrone_fahrzeit<-function(long=12.117919921875002,lat=51.07333122768277,
                                                   gesuchter_typ=c("Supermarkt"),
                                                   max_fahrzeit=600,
                                                   mode="CAR")
{
  
  available_modes<-c("TRANSIT", "WALK",
                     "BICYCLE",
                     "CAR", "BUS",
                     "RAIL", "SUBWAY",
                     "TRAM", "FERRY", "BICYCLE_RENT", "BICYCLE_PARK",
                     "CAR_PARK", "CABLE_CAR", "GONDOLA", "FUNICULAR", "AIRPLANE")
  
  
  if(mode %in% available_modes==F)
  {
    return(list(error="Mode is not available"))
  }
  
  x<- httr::GET("http://otp15.bunsencloud.de/otp/routers/default") %>%
    content(.,as="text") %>% fromJSON() %>%
    keep(names(.) %in% c("polygon"))
  
  extent<-x$polygon$coordinates %>% matrix(.,ncol=2) %>% list() %>%
    st_polygon() %>% st_sfc(.,crs=4326)
  
  
  fromPlace =  st_point(as.numeric(c(long,lat)))
  
  fromPlace<-st_sfc(fromPlace,crs=4326) %>% st_sf() %>%
    mutate(fromPlaceID="Starting_Point")
  
  #is starting point in extent?
  
  if(!st_intersects(fromPlace,extent,sparse = F)[1])
  {
    
    
    return(list(error="Starting Point "))
    
    
  }else{
    
    ## 1. Isochrone berechnen ----
    
    isochrone<-opentripplanner::otp_isochrone(otpcon,
                                              fromPlace = fromPlace,
                                              mode = mode,
                                              cutoffSec=c(max_fahrzeit))
    
    
    con <- mongo(db="raeumliche_analysen",collection="einrichtungen", url=url)
    
    x<-con$find()
    
    geom<-x$geometry
    sfc <- lapply(geom$coordinates, sf::st_point)
    sfc <- sf::st_sfc(sfc)
    x$geometry<-sfc
    einrichtungen<-st_sf(x,crs=4326) %>%
      filter(Typ %in% gesuchter_typ)
    
    if(nrow(einrichtungen)==0)
    {
      return(list(error=paste0("Es gibt keinen Einrichtungstypen ",gesuchter_typ)))
    }
    
    einrichtungen<-einrichtungen %>%
      mutate(Einrichtungs_ID=1:nrow(einrichtungen)) %>%
      mutate(Einrichtungs_ID=as.character(Einrichtungs_ID))
    
    einrichtungen_within_isochrone<-st_intersection(einrichtungen,isochrone)
    
    
    if(nrow(einrichtungen_within_isochrone)>1)
    {
      routes <- otp_plan(otpcon = otpcon,
                         fromPlace = fromPlace,
                         toPlace = einrichtungen_within_isochrone,
                         fromID = fromPlace$fromPlaceID,
                         toID = einrichtungen_within_isochrone$Einrichtungs_ID,
                         get_geometry = FALSE,
                         distance_balance = TRUE,
                         ncores = 9)
    } else if(nrow(einrichtungen_within_isochrone)==1) {
      routes <- otp_plan(otpcon,
                         fromPlace =fromPlace,
                         toPlace = einrichtungen_within_isochrone,
                         toID = einrichtungen_within_isochrone$Einrichtungs_ID,
                         get_geometry = FALSE,
                         ncores = 9)
      
      resp[[1]]<-routes[,c("fromPlace","toPlace","duration")] %>%
        arrange(duration) %>%
        left_join(.,
                  (einrichtungen_within_isochrone %>% select(Kategorie,Typ,Name,Einrichtungs_ID)),
                  by=c("toPlace"="Einrichtungs_ID")) %>%
        dplyr::rename(Einrichtungs_ID=toPlace) %>%
        select(-fromPlace)
      
      resp[[2]]<-isochrone
      
      return(resp)
      
    }
    else{
      resp<-list()
      
      resp[[1]]<-"Keine Einrichtungstypen gefunden"
      resp[[2]]<-isochrone
      
      return(resp)
    }
    
  }
  
}

parse_surface <- function(x, get_data){
  response <- list()
  if(get_data){
    dat <- x$data
    dat <- unlist(dat, recursive = FALSE)
    dat <- list2df(dat)
    dat$minutes <- seq(1, nrow(dat))
    response$data <- dat
  }
  
  times <- x$times
  times[times == 2147483647] <- NA
  
  response$times <- times
  return(response)
}

list2df <- function(l) {
  class(l) <- "data.frame"
  attr(l, "row.names") <- .set_row_names(length(l[[1]]))
  l
}

otp_isochrone<-function (otpcon = NA, fromPlace = NA, fromID = NULL, mode = "CAR", 
          date_time = Sys.time(), arriveBy = FALSE, maxWalkDistance = 1000, 
          routingOptions = NULL, cutoffSec = c(600, 1200, 1800, 2400, 
                                               3000, 3600), ncores = max(round(parallel::detectCores() * 
                                                                                 1.25) - 1, 1), timezone = otpcon$timezone) 
{
  if (!is.null(otpcon$otp_version)) {
    if (otpcon$otp_version >= 2) {
      stop("Isochrones are not supported by OTP v2.X")
    }
  }
  checkmate::assert_numeric(ncores, lower = 1, len = 1, upper = max(c(round(parallel::detectCores() * 
                                                                              1.25) - 1, 2)))
  checkmate::assert_class(otpcon, "otpconnect")
  fromPlace <- otp_clean_input(fromPlace, "fromPlace")
  mode <- toupper(mode)
  checkmate::assert_subset(mode, choices = c("TRANSIT", "WALK", 
                                             "BICYCLE", "CAR", "BUS", "RAIL", "SUBWAY", "TRAM", "FERRY", 
                                             "BICYCLE_RENT", "BICYCLE_PARK", "CAR_PARK", "CABLE_CAR", 
                                             "GONDOLA", "FUNICULAR", "AIRPLANE"), empty.ok = FALSE)
  mode <- paste(mode, collapse = ",")
  checkmate::assert_posixct(date_time)
  date <- format(date_time, "%m-%d-%Y", tz = timezone)
  time <- tolower(format(date_time, "%I:%M%p", tz = timezone))
  checkmate::assert_numeric(cutoffSec, lower = 0)
  checkmate::assert_logical(arriveBy)
  arriveBy <- tolower(as.character(arriveBy))
  fromPlace <- format(fromPlace, scientific = FALSE, digits = 9, 
                      trim = TRUE)
  fromPlace <- paste0(fromPlace[, 1], "%2C", fromPlace[, 2])
  if (!is.null(fromID)) {
    if (length(fromID) != length(fromPlace)) {
      stop("The length of fromID and fromPlace are not the same")
    }
  }
  routerUrl <- make_url(otpcon)
  routerUrl <- paste0(routerUrl, "/isochrone")
  query <- list(mode = mode, date = date, time = time, maxWalkDistance = maxWalkDistance, 
                arriveBy = arriveBy)
  cutoffSec <- as.list(cutoffSec)
  names(cutoffSec) <- rep("cutoffSec", length(cutoffSec))
  query <- c(query, cutoffSec)
  if (!is.null(routingOptions)) {
    query <- c(query, routingOptions)
  }
  urls <- build_urls(routerUrl, fromPlace, toPlace = NULL, 
                     query)
  message(Sys.time(), " sending ", length(urls), " isochrone requests using ", 
          ncores, " threads")
  progressr::handlers("cli")
  results <- progressr::with_progress(otp_async(urls, ncores, 
                                                TRUE))
  if (is.null(fromID)) {
    fromID <- gsub("%2C", ",", fromPlace, fixed = TRUE)
  }
  results_sf <- purrr::map2(results, fromID, otp_process_results_iso)
  results_sf <- data.table::rbindlist(results_sf, use.names = TRUE)
  if (nrow(results_sf) > 0) {
   # results_sf$geometry <- sf::st_sfc(results_sf$geometry, 
    #                                  recompute_bbox = TRUE)
    results_sf <- sf::st_as_sf(results_sf)
  }
  else {
    stop("No results returned, check your connection")
  }
  return(results_sf)
}


#' Process results
#'
#' @param text the text returned by OTP
#' @param fromID passed from main func
#' @family internal
#' @noRd

otp_process_results_iso <- function(text, fromID){
  
  response <- try(sf::st_read(text, quiet = TRUE), silent = TRUE)
  if(inherits(response, "try-error")){
    warning("Isochrone failed: ",text)
    return(NULL)
  }
  
  response$id <- seq(1, nrow(response))
  response$fromPlace <- fromID
  
  if (any(!sf::st_is_valid(response))) {
    suppressMessages(suppressWarnings(response <- sf::st_make_valid(response)))
  }
  
  
  response <- response[!sf::st_is_empty(response),]
  if(nrow(response) == 0){
    warning("Isochrone had empty geometry ")
    return(NULL)
  } else {
    response <- sf::st_cast(response, "MULTIPOLYGON")
  }
  
  
  return(response)
  
}

