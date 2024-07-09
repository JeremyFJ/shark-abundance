get_inat_obs2 <- function(query = NULL, taxon_name = NULL, taxon_id = NULL,
                         place_id = NULL, quality = NULL, geo = NULL,
                         annotation = NULL, year = NULL, month = NULL,
                         day = NULL, bounds = NULL, start_date = NULL, end_date = NULL,
                         maxresults = 100, meta = FALSE) {
  
  if (!curl::has_internet()) {
    message("No Internet connection.")
    return(invisible(NULL))
  }
  
  base_url <- "http://www.inaturalist.org/"
  if (httr::http_error(base_url)) {
    message("iNaturalist API is unavailable.")
    return(invisible(NULL))
  }
  
  search <- ""
  
  if(!is.null(query)){
    search <- paste0(search, "&q=", gsub(" ", "+", query))
  }
  
  if(!is.null(quality)){
    if(!quality %in% c("casual", "research")){
      stop("Please enter a valid quality flag, 'casual' or 'research'.")
    }
    search <- paste0(search, "&quality_grade=", quality)
  }
  
  if(!is.null(taxon_name)){
    search <- paste0(search, "&taxon_name=", gsub(" ", "+", taxon_name))
  }
  
  if(!is.null(taxon_id)){
    search <- paste0(search, "&taxon_id=", taxon_id)
  }
  
  if(!is.null(place_id)){
    search <- paste0(search, "&place_id=", place_id)
  }
  
  if(!is.null(geo) && geo == TRUE){
    search <- paste0(search, "&has[]=geo")
  }
  
  if(!is.null(annotation)){
    if(length(annotation) != 2){
      stop("Annotation needs to be a vector of length 2.")
    }
    search <- paste0(search, "&term_id=", annotation[1], "&term_value_id=", annotation[2])
  }
  
  if(!is.null(year)){
    search <- paste0(search, "&year=", year)
  }
  
  if(!is.null(month)){
    search <- paste0(search, "&month=", month)
  }
  
  if(!is.null(day)){
    search <- paste0(search, "&day=", day)
  }
  
  if(!is.null(bounds)){
    search <- paste0(search, "&swlat=", bounds[1], "&swlng=", bounds[2],
                     "&nelat=", bounds[3], "&nelng=", bounds[4])
  }
  
  # Adding start_date and end_date to the search query
  if (!is.null(start_date)) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", start_date)) {
      stop("Start date must be in YYYY-MM-DD format.")
    }
    search <- paste0(search, "&d1=", start_date)
  }
  if (!is.null(end_date)) {
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", end_date)) {
      stop("End date must be in YYYY-MM-DD format.")
    }
    search <- paste0(search, "&d2=", end_date)
  }
  
  if (maxresults > 10000) {
    stop("Please provide a maxresults value <= 10000.")
  }
  
  q_path <- "observations.csv"
  ping_path <- "observations.json"
  ping_query <- paste0(search, "&per_page=1&page=1")
  
  # Initial ping to get the total number of results
  ping <- httr::GET(url = paste0(base_url, ping_path), query = ping_query)
  total_res <- as.numeric(httr::headers(ping)[["x-total-entries"]])
  
  if(total_res == 0){
    stop("Your search returned zero results. Either your species of interest has no records or you entered an invalid search.")
  } else if(total_res > 200000) {
    stop("Your search returned too many results, please consider breaking it up into smaller chunks by year or month.")
  }
  
  data_out <- data.frame() # Initializing the output dataframe
  pages <- ceiling(min(maxresults, total_res) / 200)
  
  for (page in 1:pages) {
    page_query <- paste0(search, "&per_page=200&page=", page)
    data <- httr::GET(url = paste0(base_url, q_path), query = page_query)
    temp_data <- read.csv(text = httr::content(data, "text"), stringsAsFactors = FALSE)
    data_out <- rbind(data_out, temp_data)
    if (nrow(data_out) >= maxresults) {
      break
    }
  }
  
  if (nrow(data_out) > maxresults) {
    data_out <- data_out[1:maxresults, ]
  }
  
  if(meta){
    meta_info <- list(found = total_res, returned = nrow(data_out))
    return(list(meta = meta_info, data = data_out))
  } else {
    return(data_out)
  }
}
