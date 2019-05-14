###Marvel Hash Parameters Function

marvel_hash_params <- function() {
  
  ts <- round(as.numeric(Sys.time())*1000) # can totally be just Sys.time(), too
  public_key <- "1318532903321ecb1fe08f70c285ac5d"
  private_key <- "efba232189e63e19886950915edf0ce3f0c87468"
  to_hash <- sprintf("%s%s%s",
                     ts,
                     Sys.getenv("MARVEL_API_PRIVATE_KEY"),
                     Sys.getenv("MARVEL_API_PUBLIC_KEY"))
  
  list(
    ts=ts,
    hash=digest::digest(to_hash, "md5", FALSE),
    apikey=Sys.getenv("MARVEL_API_PUBLIC_KEY")
  )
  
}

###Marvel Get Character Info Function

get_character_info <- function(name) {
  
  params <- marvel_hash_params()
  params$name <- name
  
  res <- httr::GET("https://gateway.marvel.com:443/v1/public/characters",
                   query=params)
  
  info<- content(x = res,as = "text")
  
  info <- fromJSON(info,flatten = TRUE)
  
  info <- as.data.frame(info)
  
}