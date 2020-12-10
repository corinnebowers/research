require(RCurl)

url_list <- list()
url_list[[1]] <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/USDM_Products/soil/total/daily/"
url_list[[2]] <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/USDM_Products/soil/anom/daily/"
url_list[[3]] <- "ftp://ftp.cpc.ncep.noaa.gov/GIS/USDM_Products/soil/percentile/daily/"

root <- list()
root[[1]] <-"C:/Users/cbowers/Desktop/Research/_data/soilmoisture/total/"
root[[2]] <-"C:/Users/cbowers/Desktop/Research/_data/soilmoisture/anomaly/"
root[[3]] <-"C:/Users/cbowers/Desktop/Research/_data/soilmoisture/percentile/"

for (i in 1:3) {
  url <- url_list[[i]]
  filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(filenames, "\r\n")
  filenames <- unlist(filenames)
  
  setwd(root[[i]])
  for (filename in filenames) {
    fileloc <- paste('.', filename, sep = '/')
    download.file(paste(url, filename, sep = ""), fileloc)
    unzip(fileloc, overwrite = TRUE, exdir = gsub('.zip', '', fileloc))
  }
  file.remove(paste('.', list.files(path = '.', pattern = '.zip'), sep = '/'))
}
            