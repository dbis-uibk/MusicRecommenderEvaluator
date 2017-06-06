library("reshape2")

loadData <- function(path) {
  
  # load data from a <user,artist,track,cluster> csv file
  data <- read.csv(path)
  data <- data[,-2]
  colnames(data) <- c("user","track","cluster")
  data$interaction <- 1
  
  # create interaction = 0/1 for all <user,track,cluster>-pairs
  data <- dcast(data, user+track~cluster, value.var="interaction")
  data[is.na(data)] <- 0
  data <- melt(data, id.vars=c(1,2))
  colnames(data) <- c("user","track","cluster","interaction")
  
  # md5 to int and 0/1 to factor
  data$interaction <- as.factor(data$interaction)
  data$track <- as.numeric(data$track)
  data$user <- as.numeric(data$user)
  
  #return data ready for fm
  return(unique(data))
}

loadCBCCBFData<- function(path1,path2) {
  
  # load data from a <user,artist,track,cluster> csv file
  data <- read.csv(path1)
  data <- data[,-2]
  colnames(data) <- c("user","track","cluster")
  data$interaction <- 1
  
  # create interaction = 0/1 for all <user,track,cluster>-pairs
  data <- dcast(data, user+track~cluster, value.var="interaction")
  data[is.na(data)] <- 0
  data <- melt(data, id.vars=c(1,2))
  colnames(data) <- c("user","track","cluster","interaction")
  
  # load data from a <user,artist,track,cluster,cbf> csv file
  
  cbcdata <- fread(path2)
  cbcdata <- cbcdata[,-c(1,2)]
  
  colnames(cbcdata) <- c('track',
                         'cbc',
                         'acousticness',
                         'danceability',
                         'energy',
                         'instrumentalness',
                         'liveness',
                         'loudness',
                         'speechiness',
                         'tempo',
                         'valence')

  # merge with rec data
  data <- merge(data,cbcdata, by="track")
  
  # md5 to int and 0/1 to factor
  data$interaction <- as.factor(data$interaction)
  data$track <- as.numeric(data$track)
  data$user <- as.numeric(data$user)
  
  #return data ready for fm
  return(unique(data))
}

loadClusterData<- function(path1,path2) {
  
  # load data from a <user,track,cb-features> csv file
  data <- fread(path1)
  colnames(data) <- c('user',
                      'track',
                      'danceability',
                      'energy',
                      'speechiness',
                      'acousticness',
                      'instrumentalness',
                      'liveness',
                      'valence', 
                      'tempo')
  data <- unique(data)
  data$interaction <- 1
  
  # load data from a <user,country,cluster> csv file
  cdata <- read.csv(path2)
  
  # merge with rec data
  data <- merge(data,cdata, by="user")
  
  # md5 to int and 0/1 to factor
  data$interaction <- as.factor(data$interaction)
  data$track <- as.numeric(data$track)
  data$user <- as.numeric(data$user)
  
  #return data ready for fm
  return(unique(data))
}