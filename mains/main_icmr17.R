### load data and config ###

## load required packages and functions ##

source("./lib/dependencies.R")
source("./lib/loadData.R")
source("./lib/recLibrary.R")
source("./lib/createPlots.R")

# libfm path
lfmpath <- "./dependencies/libfm/bin"

## config for recsys evaluation ##

set.seed(1234)
nrCores = 5
nrFolds = 5
dims = 5
evalPercentage = 1.0 # percentage value of the data to evaluate
reload = TRUE # re-load data?
file = "../clusteredDataset23.csv"
eval = "user" # evaluate each user and compute average

## config for eval  ##

maxRecs = 100 # maximum number of recommendations for the evluation
digits = 3 # round the rating prediction measures to 3 digits

# set eval type to split to whole dataset into k folds (getDataFold) or the user profile into k folds (getUserFold)

switch (eval,
        data = {getFold = getDataFold},
        user = {getFold = getUserFold}
)

## load data ##

if (reload == TRUE) {
  allData <- loadData(file)
  allData <- unique(allData)
  save(allData, file="evalDataICMR.Rdata")
} else {
  load("evalDataICMR.Rdata")
}

users <- sample(allData$user,round(length(unique(allData$user))*evalPercentage,0))
testData <- allData[allData$user %in% users,]

### evaluate classifier-based recommender systems ###

## start factorization machine based classifier
model = "interaction ~ user + track + cluster"
start.time =  Sys.time()
predDF_fm <- e$startFMmc(testData, dims, model)
end.time <- Sys.time()
time.taken <- end.time - start.time
writeLines(paste0("Computing FM took: ", time.taken))
colnames(predDF_fm) <-c("fm","user","track","cluster","interaction")
predDF_fm$id <- 1:nrow(predDF_fm)
predDF_fm <- predDF_fm[,c(6,1,2,3,4,5)]

# convert factor to numeric
predDF_fm$interaction <- as.numeric(as.character(predDF_fm$interaction))
predDF_fm$fm <- as.numeric(as.character(predDF_fm$fm))

## start random forest based classifier
start.time =  Sys.time()
predDF_rf <- e$startRFmc(testData)
end.time <- Sys.time()
time.taken <- end.time - start.time
writeLines(paste0("Computing RF took: ", time.taken))
colnames(predDF_rf) <-c("rf","user","track","cluster","interaction")

# convert factor to numeric
predDF_rf$interaction <- as.numeric(as.character(predDF_rf$interaction))
predDF_rf$rf <- as.numeric(as.character(predDF_rf$rf))

### evaluate cf-based recommender system ###

UBCF_ratings <- e$startCFmc(testData[testData$interaction==1,-3], 30)
SVD_ratings  <- e$startSVDmc(testData[testData$interaction==1,-3], 10)
UBCF_ratings <- unique(UBCF_ratings)
SVD_ratings  <- unique(SVD_ratings)

### evaluate pre-filtering-based recommender systems ###

all_pre_UBCF_ratings <- data.frame()
all_pre_SVD_ratings <- data.frame()

for (c in unique(testData$cluster)) {
  filtered_testData <- testData[testData$cluster==c & testData$interaction==1,-3]
  if (length(unique(filtered_testData$user)) < 5 | 
      length(unique(filtered_testData$track)) < 5) {
    print(paste0("Skipping Cluster ",c))
    next
  }
  
  print(paste0("Computing CF results for Cluster ",c))
  pre_UBCF_ratings <- e$startCFmc(filtered_testData, 30)
  pre_UBCF_ratings$cluster <- as.numeric(c)
  all_pre_UBCF_ratings <- rbind(all_pre_UBCF_ratings,pre_UBCF_ratings)
  rm(pre_UBCF_ratings)

  print(paste0("Computing SVD results for Cluster ",c))
  pre_SVD_ratings <- e$startSVDmc(filtered_testData, 10)
  pre_SVD_ratings$cluster <- as.numeric(c)
  all_pre_SVD_ratings  <- rbind(all_pre_SVD_ratings,pre_SVD_ratings)
  rm(pre_SVD_ratings)

  
}

colnames(all_pre_UBCF_ratings) <- c("user","track","PreCFrating","cluster")
colnames(all_pre_SVD_ratings)  <- c("user","track","PreSVDrating","cluster")


### merge all results into an overall result dataframe ###

## add random forest results 
fullDF <- merge(predDF_fm,predDF_rf,all.x=TRUE,sort=FALSE)

## add CF results 
fullDF <- merge(fullDF,UBCF_ratings, all.x=TRUE, by=c("user","track"), sort=FALSE)
fullDF[is.na(fullDF$CFrating),]$CFrating <- 0

## add SVD results 
fullDF <- merge(fullDF,SVD_ratings,  all.x=TRUE, by=c("user","track"), sort=FALSE)
fullDF[is.na(fullDF$SVDrating),]$SVDrating <- 0

## add pre-filtering CF results 
fullDF <- merge(fullDF,all_pre_UBCF_ratings, all.x=TRUE, by=c("user","track","cluster"), sort=FALSE)
fullDF[is.na(fullDF$PreCFrating),]$PreCFrating <- 0

## add pre-filtering SVD results 
fullDF <- merge(fullDF,all_pre_SVD_ratings,  all.x=TRUE, by=c("user","track","cluster"), sort=FALSE)
fullDF[is.na(fullDF$PreSVDrating),]$PreSVDrating <- 0

fullDF <- fullDF[order(fullDF$id),] 

### range 0 and 1 ###

fullDF$CFrating <- range01(fullDF$CFrating)
fullDF$SVDrating <- range01(fullDF$SVDrating)
fullDF$PreCFrating <- range01(fullDF$PreCFrating)
fullDF$PreSVDrating <- range01(fullDF$PreSVDrating)

### evaluation of predicted ratings ###

writeLines("Collaborative Filtering Rating Prediction")
rpm_all <- computeRPM(fullDF$CFrating, fullDF$interaction)
rpm_top50 <- computeRPM(fullDF[order(-fullDF$CFrating),]$CFrating[1:50], fullDF[order(-fullDF$CFrating),]$interaction[1:50])
rpm_positive <- computeRPM(fullDF[fullDF$CFrating>0.5,]$CFrating, fullDF[fullDF$CFrating>0.5,]$interaction)
writeLines(paste0("RMSE: ",round(rpm_positive[1],digits)))
writeLines(paste0("MAD:  ",round(rpm_positive[2],digits)))
writeLines(paste0("MAPE: ",round(rpm_positive[3],digits)))
writeLines("#################################")
writeLines("Collaborative Filtering Rating Prediction with Pre-filtering")
rpm_all <- computeRPM(fullDF$PreCFrating, fullDF$interaction)
rpm_top50 <- computeRPM(fullDF[order(-fullDF$PreCFrating),]$PreCFrating[1:50], fullDF[order(-fullDF$PreCFrating),]$interaction[1:50])
rpm_positive <- computeRPM(fullDF[fullDF$PreCFrating>0.5,]$PreCFrating, fullDF[fullDF$PreCFrating>0.5,]$interaction)
writeLines(paste0("RMSE: ",round(rpm_positive[1],digits)))
writeLines(paste0("MAD:  ",round(rpm_positive[2],digits)))
writeLines(paste0("MAPE: ",round(rpm_positive[3],digits)))
writeLines("#################################")
writeLines("SVD Rating Prediction")
rpm_all <- computeRPM(fullDF$SVDrating, fullDF$interaction)
rpm_top50 <- computeRPM(fullDF[order(-fullDF$SVDrating),]$SVDrating[1:50], fullDF[order(-fullDF$SVDrating),]$interaction[1:50])
rpm_positive <- computeRPM(fullDF[fullDF$SVDrating>0.5,]$SVDrating, fullDF[fullDF$SVDrating>0.5,]$interaction)
writeLines(paste0("RMSE: ",round(rpm_positive[1],digits)))
writeLines(paste0("MAD:  ",round(rpm_positive[2],digits)))
writeLines(paste0("MAPE: ",round(rpm_positive[3],digits)))
writeLines("#################################")
writeLines("SVD Rating Prediction with Pre-filtering")
rpm_all <- computeRPM(fullDF$PreSVDrating, fullDF$interaction)
rpm_top50 <- computeRPM(fullDF[order(-fullDF$PreSVDrating),]$PreSVDrating[1:50], fullDF[order(-fullDF$PreSVDrating),]$interaction[1:50])
rpm_positive <- computeRPM(fullDF[fullDF$PreSVDrating>0.5,]$PreSVDrating, fullDF[fullDF$PreSVDrating>0.5,]$interaction)
writeLines(paste0("RMSE: ",round(rpm_positive[1],digits)))
writeLines(paste0("MAD:  ",round(rpm_positive[2],digits)))
writeLines(paste0("MAPE: ",round(rpm_positive[3],digits)))
writeLines("#################################")
writeLines("Random Forest Rating Prediction")
rpm_all <- computeRPM(fullDF$rf, fullDF$interaction)
rpm_top50 <- computeRPM(fullDF[order(-fullDF$rf),]$rf[1:50], fullDF[order(-fullDF$rf),]$interaction[1:50])
rpm_positive <- computeRPM(fullDF[fullDF$rf>0.5,]$rf, fullDF[fullDF$rf>0.5,]$interaction)
writeLines(paste0("RMSE: ",round(rpm_positive[1],digits)))
writeLines(paste0("MAD:  ",round(rpm_positive[2],digits)))
writeLines(paste0("MAPE: ",round(rpm_positive[3],digits)))
writeLines("#################################")
writeLines("FM Prediction")
rpm_all <- computeRPM(fullDF$fm, fullDF$interaction)
rpm_top50 <- computeRPM(fullDF[order(-fullDF$fm),]$fm[1:50], fullDF[order(-fullDF$fm),]$interaction[1:50])
rpm_positive <- computeRPM(fullDF[fullDF$fm>0.5,]$fm, fullDF[fullDF$fm>0.5,]$interaction)
writeLines(paste0("RMSE: ",round(rpm_positive[1],digits)))
writeLines(paste0("MAD:  ",round(rpm_positive[2],digits)))
writeLines(paste0("MAPE: ",round(rpm_positive[3],digits)))

### top-n evaluations ###

o = "performance"
fm_result <- computeTopNperUser(fullDF[,c(6,1,2,3,4)],1,o)
rf_result <- computeTopNperUser(fullDF[,c(7,1,2,3,4)],1,o)
cf_result <- computeTopNperUser(fullDF[,c(8,1,2,3,4)],1,o)
svd_result <- computeTopNperUser(fullDF[,c(9,1,2,3,4)],1,o)
prcf_result <- computeTopNperUser(fullDF[,c(10,1,2,3,4)],1,o)
prsvd_result <- computeTopNperUser(fullDF[,c(11,1,2,3,4)],1,o)


### overall top-n evaluations ###

result <- rbind(fm_result,rf_result,cf_result,prcf_result,svd_result,prsvd_result)
result$algorithm <- c(rep("FM",maxRecs),
                      rep("RF",maxRecs),
                      rep("CF",maxRecs),
                      rep("PR_CF",maxRecs),
                      rep("SVD",maxRecs),
                      rep("PR_SVD",maxRecs))

prePlot <- createPlot(result,"precision")
recPlot <- createPlot(result,"recall")
f1Plot  <- createPlot(result,"f1")
prePlot
recPlot
f1Plot

### result table ###

## overall precision/recall/f1
overall_result <- aggregate(. ~ algorithm, data=result, FUN=mean)[,-2]
overall_result[order(-overall_result$f1),]

## precision/recall/f1@n
result[result$at %in% c(1, 5, 10, 20, 25, 50),]