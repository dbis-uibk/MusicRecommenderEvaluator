## this file contains all neded auxiliary functions ##

# create environment
e <- new.env()

# split dataset into k folds
getDataFold <- function(data,i,k) {
  folds <- cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
  result = list()
  testIndexes <- which(folds==i,arr.ind=TRUE)
  result[["train"]] <- data[-testIndexes, ]
  result[["test"]]  <- data[testIndexes, ]
  
  return(result)
}

# split dataset into k folds per user
getUserFold <- function(data,i,k) {
  result = list()
  data = data.table(data)
  for (u in unique(data$user))
  {
    userData = data[data$user==u,]
    folds <- cut(seq(1,nrow(userData)),breaks=k,labels=FALSE)
    testIndexes <- which(folds==i,arr.ind=TRUE)
    result[["train"]] <- rbind(result[["train"]], userData[-testIndexes, ])
    result[["test"]]  <- rbind(result[["test"]], userData[testIndexes, ])
  }
  
  return(result)
}

# start libfm 
e$startFMsc <- function(data, dimensions) {
  predFM = data.frame()
  for (i in 1:nrFolds) {
    predFM <- rbind(predFM,
                    cbind(prediction=libFM(getFold(data,i,nrFolds)$train,
                                           getFold(data,i,nrFolds)$test, 
                                           global_bias = TRUE,
                                           variable_bias = TRUE,
                                           interaction ~ user + track + cluster, 
                                           task = "c",
                                           dims = dimensions,
                                           method = "mcmc",
                                           iter = 1000,
                                           exe_loc = lfmpath),
                          test=getFold(data,i,nrFolds)$test$interaction))
  }
  return(predFM)
}

# start libfm for k folds parallel
e$startFMmc <- function(data, dimensions, model) {
  
  # init cluster
  cl <- makeCluster(nrCores)
  registerDoParallel(cl)
  
  # compute k folds in parallel
  predFM <-foreach(i=1:nrFolds, 
                   .packages = c("libFMexe","data.table"),
                   .export = c("getFold","nrFolds","lfmpath"),
                   .combine="rbind") %dopar% {
                     to.predFM = libFM(getFold(data,i,nrFolds)$train,
                                       getFold(data,i,nrFolds)$test, 
                                       global_bias = TRUE,
                                       variable_bias = TRUE,
                                       as.formula(model), 
                                       task = "c",
                                       dims = dimensions,
                                       method = "mcmc",
                                       iter = 1000, 
                                       exe_loc = lfmpath)
                     as.data.frame(cbind(prediction=to.predFM,test=getFold(data,i,nrFolds)$test))
                   }
  
  stopCluster(cl)
  return(predFM)
}

# start higher-order FM for k folds parallel
e$startHOFMmc <- function(data, model) {
  
  # init cluster
  cl <- makeCluster(nrCores)
  registerDoParallel(cl)
  
  # compute k folds in parallel
  predHOFM <-foreach(i=1:nrFolds, 
                     .packages = c("FactoRizationMachines","data.table"),
                     .export = c("getFold","nrFolds"),
                     .combine="rbind") %dopar% {
                       to.model=HoFM.train(getFold(data,i,nrFolds)$train[,-c("interaction")],
                                           as.numeric(as.character(getFold(data,i,nrFolds)$train[,c("interaction")]$interaction)),
                                           model,
                                           iter = 1000, 
                                           regular = 0,
                                           intercept = TRUE)
                       to.predHOFM = predict(to.model,getFold(data,i,nrFolds)$test[,-c("interaction")])
                       as.data.frame(cbind(prediction=to.predHOFM,test=getFold(data,i,nrFolds)$test))
                     }
  
  stopCluster(cl)
  return(predHOFM)
}

# start random forest classification for k folds in parallel
e$startRFmc <- function(data, dimensions) {
  
  # init cluster
  cl <- makeCluster(nrCores)
  registerDoParallel(cl)
  
  # combine random forests traing on different training sets
  
  rf_combine_chunked <- function (...) {
    pad0 <- function(x, len) c(x, rep(0, len - length(x)))
    padm0 <- function(x, len) rbind(x, matrix(0, nrow = len - 
                                                nrow(x), ncol = ncol(x)))
    rflist <- list(...)
    areForest <- sapply(rflist, function(x) inherits(x, "randomForest"))
    if (any(!areForest)) 
      stop("Argument must be a list of randomForest objects")
    rf <- rflist[[1]]
    classRF <- rf$type == "classification"
    trees <- sapply(rflist, function(x) x$ntree)
    ntree <- sum(trees)
    rf$ntree <- ntree
    nforest <- length(rflist)
    haveTest <- !any(sapply(rflist, function(x) is.null(x$test)))
    vlist <- lapply(rflist, function(x) rownames(importance(x)))
    numvars <- sapply(vlist, length)
    if (!all(numvars[1] == numvars[-1])) 
      stop("Unequal number of predictor variables in the randomForest objects.")
    for (i in seq_along(vlist)) {
      if (!all(vlist[[i]] == vlist[[1]])) 
        stop("Predictor variables are different in the randomForest objects.")
    }
    haveForest <- sapply(rflist, function(x) !is.null(x$forest))
    if (all(haveForest)) {
      nrnodes <- max(sapply(rflist, function(x) x$forest$nrnodes))
      rf$forest$nrnodes <- nrnodes
      rf$forest$ndbigtree <- unlist(sapply(rflist, function(x) x$forest$ndbigtree))
      rf$forest$nodestatus <- do.call("cbind", lapply(rflist, 
                                                      function(x) padm0(x$forest$nodestatus, nrnodes)))
      rf$forest$bestvar <- do.call("cbind", lapply(rflist, 
                                                   function(x) padm0(x$forest$bestvar, nrnodes)))
      rf$forest$xbestsplit <- do.call("cbind", lapply(rflist, 
                                                      function(x) padm0(x$forest$xbestsplit, nrnodes)))
      rf$forest$nodepred <- do.call("cbind", lapply(rflist, 
                                                    function(x) padm0(x$forest$nodepred, nrnodes)))
      tree.dim <- dim(rf$forest$treemap)
      if (classRF) {
        rf$forest$treemap <- array(unlist(lapply(rflist, 
                                                 function(x) apply(x$forest$treemap, 2:3, pad0, 
                                                                   nrnodes))), c(nrnodes, 2, ntree))
      }
      else {
        rf$forest$leftDaughter <- do.call("cbind", lapply(rflist, 
                                                          function(x) padm0(x$forest$leftDaughter, nrnodes)))
        rf$forest$rightDaughter <- do.call("cbind", lapply(rflist, 
                                                           function(x) padm0(x$forest$rightDaughter, nrnodes)))
      }
      rf$forest$ntree <- ntree
      if (classRF) 
        rf$forest$cutoff <- rflist[[1]]$forest$cutoff
    }
    else {
      rf$forest <- NULL
    }
    
    if (classRF) {
      rf$confusion <- NULL
      rf$err.rate <- NULL
      if (haveTest) {
        rf$test$confusion <- NULL
        rf$err.rate <- NULL
      }
    }
    else {
      rf$mse <- rf$rsq <- NULL
      if (haveTest) 
        rf$test$mse <- rf$test$rsq <- NULL
    }
    rf
  }
  
  # compute k folds in parallel
  predRF <-foreach(i=1:nrFolds, 
                   .packages = c("randomForest","data.table","ff"),
                   .export = c("getFold","nrFolds"),
                   .combine="rbind") %dopar% {
                     for (j in chunk(from=1, to=nrow(getFold(data,i,nrFolds)$train), by=100000)) {
                       rf.chunk = randomForest(interaction ~ user + track + cluster, 
                                               getFold(data,i,nrFolds)$train[min(j):max(j), ],
                                               ntree=500)
                       if(exists("rf.model")) {
                         rf.model <- rf_combine_chunked(rf.model,rf.chunk)
                       } else {
                         rf.model <- rf.chunk
                       }
                     }
                     
                     to.predRF = predict(rf.model, as.data.frame(getFold(data,i,nrFolds)$test)[,-4], type="prob")[,c("1")]
                     as.data.frame(cbind(prediction=to.predRF,test=getFold(data,i,nrFolds)$test))
                   }
  stopCluster(cl)
  return(predRF)
}

# start cf recommendation computation for k fold in parallel
e$startCFmc <- function(data, n) {
  
  # remove dublicate rows
  data <- unique(data)
  
  # init cluster
  cl <- makeCluster(nrCores)
  registerDoParallel(cl)
  
  # compute k folds in parallel
  predCF <- foreach(i=1:nrFolds,
                    .packages = c("recommenderlab","data.table"),
                    .export = c("getFold","nrFolds"),
                    .combine="rbind") %dopar% {
                      cf_trainData <- getFold(data,i,nrFolds)$train[interaction==as.factor(1)]
                      cf_testData  <- getFold(data,i,nrFolds)$test[interaction==as.factor(1)]
                      
                      if (max(cf_trainData$track) > max(cf_testData$track)) {
                        a <- data.table(cbind(1,max(cf_trainData$track),1))
                        colnames(a) <- colnames(cf_testData)
                        cf_testData  <- rbind(cf_testData,a) 
                      } else if (max(cf_testData$track) > max(cf_trainData$track)) {
                        a <- data.table(cbind(1,max(cf_testData$track),1))
                        colnames(a) <- colnames(cf_trainData)
                        cf_trainData  <- rbind(cf_trainData,a)
                      }
                      
                      ratingMatrix = sparseMatrix(cf_trainData$user,
                                                  cf_trainData$track,
                                                  x = (as.integer(cf_trainData$interaction)))
                      r <- as(ratingMatrix, "realRatingMatrix")
                      r <- r[unique(cf_trainData$user),unique(c(cf_trainData$track,cf_testData$track))]
                      rownames(r) <- unique(cf_trainData$user)
                      colnames(r) <- unique(c(cf_trainData$track,cf_testData$track))
                      
                      ratingMatrix = sparseMatrix(cf_testData$user,
                                                  cf_testData$track, 
                                                  x = (as.integer(cf_testData$interaction)))
                      tr <- as(ratingMatrix, "realRatingMatrix")
                      tr <- tr[unique(cf_testData$user),unique(c(cf_trainData$track,cf_testData$track))]
                      rownames(tr) <- unique(cf_testData$user)
                      colnames(tr) <- unique(c(cf_trainData$track,cf_testData$track))
                      
                      UBCF_recommender <- Recommender(r, "UBCF", param=list(nn=n))
                      UBCF_predict     <- predict(UBCF_recommender, tr, type="ratingMatrix")
                      UBCF_ratings <- as.matrix(UBCF_predict@data)
                      
                      UBCF_ratings <- melt(UBCF_ratings)
                      colnames(UBCF_ratings) <- c("user","track","CFrating")
                      UBCF_ratings <- as.data.table(UBCF_ratings)
                      
                      r_UBCF_ratings <- data.table()
                      for (u in unique(UBCF_ratings$user)) {
                        tempDT <- UBCF_ratings[user==u]
                        setorder(tempDT,-CFrating)
                        tempDT <- tempDT[1:nrow(cf_testData[cf_testData$user==u,]),]
                        r_UBCF_ratings <- rbind(r_UBCF_ratings,tempDT)
                      } 
                      r_UBCF_ratings
                    }
  stopCluster(cl)
  return(predCF)
}

# start cf recommendation computation for k folds in parallel
e$startSVDmc <- function(data, n) {
  
  # remove dublicate rows
  data <- unique(data)
  
  # init cluster
  cl <- makeCluster(nrCores)
  registerDoParallel(cl)
  
  # compute k folds in parallel
  predSVD <- foreach(i=1:nrFolds,
                     .packages = c("recommenderlab","data.table"),
                     .export = c("getFold","nrFolds"),
                     .combine="rbind") %dopar% {
                       cf_trainData <- getFold(data,i,nrFolds)$train[interaction==as.factor(1)]
                       cf_testData  <- getFold(data,i,nrFolds)$test[interaction==as.factor(1)]
                       
                       if (max(cf_trainData$track) > max(cf_testData$track)) {
                         a <- data.table(cbind(1,max(cf_trainData$track),1))
                         colnames(a) <- colnames(cf_testData)
                         cf_testData  <- rbind(cf_testData,a) 
                       } else if (max(cf_testData$track) > max(cf_trainData$track)) {
                         a <- data.table(cbind(1,max(cf_testData$track),1))
                         colnames(a) <- colnames(cf_trainData)
                         cf_trainData  <- rbind(cf_trainData,a)
                       }
                       
                       ratingMatrix = sparseMatrix(cf_trainData$user,
                                                   cf_trainData$track,
                                                   x = (as.integer(cf_trainData$interaction)))
                       r <- as(ratingMatrix, "realRatingMatrix")
                       r <- r[unique(cf_trainData$user),unique(c(cf_trainData$track,cf_testData$track))]
                       rownames(r) <- unique(cf_trainData$user)
                       colnames(r) <- unique(c(cf_trainData$track,cf_testData$track))
                       
                       ratingMatrix = sparseMatrix(cf_testData$user,
                                                   cf_testData$track, 
                                                   x = (as.integer(cf_testData$interaction)))
                       tr <- as(ratingMatrix, "realRatingMatrix")
                       tr <- tr[unique(cf_testData$user),unique(c(cf_trainData$track,cf_testData$track))]
                       rownames(tr) <- unique(cf_testData$user)
                       colnames(tr) <- unique(c(cf_trainData$track,cf_testData$track))
                       
                       SVD_recommender <- Recommender(r, 
                                                      "SVDF", 
                                                      param=list(k = n,
                                                                 min_epochs = 50, 
                                                                 max_epochs = 1000))
                       SVD_predict <- predict(SVD_recommender, tr, type="ratingMatrix")
                       SVD_ratings <- as.matrix(SVD_predict@data)
                       SVD_ratings <- melt(SVD_ratings)
                       colnames(SVD_ratings) <- c("user","track","SVDrating")
                       
                       SVD_ratings <- as.data.table(SVD_ratings)
                       r_SVD_ratings <- data.table()
                       for (u in unique(SVD_ratings$user)) {
                         tempDT <- SVD_ratings[user==u]
                         setorder(tempDT,-SVDrating)
                         tempDT <- tempDT[1:nrow(cf_testData[cf_testData$user==u,]),]
                         r_SVD_ratings <- rbind(r_SVD_ratings,tempDT)
                       } 
                       r_SVD_ratings
                     }
  stopCluster(cl)
  return(predSVD)
}

# compute ir measures based on a ct
computeIRM <- function(ct) {
  tp = ct["1","1"]
  fp = ct["1","0"]
  tn = ct["0","0"]
  fn = ct["0","1"]
  
  precision = tp/(tp+fp)
  recall = tp/(tp+fn)
  accuracy = (tp+tn)/(tp+tn+fp+fn)
  f1 = 2*(precision*recall)/(precision+recall)
  return(c(precision,recall,f1,accuracy))
}

# compute rating prediction error measures

computeRPM <- function(predicted, actual) {
  rmse <- sqrt(sum(((predicted - as.numeric(as.character(actual)))^2) , na.rm = TRUE)/length(predicted))
  mae  <- sum(abs(predicted - as.numeric(as.character(actual))), na.rm = TRUE)/length(predicted)
  mape <- sum(abs(((as.numeric(as.character(actual))+1)-(predicted+1))/(as.numeric(as.character(actual))+1)), na.rm = TRUE)/length(predicted)
  return(c(rmse,mae,mape))
}

## compute top-n recommendation evaluation over whole DF

computeTopN <- function(predFM,c,recallComp=1,order="") {
  pb <- txtProgressBar(min = 0, max = maxRecs, style = 3)
  
  start.time =  Sys.time()
  evalDF <- data.table(predFM[,c(2,3,4,5,c)])
  result <- data.frame()
  fold_results <- data.frame()
  colnames(evalDF) <- c("user","track","cluster","interaction","prediction")
  
  tempData = data.frame()
  for (i in 1:nrFolds) {
    l = (i-1)*(nrow(testData)/nrFolds)+1
    u = i*nrow(testData)/nrFolds
    
    if (order == "performance") {
      tempData = rbind(tempData,evalDF[l:u,][order(-evalDF[l:u,]$prediction),][1:maxRecs,])
    } else if (order == "random") {
      tempData = rbind(tempData,evalDF[l:u,][sample(length(l:u)),][1:maxRecs,])
    }
  }
  
  fromBins = seq(1,nrow(tempData),maxRecs)
  
  for (at in c(1:maxRecs)) {
    # create bins for each user
    bins = c()
    for (i in 1:length(fromBins)){
      bins = c(bins,seq(fromBins[i],fromBins[i]+at-1))
      ct <- table(ifelse(tempData$prediction[seq(fromBins[i],fromBins[i]+at-1)]>=0.5,1,0),tempData$interaction[seq(fromBins[i],fromBins[i]+at-1)])
      
      if (nrow(ct) < 2) {
        if (ncol(ct) < 2) {
          if("0" %in% rownames(ct)) {
            ct <- rbind(ct,c(0))
          } else {
            ct <- rbind(c(0),ct)
          }
          row.names(ct) <- c(0,1)
        } else
          if("0" %in% rownames(ct)) {
            ct <- rbind(ct,c(0,0))
          } else {
            ct <- rbind(c(0,0),ct)
          }
        row.names(ct) <- c(0,1)
      }
      
      if (ncol(ct) < 2) { 
        if("0" %in% colnames(ct)) {
          ct <- cbind(ct,c(0,0))
        } else {
          ct <- cbind(c(0,0),ct)
        }
        colnames(ct) <- c(0,1)
      }
      
      irm <- computeIRM(ct)
      irm[is.infinite(irm)] <- 0
      irm[is.nan(irm)] <- 0
      if (recallComp == 2) {
        # compute number of relevant items in the test set
        relevantItems <- length(tempData$interaction[seq(fromBins[i],fromBins[i]+maxRecs-1)][tempData$interaction[seq(fromBins[i],fromBins[i]+maxRecs-1)]>0])
        # compute recall 
        irm[2] <- ct["1","1"]/relevantItems
        # recompute f1
        irm[3] <- 2*(irm[1]*irm[2])/(irm[1]+irm[2])
      }
      fold_results <- rbind(fold_results,irm)
    }
    fold_results <- colMeans(fold_results)
    result <- rbind(result,c(at,fold_results))
    setTxtProgressBar(pb, at)
  }
  colnames(result) <- c("at","precision","recall","f1","accuracy")
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  close(pb)
  writeLines(paste0("  Computing IR Measures took: ", time.taken,"\n"))
  
  return(result)
}

## compute top-n recommendation evaluation per user

computeTopNperUser <- function(predFM,c,recallComp=1,order="") {
  pb <- txtProgressBar(min = 0, max = maxRecs, style = 3)
  
  start.time =  Sys.time()
  evalDF <- data.table(predFM[,c(2,3,4,5,c)])
  result <- data.frame()
  user_results <- data.frame()
  colnames(evalDF) <- c("user","track","cluster","interaction","prediction")
  evalDF$interaction <- as.numeric(as.character(evalDF$interaction))
  
  if (order == "random") {
    evalDF <- evalDF[sample(nrow(evalDF)),]
  } 
  
  # handle user with < maxRecs interactions?
  tempData = data.frame()
  
  for (u in unique(evalDF$user)) {
    if (order == "performance"){
      tempData = rbind(tempData,evalDF[evalDF$user==u,][order(-evalDF[evalDF$user==u,]$prediction),][1:maxRecs,])
    } else {
      tempData = rbind(tempData,evalDF[evalDF$user==u,][1:maxRecs,])
    }
  }
  
  fromBins = seq(1,nrow(tempData),maxRecs)
  
  for (at in c(1:maxRecs)) {
    # create bins for each user
    for (i in 1:length(fromBins)){
      ct <- table(ifelse(tempData$prediction[seq(fromBins[i],fromBins[i]+at-1)]>=0.5,1,0),tempData$interaction[seq(fromBins[i],fromBins[i]+at-1)])
      
      if (nrow(ct) < 2) {
        if (ncol(ct) < 2) {
          if("0" %in% rownames(ct)) {
            ct <- rbind(ct,c(0))
          } else {
            ct <- rbind(c(0),ct)
          }
          row.names(ct) <- c(0,1)
        } else
          if("0" %in% rownames(ct)) {
            ct <- rbind(ct,c(0,0))
          } else {
            ct <- rbind(c(0,0),ct)
          }
        row.names(ct) <- c(0,1)
      }
      
      if (ncol(ct) < 2) { 
        if("0" %in% colnames(ct)) {
          ct <- cbind(ct,c(0,0))
        } else {
          ct <- cbind(c(0,0),ct)
        }
        colnames(ct) <- c(0,1)
      }
      
      irm <- computeIRM(ct)
      if (recallComp == 2) {
        # compute number of relevant items in the test set
        relevantItems <- length(tempData$interaction[seq(fromBins[i],fromBins[i]+maxRecs-1)][tempData$interaction[seq(fromBins[i],fromBins[i]+maxRecs-1)]>0])
        # compute recall 
        irm[2] <- ct["1","1"]/relevantItems
        irm[3] <- irm[3] <- 2*(irm[1]*irm[2])/(irm[1]+irm[2])
      }
      irm[is.infinite(irm)] <- 0
      irm[is.nan(irm)] <- 0
      user_results <- rbind(user_results,irm)
    }
    user_results <- colMeans(user_results)
    result <- rbind(result,c(at,user_results))
    setTxtProgressBar(pb, at)
  }
  colnames(result) <- c("at","precision","recall","f1","accuracy")
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  close(pb)
  writeLines(paste0("  Computing IR Measures took: ", time.taken,"\n"))
  
  return(result)
}

## compute ir measures for each fold and average ##

computeIRMperFold <- function(predFM,c) {
  precision = recall = accuracy = f1 = 0
  for (i in 1:nrFolds) {
    l = (i-1)*length(getFold(testData,i,nrFolds)$test$interaction)+1
    u = i*length(getFold(testData,i,nrFolds)$test$interaction)
    ct <- table(ifelse(predFM[,c][l:u]>=0.5,1,0),predFM[,5][l:u])
    
    if (nrow(ct) < 2) {
      if (ncol(ct) < 2) {
        if("0" %in% rownames(ct)) {
          ct <- rbind(ct,c(0))
        } else {
          ct <- rbind(c(0),ct)
        }
        row.names(ct) <- c(0,1)
      } else
        if("0" %in% rownames(ct)) {
          ct <- rbind(ct,c(0,0))
        } else {
          ct <- rbind(c(0,0),ct)
        }
      row.names(ct) <- c(0,1)
    }
    
    if (ncol(ct) < 2) { 
      if("0" %in% colnames(ct)) {
        ct <- cbind(ct,c(0,0))
      } else {
        ct <- cbind(c(0,0),ct)
      }
      colnames(ct) <- c(0,1)
    }
    
    precision = precision + computeIRM(ct)[1]
    recall = recall + computeIRM(ct)[2]
    accuracy = accuracy + computeIRM(ct)[4]
  }
  
  precision = precision/nrFolds
  recall = recall/nrFolds
  f1 = 2*(precision*recall)/(precision+recall)
  accuracy = accuracy/nrFolds
  
  return(c(precision,recall,f1,accuracy))
}

## range standardization ##

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
