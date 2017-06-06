is.installed <- function(packageName) {
  installedPackageNames <- rownames(installed.packages());
  return(is.element(packageName, installedPackageNames));
} 

ensure.package <- function(packageName) {
  if(!is.installed(packageName)) {
    install.packages(packageName, dep=TRUE, repos="http://cran.at.r-project.org/");
  }
}

ensure.package("libFMexe")
ensure.package("foreach")
ensure.package("doParallel")
ensure.package("ggplot2")
ensure.package("data.table")
ensure.package("reshape2")
ensure.package("recommenderlab")
ensure.package("FactoRizationMachines")
ensure.package("randomForest")
ensure.package("ff")
ensure.package("ffbase")

update.packages(repos="http://cran.at.r-project.org/", ask=F)

suppressMessages(library(libFMexe))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
suppressMessages(library(ggplot2))
suppressMessages(library(data.table))
suppressMessages(library(reshape2))
suppressMessages(library(recommenderlab))
suppressMessages(library(FactoRizationMachines))
suppressMessages(library(randomForest))
suppressMessages(library(ff))
suppressMessages(library(ffbase))