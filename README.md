# MusicRecommenderEvaluator

Evaluation Framework for the Music Recommender System presented at the ICMR 2017.

## Prerequisites

In case you want to compute the predicted ratings of the items using the original libFM implementation provided by S. Rendle, please install [libFM](https://github.com/srendle/libfm) and set the *lfmpath* variable accordingly. Alternatively, you can use the R implementation [FactoRizationMachines](https://cran.r-project.org/web/packages/FactoRizationMachines/index.html) by J. Knoll.

## Input data

The input read by the function *loadData* must be a CSV file formatted according to the following scheme:

<user,artist,track,cluster>

The IDs can be arbitrary strings or integers, uniquely identifying the users, artists, tracks and clusters.

## Start Evaluation

To start the evaluation, please adapt and run the [main_icmr17.R](mains/main_icmr17.R). The R script will automatically install or update all required R packages.