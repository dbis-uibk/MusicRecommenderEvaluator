createPlot <- function(result, type="f1") {
  if (type == "precision") {
    colums = c(1,2,6)
    ylabLabel = "Precision"
  }
  else if (type == "recall") {
    colums = c(1,3,6)
    ylabLabel = "Recall"
  }
  else if (type == "ir") {
    colums = c(1,2,3,6)
    ylabLabel = "Precision/Recall"
  }
  else if (type == "f1") {
    colums = c(1,4,6)
    ylabLabel = "F1"
  }
  
  resultPlot <- ggplot(data=melt(result[,colums], id=c("at","algorithm")), 
                       aes(x=at, y=value, 
                           shape=interaction(variable,algorithm, sep = " "), 
                           lty=interaction(variable,algorithm, sep = " "), 
                           colour=interaction(variable,algorithm, sep = " "))) +
    geom_line() +
    geom_point() +
    scale_x_log10(breaks = c(0,5,10,20,30,40,50,60,80,100)) +
    labs(shape = "Measure", color = "Measure", lty = "Measure") + 
    xlab("Recommendations n") +
    ylab(ylabLabel) +
    theme_bw() +
    theme(legend.position="bottom")
  return(resultPlot)
}

createPaperPlot <- function(result, type="f1") {
  if (type == "precision") {
    colums = c(1,2,6)
    ylabLabel = "Precision"
  }
  else if (type == "recall") {
    colums = c(1,3,6)
    ylabLabel = "Recall"
  }
  else if (type == "ir") {
    colums = c(1,2,3,6)
    ylabLabel = "Precision/Recall"
  }
  else if (type == "f1") {
    colums = c(1,4,6)
    ylabLabel = "F1"
  }
  
  resultPlot <- ggplot(data=melt(result[,colums], id=c("at","algorithm")), 
                       aes(x=at, y=value,
                           shape=algorithm, 
                           lty=algorithm, 
                           colour=algorithm)) +
    geom_line(size=1.5) +
    geom_point(size=1.5) +
    scale_x_log10(breaks = c(0,5,10,20,30,40,50,60,80,100)) +
    labs(shape = "Model", color = "Model", lty = "Model") + 
    xlab("Recommendations n") +
    ylab(ylabLabel) +
    theme_bw() +
    theme(legend.position=c(0.75,0.5),
          legend.text=element_text(size=12),
          legend.title=element_text(size=12),
          legend.key.height = unit(1, "cm"),
          legend.key.width = unit(3, "cm"),
          axis.text=element_text(size=12))
  return(resultPlot)
}

