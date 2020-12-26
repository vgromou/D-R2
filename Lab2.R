library(ggplot2)

#Reads data from source (.csv file); needs path to file (string type)
readData = function(source){
  data <- read.csv(source)
  return(data)
}

#Builds a plot, but doesn't print it; needs dataframe
buildPlot = function(args){
  
  if(length(args) == 2){
    names = colnames(args, do.NULL = TRUE, prefix = "col")
    gplot = ggplot(data = args, aes(x = args[,1], y = args[,2])) + geom_line(size = 2, color = "forestgreen") +
      xlab(names[1]) + ylab(names[2])
    return(gplot)
  }
  
  else if(length(args) == 3){
    names = colnames(args, do.NULL = TRUE, prefix = "col")
    gplot = ggplot(data = args, aes(x = args[,1], y = args[,2], color = args[,3])) + geom_point(size = 2) +
      xlab(names[1]) + ylab(names[2]) + scale_color_gradient(low = "blue", high = "orange") + labs(colour = names[3])
    return(gplot)
  }
  else return()
}

#Selects necessary columns of dataframe; needs dataframe, and names of necessary columns (string type; from 2 to 3 columns)
selectColumns = function(data, x, y, u = NA){
  xIndex = grep(x, colnames(data))
  yIndex = grep(y, colnames(data))
  if(!is.na(u)){
    uIndex = grep(u, colnames(data))
    res = data.frame(data[,xIndex], data[,yIndex], data[,uIndex])
  } 
  else{
    res = data.frame(data[,xIndex], data[,yIndex])
  }
  names(res) <- c(x, y)
  if(length(res) == 3)
    names(res)[3] <- u
  return(res)
}


#Prints a plot we need; requires path to csv file and from 2 to 3 names of necessary columnts (all are string type)
printPlot = function(dataSource, x, y, u = NA){
  data = readData(dataSource)
  args = selectColumns(data, x, y, u)
  gplot = buildPlot(args)
  print(gplot)
}

printPlot("as.csv", "V1", "V5", "V6")
printPlot("as.csv", "V1", "V8")
printPlot("as.csv", "V1", "V7")
printPlot("as.csv", "V1", "V6")

