checkClass <- function(datasources=NULL, obj=NULL){
  # check the class of the input object
  cally <- paste0("class(", obj, ")")
  classesBy <- DSI::datashield.aggregate(datasources, cally, async = FALSE)
  classes <- unique(unlist(classesBy))
  for (n in names(classesBy)) {
    if (!all(classes == classesBy[[n]])) {
      message("The input data is not of the same class in all studies!")
      message("Use the function 'ds.class' to verify the class of the input object in each study.")
      stop(" End of process!", call.=FALSE)
    }
  }
  return(classes)
}

extract <- function(input){
  input <- unlist(input)
  output1 <- c()
  output2 <- c()
  for (i in 1:length(input)){
    inputterms <- unlist(strsplit(input[i], "\\$", perl=TRUE))
    if(length(inputterms) > 1){
      obj1 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][1]
      obj2 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][2]
    }else{
      obj1 <- NA
      obj2 <- strsplit(input[i], "\\$", perl=TRUE)[[1]][1]
    }
    output1 <- append(output1, obj1)
    output2 <- append(output2, obj2)
  }
  output <- list('holders'=output1, 'elements'=output2)
  return(output)
}

#'
isDefined <- function(datasources=NULL, obj=NULL){
  
  stdnames <- names(datasources)
  
  inputnames <- c()
  inputobj <- unlist(obj)
  for(i in 1:length(inputobj )){
    chnames <- extract(inputobj[i])
    if(is.na(chnames[[1]])){
      inputnames <- append(inputnames, chnames[[2]])
    }else{
      inputnames <- append(inputnames, chnames[[1]])
    }
  }
  
  myObjects <- inputnames
  results <- c()
  for(i in 1:length(myObjects)){
    cally <- call('exists', myObjects[i])
    x <- DSI::datashield.aggregate(datasources, cally)
    results <- append(results, mean(unlist(x)))
  }
  if(mean(results) != 1){
    idx <- which(results == FALSE)
    stop("The input object(s) ", paste(myObjects[idx],collapse=", ")," is(are) not defined on one or more of the studies!", call.=FALSE)
  }else{
    return(TRUE)
  }
  
}

ds.scatterPlotJOHAN <- ds.scatterPlot <- function (x=NULL, y=NULL, method='deterministic', k=3, noise=0.25, type="split", datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(x)){
    stop("Please provide the x-variable", call.=FALSE)
  }
  
  if(is.null(y)){
    stop("Please provide the y-variable", call.=FALSE)
  }
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x, y)
  xnames <- extract(objects)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  typ.x <- checkClass(datasources, x)
  typ.y <- checkClass(datasources, y)
  
  # the input objects must be numeric or integer vectors
  if(!('integer' %in% typ.x) & !('numeric' %in% typ.x)){
    message(paste0(x, " is of type ", typ.x, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  if(!('integer' %in% typ.y) & !('numeric' %in% typ.y)){
    message(paste0(y, " is of type ", typ.y, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  x.lab <- xnames[[length(xnames)]]
  ynames <- extract(y)
  y.lab <- ynames[[length(ynames)]]
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  if(method=='deterministic'){ method.indicator <- 1 }
  if(method=='probabilistic'){ method.indicator <- 2 }
  
  # call the server-side function that generates the x and y coordinates of the centroids
  call <- paste0("scatterPlotDS(", x, ",", y, ",", method.indicator, ",", k, ",", noise, ")")
  output <- DSI::datashield.aggregate(datasources, call)
  
  pooled.points.x <- c()
  pooled.points.y <- c()
  for (i in 1:num.sources){
    pooled.points.x[[i]] <- output[[i]][[1]]
    pooled.points.y[[i]] <- output[[i]][[2]]
  }
  pooled.points.x <- unlist(pooled.points.x)
  pooled.points.y <- unlist(pooled.points.y)
  
  # plot and return the scatter plot depending on the argument "type"
  if(type=="combine"){
    numr <- 1
    numc <- 1
    graphics::par(mfrow=c(numr,numc))
    graphics::plot(pooled.points.x, pooled.points.y, xlab=x.lab, ylab=y.lab, main=paste0("Combined scatter plot"))
    return.message <- "Combined plot created"
    return(return.message)
  }else{
    if(type=="split"){
      # set the graph area and plot
      if(num.sources > 1){
        if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
        numc <- 2
        graphics::par(mfrow=c(numr,numc))
        scatter <- list()
      }
      for(i in 1:num.sources){
        title <- paste0("Scatter plot of ", stdnames[i])
        x <- output[[i]][[1]]
        y <- output[[i]][[2]]
        graphics::plot(x, y, xlab=x.lab, ylab=y.lab, main=title)
      }
      return.message <- "Split plot created"
      return(list(x,y,return.message))
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}