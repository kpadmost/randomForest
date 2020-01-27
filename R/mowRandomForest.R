#' @import rpart
#' @import parallel




#' @name mowRandomForest
#' @title Random Forest
#' @description Create random forest based on rpart trees
#' @param formula - formula of prediction response ~ xvalues, eg y ~ x1 + x2 + x3
#' @param data - dataset, @note fields should match formula case
#' @param ntrees - number of trees to grow
#' @param samplingAttributes - number of attributes to sample from, equal to square root of total attributes by default
#' @param ... - params of a single tree, according to rpart, exsample - minsplit
#' @return - random forest
#' @export
mowRandomForest <- function(formula, data, ntrees=500, samplingAttributes=NULL, debug=FALSE,...) {
  len <- nrow(data)

  responsecol <- all.vars(formula)[1] # TODO: move to validateParms
  y <- data[[responsecol]]
  ylevels <- levels(y)
  if(!is.factor(y)) {
    ylevels <- levels(as.factor(y))
  }
  classes <- length(ylevels)
  totalAttrs <- ncol(data) - 1
  if(is.null(samplingAttributes)) {
    samplingAttributes <- round(sqrt(totalAttrs))
  }
  if(debug)
  print(paste('ta', totalAttrs, 'sa', samplingAttributes, 'yl', length(y)))
  parms <- list(totalAttributes=totalAttrs, classes=classes, samplingAttributes=samplingAttributes, ylevels=ylevels)
  parms$debug = debug
  cl <- makeCluster(detectCores())

  bagged_models <- parLapply(cl, 1:ntrees, function(x) {
    new_sample=sample.int(len, size=len,replace=T)
    singleTree(formula, data=data[new_sample,], parms, ...)
  })
  stopCluster(cl)
  ans <- list(trees=bagged_models)
  class(ans) <- 'mowRandomForest'
  ans
}


#' @name singleTree
#' @title Single rpart tree
#' @description create single rpart tree based on attributes sampling
#' @param formula - formula of prediction response ~ xvalues, eg y ~ x1 + x2 + x3
#' @param data - dataset, @note fields should match formula case
#' @param parms - atttibutes for sampling, includes
#' totalAttributes - total number of data attributes, samplingAttributes - number of attributes
#' to sample at each split(square root of totalAttributes by default)
#' classes - number of classes in response value
#' @param ... params of rpart, lookup rpart documentation for more
#' @export
singleTree <- function(formula, data, parms=NULL, ...) {
    if(is.null(parms))
      parms <- list()

    if(is.null(parms$totalAttributes)) {
      totalAttrs <- ncol(data) - 1
      parms$totalAttributes <- totalAttrs
    }

    if(is.null(parms$classes)) {
      responsecol <- all.vars(formula)[1]
      ylevels <- levels(data[[responsecol]])
      classes <- length(ylevels)
      parms$ylevels <- ylevels
      parms$classes <- classes
    }

    if(is.null(parms$samplingAttributes)) {
      parms$samplingAttributes <- as.numeric(round(sqrt(parms$totalAttributes)))
    }

    ulist <- list(eval = gini_eval, split = gini_split, init = gini_init)
    t <- rpart(formula, data=data,
          method=ulist, parms=parms, ...)
    attr(t, 'ylevels') <-  parms$ylevels
    t
}


ratest <- function() {
  mystate <- data.frame(state.x77, region=state.region)
  names(mystate) <- casefold(names(mystate)) #remove mixed case

  #ulist <- list(eval = etemp, split = stemp, init = itemp)
  ulist <- list(eval = etempg, split = ginisplit, init = itempg)
  fit1 <- rpart(murder ~ population + illiteracy + income + life.exp +
                  hs.grad + frost + region, data = mystate,
                method = ulist, minsplit = 10, parms=list(totalAttributes=7, samplingAttributes=3))
  mykip <- data.frame(kyphosis)
  formula <- kyphosis ~ age + number + start
  names(mykip) <- casefold(names(mykip)) #remove mixed case
  fit1 <- rpart(Kyphosis ~ Age + Number + Start, data=mykip,
                method=ulist,minsplit = 10, parms=list(totalAttributes=3, samplingAttributes=1))
  par(xpd = TRUE)
  plot(fit1)
  text(fit1, use.n=TRUE)
  summary(fit1)
}


#' @name predict
#' @title Predict classes
#' @description predict funtion for mowRandomForest
#' @param forest - mowRandomForest
#' @param data - data for prediction
#' @return numeric vector of predicted classes
#' @export
predict.mowRandomForest <- function(forest, data) {
  df <- data.frame(data)
  trees <- forest$trees
  # voting
  votes <- sapply(trees, function(tree) {
    predicted <- predict(tree, data, type='vector')
    #predicted <- predict(tree, df)
    predicted
  })

  votes <- apply(votes, 1, function(val) {
    res <- sort(table(val), decreasing = T)[1]
    as.numeric(names(res))
  })
  unname(votes)
}
