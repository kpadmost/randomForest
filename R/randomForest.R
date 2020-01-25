#' @import rpart


mowRandomForest <- function(formula, data, ntrees=500, maxdepth=30, samplingAttributes=NULL, treeParams) {
  len <- nrow(data)

  responsecol <- all.vars(formula)[1] # TODO: move to validateParms
  ylevels <- levels(data[[responsecol]])
  classes <- length(ylevels)
  totalAttrs <- ncol(data) - 1
  if(is.null(samplingAttributes)) {
    samplingAttributes <- sqrt(totalAttrs)
  }
  parms <- list(totalAttributes=totalAttrs, classes=classes, samplingAttributes=samplingAttributes)

  bagged_models=list()

  for (i in 1:ntrees)
  {
    new_sample=sample.int(len, size=len,replace=T)
    bagged_models=c(bagged_models,list(singleTree(formula, data=data[new_sample,], parms = parms, unlist(treeParams))))
  }
  ans <- list(trees=bagged_models)
  class(ans) <- 'mowRandomForest'
  ans
}

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
      parms$classes <- classes
    }

    if(is.null(parms$samplingAttributes)) {
      parms$samplingAttributes <- sqrt(parms$totalAttributes)
    }

    ulist <- list(eval = etempg, split = ginisplit, init = itempg)
    t <- rpart(formula, data=data,
          method=ulist, parms=parms, ...)
    attr(t, 'ylevels') <-  ylevels
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

predict.mowRandomForest <- function(forest, data) {
  df <- data.frame(data)
  trees <- forest$trees
  # voting
  # votes <- sapply(trees, function(tree) {
  #   predicted <- predict(tree, data, type='class')
  #   #predicted <- predict(tree, df)
  #   predicted
  # })
  #
  # votes <- apply(votes, 1, function(val) {
  #   res <- sort(table(val), decreasing = T)[1]
  #   as.numeric(res)
  # })
  # votes
  for(i in 1:nrow(data)) {

  }
}
