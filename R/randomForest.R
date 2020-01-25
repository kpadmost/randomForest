#' @import rpart

#' Create random forest
#' @param formula - formula of prediction response ~ xvalues, eg y ~ x1 + x2 + x3
#' @param data - dataset, @note fields should match formula case
#' @param ntrees - number of trees to grow
#' @param samplingAttributes - number of attributes to sample from, equal to square root of total attributes by default
#' @param ... - params of a single tree, according to rpart, exsample - minsplit
#' @return - random forest
#' @export
mowRandomForest <- function(formula, data, ntrees=500, samplingAttributes=NULL, ...) {
  len <- nrow(data)

  responsecol <- all.vars(formula)[1] # TODO: move to validateParms
  ylevels <- levels(data[[responsecol]])
  classes <- length(ylevels)
  totalAttrs <- ncol(data) - 1
  if(is.null(samplingAttributes)) {
    samplingAttributes <- sqrt(totalAttrs)
  }
  parms <- list(totalAttributes=totalAttrs, classes=classes, samplingAttributes=samplingAttributes, ylevels=ylevels)

  bagged_models=list()

  for (i in 1:ntrees)
  {
    new_sample=sample.int(len, size=len,replace=T)
    bagged_models=c(bagged_models,list(singleTree(formula, data=data[new_sample,], parms, ...)))
  }
  ans <- list(trees=bagged_models)
  class(ans) <- 'mowRandomForest'
  ans
}

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
