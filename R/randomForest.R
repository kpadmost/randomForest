#' @import rpart
randomForest <- function(formula, data, params) {
  # for data
  # build n threes
  buildTree(formula, bag, treeParams)
}

ratest <- function() {
  mystate <- data.frame(state.x77, region=state.region)
  names(mystate) <- casefold(names(mystate)) #remove mixed case

  ulist <- list(eval = etemp, split = stemp, init = itemp)
  fit1 <- rpart(murder ~ population + illiteracy + income + life.exp +
                  hs.grad + frost + region, data = mystate,
                method = ulist, minsplit = 10, parms=list(totalAttributes=7, samplingAttributes=3))

  par(xpd = TRUE)
  plot(fit1)
  text(fit1, use.n=TRUE)
}
