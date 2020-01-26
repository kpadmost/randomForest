#' @import parallel
#'
# Here is a set of user split functions of rpart. For details, look for rpart user-written split functions

mParSapply <- function(...) {
  cl <- makeCluster(detectCores())
  res <- parSapply(cl, ...)
  stopCluster(cl)
  res
}
mParLapply <- function(...) {
  cl <- makeCluster(detectCores())
  res <- parLapply(cl, ...)
  stopCluster(cl)
  res
}

gini_init <- function(y, offset, parms, wt) {
  if (is.matrix(y) && ncol(y) > 1)
    stop("Matrix response not allowed")
  if (!missing(parms) && length(parms) > 0)
    warning("parameter argument ignored")
  if (length(offset)) y <- y - offset
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    paste(" mean=", format(signif(yval, digits)),
          ", MSE=" , format(signif(dev/wt, digits)),
          "levels=", ylevel,sep = '')
  }

  initSampling(parms) # initialize sampling

  # debug params
  if(is.null(parms$random)) {
    parms$random <- FALSE
  }
  if(is.null(parms$debug))
    parms$debug <- FALSE


  environment(sfun) <- .GlobalEnv
  numr <- parms$classes + 1
  list(y = c(y), ylevels=(levels(as.factor(y))), parms = parms, numresp = numr, numy = 1,
     print = function(yval, ylevel, digits) {
    temp <- if (is.null(ylevel)) as.character(yval[, 1L])
    else ylevel[yval[, 1L]]

    nclass <- ncol(yval) - 1L
    yprob <- if (nclass < 5L)
      format(yval[, 1L + 1L:nclass],
             digits = digits)
    else format(yval[, 1L + 1L:nclass], digits = 2L)
    if (!is.matrix(yprob)) #this case only occurs for no split trees
      yprob <- matrix(yprob, nrow = 1L)

    temp <- paste0(temp, " (", yprob[, 1L])
    for (i in 2L:ncol(yprob)) temp <- paste(temp, yprob[, i], sep = " ")
    temp <- paste0(temp, ")")
    temp
  },
  summary = function(yval, dev, wt, ylevel, digits) {
    nclass <- ncol(yval) - 1L
    group <- yval[, 1L]
    yprob <- yval[, 1L + 1L:nclass]
    if (!is.null(ylevel)) group <- ylevel[group]

    temp2 <- format(yprob, format = "%5.3f")
    if (nclass >1) {
      temp2 <- apply(matrix(temp2, ncol = nclass), 1L,
                     paste, collapse = " ")
    }
    #dev <- dev/(wt[1L] * nodeprob)
    paste0("  predicted class=", format(group, justify = "left"),
           "  expected loss=", format(dev, digits),
           "   probabilities: ", temp2)
  },
  text = function(yval, dev, wt, ylevel, digits, n, use.n) {
    nclass <- (ncol(yval) - 2L)/2L
    group <- yval[, 1L]
    counts <- yval[, 1L+ (1L:nclass)]
    if (!is.null(ylevel)) group <- ylevel[group]

    temp1 <- format(counts, digits)
    if (nclass > 1L)
      temp1 <- apply(matrix(temp1, ncol = nclass), 1L,
                     paste, collapse = "/")
    if (use.n)  paste0(format(group, justify = "left"), "\n", temp1)
    else format(group, justify = "left")
  })
}


gini_impurity <- function(y) {  # calculating gini impurity
  1 - sum((table(y) / length(y)) ** 2)
}


gini_eval <- function(y, wt, parms) {
  if(parms$debug)
    print('at eval1')
  newSampling()

  classes <- parms$classes
  counts <- table(1:classes)
  for(i in 1:classes) {
    counts[i] <- 0
    counts[i] <- length(y[y == i])
  }
  probs <- counts / length(y)

  chosen_n <- as.numeric(names((sort(counts, decreasing = T)[1])))
  lab <- c(chosen_n, unname(probs))

  wmean <- sum(y*wt)/sum(wt)
  rss <- sum(wt*(y-wmean)^2)
  list(label = lab, deviance = rss)
}


gini_split <- function(y, wt, x, parms, continuous) {

  debug <- parms$debug
  random <- parms$random
  n <- length(y)
  chosenSapply <- sapply
  if(n > 6000) {
      chosenSapply <- mParSapply
  }
  nclasses <- parms$classes
  isNotSampled <- !isSampledAttribute()
  if(debug) {
    print(paste('at split', isNotSampled, continuous, n))
  }

  if(isNotSampled)
    if(continuous)
      return(list(goodness=rep(0, n - 1), direction=rep(1, n - 1)))
    else {
      nu <- length(unique(x))
      return(list(goodness=rep(0, nu - 1), direction=rep(1, nu)))
    }

  max_impurity <- 1 - (1 / nclasses)
  if(continuous) {
    goodness <- chosenSapply(X=1:(n - 1), FUN=function(i) {
      if(debug) {
        print(paste('at splitc', i))
      }
      y_left <- y[1:i]
      y_right <- y[(i + 1):n]

      g_left <- gini_impurity(y_left)
      g_right <- gini_impurity(y_right)
      gnode <- 2 * max_impurity - ((g_left * i + g_right * (n - i)) / n)
    })
    # compare with random split
    if(random) {
      goodness <- rep(0.33, n - 1)
      goodness[(n - 1) %/% 2] <- 1.6
    }

    list(goodness=goodness, direction=rep(1, n - 1))
  } else {
    xUnique <- unique(x)
    n <- length(xUnique)
    if(debug)
      print(paste('xvelels', n))
    ny <- length(y)
    gini_val <- lapply(xUnique, function(val) {
      pass <- x == val
      y_left <- y[pass]
      nleft <- length(y_left)
      y_right <- y[!pass]

      g_left <- gini_impurity(y_left)
      g_right <- gini_impurity(y_right)
      impurity <- 2 * max_impurity - ((g_left * nleft + g_right * (ny - nleft)) / ny)
      list(impurity=impurity, val=val)
    })
    xorder <- order(sapply(gini_val, function(x) x$impurity))
    sorted_by_val <- gini_val[xorder]
    goodness <- sapply(sorted_by_val, function(x) x$impurity)[-n]
    if(random) {
      goodness <- rep(0.33, n - 1)
      goodness[(n - 1) %/% 2] <- 1.6
    }
    list(goodness=goodness,
         direction=sapply(sorted_by_val, function(x) x$val))
  }
}
