#' implementation of sampling attributes function
#' implementation based on custom environment
#' Since package of environment cannot be modified at runtime, we use trick - replacing env. value as in saveToStorage function
#' in beforemention storage we are keeping both initial parameters as well as attribute index, with each call we increase it
#' Thus we guarantee that in each split there are exactly nsampled attributes selected


storage <- new.env(parent = emptyenv())
storage$samplingParams <- list()
storage$samplingMode <- list()

getStorage <- function() storage

initStorage <- function(totalAttributes, samplingSize) {
  samplingParams <- list(totalAttributes=totalAttributes, samplingSize=samplingSize)
  samplingMode <- list(index=1, samples=c(samplingSize))
  saveToStorage('samplingParams', samplingParams)
}

saveToStorage <- function(valueName, value) {
  oldV <- storage$valueName
  storage[[valueName]] <- value
  invisible(oldV)
}


# function for initialization of sampling, pass params to storage
initSampling <- function(params) {
  totalAttributes <- params$totalAttributes
  samplingAttributes <- params$samplingAttributes
  # save to parent environment
  initStorage(totalAttributes, samplingAttributes)
}


# new sampling, sample attributes based on samplingAttribute params
newSampling <- function() {
  storage <- getStorage()
  params <- storage$samplingParams

  samples <- sample.int(params$totalAttributes, params$samplingSize)
  saveToStorage('samplingMode', list(index=1, samples=samples))
}


# check if attribute sampled, if positive, remove
isSampledAttribute <- function() {
  storage <- getStorage()
  samplingMode <- storage$samplingMode
  currentIndex <- samplingMode$index
  samples <- samplingMode$samples

  # test if attribute is among samples
  ind <- samples == currentIndex
  isSampled <- length(samples[ind]) == 1 # samples[ind] returns numeric(0) if not present

  if(isSampled) {
    samplingMode$samples <- samples[!ind]
  }
  samplingMode$index <- currentIndex + 1
  saveToStorage('samplingMode', samplingMode)
  isSampled
}

