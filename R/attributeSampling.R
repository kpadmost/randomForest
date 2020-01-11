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


initSampling <- function(params) {
  # get N of total arguments
  totalAttributes <- params$totalAttributes
  # get n of sampling
  samplingAttributes <- params$samplingAttributes
  # save to parent environment
  initStorage(totalAttributes, samplingAttributes)
  #saveToStorage('samplingParams',
               # list(totalAttributes=totalAttributes, samplingSize=samplingAttributes,
                #     index=2, samples=rep(0, samplingAttributes)))
}

newSampling <- function() {
  storage <- getStorage()
  params <- storage$samplingParams
  #saveToStorage('index', 1)
  #saveToStorage()

  samples <- sample.int(params$totalAttributes, params$samplingSize)
  saveToStorage('samplingMode', list(index=1, samples=samples))
}

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

