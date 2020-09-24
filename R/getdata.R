"getdata" <-
function(arraydata) {      
  # To run ade4 the arraydata needs to be in a data.frame format.
  y <- NULL
  if (is.matrix(arraydata)) {
    if (!is.numeric(arraydata))
      stop("Arraydata is a matrix, but is not numeric.")
    y <- data.frame(arraydata)
  } else if (is.data.frame(arraydata)) {
    if (!all(sapply(arraydata, is.numeric)))
      stop("Arraydata is a data.frame, but contains non-numeric columns.")
    y <- arraydata
  } else if ("ExpressionSet" %in% class(arraydata)) {
    if (!"affy" %in% rownames(utils::installed.packages()))
      stop("affy package is required but not installed. Please install it.")
    y <- data.frame(affy::exprs(arraydata))
  } else if ("marrayRaw" %in% class(arraydata)) {
    nrslides = as.integer(ncol(arraydata@maRf))
    nrspots = as.integer(nrow(arraydata@maRf))
    tmp = matrix(NA, nrow = nrspots, ncol = 2 * nrslides)
    tmp[, (1:nrslides) * 2 - 1] = arraydata@maGf - arraydata@maGb
    tmp[, (1:nrslides) * 2] = arraydata@maRf - arraydata@maRb
    tmp.names = vector(mode = "character", length = 2 * nrslides)
    tmp.names[(1:nrslides) * 2 - 1] = paste("G",colnames(arraydata@maGf),sep="_")
    tmp.names[(1:nrslides) * 2] = paste("R",colnames(arraydata@maRf),sep="_")
    colnames(tmp) = tmp.names
    y <- as.data.frame(tmp)
  } else if ("RangedSummarizedExperiment" %in% class(arraydata)) {
    y <- SummarizedExperiment::assay(arraydata)
  } else {
    stop(paste("Arraydata has class ", paste(class(arraydata), sep = ","), ". None is handled.", sep=""))
  }

  return(y)
}
