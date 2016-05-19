#' Virtual class "fcm" for a feature-co-occurrence matrix
#' 
#' @description The fcm class of object is a special type of \link{dfm-class}.
#'   
#' @slot context the context definition
#' @slot window the size of the window, if \code{context = "window"}
#' @slot weights context weighting for distance from target feature, equal in length to \code{window}
#' @seealso \link{fcm}
#' @export
#' @import methods
#' @docType class
#' @name fcm-class
setClass("fcm",
         slots = c(context = "character", window = "integer", weights = "numeric"),
#         prototype = list(Dimnames = list(contextFeatures = NULL, targetFeatures = NULL)),
         contains = c("dfm", "dgCMatrix"))


#' create a feature co-occurrence matrix
#' 
#' Create a sparse feature co-occurrence matrix within a user-defined context. 
#' The context can be defined as a document or a window within a collection of
#' documents, with an optional vector of weights applied to the co-occurrence
#' counts.
#' @param x character vector, corpus, or tokenized texts from which to generate
#'   the context-feature co-occurrence matrix.
#' @param ... additional arguments passed to \link{tokenize}, which can include
#'   for instance \code{ngrams} and \code{concatenator} for tokenizing
#'   multi-token sequences
#' @import Matrix
#' @export
#' @name fcm
fcm <- function(x, ...) {
    UseMethod("fcm")
}

#' @rdname fcm
#' @param context the context in which to consider term co-occurrence: 
#'   \code{"document"} for co-occurrence counts within document; \code{"window"}
#'   for co-occurrence within a defined window of words, which requires a 
#'   postive integer value for \code{window}
#' @param window positive integer value for the size of a window on either side 
#'   of the target feature, default is 5, meaning 5 words before and after the 
#'   target feature
#' @param weights a vector of weights applied to each distance from
#'   \code{1:window}, strictly decreasing and of the same length as
#'   \code{length(weights)}
#' @param spanSentence if \code{FALSE}, then word windows will not span
#'   sentences
#' @param tri if \code{TRUE} return only upper triangle (including diagonal)
#' @examples
#' (txts <- c(paste(letters[c(1, 1:3)], collapse = " "), 
#'            paste(letters[c(1, 3, 5)], collapse = " "), 
#'            paste(letters[c(5, 6, 7)], collapse = " ")))
#' toks <- tokenize(toLower(txts), removePunct = TRUE)
#' fcm(toks, context = "document")
#' 
#' txt <- c("The quick brown fox jumped over the lazy dog.",
#'          "The dog jumped and ate the fox.")
#' toks <- tokenize(toLower(txt), removePunct = TRUE)
#' fcm(toks, context = "window", window = 3)
#' fcm(toks, context = "window", window = 2)
#' @import data.table
#' @import Matrix
#' @export
fcm.tokenizedTexts <- function(x, context = c("document", "window"), window = 5L,
                               weights = rep(1, length(window)),
                               spanSentence = TRUE, tri = TRUE, ...) {
    context <- match.arg(context)
    
    feature <- V1 <- NULL  # to avoid no visible binding errors in CHECK
    
    # could add a warning if not roundly coerced to integer
    window <- as.integer(window)
    
    if (!spanSentence) 
        warning("spanSentence = FALSE not yet implemented")
    
    if (context == "document") {
        
        x <- tf(dfm(x, toLower = FALSE, verbose = FALSE), "boolean")
        # get co-occurrence counts
        result <- t(x) %*% x
        # remove 1 from the diagonal so that target features are not counted as their own context
        Matrix::diag(result) <- ifelse(Matrix::diag(result) > 0, Matrix::diag(result) - 1, 0)
        # return triangular result only if tri == TRUE
        if (tri) 
            result[lower.tri(result, diag = FALSE)] <- 0
        window = 0L
        weights = 1
        # make sure that zeros are sparse
        # result[result==0] <- 0

    } else if (context == "window") {
        
        x <- ngrams(unlist(x), n = window, skip = 0:(window - 1)) #, ...)
        x <- strsplit(x, "_")
        # create a data.table of tokens
        x <- data.table(do.call(rbind, x))
        setnames(x, 1:2, c("context", "feature"))
        x <- x[, paste(feature, collapse = " "), by = context]
        # make into a character vector
        contexts <- x[, V1]
        names(contexts) <- x[, context]
        rm(x)
        result <- dfm(contexts, toLower = FALSE, verbose = FALSE)
        
    }

    result <- new("fcm", as(result, "dgCMatrix"), context = context, window = window, weights = weights)
    names(result@Dimnames) <- c("contextFeatures", "targetFeatures")
    result
}     



#' @rdname fcm
#' @export
fcm.corpus <- function(x, ...) {
    x <- tokenize(x, ...)
    fcm(x)
}

#' @rdname fcm
#' @export
fcm.character <- function(x, ...) {
    x <- tokenize(x, ...)
    fcm(x)
}


#' @rdname print.dfm
#' @export
setMethod("print", signature(x = "fcm"), 
          function(x, show.values = FALSE, show.settings = FALSE, show.summary = TRUE, nfeature = 20L, ...) {
              ndoc <- nfeature
              if (show.summary) {
                  cat("Context-feature matrix of: ",
                      format(ndoc(x), , big.mark=","), " context feature",
                      ifelse(ndoc(x)>1 | ndoc(x)==0, "s, ", ", "),
                      format(nfeature(x), big.mark=","), " target feature",
                      ifelse(nfeature(x)>1 | nfeature(x)==0, "s", ""),
                      ifelse(is.resampled(x), paste(", ", nresample(x), " resamples", sep=""), ""),
                      ".\n", sep="")
              }
              if (show.settings) {
                  cat("Settings: TO BE IMPLEMENTED.")
              }
              if (show.values | (nrow(x)<=ndoc & ncol(x)<=nfeature)) {
                  Matrix::printSpMatrix2(x[1:min(ndoc, ndoc(x)), 1:min(nfeature, nfeature(x))], 
                                         col.names=TRUE, zero.print=0, ...)
              }
          })

#' @rdname print.dfm
setMethod("show", signature(object = "fcm"), function(object) print(object))


 







      

