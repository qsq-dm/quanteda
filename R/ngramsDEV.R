
#' Create ngrams and skipgrams using potentially faster R method
#' 
#' Create a set of ngrams (tokens in sequence) from character vectors or
#' tokenized text objects, with an optional skip argument to form skipgrams. 
#' Both the ngram length and the skip lengths take vectors of arguments to form 
#' multiple lengths or skips in one pass.  \code{ngrams()} is implemented in C++
#' for efficiency.
#' @author Adam Obeng, Kohei Watanabe and Ken Benoit
#' @return a tokenizedTexts object consisting a list of character vectors of 
#'   ngrams, one list element per text, or a character vector if called on a 
#'   simple character vector
#' @param x a tokenizedText object or a character vector of tokens
#' @param n integer vector specifying the number of elements to be concatenated 
#'   in each ngram
#' @param skip integer vector specifying the adjacency skip size for tokens 
#'   forming the ngrams, default is 0 for only immediately neighbouring words. 
#'   For \code{skipgrams}, \code{skip} can be a vector of integers, as the 
#'   "classic" approach to forming skip-grams is to set skip = \eqn{k} where
#'   \eqn{k} is the distance for which \eqn{k} or fewer skips are used to construct
#'   the \eqn{n}-gram.  Thus a "4-skip-n-gram" defined as \code{skip = 0:4}
#'   produces results that include 4 skips, 3 skips, 2 skips, 1 skip, and 0
#'   skips (where 0 skips are typical n-grams formed from adjacent words).  See
#'   Guthrie et al (2006).
#' @param concatenator character for combining words, default is \code{_} 
#'   (underscore) character
#' @param ... not used
#' @export
#' @examples
#' # ngrams
#' ngrams(LETTERS, n = 2)
#' ngramsNew(LETTERS, n = 2)
#' ngrams(LETTERS, n = 2, skip = 1)
#' ngrams(LETTERS, n = 2, skip = 0:1)
#' ngrams(LETTERS, n = 1:2)
#' ngramsNew(LETTERS, n = 1:2)
#' ngrams(LETTERS, n = c(2,3), skip = 0:1)
#' 
#' tokens <- tokenize(c(testText = "the quick brown fox jumped over the lazy dog."), 
#'                    removePunct = TRUE)
#' ngrams(tokens, n = 1:3)
#' ngramsNew(tokens, n = 1:3)
#' ngrams(tokens, n = c(2,4), concatenator = " ")
#' ngramsNew(tokens, n = c(2,4), concatenator = " ")
#' ngrams(tokens, n = c(2,4), skip = 1, concatenator = " ")
#' 
#' # skipgrams
ngramsNew <- function(x, ...) {
  UseMethod("ngramsNew")
}

#' @rdname ngramsNew
#' @importFrom stats complete.cases
#' @examples 
#' \dontrun{
#' # performance benchmarking
#' toks <- tokenize(inaugTexts, removePunct = TRUE)
#' rbenchmark::benchmark(new = ngramsNew(toks, n = 1:4),
#'                       old = ngrams(toks, n = 1:4),
#'                       replications = 2)
#' ##   test replications elapsed relative user.self sys.self user.child sys.child
#' ## 1  new            2   0.499    1.000     0.494    0.006      0.000     0.000
#' ## 2  old            2  76.589  153.485    67.438    8.899      0.022     0.038
#' }
#' @export
ngramsNew.character <- function(x, n = 2L, skip = 0L, concatenator = "_", ...) {
    if (any(stringi::stri_detect_fixed(x, " ")) & concatenator != " ")
        stop("whitespace detected: please tokenize() before using ngrams()")
    if (length(x) < min(n)) return(NULL)
    if (identical(n, 1)) {
        if (!identical(n, 1))
            warning("skip argument ignored for n = 1")
        return(x)
    }

    ngram_result <- c()
    for (current_n in n) {
        offset_tokens <- list(x[1:(length(x) - (current_n-1) - skip)])
        for (i in 2:current_n)
            ix <- seq(from=i+skip, length.out=length(offset_tokens[[1]]))
            offset_tokens <- c(
              offset_tokens, 
               list(x[ix])
            )
        ngram_result <- c(
          ngram_result, 
          do.call("paste", c(offset_tokens, sep = concatenator))
        )
    }
    ngram_result
}


#' @rdname ngramsNew
#' @export
ngramsNew.tokenizedTexts <- function(x, n = 2L, skip = 0L, concatenator = "_", ...) {
    ngramsResult <- lapply(x, ngramsNew.character, n, skip, concatenator)
    # removed mclapply because not faster
    # ngramsResult <- parallel::mclapply(x, ngrams.character, n, skip, concatenator, ...)
    class(ngramsResult) <- c("tokenizedTexts", class(ngramsResult))
    attributes(ngramsResult) <- attributes(x)
    ngramsResult
}

###
### NEED TO ADD SKIPGRAMS
###
