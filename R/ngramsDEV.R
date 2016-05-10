
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
#' library(data.table)
#' 
#' benches = data.table()
#' for (n_start in 1:3) {
#' for (n_end in (n_start):3) {
#' for (skip_start in 0:0) {
#' for (skip_end in (skip_start):0) {
#'                        print(paste(list(n_start, n_end, skip_start, skip_end)))
#'                        n <- n_start:n_end
#'                        skip <- skip_start:skip_end
#'                        b <- rbenchmark::benchmark(
#'                                               old <- ngrams(toks, n = n, skip=skip),
#'                                               new <- ngramsNew(toks, n = n, skip=skip),
#'                                               replications = 2
#'                        )
#'                        b$n_start <- c(n_start,n_start)
#'                        b$n_end <- c(n_end,n_end)
#'                        b$skip_start <- c(skip_start,skip_start)
#'                        b$skip_end <- c(skip_end,skip_end)
#'                        #  Compare set differences in case the order is different
#'                        b$same <- setequal(old[[1]],new[[1]])
#'                        print(b)
#'                        benches <- rbind(benches, b)
#' }}}}
#' 
#'                                           test replications elapsed relative user.self sys.self user.child sys.child n_start n_end skip_start skip_end same
#'  1: new <- ngramsNew(toks, n = n, skip = skip)            2   0.001    1.000     0.002    0.000          0         0       1     1          0        0 TRUE
#'  2:    old <- ngrams(toks, n = n, skip = skip)            2   0.019   19.000     0.019    0.001          0         0       1     1          0        0 TRUE
#'  3: new <- ngramsNew(toks, n = n, skip = skip)            2   0.003    1.000     0.003    0.000          0         0       1     2          0        0 TRUE
#'  4:    old <- ngrams(toks, n = n, skip = skip)            2   0.086   28.667     0.083    0.004          0         0       1     2          0        0 TRUE
#'  5: new <- ngramsNew(toks, n = n, skip = skip)            2   0.004    1.000     0.004    0.000          0         0       1     3          0        0 TRUE
#'  6:    old <- ngrams(toks, n = n, skip = skip)            2   0.213   53.250     0.189    0.024          0         0       1     3          0        0 TRUE
#'  7: new <- ngramsNew(toks, n = n, skip = skip)            2   0.002    1.000     0.002    0.000          0         0       2     2          0        0 TRUE
#'  8:    old <- ngrams(toks, n = n, skip = skip)            2   0.021   10.500     0.021    0.000          0         0       2     2          0        0 TRUE
#'  9: new <- ngramsNew(toks, n = n, skip = skip)            2   0.004    1.000     0.004    0.000          0         0       2     3          0        0 TRUE
#' 10:    old <- ngrams(toks, n = n, skip = skip)            2   0.092   23.000     0.087    0.005          0         0       2     3          0        0 TRUE
#' 11: new <- ngramsNew(toks, n = n, skip = skip)            2   0.002    1.000     0.002    0.000          0         0       3     3          0        0 TRUE
#' 12:    old <- ngrams(toks, n = n, skip = skip)            2   0.023   11.500     0.022    0.000          0         0       3     3          0        0 TRUE
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
      for (s in skip) {
        s <- ifelse(current_n==1, 0, s)
        offset_tokens <- list()
          for (i in 1:current_n) {
              start <- ifelse(i==1, 1, i+s)
              length.out <- length(x) - (current_n-1) - s
              ix <- seq(from=start, length.out=length.out)
              offset_tokens <- c(
                offset_tokens, 
                 list(x[ix])
              )
          }
          ngram_result <- c(
            ngram_result, 
            do.call("paste", c(offset_tokens, sep = concatenator))
          )
      }
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
