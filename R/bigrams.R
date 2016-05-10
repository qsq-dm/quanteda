#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' toks <- toLower(toks)
#' 
#' negation <- stopwords()[81:98]
#' negation <- c(negation, 'never', 'not', 'no')
#' toks2 <- bigramsSelective(toks, negation)
#' print(toks[17])
#' print(toks2[17])
#' 
#' @export
bigramsSelective <- function(x, target, skip=0:100, concatenator='_'){

  bigram_selective_cppl(x, target, skip, concatenator)
  
}