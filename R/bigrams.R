#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' toks <- toLower(toks)
#' 
#' negation <- stopwords()[81:98]
#' negation <- c(negation, 'never', 'not')
#' toks2 <- bigramsSelective(toks, negation)
#' head(toks2)
#' 
#' @export
bigramsSelective <- function(x, target, skip=0:100, concatenator='_'){

  bigram_selective_cppl(x, target, skip, concatenator)
  
}