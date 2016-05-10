#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), removePunct = TRUE)
#' toks <- toLower(toks)
#' 
#' negation <- stopwords()[81:98]
#' negation <- c(negation, 'never', 'not', 'no')
#' toks2 <- bigramsSelective(toks, negation)
#' print(toks[8240])
#' print(toks2[8240])
#' 
#' toks3 <- bigramsSelective(toks, c('in', 'to', 'for', 'of'), -1)
#' head(toks3)
#' 
#' 
#' @export
bigramsSelective <- function(x, features, skip=0:100, concatenator='_'){

  bigram_selective_cppl(x, features, skip, concatenator, TRUE)
  
}