#' @examples
#' data(SOTUCorpus, package = "quantedaData")
#' toks <- tokenize(tokenize(SOTUCorpus, what='sentence', simplify = TRUE), 
#'                  removePunct = TRUE, removeHyphens=TRUE)
#' toks <- toLower(toks)
#' 
#' negation <- 
#' c("no", "never", "not", "isn't", "aren't", "wasn't", "weren't", "hasn't", 
#' "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", 
#' "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "isn't",
#' "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", 
#' "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't",
#' "cannot", "couldn't", "mustn't")
#' rpron <- c("that", "which", "where", "who", "whom")
#' conjunc <- c("and", "or", "but")
#' 
#' print(toks[8240])
#' toks2 <- bigramsSelective(toks, negation, rpron) # Prefix by nagation
#' print(toks2[8240])
#' toks3 <- bigramsSelective(toks, negation, ignore=stopwords()) # Prefix by nagation ignoreing stopwords
#' print(toks3[8240])
#' toks4 <- bigramsSelective(toks, negation, rpron, stopwords()) # Prefix by nagation until the next relational pronaun
#' print(toks4[8240])
#' toks5 <- bigramsSelective(toks, negation, substitute="NOT") # Prefix by NOT
#' print(toks5[8240])
#' toks6 <- bigramsSelective(toks, negation, rpron, 0:10, stopwords(), "+", "NOT") # Prefix by NOT
#' print(toks6[8240])
#'
#' # Other applications  
#' bigramsSelective(toks[7], c('in', 'to', 'for', 'of'), skip=-1) # Join prepositions
#' bigramsSelective(tokenize(encodedTexts[1]), c("€", "§"), skip=1, concatenator="") # Join currency marks
#' 
#' @export
bigramsSelective <- function(x, features, stop, ignore, skip=0:100, concatenator='_', substitute=""){
  if(missing(stop)) stop = ""
  if(missing(ignore)) ignore = ""
  ignore <- c(features, ignore)
  bigram_selective_cppl(x, features, stop, ignore, skip, concatenator, substitute)
}