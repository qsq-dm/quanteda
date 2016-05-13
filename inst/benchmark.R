library(data.table)
library(tokenizers)
library(ggplot2)
library(devtools)
load_all('~/code/quanteda/')
load_all('~/code/tokenizers/')

tokenizers_wrapper_performance <- function(toks, n, skip=0L, collapse='_') {
     ret <- c()
     for (i in n) {
         for (j in skip) {
              ## Would need to expose tokenizers::skip_ngrams
              ret <- lapply(toks, skip_ngrams, n=i, k=j)
         }
     }
}

tokenizers_wrapper_behaviour <- function(toks, n, skip=0L, collapse='_') {
     ret <- c()
     for (i in n) {
         for (j in skip) {
              ## Would need to expose tokenizers::skip_ngrams
              ret <- lapply(toks, skip_ngrams, n=i, k=j)
         }
     }
     lapply(ret, function(x)gsub(' ', collapse, x))
}


#### Behaviour
toks <- tokenize('The quick brown fox jumps over the lazy dog. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.')

fun.names = c('tokenizers_wrapper_behaviour', 'ngrams2', 'ngrams3')

behaviour <- data.table()
for (start_n in seq(1,4)) { for (end_n in seq(start_n,4)) {
           for (start_skip in seq(1,4)) { for (end_skip in seq(start_skip,4)) { 
        for (fn in fun.names) {
            print(paste(fn, start_n, end_n, start_skip, end_skip))
            old <- ngrams(toks, n=start_n:end_n, skip=start_skip:end_skip)
            new <- match.fun(fn)(toks, n=start_n:end_n, skip=start_skip:end_skip)
            same <- setequal(old[[1]],new[[1]])
            behaviour <- rbind(
               behaviour,
                data.table(data.frame(list(
                   func=fn, start_n=start_n, end_n=end_n, start_skip=start_skip, end_skip=end_skip, same=same)))
               )
            
        }
    }
}}}

if (any(!behaviour$same)) {
    warning('Some functions do not have the same behaviour')
    (behaviour[same==F])
}
####




do_benchmarks <- function(fun.names, texts, n.range=1:5, skip.range=0:3) {
    benches = data.table()
    for (t in texts) {
    for (n_end in n.range) {
    #for (skip_end in skip.range) {
       #print(paste(list(t, n_end, skip_end)))
       print(paste(t, n_end))
           print('\tngrams')
       toks <- get(t)
       b <- rbenchmark::benchmark(
       #old <- ngrams(toks, n = n_start:n_end, skip=0:skip_end),
       old <- ngrams(toks, n = 1:n_end),
       replications = 2
       )
       b$same <- TRUE
       b$method <- 'ngrams'
       for (fn in fun.names) {
           print(paste('\t', fn))
           newb <- rbenchmark::benchmark(
           #new <- match.fun(fn)(toks, n = n_start:n_end, skip=0:skip_end),
           new <- match.fun(fn)(toks, n = 1:n_end),
           replications = 2
           )
       if (fn=='tokenizers_wrapper')
       {
           new <- lapply(new[[1]], function(x)gsub(' ', collapse, x))
           newb$same <- setequal(old[[1]],new)
       }
       else {
           newb$same <- setequal(old[[1]],new[[1]])#  Compare set differences in case the order is different
       }
           newb$method <- fn
           b <- rbindlist(list(b, newb))
       }
       b$n_end <- n_end
       #b$skip_end <- skip_end
       b$text <- t
       print(b)
       benches <- rbind(benches, b)
    }}

    benches <- data.table(benches)
    benches[,c('relative', 'user.self', 'sys.self', 'user.child', 'sys.child'):=rep(NULL, 5)]
    benches
}

data(SOTUCorpus, package = "quantedaData")
SOTU_sents <- tokenize(SOTUCorpus, what='sentence', simplify = TRUE)
SOTU_sent_tokens <- tokenize(SOTU_sents, removePunct = TRUE)
SOTU_tokens <- tokenize(SOTUCorpus, removePunct = TRUE)
first_Inaug_tokens <- tokenize(inaugTexts[[1]], removePunct = TRUE)

benches <- do_benchmarks(
      fun.names = c('tokenizers_wrapper_performance', 'ngrams2', 'ngrams3'),
     texts = c('SOTU_tokens', 'SOTU_sent_tokens', 'first_Inaug_tokens')
     n.range = 1:3
)

(benches)

benches[same==F]

benches[,list(min.elapsed=min(elapsed), best.method=method[which.min(elapsed)],
              max.elapsed=max(elapsed), worst.method=method[which.max(elapsed)]),
              by=list(n_end, text)]


