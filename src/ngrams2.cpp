#include <Rcpp.h>
#include <string>
#include <algorithm>

using namespace Rcpp;
using namespace std;

std::string join(std::vector< std::string > ngram, 
                 std::string delim){
    if(ngram.size() == 0) return "";
    std::string token_ngram = ngram[0];
    int len_ngram = ngram.size();
    for (int i = 1; i < len_ngram; i++) {
        token_ngram = token_ngram + delim + ngram[i];
    }
    return token_ngram;
}

String join2(std::vector< std::string > ngram, 
                 std::string delim){
  if(ngram.size() == 0) return "";
  std::string token_ngram = ngram[0];
  int len_ngram = ngram.size();
  for (int i = 1; i < len_ngram; i++) {
    token_ngram = token_ngram + delim + ngram[i];
  }
  return token_ngram;
}


void skip(const std::vector< string > &tokens,
          const unsigned int start,
          const unsigned int n, 
          const std::vector< int > skips,
          std::vector< string > ngram,
          std::vector< vector<string> > &ngrams,
          int &e, int &f
){
    
    int len_tokens = tokens.size();
    int len_skips = skips.size();
    
    //Rcout << "Add " << tokens[start] << " " << ngram.size() << " " << e << "\n";
    ngram[e] = tokens[start];
    e++;
    
    if(e < n){
        for (int j = 0; j < len_skips; j++){
            int next = start + skips[j];
            if(next > len_tokens - 1) break;
            skip(tokens, next, n, skips, ngram, ngrams, e, f);
        }
    }else{
        ngrams[f] = ngram;
        ngram.clear();
        e = 0;
        f++;
    }
}

// [[Rcpp::export]]
CharacterVector skipgram_cpp2(const vector < string > &tokens,
                              const vector < int > &ns, 
                              const vector < int > &skips, 
                              const string &delim) {
    
    // Generate skipgrams recursively
    int len_ns = ns.size();
    int len_skips = skips.size();
    int len_tokens = tokens.size();
    int e = 0; // Global index for tokens in ngram
    int f = 0; // Global index for ngrams 
    vector< vector<string> > ngrams(len_ns * len_tokens * len_skips); // For the recursive function
    
    
    for (int g = 0; g < len_ns; g++) {
        int n = ns[g];
        vector< string > ngram(n);
        for (int start = 0; start < len_tokens; start++) {
            skip(tokens, start, n, skips, ngram, ngrams, e, f); // Get ngrams as reference
        }
    }
    
    // Join elements of ngrams
    CharacterVector tokens_ngram(f);
    for (int h = 0; h < f; h++) {
        tokens_ngram[h] = join(ngrams[h], delim);
    }
    return tokens_ngram;
}


void skip3(const std::vector< string > &tokens,
          const unsigned int start,
          const unsigned int n, 
          const std::vector< int > skips,
          std::vector< string > ngram,
          CharacterVector &ngrams,
          int &e, int &f, const string &delim
){
  
  int len_tokens = tokens.size();
  int len_skips = skips.size();
  
  //Rcout << "Add " << tokens[start] << " " << n << " " << e << "\n";
  ngram[e] = tokens[start];
  e++;
  
  if(e < n){
    for (int j = 0; j < len_skips; j++){
      int next = start + skips[j];
      if(next > len_tokens - 1) break;
      skip3(tokens, next, n, skips, ngram, ngrams, e, f, delim);
    }
  }else{
    ngrams[f] = join2(ngram, delim);
    ngram.clear();
    e = 0;
    f++;
  }
}

// [[Rcpp::export]]
CharacterVector skipgram_cpp3(const vector < string > &tokens,
                              const vector < int > &ns, 
                              const vector < int > &skips, 
                              const string &delim) {
  
  // Generate skipgrams recursively
  int len_ns = ns.size();
  int len_skips = skips.size();
  int len_tokens = tokens.size();
  int e = 0; // Global index for tokens in ngram
  int f = 0; // Global index for ngrams 
  CharacterVector ngrams(len_ns * len_tokens * len_skips); // For the recursive function
  
  for (int g = 0; g < len_ns; g++) {
    int n = ns[g];
    vector< string > ngram(n);
    for (int start = 0; start < len_tokens - len_ns; start++) {
      skip3(tokens, start, n, skips, ngram, ngrams, e, f, delim); // Get ngrams as reference
    }
  }
  
  return ngrams[seq(1, (f - 1))];
}

