#include <Rcpp.h>
#include <string>
#include <algorithm>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>

using namespace Rcpp;
using namespace std;

String join(std::vector< std::string > ngram, 
                 std::string delim){
    if(ngram.size() == 0) return "";
    String token_ngram = ngram[0];
    int len_ngram = ngram.size();
    for (int i = 1; i < len_ngram; i++) {
        token_ngram += token_ngram;
        token_ngram += delim;
        token_ngram += ngram[i];
    }
    token_ngram.set_encoding(CE_UTF8);
    return token_ngram;
}


void skip(const std::vector< string > &tokens,
          const unsigned int start,
          const unsigned int n, 
          const std::vector< int > skips,
          std::vector< string > ngram,
          std::vector< vector<string> > &ngrams,
          int e, int &f
){
    
    //Rcout << "Start " << tokens[start] << " " << start << " " << n << "\n";
    
    ngram[e] = tokens[start];
    e++;
    
    if(e < n){
        for (int j = 0; j < skips.size(); j++){
            int next = start + skips[j];
            if(next > tokens.size() - 1) break;
            skip(tokens, next, n, skips, ngram, ngrams, e, f);
        }
    }else{
        //Rcout << "Add " << join(ngram, "+")<< " " << n << " " << e << "\n";
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
    int e = 0; // Local index for tokens in ngram
    int f = 0; // Global index for ngrams 
    vector< vector<string> > ngrams(len_ns * len_tokens * len_skips); // For the recursive function
    
    
    for (int g = 0; g < len_ns; g++) {
        int n = ns[g];
        vector< string > ngram(n);
        for (int start = 0; start < len_tokens - (n - 1); start++) {
          skip(tokens, start, n, skips, ngram, ngrams, e, f); // Get ngrams as reference
        }
    }
    
    // Join elements of ngrams
    CharacterVector tokens_ngram(f);
    for (int k = 0; k < f; k++) {
        tokens_ngram[k] = join(ngrams[k], delim);
    }
    return tokens_ngram;
}


// [[Rcpp::export]]
List skipgram_cppl2(SEXP x, 
                    const vector < int > &ns, 
                    const vector < int > &skips, 
                    const string &delim) {
  
  List texts(x);
  int len = texts.size();
  List texts_skipgram(len);
  for (int h = 0; h < len; h++){
    texts_skipgram[h] = skipgram_cpp2(texts[h], ns, skips, delim);
  }
  return texts_skipgram;
}

// [[Rcpp::export]]
List bigram_selective_cppl(SEXP x,
                           const vector<string> &types,
                           const vector<string> &types_stop,
                           const vector<string> &types_ignore,
                           const vector<int> &skips, 
                           const string &delim,
                           const string &token_sub
) {
  
  List texts(x);
  List texts_temp(texts.size());
  int len = texts.size();
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  std::unordered_set<std::string> set_types_stop (types_stop.begin(), types_stop.end());
  std::unordered_set<std::string> set_types_ignore (types_ignore.begin(), types_ignore.end());
  for (int h = 0; h < len; h++){
    //Rcout << "Text " <<  h << "\n";
    CharacterVector text = texts[h];
    CharacterVector text_temp = clone(text);
    int len = text.size();
    int len_skips = skips.size();
    for (int i=0; i < len; i++){
      //Rcout << "Now " << text[i] << " " << i << "\n";
      String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(token_sub != "") token = token_sub; // substitute by arbitary token
      if(is_in){
        int k;
        //Rcout << "Match " << text[i] << " " << i << "\n";
        for (int j=0; j < len_skips; j++){
          //Rcout << "Skip " << skips[j] << "\n";
          k = i + skips[j];
          if(k < 0 || k > len - 1) break; // only within the length of text
          String token2 = text[k];
          
          // Interrupt
          bool is_in_stop = set_types_stop.find(token2) != set_types_stop.end();
          if(is_in_stop) break;
          
          // Skip
          bool is_in_ignore = set_types_ignore.find(token2) != set_types_ignore.end();
          if(is_in_ignore) continue;
          
          // JOin tokens
          String token_bigram;
          if(k < i){
            //Rcout << "Join left " << text[k] << " " << k << "\n";
            token_bigram = token2;
            token_bigram += delim;
            token_bigram += token;
          }else if(i < k){
            //Rcout << "Join right " << text[k] << " " << k << "\n";
            token_bigram =  token;
            token_bigram += delim;
            token_bigram += token2;
          }
          //token_bigram.set_encoding(CE_UTF8); // This causes crash somethimes 
          text_temp[k] = token_bigram;
        }
      }
    }
    texts_temp[h] = text_temp;
  }
  return texts_temp;
}
