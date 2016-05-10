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
    std::string token_ngram = ngram[0];
    int len_ngram = ngram.size();
    for (int i = 1; i < len_ngram; i++) {
        token_ngram = token_ngram + delim + ngram[i];
    }
    String token_ngram_enc(token_ngram);
    token_ngram_enc.set_encoding(CE_UTF8);
    return token_ngram_enc;
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
                           const vector<int> &skips, 
                           const string &delim,
                           bool overwrite
) {
  
  List texts(x);
  List texts_temp(texts.size());
  int len = texts.size();
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  for (int h = 0; h < len; h++){
    
    CharacterVector text = texts[h];
    CharacterVector text_temp = clone(text);
    int len = text.size();
    int len_skips = skips.size();
    for (int i=0; i < len;){
      //Rcout << "Now " << text[i] << " " << i << "\n";
      String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in){
        int k;
        //Rcout << "Match " << text[i] << " " << i << "\n";
        for (int j=0; j < len_skips; j++){
          //Rcout << "Skip " << skips[j] << "\n";
          k = i + skips[j];
          if(k < 0 || k > len - 1) break; // only within the length of text
          
          String token2 = text[k];
          bool is_in2 = set_types.find(token2) != set_types.end();
          if(is_in2) continue; //Skip target types
          
          if(k < i){
            //Rcout << "Join left " << text[k] << " " << k << "\n";
            text_temp[k] =  token2;
            text_temp[k] += delim;
            text_temp[k] += token;
          }else if(i < k){
            //Rcout << "Join right " << text[k] << " " << k << "\n";
            text_temp[k] =  token;
            text_temp[k] += delim;
            text_temp[k] += token2;
          }
        }
        if(overwrite){
          i++;
        }else{
          i = k;  
        }
      }else{
        i++;
      }
    }
    texts_temp[h] = text_temp;
  }
  return texts_temp;
}


// [[Rcpp::export]]
List bigram_selective_cppl2(SEXP x,
                           const vector<string> &types,
                           const vector<int> &skips, 
                           const string &delim,
                           const bool &remove
) {
  
  List texts(x);
  List texts_temp(texts.size());
  std::unordered_set<std::string> set_types (types.begin(), types.end());
  for (int h = 0; h < texts.size(); h++){
    
    CharacterVector text = texts[h];
    int len_text = text.size();
    int len_skips = skips.size();
    CharacterVector text_temp(len_text * len_skips);
    int l = 0;
    for (int i=0; i < len_text; i++){
      //Rcout << "Now " << text[i] << " " << i << "\n";
      String token = text[i];
      bool is_in = set_types.find(token) != set_types.end();
      if(is_in){
        //Rcout << "Match " << text[i] << " " << i << "\n";
        for (int j=0; j < len_skips; j++){
          //Rcout << "Skip " << skips[j] << "\n";
          int k = i + skips[j];
          if(k < 0 || k > len_text - 1) break; // only within the length of text
          
          String token2 = text[k];
          bool is_in2 = set_types.find(token2) != set_types.end();
          if(is_in2) continue; //Skip target types
          
          if(k < i){
            //Rcout << "Join left " << text[k] << " " << k << "\n";
            text_temp[l] =  token2;
            text_temp[l] += delim;
            text_temp[l] += token;
            l++;
          }else if(i < k){
            //Rcout << "Join right " << text[k] << " " << k << "\n";
            text_temp[l] =  token;
            text_temp[l] += delim;
            text_temp[l] += token2;
            l++;
          }
        }
      }else{
        text_temp[l] = token;
        l++;
      }
    }
    texts_temp[h] = text_temp[seq(0, l - 1)];
  }
  return texts_temp;
}