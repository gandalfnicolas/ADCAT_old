#' Wordnet hyponym retriever
#'
#' Gets the synsets/words (depending on Syns) of a term's hyponym hierarchy. 
#' @param synsets synsets to obtain hyponyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @export
#' @examples
#' get_Hypos(term_synset)
get_Hypos = function(synsets,Syns=T){
  final_list = list()
  tryCatch({
    if(length(synsets)==1){
      synsets = c(synsets,synsets)
    }
    while(T){
      print("W")
      synsets = tryCatch(unlist(sapply(synsets, wordnet::getRelatedSynsets, pointerSymbol = "~")),
                         error = function(s){NA})
      if(sum(!is.null(synsets)) < 1){break}
      final_list = c(final_list,synsets)
    }
    ifelse(Syns == F,
           return(sapply(final_list,wordnet::getWord)),
           return(final_list))},
    error = function(s){
      message(paste0("ERROR in get_Hypos"))
      return(NA)})
}