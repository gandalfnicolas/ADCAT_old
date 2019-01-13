#' Wordnet member holonym retriever
#'
#' Gets member holonyms for a term's synset
#' @param synsets synsets to obtain member holonyms  for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @export
#' @examples
#' get_Member_holonym(term_synset)
#' 
get_Member_holonym= function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      result = wordnet::getRelatedSynsets(synsets, pointerSymbol = "%m")}
    else{
      result = sapply(synsets,wordnet::getRelatedSynsets, pointerSymbol = "%m")}
    if(Syns == F){
      return(sapply(unlist(result),wordnet::getWord))}
    else{
      return(unlist(result))}},
    error = function(s){
      message(paste0("ERROR in get_Member_holonym"))
      return(NA)})
}