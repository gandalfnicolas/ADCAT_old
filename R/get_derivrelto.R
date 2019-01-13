#' Wordnet derivationally related terms retriever
#'
#' Gets derivationally related forms for a term's synset
#' @param synsets synsets to obtain part meronyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @export
#' @examples
#' get_derivrelto(term_synset)

get_derivrelto = function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      result = wordnet::getRelatedSynsets(synsets, pointerSymbol = "+")}
    else{
      result = sapply(synsets,wordnet::getRelatedSynsets, pointerSymbol = "+")}
    if(Syns == F){
      return(sapply(unlist(result),wordnet::getWord))}
    else{
      return(unlist(result))}},
    error = function(s){
      message(paste0("ERROR in get_derivrelto"))
      return(NA)})
}