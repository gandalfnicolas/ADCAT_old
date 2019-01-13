#' Wordnet part meronyms retriever
#'
#' Gets part meronyms for a term's synset
#' @param synsets synsets to obtain part meronyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @export
#' @examples
#' get_part_meronyms(term_synset)

get_part_meronyms = function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      result = wordnet::getRelatedSynsets(synsets, pointerSymbol = "#p")}
    else{
      result = sapply(synsets,wordnet::getRelatedSynsets, pointerSymbol = "#p")}
    if(Syns == F){
      return(sapply(unlist(result),wordnet::getWord))}
    else{
      return(unlist(result))}},
    error = function(s){
      message(paste0("ERROR in get_part_meronyms"))
      return(NA)})
}