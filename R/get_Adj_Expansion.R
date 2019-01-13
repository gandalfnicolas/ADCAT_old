#' Wordnet adjective expansion
#'
#' Gets see also, similar, and attribute for Adjectives
#' @param synsets synsets to obtain part meronyms for
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the various words that make up the synsets.
#' @export
#' @examples
#' get_derivrelto(term_synset)

get_Adj_Expansion = function(synsets, Syns=T){
  tryCatch({
    if(length(synsets)==1){
      seealso = wordnet::getRelatedSynsets(synsets,pointerSymbol = "^")
      similar = wordnet::getRelatedSynsets(synsets,pointerSymbol = "&")
      attribute = wordnet::getRelatedSynsets(synsets,pointerSymbol = "\\=")
      Expansion = c(seealso,similar,attribute)}
    else{
      seealso = sapply(synsets,wordnet::getRelatedSynsets, pointerSymbol = "^")
      similar = sapply(synsets,wordnet::getRelatedSynsets, pointerSymbol = "&")
      attribute = sapply(synsets,wordnet::getRelatedSynsets, pointerSymbol = "\\=")
      Expansion = c(seealso,similar,attribute)}
    if(Syns == F){
      return(sapply(unlist(Expansion),wordnet::getWord))}
    else{
      return(unlist(Expansion))}},
    error = function(s){
      message(paste0("ERROR in get_Adj_Expansion"))
      return(NA)})
}