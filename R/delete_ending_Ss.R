#' Singular transform
#'
#' This function deletes the letter s from the input whenever it trails the word
#' @param word word to transform, if multiple use loop/apply/dplyr
#' @export
#' @examples
#' delete_ending_Ss('hello')

delete_ending_Ss = function(word){
  isS = stringi::stri_sub(word,-1,-1)
  return(ifelse(isS == "s" | isS == "S", stringi::stri_sub(word,1,-2),word))
}