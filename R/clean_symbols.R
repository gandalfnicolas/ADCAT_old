#' Remove symbols
#'
#' This function allows you to remove symbols from responses.
#' @param word word to clean, if multiple use loop/apply/dplyr
#' @export
#' @examples
#' clean_symbols('hello')

clean_symbols = function(word){
  cleaned = gsub(" ","",word,fixed = T)		
  cleaned = gsub("-","",cleaned,fixed = T)
  cleaned = gsub("'","",cleaned,fixed = T)
  cleaned = gsub('"',"",cleaned,fixed = T)
  cleaned = gsub(',',"",cleaned,fixed = T)
  cleaned = gsub('/',"",cleaned,fixed = T)
  cleaned = gsub('#',"",cleaned,fixed = T)
  cleaned = gsub('?',"",cleaned,fixed = T)
  cleaned = gsub('.',"",cleaned,fixed = T)
  cleaned = gsub('%',"",cleaned,fixed = T)
  cleaned = gsub('$',"",cleaned,fixed = T)
  cleaned = gsub(';',"",cleaned,fixed = T)
  cleaned = gsub(':',"",cleaned,fixed = T)
  cleaned = gsub('(',"",cleaned,fixed = T)
  cleaned = gsub(')',"",cleaned,fixed = T)
  cleaned = gsub('&',"",cleaned,fixed = T)
  cleaned = gsub('`',"",cleaned,fixed = T)
  return(cleaned)		
}		