#This function converts all non-ASCII characters into the Unicode format that rtf understands
#This helps to make everything work regardless of the locale (= regional settings / language)
#Works with vectors, but not matrices

.Unicodify <- function(x)
{ 
  
  n <- length(x)
  result <- vector(length=n)
  
  for(i in 1:n)
  {
    
    xi <- as.character(x[i])
    xx <- unlist(strsplit(xi, split=NULL))
    j <- which(is.na(charToInt(xx)))
    
    for(k in j) 
    {
      
      y <- stri_trans_general(xx[k], "Any-Hex/Unicode")
      yy <- substr(y, start=3, stop=nchar(y))
      yy <- strtoi(paste("0x", yy, collapse="", sep=""))
      xx[k] <- paste("\\u", yy, "\\3 ", collapse="", sep="")
      
    }
    
    xx<- paste(xx, collapse="", sep="")
    result[i] <- xx
    
  }
    
  return(result)
  
}
