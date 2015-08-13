.showData <- function(la,datalocal)
{
  # Read the texts messages from the file MultilanguageShowData.txt
  tt <- as.matrix(read.delim(system.file("external/MultilanguageShowData.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
  colnames(tt) <- NULL
  
  # Check if myData matrix of data exists
  if(!exists("datalocal"))
  {
    gmessage(tt[4,la], title="Error", icon="error")
    return()
  }
  
  # Create window  
  w7 <- gwindow(tt[1,la], visible=FALSE, width=900, height=500, parent=c(0,0)) 
  g10 <- ggroup(horizontal=FALSE, spacing= 0, container=w7)
  size(g10)<-c(890,490)
  gt1 <- gtable(datalocal, chosencol = 1, multiple=TRUE, container=g10, index=TRUE)
  size(gt1) <- c(880,450)
  g11<-ggroup(horizontal=TRUE,spacing=0,container=g10)
  addSpring(g11)
  gb1 <- gbutton(tt[2,la], handler = function(h, ...){dispose(w7)}, container=g11)
  visible(w7)<-TRUE
  
  
}
