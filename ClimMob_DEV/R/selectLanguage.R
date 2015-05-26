.selectLanguage <- function()
{
  
  tl <- as.matrix(read.delim(system.file("external/MultilanguageSelectLanguage.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
  colnames(tl) <- NULL
    
  w1 <- gwindow(visible=FALSE, title=paste(tl[1,]))
     
  g1 <- ggroup(container=w1, horizontal=FALSE)
  a <- gimage(system.file("external/ClimMob-logo.gif", package="ClimMob"), container=g1)
   
  gl <- glabel(paste(as.vector(tl[2:3,1]), collapse="\n"), container=g1)
  font(gl) <- list(size=12)
  
  s <- 1
  sgl <- Sys.getlocale(category = "LC_COLLATE")
  if(!is.null(grep("sp", sgl)) | !is.null(grep("Sp", sgl))) {s <- 2}
  lang <- gradio(items=as.vector(tl[2:3,2]), selected=s, horizontal=TRUE, container=g1)
  font(lang) <- list(size=14)
  
  g2 <- ggroup(container=g1, horizontal=TRUE)
  
  addSpring(g2)
  
  hh <- function(h, ...)
  {
    
    langn <- ifelse(svalue(lang)=="English",1,2) #TODO Needs to change if there is a third language
    la <- langn
    dispose(w1)
    .menuClimMob(la)
    
  }
  
  gb <- gbutton("Continue", container=g2, handler=hh)
    
  font(gb) <- list(size=12)
  visible(w1) <- TRUE
  focus(w1) <- TRUE
    
}

