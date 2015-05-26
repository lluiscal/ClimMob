.mergeData <- function(la){
    
  tl <- as.matrix(read.delim(system.file("external/MultilanguageMergeData.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
  colnames(tl) <- NULL
  
  w <- gwindow(title=tl[1,la], visible=FALSE)

  g1 <- ggroup(container=w, horizontal=FALSE)
  
  ttitle <- glabel(tl[2,la], container=g1)
  font(ttitle) <- list(size=16)
  
  gl1 <- glabel(tl[3,la], container=g1)
  font(gl1) <- list(size=12)
  
  a <- gfilebrowse(text = tl[4,la],
                   type = "open",
                   container = g1,
                   filter= list("CSV" = list(patterns = c("*.csv"))))

  gl2 <- glabel(tl[5,la], container=g1)
  font(gl2) <- list(size=12)
  
  b <- gfilebrowse(text = tl[4,la],
                   type = "open",
                   container = g1,
                   filter= list("CSV" = list(patterns = c("*.csv"))))
  
  gl6 <- glabel(tl[6,la], container=g1)
  font(gl6) <- list(size=12)
  d <- gfilebrowse(text=tl[13,la], type="selectdir", container=g1)
  svalue(d) <- getwd()
  
  group2 <- ggroup(horizontal=TRUE, spacing=5, container=g1)
  gl7 <- glabel(tl[7,la], container=group2)
  font(gl7) <- list(size=11)
  decsepI <- ifelse(Sys.localeconv()["decimal_point"] == ".", 1, 2)
  decsep <- gradio(items=c(tl[8,la], tl[9,la]), selected=decsepI, horizontal=TRUE, container=group2)
  gl8 <- glabel(tl[10,la], container=group2)
  font(gl8) <- list(size=11)
  MSExcel <- gradio(items=c(tl[11,la], tl[12,la]), selected=1, horizontal=TRUE, container=group2)
    
  gb3 <- gbutton(tl[13,la], handler = function(h, ...){
    
    x <- .loadData2(svalue(a))
    
    y <- .loadData2(svalue(b))
    
    myDataMerged <- merge(x, y, all = TRUE)
    
    aa1 <- strsplit(svalue(a), "/")[[1]]
    bb1 <- strsplit(svalue(b), "/")[[1]]
    aa2 <- strsplit(aa1[length(aa1)], "\\.")[[1]][1]
    bb2 <- strsplit(bb1[length(bb1)], "\\.")[[1]][1]    
    fn <- paste(svalue(d), "/", aa2, "_", bb2, "_", tl[14,la], ".csv", sep="")
        
    if(svalue(decsep) == tl[8,la] & svalue(MSExcel) == tl[12,la]){write.csv(myDataMerged, fn, row.names=F)}
    if(svalue(decsep) == tl[9,la] & svalue(MSExcel) == tl[12,la]){write.csv2(myDataMerged, fn, row.names=F)}
    if(svalue(decsep) == tl[8,la] & svalue(MSExcel) == tl[11,la]){
      
      fl <- file(fn)
      writeLines("sep=,", con=fl)
      close(fl)
      suppressWarnings(write.table(myDataMerged, fn, append=TRUE, sep=",", dec=".", row.names=FALSE, col.names=TRUE))
      
    }
    if(svalue(decsep) == tl[9,la] & svalue(MSExcel) == tl[11,la]){
      
      fl <- file(fn)
      writeLines("sep=;", con=fl)
      close(fl)
      suppressWarnings(write.table(myDataMerged, fn, append=TRUE, sep=";", dec=",", row.names=FALSE, col.names=TRUE))
      
    }
    
    if(exists("myDataMerged")) {
      
      gmessage(paste(tl[16,la], fn), title=tl[15,la], icon="info")
      
    } else {
      
      gmessage(tl[17,la], title=tl[18,la], icon="error")
      
    }
    
    
    dispose(w)
    
  }, container=g1)
  
  font(gb3) <- list(size=12)
  
  visible(w) <- TRUE
  
  .loadData2 <- function(myDat,la)
  {
    tl <- as.matrix(read.delim(system.file("external/MultilanguageReadData.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
    colnames(tl) <- NULL
    f1 <- file(myDat)
    rL <- readLines(f1, n=3)
    close(f1)
    ncomma <- 0
    nsemicolon <- 0
    
    if(length(strsplit(rL[2], ",")) == length(strsplit(rL[3], ","))) ncomma <- length(strsplit(rL[1], ",")[[1]])
    if(length(strsplit(rL[2], ";")) == length(strsplit(rL[3], ";"))) nsemicolon <- length(strsplit(rL[2], ";")[[1]])
    
    if(1 > (ncomma+nsemicolon))
    {
      gmessage(tl[8,la], title=tl[9,la], icon="error")
    } 
    else
    {
      delim <- ifelse(ncomma<=nsemicolon, ";", ",")
      if(length(strsplit(rL[1], delim)) == length(strsplit(rL[2], delim))){s <- 0} else{s <- 1}
      if(delim == ","){myData <- read.csv(myDat, skip=s)}
      if(delim == ";"){myData <- read.csv2(myDat, skip=s)}
    }
    
    if(exists("myData"))
    {
      gmessage(tl[10,la], title=tl[11,la], icon="info")
      enabled(b45) <- TRUE
      enabled(b4) <- TRUE
      
    }
    else
    {
      gmessage(tl[8,la], title=tl[9,la], icon="error")
    }
    
    # Assign the results of loading 
    return(myData)
  }
}