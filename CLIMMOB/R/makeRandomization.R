.makeRandomization <- function(la)
{
  
  # Read the multilangaje file and get the string matrix
  tl <- as.matrix(read.delim(system.file("external/MultilanguageMakeRandomization.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
  colnames(tl) <- NULL
  
  # Create the window and the main group, put the title.
  w1 <- gwindow(title=tl[1,la], visible=FALSE, width=900, height=550, parent=c(0,0)) 
  
  # Create the main group where we'll put the panel
  groupx <- ggroup(horizontal=FALSE,container=w1,width=900,spacing=10)
  
  # Put the title and a separator
  ttitle <- glabel(tl[2,la], container=groupx)
  font(ttitle) <- list(size=16)
  gseparator(container=groupx)
  
  # Create the gpanelgroup to split the screen in two parts
  panelx <- gpanedgroup(container=groupx)
  
  # Create a new group where we'll put the info at the left of the panel
  group1 <- ggroup(horizontal=FALSE, spacing= 5, container=panelx)

  # Write extra line where stating which fields are compolsury and which ones optional
  gl12 <- glabel(tl[18,la], container=group1)
  font(gl12) <- list(family="helvetica",size=12)
  
  # Create a grup where ID of the experiment is defined
  group6 <- ggroup(horizontal=TRUE, spacing=5, container=group1, expand=TRUE)
  gl2 <- glabel(tl[3,la], container=group6)
  font(gl2) <- list(family="helvetica",size=10)
  setid <- gtext(container=group6, height=1, width=2)
  size(setid) <- c(50,20)
  
  # Create a group where the number of observers are defined
  group3 <- ggroup(horizontal=TRUE, spacing=5, container=group1, expand=TRUE)
  gl3 <- glabel(tl[4,la], container=group3)
  font(gl3) <- list(family="helvetica",size=10)
  setnobservers <- gtext(container=group3, width=2, height=1)
  size(setnobservers) <- c(50,20)
  
  # Create a group where the number of varieties is defined
  group4 <- ggroup(horizontal=TRUE, spacing=5, container=group1, expand=TRUE)
  gl4 <-glabel(tl[5,la], container=group4)
  font(gl4) <- list(family="helvetica",size=10)
  setnitems <- gtext(container=group4, width=2, height=1)
  size(setnitems) <- c(50,20)
  
  # Create a group where the names of the varieties are specified
  group5 <- ggroup(horizontal=TRUE, spacing=5, container=group1)
  gl5 <- glabel(tl[6,la], container=group5)
  font(gl5) <- list(family="helvetica",size=10)
  setitemnames <- gtext(container=group5, width=10, height=20)
  size(setitemnames) <- c(200,100)
  addHandlerRightclick(setitemnames, handler=function(h, ...) svalue(setitemnames) <- readLines(file("clipboard")))

  # Write one group where we state that the default characteristic is evaluated
  group2 <- ggroup(horizontal=TRUE, spacing=5, container=group1)
  gl9 <- glabel(tl[23,la], container=group2)
  font(gl9) <- list(family="helvetica",size=10)
  
  # Write an extra group and ask if they are comparing their varieties with the local variety
  group12 <- ggroup(horizontal=TRUE, spacing=5, container=group1)
  gl7 <- glabel(tl[37,la], container=group12)
  font(gl7) <- list(family="helvetica",size=10)
  local_item <- gradio(items=c(tl[35,la], tl[36,la]), selected=1, horizontal=TRUE, container=group12)
  
  # Create a new main group for the second column at the right of the gpanedgroup
  groupr <- ggroup(horizontal=FALSE, spacing= 5, container=panelx)
  svalue(panelx) <- 0.76

  # Create a group where the names of the characteristics are specified
  gl10 <- glabel(tl[24,la], container=groupr)
  font(gl10) <- list(family="helvetica",size=10)
  group11 <- ggroup(horizontal=TRUE, spacing=5, container=groupr)
  gl11 <- glabel(tl[25,la], container=group11)
  font(gl11) <- list(family="helvetica",size=10)
  setcharacnames <- gtext(container=group11, width=10, height=20)
  size(setcharacnames) <- c(200,100)
  addHandlerRightclick(setcharacnames, handler=function(h, ...) svalue(setcharacnames) <- readLines(file("clipboard")))
  
  # Write an extra line explaining what are the the explanatory variables
  gl7 <- glabel(tl[21,la], container=groupr)
  font(gl7) <- list(family="helvetica",size=10)
  
  # Create a group where the names of the explanatory variables are specified
  group8 <- ggroup(horizontal=TRUE, spacing=5, container=groupr)
  gl8 <- glabel(tl[22,la], container=group8)
  font(gl8) <- list(family="helvetica",size=10)
  setexplanatorynames <- gtext(container=group8, width=10, height=20)
  size(setexplanatorynames) <- c(200,100)
  addHandlerRightclick(setexplanatorynames, handler=function(h, ...) svalue(setexplanatorynames) <- readLines(file("clipboard")))  
  
  # Create a group asking for the folder where we'll write the documents
  group312 <- ggroup(horizontal=FALSE, container=groupr,spacint=0)
  glabel3121 <- glabel(tl[49,la], container=group312)
  font(glabel3121) <- list(size=14)
  gfilebrowse3121 <- gfilebrowse(text=tl[49,la], type="selectdir", container=group312)
  svalue(gfilebrowse3121) <- getwd()
  
  # Put a separator
  gseparator(containder=groupx)
  
  # Add button for creating the file
  group3 <- ggroup(horizontal=TRUE, spacing=5, container=groupx)
  addSpring(group3)
  b <- gbutton(tl[14,la], handler = function(h, ...){
    
    # Check if there's something written at the complusory fields
    if(is.null(svalue(setnitems)) | is.null(svalue(setitemnames)) | 
         is.null(svalue(setnobservers)) | is.null(svalue(setid)) | 
         svalue(setnitems) == "" | svalue(setitemnames) == "" | 
         svalue(setnobservers) == "" | svalue(setid) == "" )
    {
      gmessage(tl[15,la])
      options(show.error.messages = FALSE)
      stop()
      options(show.error.messages = TRUE) 
    }
    
    # Assign variables to what's written by the user
    nitems <- as.integer(svalue(setnitems))
    itemnames <- unlist(strsplit(svalue(setitemnames), "\n"))
    nobservers <- as.integer(svalue(setnobservers))
    identification <- svalue(setid)
    explanatory <- unlist(strsplit(svalue(setexplanatorynames), "\n"))
    overperf <- toString(tl[35,la])
    localitem <- svalue(local_item)
    charac <- unlist(strsplit(svalue(setcharacnames),"\n"))
    
    # If there's any complementary characteristic written, add the possible answers
    if (length(charac>=1)){
      for (i in 1:length(charac)) {
        charac[i] <- paste(charac[i],tl[40,1],sep="")
      }
    }
    
    # If overperf=="yes" then add it to the characteristics at the end
    if (overperf==toString(tl[35,la])){
      # Check if localitem is yes, if so, add an extra characteristic
      if (localitem==toString(tl[35,la])){
        charac <- c(charac,paste(tl[19,la],tl[40,1],sep=""),toString(tl[39,la]))
      } else {
        charac <- c(charac,paste(tl[19,la],tl[40,1],sep=""))
      }
    } 
    
    # Number of rows is the number of the Farmers 
    rows <- nobservers
    
    # Number of columns:
    # - Observers  (1 column)
    # - Name of the Observers (1 column)
    # - Surname of the Observers (1 column)
    # - Complete adress of the observers ( 1 column )
    # - Phone number
    # - Explanatory (X columns)
    # - Items names (3 to 5 columns)
    # - Complementary Characteristics (performance) + Rating of complementary Characteristics(2 if 3 variables, 4...5, )
    # - Global Performance Characteristic + its rating (IF ACTIVATED)
    # - Comparison Global Performance Charactgeristic with local variety (IF ACTIVATED)
    
    # In case there's 3 varieties to compare, there will be 1 less column for each characteristic evaluation
    
    if (nitems==3){
      # In case there's comparison of Global Performance among local varieties and given varieties, add 1 extra columns
      if (localitem==toString(tl[35,la])){
        cols <- 5+nitems+length(explanatory)+((length(charac))*(nitems))+1
      } else {
        cols <- 5+nitems+length(explanatory)+(length(charac))*(nitems)   
      }
    } else {
      cols <- 3+nitems+length(explanatory)+(length(charac))*(nitems+1)
    }
    
    # Generate the matrix of empty spaces
    codes <- data.frame(matrix(nrow=rows,ncol=cols))
    
    # Put names to the rows and columns of the matrix, except for the characteristics comparisons
    if (nitems==3){
      colnames(codes) <- c(toString(tl[26,la]),toString(tl[27,la]),toString(tl[33,la]),toString(tl[47,la]),toString(tl[48,la]),explanatory,paste(tl[28,la], "_", LETTERS[1:nitems],sep=""))
    } else {
      colnames(codes) <- c(toString(tl[26,la]),toString(tl[27,la]),toString(tl[33,la]),toString(tl[47,la]),toString(tl[48,la]),explanatory,paste(tl[28,la], "_", LETTERS[1:nitems],sep=""))
    }
    
    # Put the name for the columns where the characteristics are
    # Create an empty a vector where we'll save all columns where the characteristics are
    characind<-c()
    
    # Create an  vector where the indexes of the columns names with NA will be kept 
    indexes<-which(colnames(codes) %in% NA)
    # If there's just one characteristic
    if (length(charac)==1){
      # In case we're dealing with 3 varieties
      if (nitems==3){
        colnames(codes)[indexes[1]:indexes[nitems]]<-c(paste(toString(tl[30,la]),1,sep="_"),paste(toString(tl[31,la]),1,sep="_"),paste(toString(tl[32,la]),1,sep="_"))
      } else {
        colnames(codes)[indexes[1]:indexes[1+nitems]]<-c(paste(toString(tl[30,la]),1,sep="_"),paste(paste(tl[29,la],1:nitems),1,sep="_"))
      }
      # Update characind vector
      characind<-c(characind,indexes[1])
      
    } else if (length(charac)>1 && localitem==toString(tl[35,la])){
      for (i in 1:(length(charac)-1)){
        if (nitems==3){
          colnames(codes)[indexes[1]:indexes[nitems]]<-c(paste(toString(tl[30,la]),i,sep="_"),paste(toString(tl[31,la]),i,sep="_"),paste(toString(tl[32,la]),i,sep="_"))
        } else {
          colnames(codes)[indexes[1]:indexes[1+nitems]]<-c(paste(toString(tl[30,la]),i,sep="_"),paste(paste(tl[29,la],1:nitems),i,sep="_"))
        }
        # Update characind vector
        characind<-c(characind,indexes[1])
        
        # Update the indexes
        indexes<-which(colnames(codes) %in% NA)
      }
      # Finish the columns by adding the general performance comparison with local varieties
      colnames(codes)[indexes[1]:indexes[1+nitems]]<-c(paste(toString(tl[30,la]),i+1,sep="_"),paste(paste(tl[28,la],LETTERS[1:nitems],sep=""),i+1,sep="_"))
      
      # Update characind vector
      characind<-c(characind,indexes[1])
      
    } else {
      for (i in 1:length(charac)){
        if (nitems==3){
          colnames(codes)[indexes[1]:indexes[nitems]]<-c(paste(toString(tl[30,la]),i,sep="_"),paste(toString(tl[31,la]),i,sep="_"),paste(toString(tl[32,la]),i,sep="_"))
        } else {
          colnames(codes)[indexes[1]:indexes[1+nitems]]<-c(paste(toString(tl[30,la]),i,sep="_"),paste(paste(tl[29,la],1:nitems),i,sep="_"))
        }
        
        # Update characind vector
        characind<-c(characind,indexes[1])
        
        # Update the indexes
        indexes<-which(colnames(codes) %in% NA)
      }
    }
    
    # Start filling the matrix:
    # - Name of the observers
    codes[,1] <- paste(identification, "_", 1:nobservers, sep="")
    # - Randomization of the items to each observer
    codes <- .rand2(codes, nitems, itemnames, nobservers, identification, tl, explanatory)
    #  - Put characteristics' names in the columns with the help the vector characind
    for (i in 1:length(characind)){
      codes[,characind[i]]<-charac[i]
    }
    
    # Name the file 
    fn <- paste(identification, tl[16,la], sep="")
    fl <- file(fn)
    writeLines("sep=,", con=fl)
    close(fl)
    suppressWarnings(write.table(codes, fn, append=TRUE, sep=",", dec=".", row.names=FALSE, col.names=TRUE, fileEncoding="ASCII"))
    gm1 <- gmessage(paste(tl[17,la], getwd(), "/", fn, sep=""), title=tl[20,la], icon="info")
    
    font(gm1) <- list(family="helvetica",size=10)
    
  }, container=group3)
  
  font(b) <- list(family="helvetica",size=10)  
  visible(w1) <- TRUE
  
}

# create randomization of varieties and add them to the codes matrix
.rand2 <- function(codes, nitems, itemnames, nobservers, identification, tl, explanatory)
{
  
  nvar <- length(itemnames)
  varieties <- 1:nvar    
  
  varcombinations <- t((combn(varieties, nitems)))
  ncomb <- dim(varcombinations)[1]
  n <- floor(nobservers/ncomb)
  nfixed <- ncomb*n
  
  vars1 <- varcombinations[c(rep(1:(dim(varcombinations)[1]), times=n)),]
  vars2 <- varcombinations[sample(ncomb, nobservers-nfixed),]
  vars <- rbind(vars1, vars2)
  for(i in 1:nobservers) vars[i,] <- sample(vars[i,], nitems)
  
  varOrdered <- matrix(NA, nrow=nobservers, ncol=nitems)
  ind <- sample(nobservers,1)
  varOrdered[1,] <- vars[ind,]
  vars <- vars[-ind,]
  
  .dist <- function(x) {
    
    x <- match(x,rev(vO))
    x[is.na(x)] <- 6
    Dist <- sum(ceiling(x/nitems)^1.7)
    return(Dist)
    
  }
  
  for(i in 2:nobservers)
  {
    
    vO <- as.vector(t(varOrdered[1:(i-1),]))
    ranks <- as.vector(apply(vars, 1, .dist))
    index <- which(ranks == max(ranks))
    
    #if length(index) > 1
    
    tb <- rep(0,times=nvar)
    tb1 <- table(c(as.vector(vO)))
    tb[as.integer(names(tb1))] <- as.vector(tb1)
    
    evenS <- vector(length=length(index))
    
    for(j in 1:length(index))
    {
      
      tb2 <- tb
      tb2[vars[index[j],]] <- tb2[vars[index[j],]] + 1
      evenS[j] <- -sum(tb2^2)
      
    }
    
    index <- index[which(evenS == max(evenS))[1]]
    varOrdered[i,] <- vars[index,]
    print(varOrdered)
    vars <- vars[-index,,drop=FALSE]
    
  }
  
  codes[,(6+length(explanatory)):(5+(length(explanatory))+nitems)] <- itemnames[varOrdered]
  return(codes)
  
} 
