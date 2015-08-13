.menuClimMob <- function(la)
{
  
  # Create a new environment inside the main function .menuClimMob
  ce1<-new.env()
  
  # Read the multilanguage file, remove the name of the colunns
  tl <- as.matrix(read.delim(system.file("external/MultilanguageMainMenu.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
  colnames(tl) <- NULL
  
  # Create the main window and the main group
  w1 <- gwindow(title=tl[1,la], visible=FALSE, width=300, height=500, parent=c(0,0)) 
  group1 <- ggroup(horizontal=FALSE, spacing= 10, container=w1)
  a <- gimage(system.file("external/ClimMob-logo.gif", package="ClimMob"), container=group1)

  # Create the group/button for creating a new project/randomization
  group2 <- ggroup(horizontal=FALSE, spacing= 0, container=group1)
  b1 <- gbutton(tl[2,la], handler = function(h, ...){.makeRandomization(la)}, container=group2)
  font(b1) <- list(size=12)
  
  # Create a group/button to read a .csv file
  group4 <- ggroup(horizontal=FALSE, spacing= 0, container=group1)
  b3 <- gbutton(tl[4,la], handler = function(h, ...){.readData(la)}, container=group4)
  font(b3) <- list(size=12)
  
  # Create a group/button to show the read data, only active if the previous action is performed
  group45 <- ggroup(horizontal=FALSE, spacing= 0, container=group1)
  b45 <- gbutton(tl[9,la], handler = function(h, ...){.showData(la,ce1$myData)}, container=group45)
  font(b45) <- list(size=12)
  enabled(b45) <- FALSE
  
  # Create a group/button for create models, only active if the read action is performed
  group5 <- ggroup(horizontal=FALSE, spacing= 0, container=group1)
  b4 <- gbutton(tl[5,la], handler = function(h, ...){.makeModel(la)}, container=group5)
  font(b4) <- list(size=12)
  enabled(b4) <- FALSE
  
  # Create a button to show the pdf file with the manual
  group8 <- ggroup(horizontal=FALSE, spacing= 0, container=group1)
  b7 <- gbutton(tl[8,la], handler = function(h, ...){.helpClimMob()}, container=group8)
  font(b7) <- list(size=12)

  #production version: when closing, empty workspace...
  addHandlerDestroy(w1, function(h, ...){rm(list = ls());dispose(w1)})
  visible(w1) <- TRUE
  
  # Function called by .readData function to read tables 
  .loadData <- function(myDat,la)
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

      if(delim == ","){myData <- read.csv(myDat, skip=1)}
      if(delim == ";"){myData <- read.csv2(myDat, skip=1)}
      .GlobalEnv$mierda <- myData
    }
    
    # If loaded succesfully run the checkings and send a message
    if(exists("myData"))
    {
      
      # ---------------------------------------------------------------------------------- #
      # ----------------------------- Start the checkings -------------------------------- #
      # ---------------------------------------------------------------------------------- #
    
      # Create a file where all checks will be written
      filename<-(strsplit(myDat,".csv"))[[1]][1]
      checkfile<-file(paste(filename,"_CHECKS.txt",sep=""),'w')

      # Separate the name of the file from the route 
      filename <- (strsplit(filename,"/"))[[1]][length((strsplit(filename,"/"))[[1]])]
      
      # Export filename variable to Environment ce1 to use it later
      assign("filename",filename,envir=ce1)
      
      # - First, extract the the names of the columns
      namescols<-colnames(myData)

      # Initialize vectors and counters
      numitems<-0
      itemsgiven<-c()
      characs<-c()
      explanatory<-c()
      charac_indexes<-c()
      rowstodelete<-c()
      errors<-0
      warnings2<-0
      indexes <- c()
      personal <- c()

      # The indexes of the items and characteristics and answers
      # Find the indexes where the farmers personal data is ketp
      for (i in 1:length(namescols)){
        if (tl[14,la] %in% namescols[i]){
          personal<-c(personal,i)
        } else if (tl[15,la] %in% namescols[i]){
          personal<-c(personal,i)
        } else if (tl[45,la] %in% namescols[i]){
          personal<-c(personal,i)
        } else if (tl[46,la] %in% namescols[i]){
          personal<-c(personal,i)
        } else if (tl[47,la] %in% namescols[i]){
          personal<-c(personal,i)
        }
      }  
      
      # Update indexes
      indexes <- c(indexes,personal)
      # First check the number of items 
      for (i in 1:length(namescols)){
        if (paste(tl[13,la],LETTERS[i],sep="_") %in% namescols){
          # Get the index of the column where the item is
          item_index<-match(paste(tl[13,la],LETTERS[i],sep="_"),namescols)
          # Update the indexes
          indexes<-c(indexes,item_index)
          # update the counter
          numitems<-numitems+1
          # Update the names of the items
          itemsgiven<-c(itemsgiven,paste(tl[13,la],LETTERS[i],sep="_"))
        } 
      }
      
      # Second check the number of characteristics and update the indexes
      for (i in 1:length(namescols)){
        if (paste(toString(tl[17,la]),i,sep="_") %in% namescols){
          # Find the numeric index of the name of the column
          charac_index<-match(paste(toString(tl[17,la]),i,sep="_"),namescols)
          charac_indexes<-c(charac_indexes,charac_index)
          # Split and get the first part, disregard the answer options given
          charac<-strsplit(toString(myData[2,charac_index]),"-")[[1]][1]
          # Update the names of characteristics
          characs<-c(characs,charac)
          # Update the indexes
          indexes<-c(indexes,charac_index:(charac_index+numitems))
        }
      }
      
      # Get the indexes belonging to the explanatory variables
      # they will be the elements that aren't in the indexes
      for (i in 1:length(namescols)){
        if (!(is.element(i,indexes))){
          explanatory<-c(explanatory,namescols[i])
        }
      }
      
      # Check if the last characteristic is the comparison with the local variable
      if (is.na(strsplit(characs[length(characs)],"_")[[1]][4])){
        local<-FALSE
      } else {
        local<-TRUE
      }
      
      # Get the number of rows of the matrix myData
      rows<-length(myData[,itemsgiven[1]])
      
      # - First check the explanatory variables
      for (i in explanatory){
        
        # - Check for gaps and for unique values that are not repeted (spelling mistakes)
        # - - Check all rows of each explanatory variable
        for (j in 1:rows){
          
          # Check for blanks or or NA
          if (toString(myData[j,i])=="" || is.na(myData[j,i])){
            writeLines(paste(tl[20,la],"<",i,">,",tl[21,la],j+2,sep=" "), checkfile) 
            rowstodelete<-c(rowstodelete,j)
            errors<-errors+1
          }
        }
        # - Check for gaps and for unique values that are not repeted (spelling mistakes)
        # - - First get the unique values of the variable
        explanames <- sort(unique(as.vector(unlist(myData[i]))))
        explanames<-explanames[explanames != ""]
        # - - Then loop over the names and get the number of repetitions, if 1 or 2 mark 
        for (j in explanames){
          if ((length(myData[,i][myData[,i]==j]))<3){
            writeLines(paste(tl[23,la],"<",i,">,",tl[24,la],"<",j,">",tl[25,la],sep=" "), checkfile)
            warnings2<-warnings2+1
          }
        }
      }
      
      # Create a vector where all the possible varieties is defined
      posibleitems<-c()
      for (j in 1:numitems){
        posibleitems<-c(posibleitems,LETTERS[j])
      }
      
      # Second - Then check the characteristics
      # - First loop over the characteristics depending if the local variable exists as a characteristic
      # - If local variable is activated then there will be at least 2 characteristics:
      # - - Overall Performance and Comarison with local variable
      
      if (local){
        
        # Loop over the characteristics except for the last one
        for (i in 1:(length(charac_indexes)-1)){
          
          # Loop over the rows
          for (j in 1:rows){
            
            # For the case where there's just 3 varieties
            # There will be two other columns with three posibilities: A, B C
            if (numitems==3){
              
              # Check every column after the characteristic column 
              for (k in 1:(numitems-1)){
                if (!(toString(myData[j,(charac_indexes[i])+k]) %in% posibleitems)){
                  writeLines(paste(tl[26,la],namescols[charac_indexes[i]+k],tl[27,la],j+2,".",tl[28,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
                
                # Check if both the values of the two columns are identical
                if (toString((myData[j,(charac_indexes[i])+1])) == toString((myData[j,(charac_indexes[i])+2]))){
                  writeLines(paste(tl[29,la],namescols[charac_indexes[i]+1],"and",namescols[charac_indexes[i]+2],tl[27,la],j+2,".",tl[30,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j) 
                  errors<-errors+1
                }
              }
              
              # For the case there's more than 3 varieties
              # There will be numitems columns and numitems posibilities
            } else {
              for (k in 1:(numitems)){
                if (!(toString(myData[j,(charac_indexes[i])+k]) %in% posibleitems)){
                  writeLines(paste(tl[26,la],namescols[charac_indexes[i]+k],tl[27,la],j+2,".",tl[28,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
              }
              
              # Check if there's repeated values, find pair combinations between items
              paircombinations<-combn(1:numitems,2)
              # Loop along the combinations and check if theres any repeated value
              for (k in 1:length(paircombinations[1,])){
                if (toString(myData[j,(charac_indexes[i])+paircombinations[1,k]]) == toString(myData[j,(charac_indexes[i])+paircombinations[2,k]])){
                  writeLines(paste(tl[29,la],namescols[charac_indexes[i]+paircombinations[1,k]],"and",namescols[charac_indexes[i]+paircombinations[2,k]],tl[27,la],j+2,".",tl[30,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
              }
            }
          }
        }
        
        # Now check the values of the comparison with the local variable, only two possibilities (Better/Worse)
        # Loop over the rows
        for (j in 1:rows){
          # Loop over the columns
          for (k in 1:(numitems)){
            if (!(toString(myData[j,(tail(charac_indexes,1)+k)]) %in% c(tl[31,la],tl[32,la]))){
              writeLines(paste(tl[26,la],namescols[tail(charac_indexes,1)+k],tl[27,la],j+2,".",tl[28,la],sep=" "), checkfile)
              rowstodelete<-c(rowstodelete,j)
              errors<-errors+1
            }
          }
        }
        
        # If there is not comparison with the local variable, then loop over all varables
      } else {
        # Code repeated, if someone wants to do a function, DO IT!
        # Loop over the characteristics except for the last one
        for (i in 1:(length(charac_indexes)-1)){
          
          # Loop over the rows
          for (j in 1:rows){
            
            # For the case where there's just 3 varieties
            # There will be two other columns with three posibilities: A, B C
            if (numitems==3){
              
              # Check every column after the characteristic column 
              for (k in 1:(numitems-1)){
                if (!(toString(myData[j,(charac_indexes[i])+k]) %in% posibleitems)){
                  writeLines(paste(tl[26,la],namescols[charac_indexes[i]+k],tl[27,la],j+2,".",tl[28,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
                
                # Check if both the values of the two columns are identical
                if (toString(myData[j,(charac_indexes[i])+1]) == toString(myData[j,(charac_indexes[i])+2])){
                  writeLines(paste(tl[29,la],namescols[charac_indexes[i]+1],"and",namescols[charac_indexes[i]+2],tl[27,la],j+2,".",tl[30,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
              }
              
              # For the case there's more than 3 varieties
              # There will be numitems columns and numitems posibilities
            } else {
              for (k in 1:(numitems)){
                if (!(toString(myData[j,(charac_indexes[i])+k]) %in% posibleitems)){
                  writeLines(paste(tl[26,la],namescols[charac_indexes[i]+k],tl[27,la],j+2,".",tl[28,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
              }
              
              # Check if there's repeated values, find pair combinations between items
              paircombinations<-combn(1:numitems,2)
              # Loop along the combinations and check if theres any repeated value
              for (k in 1:length(paircombinations[1,])){
                if (toString(myData[j,(charac_indexes[i])+paircombinations[1,k]]) ==  toString(myData[j,(charac_indexes[i])+paircombinations[2,k]])){
                  writeLines(paste(tl[29,la],namescols[charac_indexes[i]+paircombinations[1,k]],"and",namescols[charac_indexes[i]+paircombinations[2,k]],tl[27,la],j+2,".",tl[30,la],sep=" "), checkfile)
                  rowstodelete<-c(rowstodelete,j)
                  errors<-errors+1
                }
              }
            }
          }
        }
      }
      
      # Get the unique values of the rows that would be deleted if not corrected
      rowstodelete<-unique(rowstodelete)
      
      # Close the file
      close(checkfile)
      
      # ---------------------------------------------------------------------------------- #
      # ------------------------------- END the checkings -------------------------------- #
      # ---------------------------------------------------------------------------------- #      
      
      # Send a message       
      # Create a window and  main window and the main group
      w3 <- gwindow(title=tl[1,la], visible=TRUE, width=400, height=150, parent=c(550,0)) 
      group31 <- ggroup(horizontal=FALSE, spacing=10, container=w3)
      group32 <- ggroup(horizontal=FALSE, spacing=0, container=group31)    

      # Depending if there's warnings and or errors, show different messages on the screen
      if (warnings2>0 && errors>0){
        b321 <- glabel(paste(tl[34,la],errors,tl[35,la],tl[36,la],warnings2,tl[37,la],sep=" "), container=group32)
        b322 <- glabel(paste(tl[ 38,la],paste(filename,"_CHECKS.txt",sep=""),tl[39,la],sep=" "), container=group32)
        b323 <- glabel(tl[40,la], container=group32)
        b324 <- glabel(paste(tl[41,la],length(rowstodelete),tl[42,la],sep=" "), container=group32)
      } else if (errors>0 && warnings2==0) {
        b321 <- glabel(paste(tl[34,la],errors,tl[35,la],sep=" "), container=group32)
        b322 <- glabel(paste(tl[38,la],paste(filename,"_CHECKS.txt",sep=""),tl[39,la],sep=" "), container=group32)
        b323 <- glabel(tl[40,la], container=group32)
        b324 <- glabel(paste(tl[41,la],length(rowstodelete),tl[42,la],sep=" "), container=group32)
      } else if (errors==0 && warnings2>0){
        b321 <- glabel(paste(tl[34,la],warnings2,tl[37,la],sep=" "), container=group32)
        b322 <- glabel(paste(tl[38,la],paste(filename,"_CHECKS.txt",sep=""),tl[39,la],sep=" "), container=group32)
      } else {
        b321 <- glabel(tl[33,la], container=group32)
      }
      
      # Handler function in case they click on accept
      hg321 <- function(h, ...)
      {
        dispose(w3)
        if (length(rowstodelete>0)){myData <- myData[-c(rowstodelete),]}
        enabled(b45) <- TRUE
        enabled(b4) <- TRUE
        
        # Get the new rows of the myData
        newrows <- length(myData[,1])
        
        # Assign the results of loading and checking the data into the ce1 environment
        assign("myData",myData,envir=ce1)
        
        # Create a vector with the info of myData
        myDatainfo <- list(numitems,list(explanatory),list(characs),local,newrows,myData[,personal])
        assign("myDatainfo",myDatainfo,envir=ce1)

      }
      
      # Handler function in case they click on accept
      hg322 <- function(h, ...)
      {
        dispose(w3)
    
      }
      g32 <- ggroup(container=group31, spacing=0, horizontal=TRUE)     
      addSpring(g32)
      g321 <- gbutton(tl[43,la], container=g32, handler=hg321)
      g322 <- gbutton(tl[44,la], container=g32, handler=hg322)
      visible(w3)<-TRUE
      
    }
    # If not show a message
    else
    {
      gmessage(tl[8,la], title=tl[9,la], icon="error")
    }
  }
  
  # Function read data, creates a window and groups and buttons, calls the other function .loadData
  .readData <- function(la)
  {
    tl <- as.matrix(read.delim(system.file("external/MultilanguageReadData.txt", package="ClimMob"), header=FALSE))
    colnames(tl) <- NULL

    lD1 <- function(h, ...)
    {
      .loadData(svalue(gf1),la)
      dispose(g1)
    }
    
    lD2 <- function(h, ...)
    {
      .loadData(system.file(paste("external/", tl[7,la], sep=""), package="ClimMob"),la)
      dispose(g1)
    }
    
    # Create the window, groups and buttons
    g1 <- gwindow(tl[12,la], visible=FALSE)
    gg1 <- ggroup(horizontal=FALSE, container=g1)
    ttitle <- glabel(tl[1,la], container=gg1)
    font(ttitle) <- list(size=16)
    gg2 <- ggroup(horizontal=TRUE, container=gg1)
    gl1 <- glabel(tl[2,la], container=gg2)
    font(gl1) <- list(size=12)
    gf1 <- gfilebrowse(text=tl[3,la], filter= list("CSV" = list(patterns = c("*.csv"))), container=gg2)
    gb1 <- gbutton(tl[4,la], container=gg2, handler=lD1)
    gl2 <- glabel(paste("   ",tl[5,la]), container=gg1)
    font(gl2) <- list(size=12)
    gg3 <- ggroup(horizontal=TRUE, container=gg1)
    gl3 <- glabel(tl[6,la], container=gg3)
    font(gl3) <- list(size=12)
    gt1 <- gcombobox(tl[7,la], selected=1, container=gg3)
    svalue(gt1) <- tl[7,la]
    gb2 <- gbutton(tl[4,la], container=gg3, handler=lD2)
    visible(g1) <- TRUE
    
  # Close the function .readData
  }
  
  # Function combination. Creates 2 row matrix with all pair combinations of elements
  .combn2 <- function(x, m)
  {
    cc <- combn(rev(x), m)
    cc <- cc[c(2,1), ncol(cc):1]
    return(cc)    
  }
  
  # Function for creating the tree Models, called by 
  .treeModel <- function(la,tl)
  {
    # Get the data from environment ce1
    mainlist <- ce1$mainlist
    
    # Extract info from mainlist
    lenghtch <- mainlist[[1]][[1]][[1]]
    explanatory <- mainlist[[1]][[2]][[1]]
    numitems <- mainlist[[1]][[3]][[1]]
    report <- mainlist[[1]][[4]][[1]]
    infosh <- mainlist[[1]][[5]][[1]]
    
    # Loop over the characteristics
    for (m in 2:(lenghtch+1)){
      
      # Get the name of the characteristic we're gonna evaluate from the mainlist
      namechar <- mainlist[[m]][[1]]
      # Get myData dataframe form the mainlist and its colnames
      myData <- mainlist[[m]][[2]]
      # Get if we're dealing with global performance and local item is activated
      local <- mainlist[[m]][[3]]
      
      # Extract the columns where the items are
      itemsgivenD <- myData[,as.character(c(paste(tl[22,la],LETTERS[1:numitems],sep="_")))]
      
      # Get the unique values of the varieties of crops
      itemnames <- sort(unique(as.vector(unlist(itemsgivenD))))
      
      # Extract columns where the rankings are, depending if there's local comparison or not
      # Add the control local variety if the local comparison is activated
      if (local){
        rankingsD <- myData[,as.character(c(paste("Position",(1:(numitems+1)),sep="_")))]
        itemnames <- c(tl[26,la],itemnames)
        # Add an extra column at itemsgiven with the local variable 
        itemsgivenD<-cbind(itemsgivenD,assign(paste(tl[22,la],LETTERS[numitems+1],sep="_"),rep(tl[26,la],length(itemsgivenD[,1]))))
                
      } else {
        rankingsD <- myData[,as.character(c(paste("Position",1:numitems,sep="_")))]
      }
    
      # Define the reference item, allways the first one
      control<-itemnames[1]
      
      # The function combn2 makes all combination for their comparison in pairs
      # for example: 3 numbers
      # 1   1   2                   3   3   2
      # 2   3   3       and then    2   1   1   three different combinations
      # then flip them upside down and left-right
      cc <- .combn2(1:length(itemnames), 2)
      
      # make a NA matrix with the number of rows = number of observations or farmers
      #                           number of cols = the number of combinations for pairs
      pc <- matrix(NA, nrow=nrow(itemsgivenD), ncol= ncol(cc))

      # Complete the matrix with all comparisons
      for(i in 1:dim(itemsgivenD)[1])
      {
        aa <- .combn2(match(unlist(itemsgivenD[i,]), itemnames)[unlist(rankingsD[i,])], 2)
        for(j in 1:dim(aa)[2])
        {
          pc[i, aa[1,j] == cc[1,] & aa[2,j] == cc[2,]] <- 1
          pc[i, aa[2,j] == cc[1,] & aa[1,j] == cc[2,]] <- -1
        }
      }
      
      # Substitute 1 and -1 for its names
      if(!is.character(itemnames)) itemnames <- paste("V", itemnames, sep="")
      
      # Put the matrix in form of pair combination
      pc <- paircomp(pc, labels=itemnames)
      
      # Add it to myData
      myData$pc <- pc

      # Check all posible combinations of explanatory variables
      if (length(explanatory)>3) {
        formula_mob<-explanatory
        formula_mob<-c(formula_mob,paste(explanatory,collapse=" + "))
        
      } else if (length(explanatory)==3) {
        formula_mob<-explanatory
        combinations_explanatory<-combn(3,2)
        for (i in 1:length(combinations_explanatory[1,]))
        {
          formula_mob<-c(formula_mob,paste(explanatory[combinations_explanatory[1,i]]," + ",explanatory[combinations_explanatory[2,i]]))
        }
        formula_mob<-c(formula_mob,paste(explanatory,collapse=" + "))
        
      } else if (length(explanatory)==2) {
        formula_mob<-explanatory 
        formula_mob<-c(formula_mob,paste(explanatory,collapse=" + "))
        
      } else {
        formula_mob<-explanatory 
      }
      
      # There will be one simulation per formula_mob component and various nodesx
      # Depending of lenght(formula_mob), create a list with all those components
      # Also create a list for putting the terminal nodes once calculated
      if (length(formula_mob)>0){
        mobnames <- vector("list",length(formula_mob))
        mobmodelresult <- vector("list",length(formula_mob))
        mobmodelnodes <- vector("list",length(formula_mob))
        for (i in 1:length(formula_mob)){
          mobnames[[i]] <- formula_mob[i]
        }
      }

      # Function for extracting info from nodes
      .extract_from_nodes <- function(object, node = NULL, FUN = NULL, drop = FALSE, ...) {
        if(is.null(node)) node <- nodeids(object, terminal = FALSE)
        if(is.null(FUN)) FUN <- function(object, ...) object  
        rval <- if("object" %in% object$info$control$terminal) {
          nodeapply(object, node, function(n) FUN(info_node(n)$object))
        } else {
          lapply(refit.modelparty(object, node, drop = FALSE), FUN)
        }
        names(rval) <- node
        if(drop & length(node) == 1L) rval <- rval[[1L]]
        return(rval)
      }
          
      
      # Loop over the combinations of models and call model and graphs
      # First if there's no explanatory variables
      if (length(formula_mob)==0){
        
        # Write that we're using no explanatory variables
        mainlist[[m]][[4]] <- tl[31,la]
        
        # First call psichotools functions to create a mob model
        estfun.btReg <- function(x, ...) x$estfun
        btfit1 <- function(y, x = NULL, start = NULL, weights = NULL,offset = NULL, ...) btReg.fit(y, ref=control, ...)
        
        # Then put the results into the mainlist
        mainlist[[m]][[5]]<- mob(formula("pc ~ 1 "), data = myData, fit = btfit1, control=mob_control(maxdepth=length(explanatory)+1))
        
        # Extract the terminal node (just one because there's no explanatory variables)        
        results<-.extract_from_nodes(mainlist[[m]][[5]])
        
        # Extract the coefficients and the covariance matrix from the results
        coeff<-results[[1]]$coefficients
        matrixvcov<-results[[1]]$vcov
        
        ## add a new row at the beginning of the vcov matrix
        control_param <- rep(0,length(matrixvcov[1,]))
        matrixvcov <- rbind(control_param,matrixvcov)
        
        ## add new column a the beginning of the matrix
        control_param <- rep(0,length(matrixvcov[1,])+1)
        matrixvcov <- cbind(control_param,matrixvcov)
        
        # Assign the name of the variable "control" to control_param column and row name.
        colnames(matrixvcov)[1] <- control
        rownames(matrixvcov)[1] <- control
        
        # calculate qvcalc
        qvcalc_results <- qvcalc(matrixvcov)
        
        ## get results from estimation of qvcalc
        quasiSE_results <- qvcalc_results$qvframe[,3]
        
        ## get estimations from partykit and add the control value
        coeff_control <- c(VarA=0,coeff)
        
        # Add results to the list
        qvcalc_results$qvframe[,1] <- coeff_control
        mainlist[[m]][[6]][[1]][[1]] <- qvcalc_results$qvframe
        
      } else {  
        for (k in 1:length(formula_mob)) {
          
          # First call psichotools functions to create a mob model
          estfun.btReg <- function(x, ...) x$estfun
          btfit1 <- function(y, x = NULL, start = NULL, weights = NULL,offset = NULL, ...) btReg.fit(y, ref=control, ...)
          
          # Get how many explanatory variables will be introduced in the model, calculate max depth of the model
          maxlength <- length(strsplit(formula_mob[k],"+",fixed=TRUE)[[1]])+1
          
          # Run the model with the explanatory variable selected
          mobmodelresult[[k]] <- mob(formula(paste("pc ~ 1 | ",formula_mob[k])), data = myData, fit = btfit1, control=mob_control(maxdepth=maxlength,minsize=15))
          
          # Identify terminal nodes in case there's
          terminal_nodes<-nodeids(mobmodelresult[[k]],terminal=TRUE)
          
          # Create a empty list with the length of the terminals
          terminalnodeslist <- vector("list",length(terminal_nodes))

          # create an empty list with the number of terminal nodes
          # Loop over the therminal nodes and calculate the quasivariance of each node
          for (i in 1:length(terminal_nodes))
          {
            # Use the function to get the results of the run model
            results<-.extract_from_nodes(mobmodelresult[[k]],node=terminal_nodes[i])
            
            # from the previous variable, extract the coefficients and the covariance matrix
            coeff<-results[[1]]$coefficients
            matrixvcov<-results[[1]]$vcov
            
            ## add a new row at the beginning of the vcov matrix
            control_param <- rep(0,length(matrixvcov[1,]))
            matrixvcov <- rbind(control_param,matrixvcov)
            
            ## add new column a the beginning of the matrix
            control_param <- rep(0,length(matrixvcov[1,])+1)
            matrixvcov <- cbind(control_param,matrixvcov)
            
            # Assign the name of the variable "control" to control_param column and row name.
            colnames(matrixvcov)[1] <- control
            rownames(matrixvcov)[1] <- control
            
            # calculate qvcalc
            qvcalc_results <- qvcalc(matrixvcov)
            
            ## get results from estimation of qvcalc
            quasiSE_results <- qvcalc_results$qvframe[,3]
            
            ## get estimations from partykit and add the control value
            coeff_control <- c(VarA=0,coeff)
            
            # Add results to the list
            qvcalc_results$qvframe[,1] <- coeff_control
            terminalnodeslist[[i]] <- qvcalc_results$qvframe
            
          }    
          # Put the results of the terminalnodeslist into mobmodelnodes list
          mobmodelnodes[[k]]<-terminalnodeslist
          
        }
        mainlist[[m]][[4]] <- mobnames
        mainlist[[m]][[5]] <- mobmodelresult
        mainlist[[m]][[6]] <- mobmodelnodes
          
      }  
    }
    # Get filename for the spreadsheets and report before calling the document generator functions
    filename1 <- ce1$filename
    filename2 <- ce1$filename2
    
    # Get the route where the files will bet written
    route <- ce1$fch    
    
    # If the analysis report is activated call .analysisReport function
    if (mainlist[[1]][[4]]){
      .analysisReport(paste(route,"/",filename2,sep=""),la,mainlist)
    }
    
    # If the spreadsheet document is activated call .makeinfosheets function
    if (mainlist[[1]][[5]]){
      .createInfosheets(paste(route,"/",filename1,sep=""),la,mainlist)
    }
    
  }
  
  .makeModel <- function(la)
  {
    tl <- as.matrix(read.delim(system.file("external/MultilanguageMakeModel.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
    colnames(tl) <- NULL

    # Get myData from the environment ce1
    myData<-ce1$myData
    
    # Get the information of myData from environment ce1 as well
    numitems <- ce1$myDatainfo[[1]]
    local <- ce1$myDatainfo[[4]]
    numrows <- ce1$myDatainfo[[5]]
    personal <- ce1$myDatainfo[[6]]

    explanatory <- c()
    for (i in 1:length(ce1$myDatainfo[[2]])){
      explanatory <- c(explanatory,ce1$myDatainfo[[2]][[i]])
    }
    
    characs <- c()
    for (i in 1:length(ce1$myDatainfo[[3]])){
      characs <- c(characs,ce1$myDatainfo[[3]][[i]])
    }
    
    # Create a window and a main group where the options for the model are shown
    w2 <- gwindow(title=tl[1,la], visible=TRUE, width=1000, height=650, parent=c(0,0)) 
    
    # Create the first main group and put it into the panel1
    groupx <- ggroup(horizontal=FALSE, container=w2, spacing=10)
    
    # Put the title
    ttitle <- glabel(tl[1,la], container=groupx)
    font(ttitle) <- list(size=16)
    
    # Put a separator
    gseparator(container=groupx)
    
    # Create a gpanedgroup, to split the screen in two parts
    panel21<-gpanedgroup(container=groupx)
    
    # Create a mew group where we'll put the info of at the left of the panel
    group21 <- ggroup(horizontal=FALSE, container=panel21, spacing=10)
    
    # Create a group showing the Explanatory variables found
    group211 <- ggroup(horizontal=FALSE, container=group21,spacint=0)
    glabel2111 <- glabel(tl[2,la], container=group211)
    font(glabel2111) <- list(size=14)
    
    # Show the recomendations
    glabel2112 <- glabel(tl[3,la], container=group211)
    font(glabel2112) <- list(size=12)
    
    # - In case there up to 10 explanatory create a multicheckboxgroup otherwise a table with multiselection
    if (length(explanatory)<11){
      glabel2113 <- glabel(tl[4,la], container=group211)
      font(glabel2113) <- list(size=12)
      gform2111 <- gformlayout(container=group211)
      gcheckboxgroup(explanatory, horizontal=FALSE, label="",container=gform2111)
    } else {
      glabel2114 <- glabel(tl[5,la], container=group211)
      font(glabel2114) <- list(size=12)
      gtable2111 <- gtable(explanatory, chosencol = 1, multiple=TRUE, container=group211, index=TRUE)
    }
    
    
    # Create a group showing the Characteristics found 
    group212 <- ggroup(horizontal=FALSE, container=group21,spacint=0)
    glabel2121 <- glabel(tl[6,la], container=group211)
    font(glabel2121) <- list(size=14)
    glabel2113 <- glabel(tl[7,la], container=group211)
    font(glabel2113) <- list(size=12)
    
    # Create a multi gcheckboxgroup to mark all the characteristics, 
    # In case the global performance comparison with local variable is active split the caracteristics
    if (local){
      gform2121 <- gformlayout(container=group212)
      gcheckboxgroup(characs[1:(length(characs)-1)], horizontal=FALSE, label="",container=gform2121)
      glabel2122 <- glabel(tl[8,la], container=group212)
      font(glabel2122) <- list(size=12)
      gradio2121 <- gradio(items=as.vector(tl[9:10,la]), selected=2, horizontal=TRUE, container=group212)
    } else {
      gform2121 <- gformlayout(container=group212)
      gcheckboxgroup(characs, horizontal=FALSE, label="",container=gform2121)
    }
    
    # Create a new group in the second column of the gpanedgroup panel21
    group31 <- ggroup(horizontal=FALSE, container=panel21, spacing=10)
    if (la==1){
      svalue(panel21) <- 0.575
    } else {
      svalue(panel21) <- 0.5
      
    }
    
    # Create a group asking for generating the documents 
    group311 <- ggroup(horizontal=FALSE, container=group31,spacint=0)
    glabel3111 <- glabel(tl[11,la], container=group311)
    font(glabel3111) <- list(size=14)
    gform3111 <- gformlayout(container=group311)
    gcheckboxgroup(tl[12:13,la], horizontal=FALSE, label="",container=gform3111)
    
    # Create a new group asking for the filename of the report
    group313 <- ggroup(horizontal=FALSE, container=group31,spacint=0)
    glabel3131 <- glabel(tl[30,la], container=group313)
    font(glabel3131) <- list(size=10)
    glabel3132 <- glabel(tl[29,la], container=group313)
    font(glabel3132) <- list(size=10)
    gtext3131 <- gtext(container=group313,height=1, width=2)   
    size(gtext3131) <- c(50,25)
    
    # Create a group asking for the folder where we'll write the documents
    group312 <- ggroup(horizontal=FALSE, container=group31,spacint=0)
    glabel3121 <- glabel(tl[27,la], container=group312)
    font(glabel3121) <- list(size=14)
    gfilebrowse3121 <- gfilebrowse(text=tl[27,la], type="selectdir", container=group312)
    svalue(gfilebrowse3121) <- getwd()
    
    
    # Put a separator
    gseparator(container=groupx)
    
    # Create the last group where the accept and cancel buttons are.
    group313 <- ggroup(horizontal=TRUE,container=groupx,spacint=0)
    addSpring(group313)
    gbutton3131 <- gbutton(tl[14,la],container=group313,handler=function(h,...){
      
      # Generate a boolean variable to proceed with the calculations and documents
      proceed <- TRUE
      
      # Get the values of the checkboxgroups for explanatory variables
      ech<-unlist(sapply(gform2111$children,svalue))
      
      # Get the values of the checkboxgroups for characteristics
      cch<-unlist(sapply(gform2121$children,svalue))
      
      # Get the values of the checkboxgroups for documents  
      dch<-unlist(sapply(gform3111$children,svalue))
      
      # Get the value of the of the folder where the documents will be created
      fch <- setwd(svalue(gfilebrowse3121))
      
      # Check if local is activated
      if (local){
        if (svalue(gradio2121)==tl[9,la]){
          localch <- TRUE
        } else {
          localch <- FALSE
        }
      } else {
        localch <- FALSE
      }
        
      # Get the length of the number of characteristics
      lengthcch <- length(cch)
      
      # If the variable local is marked as yes but global performance is not marked, activate it
      if (localch && ((tail(cch,1)!=toString(tl[18,la])) || ((length(cch) == 0) && (typeof(cch) == "character")))){
        cch<-c(cch,toString(tl[18,la]))
        
        # Renew the length of the characteristics
        lengthcch <- lengthcch+1
      }
      
      # If the user hasn't chosen any characteristic to evaluate, send an error and come back
      if (length(cch)==0){
        gmessage(tl[16,la], title=tl[19,la], icon="info")
        proceed <- FALSE
      }
      
      # If the user hasn't chosen any destination folder, send an error and come back
      if (length(fch)==0){
        gmessage(tl[28,la], title=tl[19,la], icon="info")
        proceed <- FALSE
      } else {
         # put the value in the ce1 environment to use it later
        assign("fch",fch,envir=ce1)
      }
      
      # If the user hasn't chosen any document to generate, send a error and come back
      # If selected one, see which one and activate the booleans
      if (length(dch)==0){
        gmessage(tl[21,la], title=tl[19,la], icon="info")
        proceed <- FALSE
      } else if (length(dch)==1 && dch==toString(tl[12,la])){
        repan <- TRUE
        inshe <- FALSE  
        # Get the value of the report  
        if (is.null(svalue(gtext3131))){
          filename2 <- ce1$filename
        } else {
          filename2 <- svalue(gtext3131)
        }
        assign("filename2",filename2,envir=ce1)
        
      } else if (length(dch)==1 && dch==toString(tl[13,la])){
        repan <- FALSE
        inshe <- TRUE  
      } else {
        repan <- TRUE
        inshe <- TRUE
        # Get the value of the report  
        if (is.null(svalue(gtext3131))){
          filename2 <- ce1$filename
        } else {
          filename2 <- svalue(gtext3131)
        }
        assign("filename2",filename2,envir=ce1)
        
      }
      
      # If the user hasn't chosen any explanatory variable, send a warning and continue
      if (length(ech)==0){
        gmessage(tl[17,la], title=tl[20,la], icon="info")
        proceed <- TRUE
      } 
      
      # If proceed variable is TRUE, start calculating models and generating documents
      # - First Create a list and with all the characteristics
      # -- The first Component has the following sub-compoonents
      # --- 1 - Number of characteristics 
      # --- 2 - All explanatory variables chosen
      # --- 3 - Number of items 
      # --- 4 - TRUE if analysis report is activated
      # --- 5 - TRUE if the info sheets is activated
      # --- 6 - Personal data from each user
      # -- The second compoient component and onwards is the characteristics individually analysed
      # -- Inside each characteristic component:
      # --- 1 - Name of the characteristic evaluated
      # --- 2 - myData cutted for each characteristic
      # --- 3 - TRUE if the local component is activated (FALSE BY DEFAULT)
      # --- 4 - Empty space to put the name of combination of the explanatory variables used
      # --- 5 - Empty space to put the model result
      # --- 6 - Empty space to put the results from qvcalc for every model run
      
      if (proceed){
        # Create the main list with the lenght of the number of characteristics plus one for extra info:
        mainlist <- vector("list",lengthcch+1)
        # Put the characteristics in the first component
        mainlist[[1]] <-  list(lengthcch,list(ech),numitems,repan,inshe,myData)
        # Put the lists of each characteristic analysed
        for (i in 1:lengthcch){
          mainlist[[i+1]] <- vector("list",6)
          mainlist[[i+1]][[1]]<-cch[i]
          mainlist[[i+1]][[3]]<-FALSE
        }

        # Start cutting myData  and rearange the results 
        # - First select the common data every characteristic shares (items, explanatory variables)
        myDataCommon<-myData[,c(paste(tl[22,la],LETTERS[1:numitems],sep="_"),ech)]
        
        # - Then select the characteristic, recalculate the rankings. Loop over the characteristics
        for (m in 1:length(cch)){  
          
          # Find the index of the column where the caracteristic we're evaluating is
          for (i in 1:length(myData[1,])){
            if (paste(cch[m],"(A/B/C...)",sep="-")==toString(myData[1,i])){
              indexcch<-i
              break
            }
          }
          
          # -- Check if the characteristic we're dealing is the global performance and if local is activated
          if ((paste(tl[18,la],"(A/B/C...)",sep="-")==toString(myData[2,indexcch])) && localch){
            
            # Create a dataframe with numros and columns numitems +  1 local 
            dataprov <- data.frame(matrix(nrow=numrows,ncol=numitems+1))
            # Put names to the columns
            colnames(dataprov)<-c(paste("Position",1:(numitems+1),sep="_"))
            # Check for the number of items
            if (numitems==3){
              
              # Put the rankings as integers in the first 3 columns
              for (i in 1:numrows){
                
                dataprov[i,1] <- match(toString(myData[i,indexcch+1]),LETTERS)
                dataprov[i,3] <- match(toString(myData[i,indexcch+2]),LETTERS)
                dataprov[i,2] <- 6-dataprov[i,1]-dataprov[i,3]
                dataprov[i,4] <- 4
                
                # Then add the comparison with the local variable, the one in the forth column
                # Locate the best variety and look on the its comparison with the local variable
                # If worse, summ 1 to the rest of variables, then go to the second variety and onwards
                for (j in 1:numitems){
                  if (toString(myData[i,indexcch+numitems+dataprov[i,j]])==toString(tl[23,la])){
                    dataprov[i,(j+1):(numitems+1)]<- dataprov[i,j:numitems] 
                    dataprov[i,j]<-4
                    break
                  }  
                }       
              }
            } else {
              # we have more than 3 items....
              # Put the rankings as integers in the first n columns
              for (i in 1:numrows){
                
                for (j in 1:numitems){
                  dataprov[i,j] <- match(toString(myData[i,indexcch+j]),LETTERS)
                  dataprov[i,j+1] <- numitems+1
                }
                # Then add the comparison with the local variable, the one in the forth column
                # Locate the best variety and look on the its comparison with the local variable
                # If worse, summ 1 to the rest of variables, then go to the second variety and onwards
                for (j in 1:numitems){
                  if (toString(myData[i,indexcch+numitems+dataprov[i,j]])==toString(tl[23,la])){
                    dataprov[i,(j+1):(numitems+1)]<- dataprov[i,j:numitems] 
                    dataprov[i,j]<-numitems+1
                    break
                  }  
                }       
              }
            }
            # Change the local item in the mainlist
            mainlist[[m+1]][[3]]<-TRUE
          
          } else {
            # If we're not dealing with global performance and local activated then we're dealing with a 
            # characteristic wih no comparison with local 
            # Create a dataframe with numros and columns numitems and put colnames
            dataprov <- data.frame(matrix(nrow=numrows,ncol=numitems))
            # Put names to the columns
            colnames(dataprov)<-c(paste("Position",1:numitems,sep="_"))
            # Check for the number of items
            
            # Find the index of the column where the caracteristic we're evaluating is
            for (i in 1:length(myData[1,])){
              if (paste(cch[m],"(A/B/C...)",sep="-")==toString(myData[1,i])){
                indexcch<-i
                break
              }
            }
            if (numitems==3){
              
              # Put the rankings as integers in the first 3 columns
              for (i in 1:numrows){
                dataprov[i,1] <- match(toString(myData[i,indexcch+1]),LETTERS)
                dataprov[i,3] <- match(toString(myData[i,indexcch+2]),LETTERS)
                dataprov[i,2] <- 6-dataprov[i,1]-dataprov[i,3]
              }
            } else {
              # we have more than 3 items....
              # Put the rankings as integers in the first n columns
              for (i in 1:numrows){
                
                for (j in 1:numitems){
                  dataprov[i,j] <- match(toString(myData[i,indexcch+j]),LETTERS)
                }       
              }
            }
          }
          # Join both dataframes, myDataCommon and dataprov and put the result in the mainlist
          mainlist[[m+1]][[2]]<-cbind(myDataCommon,dataprov)
        }
        
        # Put mainlist in the ce1 environment and activate the partikit model run
        assign("mainlist",mainlist,envir=ce1)
        .treeModel(la,tl)
        
      } 
    })
    
    gbutton3132 <- gbutton(tl[15,la],container=group313,handler=function(h,...){dispose(w2)})
        
          
  # Close the Function makeModel   
  }
  
# Close the menuClimMob function  
}
