.createInfosheets <- function(filename,la,mainlist){

  # Read the text 
  tl <- as.matrix(read.delim(system.file("external/MultilanguageMakeInfoSheets.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))
  colnames(tl) <- NULL
  
  # Assign from the list mainlist, to different variables we'll use later
  numobservers <- length(mainlist[[2]][[2]][,1])
  numitems <- mainlist[[1]][[3]]
  
  # Get the different varieties from any characteristic
  allItems <- sort(unique(as.vector(unlist(mainlist[[2]][[2]][,1:numitems]))))
  
  # Create a new vector with the name of the items with A, B ..
  itemsobs <- c(paste(tl[8,la],LETTERS[1:numitems],sep=" "))
  
  # Get the characteristics evaluated 
  characs <- c()
  for (i in 2:(length(mainlist))){
    characs <- c(characs,mainlist[[i]][[1]])
  }
  
  # Get the personal data of the overservers from the mainlist
  IDs <- mainlist[[1]][[6]][,1]
  obsname <-  mainlist[[1]][[6]][,2]
  obssurname <-  mainlist[[1]][[6]][,3]
  obsaddress <-  mainlist[[1]][[6]][,4]
  obsphone <-  mainlist[[1]][[6]][,5]
  
  # Extract the terminal node of each observer. Always using the last characteristic, by definition is overall performance
  # Use the last run of the model, the one that has all the explanatory variables in it.
  modelused <- mainlist[[length(mainlist)]][[5]][[length(mainlist[[length(mainlist)]][[5]])]]

  # Get all the terminal nodes of the model 
  allterminals <- nodeids(modelused, terminal = TRUE)
  
  # Check the number of terminal nodes. If one we do not need to run "predict" function
  if (length(allterminals==1))  {
    terminalnodes <- rep(allterminals[1],numobservers)
  } else {
    terminalnodes <- predict(modelused,modelused$data,type="node")    
  }
  
  # Also get the explanatory variables used in that run
  evterminalnodes <- mainlist[[length(mainlist)]][[4]][[length(mainlist[[length(mainlist)]][[4]])]]
  evterminalnodes <- strsplit(evterminalnodes," + ",fixed=TRUE)
  filename <- paste(filename,"INFOSHEETS.doc",sep="_")
  rtf <- RTF(filename, font.size=14)
  
  for(j in 1:numobservers)
  {
    # Start adding the personal info of the observer
    addHeader(rtf, .Unicodify(tl[1,la]))
    addParagraph(rtf)
    addParagraph(rtf, .Unicodify(obsname[j]))
    addParagraph(rtf, .Unicodify(obssurname[j]))
    addParagraph(rtf, .Unicodify(obsaddress[j]))
    addParagraph(rtf, .Unicodify(obsphone[j]))
    addParagraph(rtf)
    addParagraph(rtf, .Unicodify(tl[6,la]))
    addParagraph(rtf)
    addParagraph(rtf, .Unicodify(tl[7,la]))
    addText(rtf, "\n") 
    
    # Add the table of items that the observer has received, use any characteristic evaluated
    itemsobsname <- c()
    for (i in 1:numitems){
      itemsobsname <- c(itemsobsname,toString(mainlist[[2]][[2]][j,paste(toString(tl[8,la]),LETTERS[i],sep="_")]))
    }
    
    # Create the first table with the items that the observer received
    buffertable <- data.frame(itemsobs,itemsobsname) 
    colnames(buffertable) <- c(.Unicodify(tl[8,la]),.Unicodify(tl[9,la]))
    addTable(rtf, buffertable)
    addParagraph(rtf)
    
    # Loop over the characteristics, check every time for overall performance and if local is activated, and in case is TRUE
    # make a different dataframe and add the local variable to the comparison
    # Add a booleand variable to check if there has been any characteristc evaluated before
    bufferchar <- FALSE
    bufferprint <- FALSE
    
    for (m in 1:length(characs)){
      
      # Check the last characteristic (Overall performance), and see if the local is TRUE (position 3 of the list)
      # Also check if there's one characteristic and if it's Overall performance 
      if (mainlist[[m+1]][[3]]){
        
        # if there's previous chars evaluated, pint them on the document
        if (bufferchar){
          # Put the dataframe in the document
          addParagraph(rtf, .Unicodify(tl[10,la])) 
          addText(rtf, "\n") 
          bufferdf <- data.frame(buffermatrix)
          # Different colnames if we have 3 or more items
          if (numitems==3){
            colnames(bufferdf) <- c(toString(tl[11,la]),toString(tl[12,la]),toString(tl[13,la]),toString(tl[14,la]))
          } else {
            colnames(bufferdf) <- c(toString(tl[11,la]),paste(tl[15,la],1:(numitems),sep=" "))
          }
          addTable(rtf, bufferdf)
          addParagraph(rtf)
          bufferprint <- TRUE
        }
        
        
        # Extract the items from the dataframe, add a new column with the local variable
        bufferitems <- c()
        for (i in 1:numitems){
          bufferitems <- c(bufferitems,toString(mainlist[[m+1]][[2]][j,i]))
        }
        # Add the local variety
        bufferitems <- c(bufferitems,toString(tl[16,la]))
        
        # Extract rankings (always lasts columns)
        bufferrankings <- c()
        bufferlength <- length(mainlist[[m+1]][[2]][j,])
        for (i in 1:(numitems+1)){
          bufferrankings <- c(bufferrankings,mainlist[[m+1]][[2]][j,bufferlength-numitems-1+i])
        }
        
        # Create a matrix where the first column will be the positions and the second will be the elements ranked
        buffermatrix <- matrix(NA,ncol=2,nrow=(numitems+1))
        
        # Loop over the items plus the local variety
        for (i in 1:(numitems+1)){
          # Complete the matrix with the rankings ordered 
          buffermatrix[i,1] <- paste(tl[15,la],i,sep=" ")
          buffermatrix[i,2] <- bufferitems[bufferrankings[i]]
        }
        
        # Create dataframe and add the colnames
        bufferdf <- data.frame(buffermatrix)
        colnames(bufferdf) <- c(tl[18,la],tl[19,la])
        
        # Put the dataframe in the document
        addParagraph(rtf, .Unicodify(tl[17,la])) 
        addTable(rtf, bufferdf)
        addPageBreak(rtf)
        
        
      } else {
        
        # Check if we're evaluating the first characteristic and eventually create the dataframe where the
        # other characteristics will add their row
        if (bufferchar==FALSE){
          
          # Extract the items from the dataframe, add a new column with the local variable
          bufferitems <- c()
          for (i in 1:numitems){
            bufferitems <- c(bufferitems,toString(mainlist[[m+1]][[2]][j,i]))
          }
          
          # Extract rankings (always lasts columns)
          bufferrankings <- c()
          bufferlength <- length(mainlist[[m+1]][[2]][j,])
          for (i in 1:(numitems)){
            bufferrankings <- c(bufferrankings,mainlist[[m+1]][[2]][j,bufferlength-numitems+i])
          }
          
          # Create a variable and put name of the characterisitic, then complete it with the elements ranked
          buffervector <- mainlist[[m+1]][[1]]
          
          # Loop over the items 
          for (i in 1:(numitems)){
            # Complete the vector with the rankings ordered 
            buffervector[i+1] <- bufferitems[bufferrankings[i]]
          }
          
          # Create dataframe and add the colnames
          buffermatrix <- matrix(buffervector,ncol=length(buffervector),nrow=1)
          
          bufferchar <- TRUE
          
        } else {
          
          # A characteristic has been evaluated already, build the vector and add it to the dataframe
          # Extract the items from the dataframe, add a new column with the local variable
          bufferitems <- c()
          for (i in 1:numitems){
            bufferitems <- c(bufferitems,toString(mainlist[[m+1]][[2]][j,i]))
          }
          
          # Extract rankings (always lasts columns)
          bufferrankings <- c()
          bufferlength <- length(mainlist[[m+1]][[2]][j,])
          for (i in 1:(numitems)){
            bufferrankings <- c(bufferrankings,mainlist[[m+1]][[2]][j,bufferlength-numitems+i])
          }
          
          # Create a variable and put name of the characterisitic, then complete it with the elements ranked
          buffervector <- mainlist[[m+1]][[1]]
          
          # Loop over the items 
          for (i in 1:(numitems)){
            # Complete the vector with the rankings ordered 
            buffervector[i+1] <- bufferitems[bufferrankings[i]]
          }
          
          # Add to the existing dataframe
          buffermatrix <- rbind(buffermatrix,buffervector)
          
        }
      }
    }
    # If the dataframe of the characteristics hasn't been printed means there's no comparison with the local variable
    # Then print the other characteristics
    # if there's previous chars evaluated, pint them on the document
    if (bufferchar && bufferprint==FALSE){
      # Put the dataframe in the document
      addText(rtf, .Unicodify(tl[10,la])) 
      addText(rtf, "\n") 
      bufferdf <- data.frame(buffermatrix)
      # Different colnames if we have 3 or more items
      if (numitems==3){
        colnames(bufferdf) <- c(toString(tl[11,la]),toString(tl[12,la]),toString(tl[13,la]),toString(tl[14,la]))
      } else {
        colnames(bufferdf) <- c(toString(tl[11,la]),paste(tl[15,la],1:(numitems),sep=" "))
      }
      addTable(rtf, bufferdf)
      addPageBreak(rtf)
    }
    
    # Once all the tables about the personal rankings of the observers have been ploted
    # We plot a new table with the order of all varieties from the node where the observer belongs (same explanatory variables)
    # Get the number of the terminal node of the observer and find the index of the node, then get it from the mainlist
    indexnode <- match(terminalnodes[j][[1]],allterminals)
    classification <- mainlist[[length(mainlist)]][[6]][[length(mainlist[[length(mainlist)]][[6]])]][[indexnode]][,1]
    namesclassification <- rownames(mainlist[[length(mainlist)]][[6]][[length(mainlist[[length(mainlist)]][[6]])]][[indexnode]])
    
    # Get the indexes of when sorting from higher to lower the classification
    indexclass <- (sort(classification,decreasing=TRUE,index.return=TRUE))$ix
    
    # Classify the names from higuer to lower classification
    # First create a matrix with two columns and number of number of varieties rows
    buffermatrix <- matrix(NA,ncol=2,nrow=length(indexclass))
    for (i in 1:length(indexclass)){
      buffermatrix[i,1] <- paste(tl[15,la],i,sep=" ")
      buffermatrix[i,2] <- namesclassification[indexclass[i]]    
    } 
    
    # Convert the buffermatrix into a dataframe and change the name of the columns
    bufferdf <- data.frame(buffermatrix)
    colnames(bufferdf) <- c(tl[18,la],tl[19,la])
    
    # Write the introduction and the table
    if (paste(evterminalnodes[[1]],collapse=", ")==tl[22,la]){
      addParagraph(rtf,.Unicodify(paste(tl[20,la],tl[21,la])))
    } else {
      addParagraph(rtf,.Unicodify(paste(tl[20,la],"(",paste(evterminalnodes[[1]],collapse=", "),")",tl[21,la])))
    }
    addTable(rtf, bufferdf)
    addPageBreak(rtf)
    
    
  }
  # Close the document
  done(rtf)
}