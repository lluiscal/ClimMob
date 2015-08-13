.analysisReport <- function(filename,la,mainlist)
{
  print("putamierda")
  # Read the text we're writting in the report, depending on the languaje
  if(la==1) {
    report_txt<-as.matrix(read.delim(system.file("external/ClimMob_report_ENG.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))  
  } else {
    report_txt<-as.matrix(read.delim(system.file("external/ClimMob_report_SPA.txt", package="ClimMob"), header=FALSE, encoding="UTF-8"))      
  }
  
  
  # Assign from the list mainlist, to different variables we'll use later
  numobservers <- length(mainlist[[2]][[2]][,1])
  numitems <- mainlist[[1]][[3]]
  
  # Get the different varieties from any characteristic
  itemnames <- sort(unique(as.vector(unlist(mainlist[[2]][[2]][,1:numitems]))))
  
  # Get the characteristics evaluated 
  characs <- c()
  for (i in 2:(length(mainlist))){
    characs <- c(characs,mainlist[[i]][[1]])
  }
  
  # The file created will be placed in the same folder of the csv table file
  filename <- paste(filename,"REPORT.doc",sep="_")
  
  # Create the .doc file and start adding text, the text comes either form ClimMob_report_ENG/SPA.txt
  rtf <- RTF(filename, font.size=12) 
  addPng(rtf, system.file("external/ClimMob-logo.png", package="ClimMob"), width=3.9, height=2.2)
  addText(rtf, "\n")
  addHeader(rtf, title=.Unicodify(report_txt[1]))
  addText(rtf, "\n")
  addParagraph(rtf, .Unicodify(report_txt[2]))
  addParagraph(rtf, "\n")
  
  # Introduction paragraphs
  addHeader(rtf, .Unicodify(report_txt[4]))
  addText(rtf, "\n")    
  addParagraph(rtf, .Unicodify(report_txt[5]))    
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[6]))    
  addParagraph(rtf, "\n")
  addPageBreak(rtf)
  
  # About ClimMob
  addHeader(rtf, .Unicodify(report_txt[7]))
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[8]))
  addText(rtf, "\n")    
  addParagraph(rtf, .Unicodify(report_txt[9]))    
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[10]))    
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[11]))
  addParagraph(rtf, "\n") 
  # How to cite
  addHeader(rtf, .Unicodify(report_txt[12]))
  addText(rtf, "\n")    
  addParagraph(rtf, .Unicodify(report_txt[13]))    
  addParagraph(rtf, "\n") 
  addPageBreak(rtf)
  
  # Interpretation
  addHeader(rtf, .Unicodify(report_txt[14]))    
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[15]))
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[16]))
  addText(rtf, "\n")    
  addParagraph(rtf, .Unicodify(report_txt[17]))    
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[18]))    
  addText(rtf, "\n")    
  addParagraph(rtf, .Unicodify(report_txt[19]))    
  addText(rtf, "\n")    
  addParagraph(rtf, .Unicodify(report_txt[45]))  
  addPageBreak(rtf)
  
  addHeader(rtf, .Unicodify(report_txt[20]))
  addText(rtf, "\n") 
  varsNamesForTable <- .Unicodify(report_txt[21:24])
  varsValuesForTable <- c(numobservers,numitems,length(characs),length(itemnames))
  varsTable <- data.frame(varsNamesForTable,varsValuesForTable)
  colnames(varsTable) <- .Unicodify(report_txt[25:26])  
  addTable(rtf, varsTable)
  addText(rtf, "\n") 
  addHeader(rtf, paste(" ", .Unicodify(report_txt[27]), "\n", sep="")) #Table with item types
  itemtypes <- as.data.frame(as.matrix(itemnames))
  colnames(itemtypes) <- .Unicodify(report_txt[28])
  addTable(rtf, itemtypes)
  addText(rtf, "\n") 
  addParagraph(rtf, .Unicodify(report_txt[29])) #Table with questions
  addText(rtf, "\n") 
  qA <- as.data.frame(as.matrix(characs))
  colnames(qA) <- .Unicodify(report_txt[30])
  addTable(rtf, qA)
  addPageBreak(rtf)
  
  
  # Set up plotting functions
  worthf <- function(info) paste(info$object$labels,format(round(worth(info$object), digits = 3)), sep = ": ")
  plotworthf <- function(modelinfo) plot(modelinfo,FUN=worthf)
  plotnode <- function(nodeinfo)
  {
    # Extract info from the matrix
    Estimation <- nodeinfo[[1]][,1]
    se <- nodeinfo[[1]][,3]
    namesvars <- rownames(nodeinfo[[1]])
    node <- nodeinfo[[2]]
    explanatoryvar <- nodeinfo[[3]]
    
    # Conditions for taking the x limits, remove values where stardart error is greater than 10 (no phisical meaning):
    xlimit<-se[se<10]
    estimationlimits <- Estimation[se<10]
    items <- 1:length(Estimation)
    par(oma=c(0,4,0,0))
    plot(Estimation,items, xlim=c(min(estimationlimits)-2*max(xlimit),max(estimationlimits)+2*max(xlimit)),xaxt='n',yaxt='n',xlab="",ylab="")
    title(main=paste(report_txt[33],node,"-",report_txt[32],explanatoryvar,sep=" ")) 
    axis(1,at=c(min(estimationlimits),max(estimationlimits)),c(report_txt[44],report_txt[43]))
    axis(2,1:length(Estimation),namesvars,las=2)
    epsilon = 0.05
    for(i in 1:length(items))
    {
      right = Estimation[i] + 1.96*se[i]
      left = Estimation[i] - 1.96*se[i]
      segments(right,items[i],left, items[i])
      segments(right, items[i]-epsilon, right, items[i]+epsilon)
      segments(left, items[i]-epsilon, left , items[i]+epsilon)
    }
  }
  
  # Loop over the Characteristics and start ploting BT Threes and qvcalc graphs and tables
  for (i in 1:length(characs)){
    
    # Add paragraph
    addParagraph(rtf, .Unicodify(paste(toString(i),"-",report_txt[31],.Unicodify(mainlist[[i+1]][[1]]),sep=" ")))
    
    # Create a boolean variable, it will be true if any of the explanatory variables (or combination) have just 1 terminal node
    oneterminal <- FALSE
    
    # Loop over the explanatory variables of the characteristic
    for (j in 1:length(mainlist[[i+1]][[4]])){
      
      # Before start writting check the number of terminal nodes, if it's just one create a list and a vector:
      # - In the list, the first and second position put the results of the model it's terminal and node, just once.
      # - The vector will keep the explanatory variables (individual or combination) that has only one terminal node
      if (length(mainlist[[i+1]][[6]][[j]])==1) {
        
        # Change the value of the bolean oneterminal if false and create otlist
        if (oneterminal==FALSE){
          oneterminal <- TRUE    
        
          # Create the list and put the result of the model in the first position and the only terminal node data in the second
          otlist <- list(mainlist[[i+1]][[5]][[j]],mainlist[[i+1]][[6]][[j]])
          
          # Put the name of explanatory variable in the vector
          otvec<-c(mainlist[[i+1]][[4]][[j]])
        } else {
          # As the list it's been created, just update the variables with one node
          otvec<-c(otvec,mainlist[[i+1]][[4]][[j]])
        }
          
      } else {
      
        # If the terminal node of the explanatory variable (or combination) isn't 1, print them the usual way
        addParagraph(rtf, .Unicodify(paste(toString(i),".",toString(j),"-",report_txt[32],.Unicodify(mainlist[[i+1]][[4]][[j]]),sep=" "))) 
        addText(rtf, "\n") 
        
        # Add BT Tree graph
        # Get the number of explanatory variables evaluated at the same time
        lengthexplanatory <- length(strsplit(mainlist[[i+1]][[4]][[j]],"[+]")[[1]])
        
        if (lengthexplanatory==0){
          addPlot(rtf, plot.fun=plotworthf, width=4, height=4, res=200, mainlist[[i+1]][[5]][[j]][[1]]) 
        } else if (lengthexplanatory==1){
          addPlot(rtf, plot.fun=plotworthf, width=4, height=5, res=200, mainlist[[i+1]][[5]][[j]][[1]]) 
        } else if (lengthexplanatory==2){
          addPlot(rtf, plot.fun=plotworthf, width=5, height=8, res=200,mainlist[[i+1]][[5]][[j]][[1]]) 
        } else if (lengthexplanatory==3){
          addPlot(rtf, plot.fun=plotworthf, width=6, height=8.5, res=200, mainlist[[i+1]][[5]][[j]][[1]]) 
        } else {
          addPlot(rtf, plot.fun=plotworthf, width=6.5, height=8.5, res=200,mainlist[[i+1]][[5]][[j]][[1]]) 
        }
        
        # Add a page break at the end of every 
        addPageBreak(rtf) 
        
        # Once the BT Tree is ploted, plot the nodes and it's nnumbers
        # Get the terminal nodes of the BT Three ploted
        tn <- nodeids(mainlist[[i+1]][[5]][j][[1]],terminal=TRUE)
        
        for (k in 1:length(tn)){
          addParagraph(rtf, .Unicodify(paste(toString(i),".",toString(j),".",toString(k),"-",report_txt[33],tn[k],sep=" ")))
          addParagraph(rtf)
          addPlot(rtf, plot.fun=plotnode, width=6.5, height=4, res=200, list(mainlist[[i+1]][[6]][[j]][[k]],tn[k],mainlist[[i+1]][[4]][[j]][[1]])) 
          addParagraph(rtf) 
          # Get the matrix of data
          itemtypes <- as.matrix(mainlist[[i+1]][[6]][[j]][[k]])
          # Round all anumbers to 3 decimals
          itemtypes <- round(itemtypes,digits=3)
          # Get the names of the rows
          buffer <- rownames(itemtypes)
          # Include the names as the first column
          itemtypes <- cbind(buffer,itemtypes)
          # Name the columnes
          colnames(itemtypes) <- c(report_txt[46],report_txt[47],report_txt[48],report_txt[49],report_txt[50])
          # Put it into the report
          itemtypes <- data.frame(itemtypes)
          addTable(rtf, itemtypes)
          # Add a page break after plotting the node's plot and table
          addPageBreak(rtf)
        }
      }
    }
    # Before going to the next characteristic, print the model and the node of all the explanatory variable (or combination)
    # that have just one terminal node
    if (oneterminal){
      
      # Print the explanation about the no significance in the report
      addParagraph(rtf, .Unicodify(paste(report_txt[51],paste(otvec,collapse=", "),report_txt[52],sep="")))
      addParagraph(rtf)
      # Add the worth plot
      addPlot(rtf, plot.fun=plotworthf, width=2, height=2, res=200, otlist[[1]])
      addParagraph(rtf)
      # Add the node plot
      addParagraph(rtf, .Unicodify(report_txt[53]))
      addParagraph(rtf)
      addPlot(rtf, plot.fun=plotnode, width=6.5, height=4, res=200, list(otlist[[2]][[1]],1,"No Significance")) 
      addParagraph(rtf)
      # Get the matrix of data
      itemtypes <- as.matrix(otlist[[2]][[1]])
      # Round all anumbers to 3 decimals
      itemtypes <- round(itemtypes,digits=3)
      # Get the names of the rows
      buffer <- rownames(itemtypes)
      # Include the names as the first column
      itemtypes <- cbind(buffer,itemtypes)
      # Name the columnes
      colnames(itemtypes) <- c(.Unicodify(report_txt[46]),.Unicodify(report_txt[47]),.Unicodify(report_txt[48]),.Unicodify(report_txt[49]),.Unicodify(report_txt[50]))
      # Put it into the report
      addTable(rtf, itemtypes)
      addPageBreak(rtf)      
    }
  }
  
  #References don't need .Unicodify
  addHeader(rtf, report_txt[35])
  addText(rtf, "\n") 
  addParagraph(rtf, paste("- ",report_txt[36]))
  addParagraph(rtf, paste("- ",report_txt[37]))
  addParagraph(rtf, paste("- ",report_txt[38]))
  addParagraph(rtf, paste("- ",report_txt[39]))
  addParagraph(rtf, paste("- ",report_txt[40]))
  addParagraph(rtf, paste("- ",report_txt[41]))
  addParagraph(rtf, paste("- ",report_txt[42]))
  
  done(rtf)
  
                 
}