##==== R-Commander Plug-in for Meta-Analysis with Correlations (MAc) =====##

#require('MAc')


.packageName <- "RcmdrPlugin.MAc"

.First.lib <- function(libname, pkgname){
if (!interactive()) return()
Rcmdr <- options()$Rcmdr
plugins <- Rcmdr$plugins
if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
Rcmdr$plugins <- c(plugins, pkgname)
options(Rcmdr=Rcmdr)
closeCommander(ask=FALSE, ask.save=TRUE)
Commander()
}
}

#== New Functions (3.07.10) ==##

# ancova to d1

ancova_to_d1cmd <- function(){
  initializeDialog(title=gettextRcmdr("ancova to mean diff (adj. SD)"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("adj. tmt grp mean (ANCOVA)"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  conVar <- tclVar(gettextRcmdr(" "))
  conFrame <- tkframe(labelsFrame)
  conEntry <- ttkentry(conFrame, width="8", textvariable=conVar)
  tkgrid(labelRcmdr(conFrame, text=gettextRcmdr("adj comp grp mean (ANCOVA)"), fg="blue"), sticky="w")
  tkgrid(conEntry, sticky="w")
  tkgrid(conFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  sdVar <- tclVar(gettextRcmdr(" "))
  sdFrame <- tkframe(labelsFrame)
  sdEntry <- ttkentry(sdFrame, width="8", textvariable=sdVar)
  tkgrid(labelRcmdr(sdFrame, text=gettextRcmdr("adj SD"), fg="blue"), sticky="w")
  tkgrid(sdEntry, sticky="w")
  tkgrid(sdFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("covar/multi cor"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  covVar <- tclVar(gettextRcmdr(" "))
  covFrame <- tkframe(labelsFrame)
  covEntry <- ttkentry(covFrame, width="8", textvariable=covVar)
  tkgrid(labelRcmdr(covFrame, text=gettextRcmdr("n of covar."), fg="blue"), sticky="w")
  tkgrid(covEntry, sticky="w")
  tkgrid(covFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
    
    con <- trim.blanks(tclvalue(conVar))
    con <- paste(', ', con, '', sep="")
    
    sd <- trim.blanks(tclvalue(sdVar))
    sd <- paste(', ', sd, '', sep="")
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    cov <- trim.blanks(tclvalue(covVar))
    cov <- paste(', ', cov, '', sep="")
    
    doItAndPrint(paste("ancova_to_d1(", tmt, con, sd, n1, n2, cor, cov,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="ancova_to_d1")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}



# ancova to d2

ancova_to_d2cmd <- function(){
  initializeDialog(title=gettextRcmdr("ancova to mean diff (pooled SD)"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("adj tmt grp mean (ANCOVA)"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  conVar <- tclVar(gettextRcmdr(" "))
  conFrame <- tkframe(labelsFrame)
  conEntry <- ttkentry(conFrame, width="8", textvariable=conVar)
  tkgrid(labelRcmdr(conFrame, text=gettextRcmdr("adj comp grp mean (ANCOVA)"), fg="blue"), sticky="w")
  tkgrid(conEntry, sticky="w")
  tkgrid(conFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  sdVar <- tclVar(gettextRcmdr(" "))
  sdFrame <- tkframe(labelsFrame)
  sdEntry <- ttkentry(sdFrame, width="8", textvariable=sdVar)
  tkgrid(labelRcmdr(sdFrame, text=gettextRcmdr("pooled SD"), fg="blue"), sticky="w")
  tkgrid(sdEntry, sticky="w")
  tkgrid(sdFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("covar/multi cor"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  covVar <- tclVar(gettextRcmdr(" "))
  covFrame <- tkframe(labelsFrame)
  covEntry <- ttkentry(covFrame, width="8", textvariable=covVar)
  tkgrid(labelRcmdr(covFrame, text=gettextRcmdr("n of covar"), fg="blue"), sticky="w")
  tkgrid(covEntry, sticky="w")
  tkgrid(covFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
    
    con <- trim.blanks(tclvalue(conVar))
    con <- paste(', ', con, '', sep="")
    
    sd <- trim.blanks(tclvalue(sdVar))
    sd <- paste(', ', sd, '', sep="")
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    cov <- trim.blanks(tclvalue(covVar))
    cov <- paste(', ', cov, '', sep="")
    
    doItAndPrint(paste("ancova_to_d2(", tmt, con, sd, n1, n2, cor, cov,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="ancova_to_d2")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# d to g

d_to_gcmd <- function(){
  initializeDialog(title=gettextRcmdr("d to unbiased g"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("d"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  conVar <- tclVar(gettextRcmdr(" "))
  conFrame <- tkframe(labelsFrame)
  conEntry <- ttkentry(conFrame, width="8", textvariable=conVar)
  tkgrid(labelRcmdr(conFrame, text=gettextRcmdr("variance of d"), fg="blue"), sticky="w")
  tkgrid(conEntry, sticky="w")
  tkgrid(conFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
    
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   
    
    con <- trim.blanks(tclvalue(conVar))
    con <- paste(', ', con, '', sep="")
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
            
    doItAndPrint(paste("d_to_g(", tmt, con, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="d_to_g")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}



# f to d

f_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("f-value to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("f-value"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
     
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
    
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
            
    doItAndPrint(paste("f_to_d(", tmt, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="f_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# f ancova to d

f.ancova_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("f-value (ANCOVA) to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("adj tmt grp mean (ANCOVA)"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("covar/multi cor"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  covVar <- tclVar(gettextRcmdr(" "))
  covFrame <- tkframe(labelsFrame)
  covEntry <- ttkentry(covFrame, width="8", textvariable=covVar)
  tkgrid(labelRcmdr(covFrame, text=gettextRcmdr("n of covar"), fg="blue"), sticky="w")
  tkgrid(covEntry, sticky="w")
  tkgrid(covFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    cov <- trim.blanks(tclvalue(covVar))
    cov <- paste(', ', cov, '', sep="")
    
    doItAndPrint(paste("f.ancova_to_d(", tmt, n1, n2, cor, cov,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="f.ancova_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}



# lor to d

lor_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("log odds ratio to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("log odds ratio (LOR)"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
     
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("variance of LOR"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
        
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
               
    doItAndPrint(paste("lor_to_d(", tmt, n1,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="lor_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# mean to d

mean_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("raw means and SDs to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("tmt grp mean"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  conVar <- tclVar(gettextRcmdr(" "))
  conFrame <- tkframe(labelsFrame)
  conEntry <- ttkentry(conFrame, width="8", textvariable=conVar)
  tkgrid(labelRcmdr(conFrame, text=gettextRcmdr("comp grp mean"), fg="blue"), sticky="w")
  tkgrid(conEntry, sticky="w")
  tkgrid(conFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  sdVar <- tclVar(gettextRcmdr(" "))
  sdFrame <- tkframe(labelsFrame)
  sdEntry <- ttkentry(sdFrame, width="8", textvariable=sdVar)
  tkgrid(labelRcmdr(sdFrame, text=gettextRcmdr("SD of tmt grp"), fg="blue"), sticky="w")
  tkgrid(sdEntry, sticky="w")
  tkgrid(sdFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("SD of comp grp"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
    
    con <- trim.blanks(tclvalue(conVar))
    con <- paste(', ', con, '', sep="")
    
    sd <- trim.blanks(tclvalue(sdVar))
    sd <- paste(', ', sd, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    
   
    
    doItAndPrint(paste("mean_to_d(", tmt, con, sd, cor, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="mean_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

# mean to d II

mean_to_d2cmd <- function(){
  initializeDialog(title=gettextRcmdr("means with pooled SD to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("tmt grp mean"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  conVar <- tclVar(gettextRcmdr(" "))
  conFrame <- tkframe(labelsFrame)
  conEntry <- ttkentry(conFrame, width="8", textvariable=conVar)
  tkgrid(labelRcmdr(conFrame, text=gettextRcmdr("comp grp mean"), fg="blue"), sticky="w")
  tkgrid(conEntry, sticky="w")
  tkgrid(conFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  sdVar <- tclVar(gettextRcmdr(" "))
  sdFrame <- tkframe(labelsFrame)
  sdEntry <- ttkentry(sdFrame, width="8", textvariable=sdVar)
  tkgrid(labelRcmdr(sdFrame, text=gettextRcmdr("pooled SD"), fg="blue"), sticky="w")
  tkgrid(sdEntry, sticky="w")
  tkgrid(sdFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
    
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
    
    con <- trim.blanks(tclvalue(conVar))
    con <- paste(', ', con, '', sep="")
    
    sd <- trim.blanks(tclvalue(sdVar))
    sd <- paste(', ', sd, '', sep="")
      
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    doItAndPrint(paste("mean_to_d2(", tmt, con, sd, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="mean_to_d2")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

# p ancova to d

p.ancova_to_d1cmd <- function(){
  initializeDialog(title=gettextRcmdr("one-tailed p-value (ANCOVA) to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("p-value from an ANCOVA"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("covar/multi cor"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  covVar <- tclVar(gettextRcmdr(" "))
  covFrame <- tkframe(labelsFrame)
  covEntry <- ttkentry(covFrame, width="8", textvariable=covVar)
  tkgrid(labelRcmdr(covFrame, text=gettextRcmdr("n of covar"), fg="blue"), sticky="w")
  tkgrid(covEntry, sticky="w")
  tkgrid(covFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    cov <- trim.blanks(tclvalue(covVar))
    cov <- paste(', ', cov, '', sep="")
    
    doItAndPrint(paste("p.ancova_to_d1(", tmt, n1, n2, cor, cov,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="p.ancova_to_d1")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

# p ancova to d II

p.ancova_to_d2cmd <- function(){
  initializeDialog(title=gettextRcmdr("two-tailed p-value (ANCOVA) to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("two-tailed p-value from an ANCOVA"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("covar/multi cor"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  covVar <- tclVar(gettextRcmdr(" "))
  covFrame <- tkframe(labelsFrame)
  covEntry <- ttkentry(covFrame, width="8", textvariable=covVar)
  tkgrid(labelRcmdr(covFrame, text=gettextRcmdr("n of covar"), fg="blue"), sticky="w")
  tkgrid(covEntry, sticky="w")
  tkgrid(covFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    cov <- trim.blanks(tclvalue(covVar))
    cov <- paste(', ', cov, '', sep="")
    
    doItAndPrint(paste("p.ancova_to_d2(", tmt, n1, n2, cor, cov,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="p.ancova_to_d2")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# p to d1

p_to_d1cmd <- function(){
  initializeDialog(title=gettextRcmdr("one-tailed p-value to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("p-value"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
     
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
    
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
            
    doItAndPrint(paste("p_to_d1(", tmt, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="p_to_d1")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

# p to d2

p_to_d2cmd <- function(){
  initializeDialog(title=gettextRcmdr("two-tailed p-value to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("p-value"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
     
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
    
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   
    
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
            
    doItAndPrint(paste("p_to_d2(", tmt, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="p_to_d2")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# prop to d

prop_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("proportions to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("proportion one"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("proportion two"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # proportion 1 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar)) #prop 2
    cor <- paste(', ', cor, '', sep="")
    
      
    doItAndPrint(paste("prop_to_d(", tmt, cor, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="prop_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# prop to or

prop_to_orcmd <- function(){
  initializeDialog(title=gettextRcmdr("proportions to odds ratio"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("proportion one"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("total sample size for group A and B"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("total sample size for group C and D"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("proportion two"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # proportion 1 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar)) #prop 2
    cor <- paste(', ', cor, '', sep="")
    
      
    doItAndPrint(paste("prop_to_or(", tmt, cor, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="prop_to_or")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# pub bias

PubBiascmd <- function(){
  initializeDialog(title=gettextRcmdr("Publication bias"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("meandiff.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  #subsetBox()
  onOK <- function(){ 
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      UpdateModelNumber(-1)
      errorCondition(recall=MeanDiffdcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
      return()
    }
    closeDialog() 
    meta <- ActiveDataSet()
	  command <- paste(paste("PubBias(", meta,")", sep=""))
    logger(paste(modelValue, " <- ", command, sep=""))
    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
    doItAndPrint(modelValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="PubBias", model=TRUE)
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for data:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid.configure(helpButton, sticky="e")
  tkgrid(variablesFrame, sticky="w")
  #tkgrid(modelNFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=4, columns=2)
}  
  

# r to d

r_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("correlation to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
    
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("sample size"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("correlation value"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  onOK <- function(){
    closeDialog()
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
           
    cor <- trim.blanks(tclvalue(corVar)) #prop 2
    cor <- paste(', ', cor, '', sep="")
    
      
    doItAndPrint(paste("r_to_d(",cor, n1,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="r_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

# t ancova to d

tt.ancova_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("t-value (ANCOVA) to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("t-value (ANCOVA)"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt grp"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp grp"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  corVar <- tclVar(gettextRcmdr(" "))
  corFrame <- tkframe(labelsFrame)
  corEntry <- ttkentry(corFrame, width="8", textvariable=corVar)
  tkgrid(labelRcmdr(corFrame, text=gettextRcmdr("covar/multi cor"), fg="blue"), sticky="w")
  tkgrid(corEntry, sticky="w")
  tkgrid(corFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  covVar <- tclVar(gettextRcmdr(" "))
  covFrame <- tkframe(labelsFrame)
  covEntry <- ttkentry(covFrame, width="8", textvariable=covVar)
  tkgrid(labelRcmdr(covFrame, text=gettextRcmdr("n of covar"), fg="blue"), sticky="w")
  tkgrid(covEntry, sticky="w")
  tkgrid(covFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # paste(' stat, ') 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
    
    cor <- trim.blanks(tclvalue(corVar))
    cor <- paste(', ', cor, '', sep="")
    
    cov <- trim.blanks(tclvalue(covVar))
    cov <- paste(', ', cov, '', sep="")
    
    doItAndPrint(paste("tt.ancova_to_d(", tmt, n1, n2, cor, cov,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="tt.ancova_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# t to d

t_to_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("t-value to d"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  
  tmtVar <- tclVar(gettextRcmdr(" "))
  tmtFrame <- tkframe(labelsFrame)
  tmtEntry <- ttkentry(tmtFrame, width="8", textvariable=tmtVar)
  tkgrid(labelRcmdr(tmtFrame, text=gettextRcmdr("t-value"), fg="blue"), sticky="w")
  tkgrid(tmtEntry, sticky="w")
  tkgrid(tmtFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
      
  n1Var <- tclVar(gettextRcmdr(" "))
  n1Frame <- tkframe(labelsFrame)
  n1Entry <- ttkentry(n1Frame, width="8", textvariable=n1Var)
  tkgrid(labelRcmdr(n1Frame, text=gettextRcmdr("n of tmt group"), fg="blue"), sticky="w")
  tkgrid(n1Entry, sticky="w")
  tkgrid(n1Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  
  n2Var <- tclVar(gettextRcmdr(" "))
  n2Frame <- tkframe(labelsFrame)
  n2Entry <- ttkentry(n2Frame, width="8", textvariable=n2Var)
  tkgrid(labelRcmdr(n2Frame, text=gettextRcmdr("n of comp group"), fg="blue"), sticky="w")
  tkgrid(n2Entry, sticky="w")
  tkgrid(n2Frame, labelRcmdr(labelsFrame, text="     "), sticky="w")
         
  onOK <- function(){
    closeDialog()
    tmt <- trim.blanks(tclvalue(tmtVar))
    tmt <- paste (' ', tmt, '', sep="")   # proportion 1 
        
    n1 <- trim.blanks(tclvalue(n1Var))
    n1 <- paste(', ', n1, '', sep="")
    
    n2 <- trim.blanks(tclvalue(n2Var))
    n2 <- paste(', ', n2, '', sep="")
      
    doItAndPrint(paste("t_to_d(", tmt, n1, n2,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="t_to_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}



##==== Calculate ES ====##


# r_from_chi

r_from_chicmd <- function(){
  initializeDialog(title=gettextRcmdr("r from chi-squared"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="8", textvariable=modnameVar)
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("sample size"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")   # paste(' stat, ') 
    n <- trim.blanks(tclvalue(modnameVar))
    n <- paste(', ', n, '', sep="")
    doItAndPrint(paste("r_from_chi(", stat, n,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="r_from_chi")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}



# r_from_d 

r_from_dcmd <- function(){
  initializeDialog(title=gettextRcmdr("r from mean difference"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported d statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="8", textvariable=modnameVar)
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("variance of d"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")   # paste(' stat, ') 
    var.d <- trim.blanks(tclvalue(modnameVar))
    var.d <- paste(', ', var.d, '', sep="")
    doItAndPrint(paste("r_from_d(", stat, var.d,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="r_from_d")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


# r_from_d1 

r_from_d1cmd <- function(){
  initializeDialog(title=gettextRcmdr("r from d (n of both grps not same)"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported d statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="8", textvariable=modnameVar)
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("n of 1st group"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  ylimVar <- tclVar(gettextRcmdr(" "))
  ylimFrame <- tkframe(labelsFrame)
  ylimEntry <- ttkentry(ylimFrame, width="8", textvariable=ylimVar)
  tkgrid(labelRcmdr(ylimFrame, text=gettextRcmdr("n of 2nd group"), fg="blue"), sticky="w")
  tkgrid(ylimEntry, sticky="w")
  tkgrid(ylimFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  varVar <- tclVar(gettextRcmdr(" "))
  varFrame <- tkframe(labelsFrame)
  varEntry <- ttkentry(varFrame, width="8", textvariable=varVar)
  tkgrid(labelRcmdr(varFrame, text=gettextRcmdr("variance of d"), fg="blue"), sticky="w")
  tkgrid(varEntry, sticky="w")
  tkgrid(varFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")   # paste(' stat, ') 
    n1 <- trim.blanks(tclvalue(modnameVar))
    n1 <- paste(', ', n1, '', sep="")
    n2 <- trim.blanks(tclvalue(ylimVar))
    n2 <- paste(', ', n2, '', sep="")
    var <- trim.blanks(tclvalue(varVar))
    var <- paste(', ', var, '', sep="")
    doItAndPrint(paste("r_from_d1(", stat, n1, n2, var,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="r_from_d1")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

# r_from_t

r_from_tcmd <- function(){
  initializeDialog(title=gettextRcmdr("r from t-test"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="8", textvariable=modnameVar)
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("sample size"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")  
    n <- trim.blanks(tclvalue(modnameVar))
    n <- paste(', ', n, '', sep="")
    doItAndPrint(paste("r_from_t(", stat, n,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="r_from_t")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}

  
# r_to_z 

r_to_zcmd <- function(){
  initializeDialog(title=gettextRcmdr("calculate z'"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("correlation"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")  
    doItAndPrint(paste("r_to_z(", stat,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="r_to_z")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}




# var_r 

var_rcmd <- function(){
  initializeDialog(title=gettextRcmdr("calculate variance of r"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("correlation"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="8", textvariable=modnameVar)
  modnameScroll <- ttkscrollbar(modnameFrame, orient="horizontal",
                             command=function(...) tkxview(modnameEntry, ...))
  tkconfigure(modnameEntry, xscrollcommand=function(...) tkset(modnameScroll, ...))
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("sample size"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameScroll, sticky="ew")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")  
    n <- trim.blanks(tclvalue(modnameVar))
    n <- paste(', ', n, '', sep="")
    doItAndPrint(paste("var_r(", stat, n,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="var_r")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}



# var_z 

var_zcmd <- function(){
  initializeDialog(title=gettextRcmdr("calculate variance of z'"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="8", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("sample size"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste (' ', stat, '', sep="")  
    doItAndPrint(paste("var_z(", stat,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="var_z")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=8, columns=2)
}


##==== Within-Study Aggregation

# MetaR function

MetaRcmd <- function(){
  initializeDialog(title=gettextRcmdr("Meta-Analysis aggregation"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("aggdata.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  onOK <- function(){ 
  closeDialog() 
  modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      UpdateModelNumber(-1)
      errorCondition(recall=MetaRcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
      return()
  }
  meta <- ActiveDataSet()
  command <- paste(paste("MetaR(", meta, ")", sep=""))
  logger(paste(modelValue, " <- ", command, sep=""))
  assign(modelValue, justDoIt(command), envir=.GlobalEnv)
  doItAndPrint(modelValue)
  tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="MetaR", model=TRUE)
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for data:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid.configure(helpButton, sticky="e")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=4, columns=2)
}

# ComplData function  # 02.10.10 need to update MAc (1.0.2) package*

ComplDatacmd <- function(){
  initializeDialog(title=gettextRcmdr("Complete Dataset"))
  variablesFrame <- tkframe(top)
  .variable <- Variables()
  xBox <- variableListBox(variablesFrame, .variable, selectmode="multiple",
            title=gettextRcmdr("variables for complete data (pick one)"))
  UpdateModelNumber()
  modelName <- tclVar(paste("data.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  onOK <- function(){
    x <- getSelection(xBox)
    closeDialog()
      if (0 == length(x)) {
        UpdateModelNumber(-1)
        errorCondition(recall=ComplDatacmd , message=gettextRcmdr("No variables selected."))
        return()
      }
      modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
          UpdateModelNumber(-1)
          errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
          return()
       }
       meta <- ActiveDataSet()
       modelN <- as.character(tclvalue(modelNVariable)) 
       command <- paste("ComplData(", meta, ", ", paste(x, collapse=","),
                        ", type= '",modelN,"')", sep="")
       #command <- paste("ComplData(", meta, ",", meta, "$", x, ")", sep="")
       logger(paste(modelValue, " <- ", command, sep=""))
       assign(modelValue, justDoIt(command), envir=.GlobalEnv)
       doItAndPrint(modelValue)
       tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="ComplData", model=TRUE)
  radioButtons(name="modelN", buttons=c("independent", "dependent"), values=c("independent", "dependent"),   
               labels=gettextRcmdr(c("independent", "dependent")), title=gettextRcmdr("Type of aggregation:"))
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(modelNFrame, sticky="w")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(subsetFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  tkgrid.configure(helpButton, sticky="e")
  dialogSuffix(rows=4, columns=2)
}

##==== Omnibus Analysis ====##
    
OmnibusEScmd <- function(){
  initializeDialog(title=gettextRcmdr("Omnibus Effect Size (Fixed and Random Effects)"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("omnidata.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  onOK <- function(){ 
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      UpdateModelNumber(-1)
      errorCondition(recall=OmnibusEScmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
      return()
    }
    closeDialog() 
    meta <- ActiveDataSet()
	modelN <- as.character(tclvalue(modelNVariable)) 
    command <- paste(paste("OmnibusES(", meta, ", var= '",modelN,"')", sep=""))
    logger(paste(modelValue, " <- ", command, sep=""))
    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
    doItAndPrint(modelValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="OmnibusES", model=TRUE)
  radioButtons(name="modelN", buttons=c("Weighted", "Unweighted"), values=c("weighted", "unweighted"),   
               labels=gettextRcmdr(c("Weighted", "Unweighted")), title=gettextRcmdr("Method"))
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for data:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid.configure(helpButton, sticky="e")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(modelNFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  dialogSuffix(rows=4, columns=2)
}


##==== Moderator ====#

# CatMod function

CatModcmd <- function(){
  initializeDialog(title=gettextRcmdr("Categorical Moderation"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("moddata.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  .factor <- Factors()
  xBox <- variableListBox(top, .factor, title=gettextRcmdr("moderator variables (pick one)"))
  onOK <- function(){
    x <- getSelection(xBox)
    if (length(x) == 0){
      errorCondition(recall=CatModcmd, message=gettextRcmdr("You must select one variable."))
      return()
    }
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      UpdateModelNumber(-1)
      errorCondition(recall=CatModcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
      return()
    }     
    closeDialog()
    meta <- ActiveDataSet()
    command <- paste("CatMod(", meta, ",", meta, "$", x, ")", sep="")
    logger(paste(modelValue, " <- ", command, sep=""))
    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
    doItAndPrint(modelValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="CatMod")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  tkgrid.configure(helpButton, sticky="e")
  dialogSuffix(rows=4, columns=2)
}

# CatModr

#CatModrcmd <- function(){
#  initializeDialog(title=gettextRcmdr("Categorical Moderation (Random)"))
#  variablesFrame <- tkframe(top)
#  UpdateModelNumber()
#  modelName <- tclVar(paste("moddata.", getRcmdr("modelNumber"), sep=""))
#  modelFrame <- tkframe(top)
#  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
#  subsetBox()
#  .factor <- Factors()
#  xBox <- variableListBox(top, .factor, title=gettextRcmdr("moderator variables (pick one)"))
#  onOK <- function(){
#    x <- getSelection(xBox)
#      if (length(x) == 0){
#        errorCondition(recall=CatModrcmd, message=gettextRcmdr("You must select one or more variables."))
#        return()
#      }
#      modelValue <- trim.blanks(tclvalue(modelName))
#        if (!is.valid.name(modelValue)){
#          UpdateModelNumber(-1)
#          errorCondition(recall=CatModrcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
#          return()
#      }
#      closeDialog()
#      meta <- ActiveDataSet()
#      command <- paste("CatModr(", meta, ",", meta, "$", x, ")", sep="")
#      logger(paste(modelValue, " <- ", command, sep=""))
#      assign(modelValue, justDoIt(command), envir=.GlobalEnv)
#      doItAndPrint(modelValue)
#      tkfocus(CommanderWindow())
#  }
#  OKCancelHelp(helpSubject="CatModr")
#  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
#  tkgrid(modelFrame, sticky="w")
#  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
#  tkgrid(variablesFrame, sticky="w")
#  tkgrid(buttonsFrame, stick="w")
#  tkgrid.configure(helpButton, sticky="e")
#  dialogSuffix(rows=4, columns=2)
#}

#CatModfQ

#CatModfQcmd <- function(){
#  initializeDialog(title=gettextRcmdr("Categorical Moderator Q-test (Fixed)"))
#  variablesFrame <- tkframe(top)
#  UpdateModelNumber()
#  modelName <- tclVar(paste("moddata.", getRcmdr("modelNumber"), sep=""))
#  modelFrame <- tkframe(top)
#  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
#  subsetBox()
#  .factor <- Factors()
#  xBox <- variableListBox(top, .factor, title=gettextRcmdr("moderator variables (pick one)"))
# onOK <- function(){
#   x <- getSelection(xBox)
#     if (length(x) == 0){
#       errorCondition(recall=CatModfQcmd, message=gettextRcmdr("You must select one or more variables."))
#       return()
#    }
#    modelValue <- trim.blanks(tclvalue(modelName))
#      if (!is.valid.name(modelValue)){
#        UpdateModelNumber(-1)
#        errorCondition(recall=CatModfcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
#        return()
#    }      
#    closeDialog()
#   meta <- ActiveDataSet()
#    command <- paste("CatModfQ(", meta, ",", meta, "$", x, ")", sep="")
#    logger(paste(modelValue, " <- ", command, sep=""))
#    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
#    doItAndPrint(modelValue)
#    tkfocus(CommanderWindow())
#  }
#  OKCancelHelp(helpSubject="CatModfQ")
#  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
#  tkgrid(modelFrame, sticky="w")
#  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
#  tkgrid(variablesFrame, sticky="w")
#  tkgrid(buttonsFrame, stick="w")
#  tkgrid.configure(helpButton, sticky="e")
#  dialogSuffix(rows=4, columns=2)
#}

#CatModrQ

#CatModrQcmd <- function(){
#  initializeDialog(title=gettextRcmdr("Categorical Moderator Q-test (Random)"))
#  variablesFrame <- tkframe(top)
#  UpdateModelNumber()
#  modelName <- tclVar(paste("moddata.", getRcmdr("modelNumber"), sep=""))
#  modelFrame <- tkframe(top)
#  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
#  subsetBox()
#  .factor <- Factors()
#  xBox <- variableListBox(top, .factor, title=gettextRcmdr("moderator variables (pick one)"))
#  onOK <- function(){
#    x <- getSelection(xBox)
#    # y <- getSelection(yBox)
#      if (length(x) == 0){
#        errorCondition(recall=CatModrQcmd, message=gettextRcmdr("You must select one or more variables."))
#        return()
#    }
#    modelValue <- trim.blanks(tclvalue(modelName))
#      if (!is.valid.name(modelValue)){
#        UpdateModelNumber(-1)
#        errorCondition(recall=CatModrcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
#        return()
#    }     
#    closeDialog()
#    meta <- ActiveDataSet()
#    command <- paste("CatModrQ(", meta, ",", meta, "$", x, ")", sep="")
#    logger(paste(modelValue, " <- ", command, sep=""))
#    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
#    doItAndPrint(modelValue)
#    tkfocus(CommanderWindow())
#  }
#  OKCancelHelp(helpSubject="CatModrQ")
#  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
#  tkgrid(modelFrame, sticky="w")
#  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
#  tkgrid(variablesFrame, sticky="w")
# tkgrid(buttonsFrame, stick="w")
#  tkgrid.configure(helpButton, sticky="e")
#  dialogSuffix(rows=4, columns=2)
#}

#CatCompf

#CatCompfcmd <- function(){
#  initializeDialog(title=gettextRcmdr("Direct Categorical Moderator Comparison (Fixed)"))
#  variablesFrame <- tkframe(top)
#  .factor <- Factors()
#  yBox <- variableListBox(variablesFrame, .factor, title=gettextRcmdr("moderator variable (pick one)"))
#  UpdateModelNumber()
#  modelName <- tclVar(paste("modcompdata.", getRcmdr("modelNumber"), sep=""))
#  modelFrame <- tkframe(top)
#  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
#  subsetBox()
#  onOK <- function(){
#    y <- getSelection(yBox)
#    closeDialog()
#      if (0 == length(y)) {
#        UpdateModelNumber(-1)
#        errorCondition(recall=CatCompfcmd, message=gettextRcmdr("You must select a moderator variable."))
#        return()
#    }
#    modelValue <- trim.blanks(tclvalue(modelName))
#      if (!is.valid.name(modelValue)){
#        UpdateModelNumber(-1)
#        errorCondition(recall=CatCompfcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
#        return()
#    }
#    meta <- ActiveDataSet()
#    modelN <- as.character(tclvalue(modelNVariable)) 
#    modelC1 <- as.character(tclvalue(modelC1Variable)) 
#    modelC2 <- as.character(tclvalue(modelC2Variable))
#    command <- paste("CatCompf(", meta, ", ", paste(y, collapse=","),
#                     ", ",modelC1,", ",modelC2,", method= '",modelN,"')", sep="")
#    logger(paste(modelValue, " <- ", command, sep=""))
#    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
#    doItAndPrint(modelValue)
#     tkfocus(CommanderWindow())
#  }
#  OKCancelHelp(helpSubject="CatCompf", model=TRUE)
#  radioButtons(name="modelN", buttons=c("post.hoc", "planned"), 
#               values=c("post.hoc", "planned"),   
#               labels=gettextRcmdr(c("post.hoc", "planned")), 
#               title=gettextRcmdr("method"))
#  radioButtons(name="modelC1", buttons=c("one", "two", "three","four", "five","six"),
#               values=c("1", "2", "3","4", "5","6"),   
#               labels=gettextRcmdr(c("one", "two", "three","four", "five","six")), 
#               title=gettextRcmdr("choose 1st levels of factor to compare"))    
#  radioButtons(name="modelC2", buttons=c("one", "two", "three","four", "five","six"),
#               values=c("1", "2", "3","4", "5","6"),   
#               labels=gettextRcmdr(c("one", "two", "three","four", "five","six")),
#                title=gettextRcmdr("choose 2nd levels of factor to compare"))    
#  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), 
#         model, sticky="w")
#  tkgrid(modelFrame, sticky="w")
#  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(yBox), sticky="nw")
#  tkgrid(modelNFrame, sticky="w")
#  tkgrid(modelC1Frame, sticky="w")
#  tkgrid(modelC2Frame, sticky="w")
#  tkgrid(variablesFrame, sticky="w")
#  tkgrid(buttonsFrame, stick="w")
#  tkgrid.configure(helpButton, sticky="e")
#  dialogSuffix(rows=4, columns=2)
#}

#CatComp

CatCompcmd <- function(){
  initializeDialog(title=gettextRcmdr("Direct Categorical Moderator Comparison"))
  variablesFrame <- tkframe(top)
  .factor <- Factors()
  yBox <- variableListBox(variablesFrame, .factor, title=gettextRcmdr("moderator variable (pick one)"))
  UpdateModelNumber()
  modelName <- tclVar(paste("modcompdata.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  onOK <- function(){
    y <- getSelection(yBox)
    closeDialog()
      if (0 == length(y)) {
        UpdateModelNumber(-1)
        errorCondition(recall=CatCompcmd, message=gettextRcmdr("You must select a moderator variable."))
        return()
    }
    modelValue <- trim.blanks(tclvalue(modelName))
      if (!is.valid.name(modelValue)){
        UpdateModelNumber(-1)
        errorCondition(recall=CatCompcmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
        return()
    }
    meta <- ActiveDataSet()
    modelN <- as.character(tclvalue(modelNVariable)) 
    modelC1 <- as.character(tclvalue(modelC1Variable)) 
    modelC2 <- as.character(tclvalue(modelC2Variable))
    command <- paste("CatComp(", meta, ", ", paste(y, collapse=","),
                     ", ",modelC1,", ",modelC2,", method= '",modelN,"')", sep="")
    logger(paste(modelValue, " <- ", command, sep=""))
    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
    doItAndPrint(modelValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="CatComp", model=TRUE)
  radioButtons(name="modelN", buttons=c("post.hoc1", "post.hoc2","planned"), values=c("post.hoc1", "post.hoc2","planned"),   
               labels=gettextRcmdr(c("post.hoc: HSD", "post.hoc: Scheffe", "planned")), title=gettextRcmdr("method"))
  radioButtons(name="modelC1", buttons=c("one", "two", "three","four", "five","six"), 
               values=c("1", "2", "3","4", "5","6"),   
               labels=gettextRcmdr(c("one", "two", "three","four", "five","six")),
               title=gettextRcmdr("choose 1st levels of factor to compare"))    
  radioButtons(name="modelC2", buttons=c("one", "two", "three","four", "five","six"), 
               values=c("1", "2", "3","4", "5","6"),   
               labels=gettextRcmdr(c("one", "two", "three","four", "five","six")), 
               title=gettextRcmdr("choose 2nd levels of factor to compare"))    
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(yBox), sticky="nw")
  tkgrid(modelNFrame, sticky="w")
  tkgrid(modelC1Frame, sticky="w")
  tkgrid(modelC2Frame, sticky="w")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(subsetFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  #tkgrid(buttonsFrame, stick="w")
  tkgrid.configure(helpButton, sticky="e")
  dialogSuffix(rows=4, columns=2)
}

#MAreg1

MAreg1cmd <- function(){
  initializeDialog(title=gettextRcmdr("Single Predictor Meta-Regression"))
  variablesFrame <- tkframe(top)
  .numeric <- Numeric()
  .variables <- Variables()
  xBox <- variableListBox(variablesFrame, .variables, title=gettextRcmdr("moderator variables (pick one)"))
  UpdateModelNumber()
  modelName <- tclVar(paste("MAmodel.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  onOK <- function(){
    x <- getSelection(xBox)
    closeDialog()
      if (0 == length(x)) {
        UpdateModelNumber(-1)
        errorCondition(recall=MAreg1cmd, message=gettextRcmdr("No moderator variable selected."))
        return()
    }
    modelValue <- trim.blanks(tclvalue(modelName))
      if (!is.valid.name(modelValue)){
        UpdateModelNumber(-1)
        errorCondition(recall=MAreg1cmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
        return()
    }
    modelFR <- as.character(tclvalue(modelFRVariable))    
    meta <- ActiveDataSet()     
    command <- (paste("MAreg1(", meta, ",",meta, "$", x, ",  method='" ,modelFR, "')", sep=""))
    logger(paste(modelValue, " <- ", command, sep=""))
    assign(modelValue, justDoIt(command), envir=.GlobalEnv)       
    doItAndPrint(modelValue)
    tkfocus(CommanderWindow())
  }    
  OKCancelHelp(helpSubject="MAreg1", model=TRUE)
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("fixed", "random"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("Model"))
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  tkgrid.configure(helpButton, sticky="e")
  dialogSuffix(rows=4, columns=1)
}

#MAreg2  

MAreg2cmd <- function(){
  initializeDialog(title=gettextRcmdr("Multiple Predictor Meta-Regression"))
  variablesFrame <- tkframe(top)
  .variable <- Variables()
  .numeric <- Numeric()
  xBox <- variableListBox(variablesFrame, .variable, selectmode="multiple",
                          title=gettextRcmdr("Moderator variables (pick one or more)"))
  yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Response variable (pick one)"))
  UpdateModelNumber()
  modelName <- tclVar(paste("MARegModel.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  subsetBox()
  onOK <- function(){
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    closeDialog()
      if (0 == length(y)) {
        UpdateModelNumber(-1)
        errorCondition(recall=MAreg2cmd, message=gettextRcmdr("You must select a response variable."))
        return()
    }
      if (0 == length(x)) {
        UpdateModelNumber(-1)
        errorCondition(recall=MAreg2cmd, message=gettextRcmdr("No explanatory variables selected."))
        return()
    }
      if (is.element(y, x)) {
        UpdateModelNumber(-1)
        errorCondition(recall=MAreg2cmd, message=gettextRcmdr("Response and explanatory variables must be different."))
        return()
    }
    subset <- tclvalue(subsetVariable)
      if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
        subset <- ""
        putRcmdr("modelWithSubset", FALSE)
    }
      else{
        subset <- paste(", subset=", subset, sep="")
        putRcmdr("modelWithSubset", TRUE)
    }
    modelValue <- trim.blanks(tclvalue(modelName))
      if (!is.valid.name(modelValue)){
        UpdateModelNumber(-1)
        errorCondition(recall=MAreg2cmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
        return()
    }
      if (is.element(modelValue, listLinearModels())) {
        if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
          UpdateModelNumber(-1)
          linearRegressionModel()
          return()
      }
    }
    modelFR <- as.character(tclvalue(modelFRVariable)) 
    meta <-ActiveDataSet()     
    command <- paste("lm(", y, "~", paste(x, collapse="+"),
                     ", data=", ActiveDataSet(), subset, ", 
                     weights=",meta, "$",modelFR,")", sep="")
    logger(paste(modelValue, " <- ", command, sep=""))
    assign(modelValue, justDoIt(command), envir=.GlobalEnv)
    #doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    # activeModel(modelValue)
    # tkfocus(CommanderWindow())
    reg <- modelValue           
	command2 <- (paste("MAreg2(", reg, ")"))
    logger(paste(modelValue, " <- ", command2, sep=""))
    assign(modelValue, justDoIt(command2), envir=.GlobalEnv)       
    doItAndPrint(modelValue)
    #activeModel(modelValue)
    tkfocus(CommanderWindow())
        
  }
  OKCancelHelp(helpSubject="MAreg2", model=TRUE)
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("wi", "wi.tau"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("model"))   
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  tkgrid(subsetFrame, sticky="w")
  tkgrid(buttonsFrame, stick="w")
  tkgrid.configure(helpButton, sticky="e")
  dialogSuffix(rows=4, columns=1)
}

##==== Graphics ====##

#Forest Plot

ForestPlotcmd <- function(){
  initializeDialog(title=gettextRcmdr("Forest Plot"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("ForestPlotGraph.", getRcmdr("modelNumber"), sep=""))
  labelsFrame <- tkframe(top)
  titleVar <- tclVar(gettextRcmdr("<auto>"))
  #ylabVar <- tclVar(gettextRcmdr("<auto>"))
  titleFrame <- tkframe(labelsFrame)
  titleEntry <- ttkentry(titleFrame, width="25", textvariable=titleVar)
  titleScroll <- ttkscrollbar(titleFrame, orient="horizontal",
  command=function(...) tkxview(titleEntry, ...))
  tkconfigure(titleEntry, xscrollcommand=function(...) tkset(titleScroll, ...))
  tkgrid(labelRcmdr(titleFrame, text=gettextRcmdr("title"), fg="blue"), sticky="w")
  tkgrid(titleEntry, sticky="w")
  tkgrid(titleScroll, sticky="ew")
  tkgrid(titleFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    title <- trim.blanks(tclvalue(titleVar))
    title <- if(title == gettextRcmdr("<auto>")) "" else paste(', title="', title, '"', sep="")
    modelFR <- as.character(tclvalue(modelFRVariable)) 
    doItAndPrint(paste("ForestPlot(", .activeDataSet, ",  method='" ,modelFR, "'",title,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="ForestPlot")
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("fixed", "random"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("Model"))
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  dialogSuffix(rows=8, columns=2)
}


#Funnel Plot


FunnelPlotcmd <- function(){
  initializeDialog(title=gettextRcmdr("Funnel Plot"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("FunnelGraph.", getRcmdr("modelNumber"), sep=""))
  labelsFrame <- tkframe(top)
  titleVar <- tclVar(gettextRcmdr("<auto>"))
  titleFrame <- tkframe(labelsFrame)
  titleEntry <- ttkentry(titleFrame, width="25", textvariable=titleVar)
  titleScroll <- ttkscrollbar(titleFrame, orient="horizontal",
  command=function(...) tkxview(titleEntry, ...))
  tkconfigure(titleEntry, xscrollcommand=function(...) tkset(titleScroll, ...))
  tkgrid(labelRcmdr(titleFrame, text=gettextRcmdr("title"), fg="blue"), sticky="w")
  tkgrid(titleEntry, sticky="w")
  tkgrid(titleScroll, sticky="ew")
  tkgrid(titleFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    title <- trim.blanks(tclvalue(titleVar))
    title <- if(title == gettextRcmdr("<auto>")) "" else paste(', title="', title, '"', sep="")
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="FunnelPlot")
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("fixed", "random"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("Model"))
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  dialogSuffix(rows=8, columns=2)
}


#MultiModGraph

MultiModGraphcmd <- function(){
  initializeDialog(title=gettextRcmdr("Multi-Predictor Moderator Graph"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("MultiModGraph.", getRcmdr("modelNumber"), sep=""))
  .factor <- Factors()
  variablesFrame <- tkframe(top)
  xBox <- variableListBox(variablesFrame, .factor, title=gettextRcmdr("categorical moderator (pick one)"))
   .numeric <- Numeric()
  yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("continuous moderator (pick one)"))
  labelsFrame <- tkframe(top)
  titleVar <- tclVar(gettextRcmdr("<auto>"))
  conmod.nameVar <- tclVar(gettextRcmdr("<auto>"))
  titleFrame <- tkframe(labelsFrame)
  titleEntry <- ttkentry(titleFrame, width="25", textvariable=titleVar)
  titleScroll <- ttkscrollbar(titleFrame, orient="horizontal",
  command=function(...) tkxview(titleEntry, ...))
  tkconfigure(titleEntry, xscrollcommand=function(...) tkset(titleScroll, ...))
  tkgrid(labelRcmdr(titleFrame, text=gettextRcmdr("title"), fg="blue"), sticky="w")
  tkgrid(titleEntry, sticky="w")
  tkgrid(titleScroll, sticky="ew")
  conmod.nameFrame <- tkframe(labelsFrame)
  conmod.nameEntry <- ttkentry(conmod.nameFrame, width="25", textvariable=conmod.nameVar)
  conmod.nameScroll <- ttkscrollbar(conmod.nameFrame, orient="horizontal",
                             command=function(...) tkxview(conmod.nameEntry, ...))
  tkconfigure(conmod.nameEntry, xscrollcommand=function(...) tkset(conmod.nameScroll, ...))
  tkgrid(labelRcmdr(conmod.nameFrame, text=gettextRcmdr("continuous moderator label"), fg="blue"), sticky="w")
  tkgrid(conmod.nameEntry, sticky="w")
  tkgrid(conmod.nameScroll, sticky="ew")
  tkgrid(titleFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  tkgrid(conmod.nameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
       x <- getSelection(xBox)
       conx <- getSelection(yBox)
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    title <- trim.blanks(tclvalue(titleVar))
    title <- if(title == gettextRcmdr("<auto>")) "" else paste(', title="', title, '"', sep="")
    conmod.name <- trim.blanks(tclvalue(conmod.nameVar))
    conmod.name <- if(conmod.name == gettextRcmdr("<auto>")) "" else paste(', conmod.name="', conmod.name, '"', sep="")
    modelFR <- as.character(tclvalue(modelFRVariable)) 
    doItAndPrint(paste("MultiModGraph(", .activeDataSet, ", ",.activeDataSet, "$", conx, ",  ",.activeDataSet, 
                                    "$", x, ", method='" ,modelFR, "'",title, conmod.name,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="MultiModGraph")
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("fixed", "random"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("Model"))
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(getFrame(xBox), sticky="nw")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  dialogSuffix(rows=8, columns=2)
}




#MAregGraph

MAregGraphcmd <- function(){
  initializeDialog(title=gettextRcmdr("Meta-Regression Graph"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("MAregGraph.", getRcmdr("modelNumber"), sep=""))
  .variables <- Variables()
  variablesFrame <- tkframe(top)
  xBox <- variableListBox(variablesFrame, .variables, title=gettextRcmdr("moderator variable (pick one)"))
  labelsFrame <- tkframe(top)
  titleVar <- tclVar(gettextRcmdr("<auto>"))
  titleFrame <- tkframe(labelsFrame)
  titleEntry <- ttkentry(titleFrame, width="25", textvariable=titleVar)
  titleScroll <- ttkscrollbar(titleFrame, orient="horizontal",
  							command=function(...) tkxview(titleEntry, ...))
  tkconfigure(titleEntry, xscrollcommand=function(...) tkset(titleScroll, ...))
  tkgrid(labelRcmdr(titleFrame, text=gettextRcmdr("title"), fg="blue"), sticky="w")
  tkgrid(titleEntry, sticky="w")
  tkgrid(titleScroll, sticky="ew")
  tkgrid(titleFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr("<auto>"))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
  modnameScroll <- ttkscrollbar(modnameFrame, orient="horizontal",
                             command=function(...) tkxview(modnameEntry, ...))
  tkconfigure(modnameEntry, xscrollcommand=function(...) tkset(modnameScroll, ...))
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("moderator label on plot"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameScroll, sticky="ew")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  ylimVar <- tclVar(gettextRcmdr("c(0,1)"))
  ylimFrame <- tkframe(labelsFrame)
  ylimEntry <- ttkentry(ylimFrame, width="25", textvariable=ylimVar)
  ylimScroll <- ttkscrollbar(ylimFrame, orient="horizontal",
                             command=function(...) tkxview(ylimEntry, ...))
  tkconfigure(ylimEntry, xscrollcommand=function(...) tkset(ylimScroll, ...))
  tkgrid(labelRcmdr(ylimFrame, text=gettextRcmdr("y-axis limits"), fg="blue"), sticky="w")
  tkgrid(ylimEntry, sticky="w")
  tkgrid(ylimScroll, sticky="ew")
  tkgrid(ylimFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
    x <- getSelection(xBox)
    closeDialog()
    .activeDataSet <- ActiveDataSet()
    title <- trim.blanks(tclvalue(titleVar))
    title <- if(title == gettextRcmdr("<auto>")) "" else paste(', title="', title, '"', sep="")
    modname <- trim.blanks(tclvalue(modnameVar))
    modname <- if(modname == gettextRcmdr("<auto>")) "" else paste(', modname="', modname, '"', sep="")
    ylim <- trim.blanks(tclvalue(ylimVar))
    ylim <- if(modname == gettextRcmdr("<auto>")) "" else paste(', ylim=', ylim, '', sep="")                               ### will use this format for the comp r section
    modelFR <- as.character(tclvalue(modelFRVariable)) 
    doItAndPrint(paste("MAregGraph(", .activeDataSet, ",  ",.activeDataSet, "$", x, ", method='" ,modelFR, "'",title, modname, ylim,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="MAregGraph")
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("fixed", "random"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("Model"))
    
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(getFrame(xBox), sticky="nw")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  dialogSuffix(rows=8, columns=2)
}



#CatModGraph

CatModGraphcmd <- function(){
  initializeDialog(title=gettextRcmdr("Categorical Moderator Graph"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("FunnelGraph.", getRcmdr("modelNumber"), sep=""))
  .factor <- Factors()
  variablesFrame <- tkframe(top)
  xBox <- variableListBox(variablesFrame, .factor, title=gettextRcmdr("moderator variable (pick one)"))
  labelsFrame <- tkframe(top)
  titleVar <- tclVar(gettextRcmdr("<auto>"))
  modnameVar <- tclVar(gettextRcmdr("<auto>"))
  titleFrame <- tkframe(labelsFrame)
  titleEntry <- ttkentry(titleFrame, width="25", textvariable=titleVar)
  titleScroll <- ttkscrollbar(titleFrame, orient="horizontal",
  command=function(...) tkxview(titleEntry, ...))
  tkconfigure(titleEntry, xscrollcommand=function(...) tkset(titleScroll, ...))
  tkgrid(labelRcmdr(titleFrame, text=gettextRcmdr("title"), fg="blue"), sticky="w")
  tkgrid(titleEntry, sticky="w")
  tkgrid(titleScroll, sticky="ew")
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
  modnameScroll <- ttkscrollbar(modnameFrame, orient="horizontal",
                             command=function(...) tkxview(modnameEntry, ...))
  tkconfigure(modnameEntry, xscrollcommand=function(...) tkset(modnameScroll, ...))
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("moderator label on plot"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameScroll, sticky="ew")
  tkgrid(titleFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  onOK <- function(){
       x <- getSelection(xBox)
    closeDialog()
   .activeDataSet <- ActiveDataSet()
    title <- trim.blanks(tclvalue(titleVar))
    title <- if(title == gettextRcmdr("<auto>")) "" else paste(', title="', title, '"', sep="")
    modname <- trim.blanks(tclvalue(modnameVar))
    modname <- if(modname == gettextRcmdr("<auto>")) "" else paste(', modname="', modname, '"', sep="")
    modelFR <- as.character(tclvalue(modelFRVariable)) 
    doItAndPrint(paste("CatModGraph(", .activeDataSet, ",  ",.activeDataSet, "$", x, ", method='" ,modelFR, "'",title, modname,")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="CatModGraph")
  radioButtons(name="modelFR", buttons=c("Fixed", "Random"), 
               values=c("fixed", "random"),
               labels=gettextRcmdr(c("fixed", "random")), 
               title=gettextRcmdr("Model"))
    
  tkgrid(modelFRFrame, sticky="w")
  tkgrid(getFrame(xBox), sticky="nw")
  tkgrid(labelsFrame, sticky="w")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  tkgrid(labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  dialogSuffix(rows=8, columns=2)
}



##==== Other ====##

# CorAtten

CorAttencmd <- function(){
  initializeDialog(title=gettextRcmdr("Correction for Attenuation"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("CorAtt.", getRcmdr("modelNumber"), sep=""))
  .variables <-Variables()
  variablesFrame <- tkframe(top)
  xBox <- variableListBox(variablesFrame, .variables, title=gettextRcmdr("reliability of predictor variable"))
  yBox <- variableListBox(variablesFrame, .variables, title=gettextRcmdr("reliability of outcome variable"))
  labelsFrame <- tkframe(top)
  onOK <- function(){
       xx <- getSelection(xBox)
       yy <- getSelection(yBox)
    closeDialog()
   .activeDataSet <- ActiveDataSet()
   doItAndPrint(paste("CorAtten(", .activeDataSet, ", ",.activeDataSet, "$", xx, ",  ",.activeDataSet, 
                                    "$", yy, ")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="CorAtten")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  dialogSuffix(rows=8, columns=2)
}



# Kappa

Kappacmd <- function(){
  initializeDialog(title=gettextRcmdr("Reliability: Kappa"))
  variablesFrame <- tkframe(top)
  UpdateModelNumber()
  modelName <- tclVar(paste("Kappa.", getRcmdr("modelNumber"), sep=""))
  .variables <-Variables()
  variablesFrame <- tkframe(top)
  xBox <- variableListBox(variablesFrame, .variables, title=gettextRcmdr("first rater of categorical variable"))
  yBox <- variableListBox(variablesFrame, .variables, title=gettextRcmdr("second rater of categorical variable"))
  labelsFrame <- tkframe(top)
  onOK <- function(){
       one <- getSelection(xBox)
       two <- getSelection(yBox)
    closeDialog()
   .activeDataSet <- ActiveDataSet()
   doItAndPrint(paste("Kappa(",.activeDataSet, "$", one, ",  ",.activeDataSet, 
                                    "$", two, ")", sep="")) 
    activateMenus()
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="Kappa")
  tkgrid(labelRcmdr(top, text=" "))
  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
  tkgrid(variablesFrame, sticky="w")
  dialogSuffix(rows=8, columns=2)
}

