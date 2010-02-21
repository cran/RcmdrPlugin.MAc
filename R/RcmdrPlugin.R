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

##==== Calculate ES ====##


# r_from_chi

r_from_chicmd <- function(){
  initializeDialog(title=gettextRcmdr("r from chi-squared"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
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
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported d statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
  modnameScroll <- ttkscrollbar(modnameFrame, orient="horizontal",
                             command=function(...) tkxview(modnameEntry, ...))
  tkconfigure(modnameEntry, xscrollcommand=function(...) tkset(modnameScroll, ...))
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("variance of d"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameScroll, sticky="ew")
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
  initializeDialog(title=gettextRcmdr("r from mean difference II"))
  variablesFrame <- tkframe(top)
  labelsFrame <- tkframe(top)
  statVar <- tclVar(gettextRcmdr(" "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported d statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
  modnameScroll <- ttkscrollbar(modnameFrame, orient="horizontal",
                             command=function(...) tkxview(modnameEntry, ...))
  tkconfigure(modnameEntry, xscrollcommand=function(...) tkset(modnameScroll, ...))
  tkgrid(labelRcmdr(modnameFrame, text=gettextRcmdr("n of 1st group"), fg="blue"), sticky="w")
  tkgrid(modnameEntry, sticky="w")
  tkgrid(modnameScroll, sticky="ew")
  tkgrid(modnameFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  ylimVar <- tclVar(gettextRcmdr(" "))
  ylimFrame <- tkframe(labelsFrame)
  ylimEntry <- ttkentry(ylimFrame, width="25", textvariable=ylimVar)
  ylimScroll <- ttkscrollbar(ylimFrame, orient="horizontal",
                             command=function(...) tkxview(ylimEntry, ...))
  tkconfigure(ylimEntry, xscrollcommand=function(...) tkset(ylimScroll, ...))
  tkgrid(labelRcmdr(ylimFrame, text=gettextRcmdr("n of 2nd group"), fg="blue"), sticky="w")
  tkgrid(ylimEntry, sticky="w")
  tkgrid(ylimScroll, sticky="ew")
  tkgrid(ylimFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  varVar <- tclVar(gettextRcmdr(" "))
  varFrame <- tkframe(labelsFrame)
  varEntry <- ttkentry(varFrame, width="25", textvariable=varVar)
  varScroll <- ttkscrollbar(varFrame, orient="horizontal",
                             command=function(...) tkxview(varEntry, ...))
  tkconfigure(varEntry, xscrollcommand=function(...) tkset(varScroll, ...))
  tkgrid(labelRcmdr(varFrame, text=gettextRcmdr("variance of d"), fg="blue"), sticky="w")
  tkgrid(varEntry, sticky="w")
  tkgrid(varScroll, sticky="ew")
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
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("reported statistic"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
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
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
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
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
  statScroll <- ttkscrollbar(statFrame, orient="horizontal",
  							command=function(...) tkxview(statEntry, ...))
  tkconfigure(statEntry, xscrollcommand=function(...) tkset(statScroll, ...))
  tkgrid(labelRcmdr(statFrame, text=gettextRcmdr("correlation"), fg="blue"), sticky="w")
  tkgrid(statEntry, sticky="w")
  tkgrid(statScroll, sticky="ew")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text="     "), sticky="w")
  modnameVar <- tclVar(gettextRcmdr(" "))
  modnameFrame <- tkframe(labelsFrame)
  modnameEntry <- ttkentry(modnameFrame, width="25", textvariable=modnameVar)
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
  statEntry <- ttkentry(statFrame, width="25", textvariable=statVar)
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
      errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
       #command <- paste("ComplData(", meta, ", ", paste(x, collapse=","),
       #                 ", predictors= '",modelN,"')", sep="")
       command <- paste("ComplData(", meta, ",", meta, "$", x, ")", sep="")
       logger(paste(modelValue, " <- ", command, sep=""))
       assign(modelValue, justDoIt(command), envir=.GlobalEnv)
       doItAndPrint(modelValue)
       tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="ComplData", model=TRUE)
  #radioButtons(name="modelN", buttons=c("One", "Two","Three","Four","Five"), values=c("1", "2","3","4","5"),  # 
  #             labels=gettextRcmdr(c("One", "Two","Three","Four","Five")), title=gettextRcmdr("Variables to reduce by:"))
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
      errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
#          errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
#        errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
#        errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
#        errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
        errorCondition(recall=ComplDatacmd, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
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
  radioButtons(name="modelN", buttons=c("post.hoc", "planned"), values=c("post.hoc", "planned"),   
               labels=gettextRcmdr(c("post.hoc", "planned")), title=gettextRcmdr("method"))
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
