# RclusTool: clustering of items in datasets
#
# Copyright 2013 Guillaume Wacquet, Pierre-Alexandre Hebert, Emilie Poisson-Caillault
#                
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' function to create the 'importTab' for data importation (features, metadata, signals and images)
#' @title Build Import tab
#' @description Generate the files importation tab of the \code{\link{RclusToolGUI}}, in which the user can select original files (features, metadata, signals or images).
#' @param mainWindow window in which the 'importTab' is created.
#' @param console frame of the RclusTool interface in which messages should be displayed. 
#' @param graphicFrame frame of the RclusTool interface in which graphics should be displayed.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @return None
#' @importFrom graphics plot
#' @importFrom tools file_path_sans_ext
#' @import tcltk tcltk2
#' @keywords internal

buildImportTab <- function(mainWindow, console, graphicFrame, RclusTool.env) {
    win1.nb <- RclusTool.env$gui$win1$env$nb
    win2.nb <- RclusTool.env$gui$win2$env$nb
	
	import.env <- RclusTool.env$gui$tabs.env$import

    fontFrame <- tkfont.create(family = "Arial", weight = "bold", size = RclusTool.env$param$visu$size)
	tkgrid(tklabel(win1.nb$env$import, text="      "), row = 3, column = 1)
	
    ## Build the 'Required files' frame
    RequiredFrametext <- StringToTitle("REQUIRED INPUT DATA FILES", RclusTool.env$param$visu$sizecm, fontsize=RclusTool.env$param$visu$size)
    RequiredFrame <- tkwidget(win1.nb$env$import, "labelframe", text = RequiredFrametext , font = fontFrame, padx = 30, pady=20, pady = 8, relief = "flat")
    tkgrid(RequiredFrame, columnspan = 3, row = 2, sticky = "w")

    # Select separator and decimal separator for csv (for features file)
    
    sepList <- c(",", ";", "\\s", "\\t")
    sepValues <- c(",", ";", "", "\t")
    names(sepValues) <- sepList
    decList <- c(".", ",")
    misList <- c("", "NA", "9999")
    
    # Import Features parameters 

    opt1 <- tklabel(RequiredFrame, text="sep")
    opt2 <- tklabel(RequiredFrame, text="dec")
    opt3 <- tklabel(RequiredFrame, text="missing")
    sepSelectFeat <- tclVar(",")
    decSelectFeat <- tclVar(".")
    misSelectFeat <- tclVar("")
    combo.Sep.Feat <- ttkcombobox(RequiredFrame, values=sepList, textvariable=sepSelectFeat, state="readonly", width = 2) 
    combo.Dec.Feat <- ttkcombobox(RequiredFrame, values=decList, textvariable=decSelectFeat, state="readonly", width = 2) 
    combo.Mis.Feat <- ttkcombobox(RequiredFrame, values=misList, textvariable=misSelectFeat, state="readonly", width = 2) 
    
    # Select the features file (.csv)
    featuresName <- tktext(RequiredFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshFeaturesName <- function()
    {
 		tkconfigure(featuresName, state="normal") 
        tkdelete(featuresName, "1.0", "end")
        tkinsert(featuresName,"end", import.env$featuresFile)
        tkconfigure(featuresName, state="disabled") 
    }

    featuresButton <- tk2button(RequiredFrame, text = "FEATURES / RDS FILE", image = "csvRDSFile", compound = "left", width = 20, 
                                command = function() {
                                    import.env$featuresFile <- tclvalue(tkgetOpenFile(filetypes = "{ {csv Files} {.csv} } { {RClusTool RDS Files} {.RDS} }"))
                                    import.env$refreshFeaturesName()
                                    if (!nchar(import.env$featuresFile)){
                                        tkmessageBox(message = "No file selected!")
                                    } else {
                                    	import.env$Dir <- substr(import.env$featuresFile, 1, (nchar(import.env$featuresFile)-nchar(basename(import.env$featuresFile)))-1)
                                    	import.env$DirDefault <- import.env$Dir 
                                    	import.env$refreshDirectoryName()
                                        ## From there, we define the local working directory
                                        ##setwd(dirname(import.env$featuresFile))
                                        if (grepl('.RDS',import.env$featuresFile)){
                                            import.env$rdsFile <- import.env$featuresFile
                                            ## Sauvegarde le repertoire du fichier par defaut
                                            import.env$featuresFile <- ""
                                        }
                                    }
                                })

    tkconfigure(opt1,font = fontFrame)
    tkconfigure(opt2,font = fontFrame)
    tkconfigure(opt3,font = fontFrame)
    tkgrid(featuresButton, row = 1, column = 1, padx = 7, sticky = "w")
    tkgrid(featuresName, row = 1, column= 2)
    tkgrid(opt1, row = 1, column = 3, sticky = "e")
    tkgrid(combo.Sep.Feat, row = 1, column = 4, sticky = "w")
    tkgrid(opt2, row = 1, column = 5, sticky = "e")
    tkgrid(combo.Dec.Feat, row = 1, column = 6, sticky = "w")
    
    tkgrid(opt3, row = 1, column = 7, sticky = "e")
    tkgrid(combo.Mis.Feat, row = 1, column = 8, sticky = "w")
   
    tkgrid(tklabel(win1.nb$env$import, text="      "), row = 1, column = 1)
    ## Build the 'Optional files' frame
    OptionalFrametext <- StringToTitle("OPTIONAL INPUT DATA FILES", RclusTool.env$param$visu$sizecm, fontsize=RclusTool.env$param$visu$size)
    OptionalFrame <- tkwidget(win1.nb$env$import, "labelframe", text = OptionalFrametext, font = fontFrame, padx = 30, pady = 20, relief = "flat")
    tkgrid(OptionalFrame, columnspan = 3, row = 6, sticky = "w")
    
    # Import signals parameters 
        
    opt1 <- tklabel(OptionalFrame, text="sep")
    opt2 <- tklabel(OptionalFrame, text="dec")
    opt3 <- tklabel(OptionalFrame, text="missing")
    sepSelectSig <- tclVar(",")
    decSelectSig <- tclVar(".")
    misSelectSig <- tclVar("")
    combo.Sep.Sig <- ttkcombobox(OptionalFrame, values=sepList, textvariable=sepSelectSig, state="readonly", width = 2) 
    combo.Dec.Sig <- ttkcombobox(OptionalFrame, values=decList, textvariable=decSelectSig, state="readonly", width = 2) 
    combo.Mis.Sig <- ttkcombobox(OptionalFrame, values=misList, textvariable=misSelectSig, state="readonly", width = 2) 
    
    # Select the signal file (.csv)
        
    signalsName <- tktext(OptionalFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshSignalsName <- function()
    {
 		tkconfigure(signalsName, state="normal") 
        tkdelete(signalsName, "1.0", "end")
        tkinsert(signalsName,"end", import.env$signalFile)
        tkconfigure(signalsName, state="disabled") 
    }

    signalButton <- tk2button(OptionalFrame, text = "SIGNALS", image = "csvFile", compound = "left", width = 20, 
                              command = function() {
                                  import.env$signalFile <- tclvalue(tkgetOpenFile(filetypes = "{{csv Files} {.csv}}"))
                                  import.env$refreshSignalsName()
                                  if (!nchar(import.env$signalFile)) {
                                      tkmessageBox(message = "No file selected!")
                              	  }
                               })
    
    tkconfigure(opt1,font = fontFrame)
    tkconfigure(opt2,font = fontFrame)
    tkconfigure(opt3,font = fontFrame)
    tkgrid(signalButton, row = 1, column = 1, padx = 20, sticky = "w")
    tkgrid(signalsName, row = 1, column = 2)
    tkgrid(opt1, row = 1, column = 3, sticky = "e")
    tkgrid(combo.Sep.Sig, row = 1, column = 4, sticky = "w")
    tkgrid(opt2, row = 1, column = 5, sticky = "e")
    tkgrid(combo.Dec.Sig, row = 1, column = 6, sticky = "w")
    tkgrid(opt3, row = 1, column = 7, sticky = "e")
    tkgrid(combo.Mis.Sig, row = 1, column = 8, sticky = "w")
  
    # Select the images directory
    imagesName <- tktext(OptionalFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshImagesName <- function()
    {
 		tkconfigure(imagesName, state="normal") 
        tkdelete(imagesName, "1.0", "end")
        tkinsert(imagesName,"end", import.env$imageDir)
        tkconfigure(imagesName, state="disabled") 
    }
    
    imageButton <- tk2button(OptionalFrame, text = "IMAGES", image = "folder", compound = "left", width = 20, command = function() {
                                 imageDir <- tk_choose.dir(default = import.env$imageDirDefault, caption = "Select folder for images.")
                                 if (is.na(imageDir)) {
                                     tkmessageBox(message = "No folder selected!")
                                 } else {
                                     import.env$imageDir <- imageDir
                                     import.env$refreshImagesName()
                                     import.env$imageDirDefault <- import.env$imageDir
                                 }
                            })
    tkgrid(imageButton, row = 2, column = 1, padx = 20, sticky = "w")
    tkgrid(imagesName, row = 2, column = 2)

    # Select the metadata file (.txt)
    metadataFileName <- tktext(OptionalFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")
    
        import.env$refreshMetadataFileName <- function()
    {
 		tkconfigure(metadataFileName, state="normal") 
        tkdelete(metadataFileName, "1.0", "end")
        tkinsert(metadataFileName,"end", import.env$metadataFile)
        tkconfigure(metadataFileName, state="disabled") 
    }

    metadataButton <- tk2button(OptionalFrame, text = "METADATA", image = "txtFile", compound = "left", width = 20,
                                command = function() {
                                    import.env$metadataFile <- tclvalue(tkgetOpenFile(filetypes = "{{txt Files} {.txt}}"))
                                    import.env$refreshMetadataFileName()
                                    if (!nchar(import.env$metadataFile)) {
                                        tkmessageBox(message = "No file selected!")
                                    } 
                                })
    tkgrid(metadataButton, row = 3, column = 1, padx = 20, sticky = "w")
	tkgrid(metadataFileName, row = 3, column = 2)
	
    ## Build the dataset with features, metadata, signals and images
    import.env$OnCompute <- function() {
        if (!nchar(import.env$featuresFile) && !nchar(import.env$rdsFile)) {
            tkmessageBox(message = "No data to import.\nPlease select a features file!", 
                         type = "ok", icon = "info", title = "Error")
        } else {
            tkconfigure(win1.nb$env$import, cursor = "watch")
            # Building step
            system.time(importSample(file.features = import.env$featuresFile, file.meta = import.env$metadataFile,
                                     file.profiles = import.env$signalFile, dir.images = import.env$imageDir, dir.save = import.env$DirDefault, 
                                     file.RDS = import.env$rdsFile, sepFeat = sepValues[tclvalue(sepSelectFeat)], decFeat = tclvalue(decSelectFeat), 
                                     naFeat=tclvalue(misSelectFeat), sepSig = sepValues[tclvalue(sepSelectSig)], decSig= tclvalue(decSelectSig),
                                     naSig=tclvalue(misSelectSig), RclusTool.env=RclusTool.env) 
            -> RclusTool.env$data.sample)
            
            # Display informations in console
            if (import.env$featuresFile==""){
            	filename=import.env$rdsFile
            	import.env$signalFile <-  RclusTool.env$data.sample$files$profiles
                import.env$refreshSignalsName()
                import.env$metadataFile <-  RclusTool.env$data.sample$files$meta
                import.env$refreshMetadataFileName()
                import.env$imageDir <-  RclusTool.env$data.sample$files$images
    			import.env$refreshImagesName()
            } else {
            	filename=RclusTool.env$data.sample$files$features
            }
            if (!is.null(RclusTool.env$data.sample)) {
            	tkinsert(console, "0.0", paste("----- Features importation -----\n",
                                           	   "Filename:  ", basename(filename), "\n",
                                           	   "Number of observations:  ", RclusTool.env$data.sample$size, "\n",
                                           	   "Number of features:  ", length(RclusTool.env$data.sample$features$initial$x), "\n\n", sep = ""))
            	if (nchar(import.env$metadataFile)) {
            		tkinsert(console, "0.0", paste(paste(RclusTool.env$data.sample$metadata$x, collapse = "\n", sep = ""), "\n\n", sep = ""))
                	tkinsert(console, "0.0", paste("----- MetaData importation -----\n",
                                              	 "Filename:  ", basename(RclusTool.env$data.sample$files$meta), "\n", sep = ""))
            	}
            	if (nchar(import.env$signalFile))
               	    tkinsert(console, "0.0", paste("----- Signals importation -----\n",
                        	                       "Filename:  ", basename(RclusTool.env$data.sample$files$profiles), "\n",
                                   	               "Number of observations:  ", length(RclusTool.env$data.sample$profiles), "\n\n", sep = ""))
            	if (nchar(import.env$imageDir))
                	tkinsert(console, "0.0", paste("----- Images importation -----\n",
                                               	   "Folder:  ", basename(RclusTool.env$data.sample$files$images), "\n",
                                                   "Number of observations:  ", length(!is.na(RclusTool.env$data.sample$images)), "\n\n", sep = ""))
                                                   
                if (nchar(import.env$DirDefault))
                	tkinsert(console, "0.0", paste("----- Working directory -----\n",
                                               	   "Folder:  ", basename(RclusTool.env$data.sample$files$dir), "\n\n"))
                                                   
            	if (!nchar(import.env$rdsFile))
                	tkinsert(console, "0.0", paste("----- RDS file -----\n",
                                               		"Creation of a RDS object\n\n", sep = ""))
                                               		

            # Active the 'Preprocessing' tab
            initPreprocessTab(mainWindow = mainWindow, console = console, 
                          graphicFrame = graphicFrame, RclusTool.env = RclusTool.env, reset=T)
                          
  			tk2delete.notetab(win2.nb)

            abdPlotTabs(RclusTool.env$data.sample$clustering, win2.nb, RclusTool.env, hscale = RclusTool.env$param$visu$hscale)

            RclusTool.env$gui$win1$env$authorization$prepro <- TRUE
            RclusTool.env$gui$win1$env$authorization$classif <- FALSE
            } else {
                message("Error: Missing file, only one column or encoding problem")
            }
            tkconfigure(win1.nb$env$import, cursor = "left_ptr")
        }
    }
    tk.compute.but <- tk2button(win1.nb$env$import, text="IMPORT", image = "data", compound = "left", width = 15, command=import.env$OnCompute)

    # Reset import Tab
    onReset <- function() {
        initImportTab(mainWindow = mainWindow, console = console, 
                      graphicFrame = graphicFrame, RclusTool.env = RclusTool.env, reset=T)
        RclusTool.env$gui$win1$env$authorization$prepro <- FALSE
        RclusTool.env$gui$win1$env$authorization$classif <- FALSE
        # Active the 'Preprocessing' tab
        initPreprocessTab(mainWindow = mainWindow, console = console, 
                          graphicFrame = graphicFrame, RclusTool.env = RclusTool.env, reset=T)
    }
    butReset <- tk2button(win1.nb$env$import, text = "Reset", image = "reset", compound = "left", width = 15, command = onReset)

    tkgrid(tk.compute.but, row = 20, column = 0)
    tkgrid(butReset, row = 20, column = 2)
    tkgrid(tklabel(win1.nb$env$import, text="      "), row = 9, column = 1)
   
 	## Build the 'Working Directory' frame
 	WdFrametext <- StringToTitle("WORKING DIRECTORY", RclusTool.env$param$visu$sizecm, fontsize=RclusTool.env$param$visu$size)
    WdFrame <- tkwidget(win1.nb$env$import, "labelframe", text = WdFrametext, font = fontFrame, padx = 30, pady = 20, relief = "flat")
    tkgrid(WdFrame, columnspan = 3, row = 7, sticky = "w")
     
    # Select the directory
    Dir <- tktext(WdFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshDirectoryName <- function()
    {
 		tkconfigure(Dir, state="normal") 
        tkdelete(Dir, "1.0", "end")
        tkinsert(Dir,"end", import.env$Dir)
        tkconfigure(Dir, state="disabled") 
    }
    
    DirButton <- tk2button(WdFrame, text = "DIRECTORY", image = "folder", compound = "left", width = 20, command = function() {
                                 Dir <- tk_choose.dir(default = import.env$DirDefault, caption = "Select folder.")
                                 if (is.na(Dir)) {
                                     tkmessageBox(message = "No folder selected!")
                                 } else {
                                     import.env$Dir <- Dir
                                     import.env$refreshDirectoryName()
                                     import.env$DirDefault <- import.env$Dir
                                 }
                            })
                            
    tkgrid(DirButton, row = 2, column = 1, padx = 20, sticky = "w")
    tkgrid(Dir, row = 2, column = 2)
 
     
 ## Build the 'Advices Frame' frame
    AdviceFrametext <- StringToTitle("ADVICES", RclusTool.env$param$visu$sizecm, fontsize=RclusTool.env$param$visu$size)
    AdviceFrame <- tkwidget(win1.nb$env$import, "labelframe", text = AdviceFrametext, font = fontFrame, padx =  20, pady = 8, relief = "flat")
    tkgrid(AdviceFrame, columnspan = 3, row = 13, sticky = "w")
    
 # What kind of CSV file for features
    FeaturesAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "How to format features data",
                            padx = 3*RclusTool.env$param$visu$size, pady = 8, relief = "groove")
    FeaturesAdviceText <- tk2label(FeaturesAdvice, text = "Data must be in a .csv file\nObservations in rows\nFeatures in columns\nMissing value must be 'empty'", width = 30)                   
    tkconfigure(FeaturesAdvice,font = fontFrame)
    tkconfigure(FeaturesAdviceText,font = fontFrame)
    tkgrid(FeaturesAdvice, columnspan = 1, column = 1, row = 1)
    tkgrid(FeaturesAdviceText)
    
  # What kind of CSV file for signal
    SignalsAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "How to format signal data", 
                            padx = 3*RclusTool.env$param$visu$size, pady = 8, relief = "groove")
    SignalsAdviceText <- tk2label(SignalsAdvice, text = "Data must be in a .csv file\nSignals in columns\nA same ID for all signal values\nMissing value must be 'empty'", width = 30)
    tkconfigure(SignalsAdvice,font = fontFrame)
    tkconfigure(SignalsAdviceText,font = fontFrame)    
    tkgrid(SignalsAdvice, columnspan = 1, column = 2, row = 1)
    tkgrid(SignalsAdviceText)

    # What is an RDS
    RDSAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "What is an RDS file ?",
                            padx = 3*RclusTool.env$param$visu$size, pady = 8, relief = "groove")
    RDSAdviceText <- tk2label(RDSAdvice, text = "After a first use of yours files\n an Rclustool RDS file is saved.\nThis file contains all the data\n you used and it's faster to load\n", width = 30)
    tkconfigure(RDSAdvice,font = fontFrame)
    tkconfigure(RDSAdviceText,font = fontFrame)     
    tkgrid(RDSAdvice, columnspan = 1, column = 3, row = 1)
    tkgrid(RDSAdviceText)
    
    # What kind of file for images
    ImagesAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "Important : how to format images data", 
                            padx = 3*RclusTool.env$param$visu$size, pady = 8, relief = "groove")
    ImagesAdviceText <- tk2label(ImagesAdvice, text = "JPEG or PNG images\nObservation's ID for filename\n\n", width = 30)
    tkconfigure(ImagesAdvice,font = fontFrame)
    tkconfigure(ImagesAdviceText,font = fontFrame)
    tkgrid(ImagesAdvice, columnspan = 1, column = 1, row = 2)
    tkgrid(ImagesAdviceText)

    # What kind of TXT file for metadata
    MetaAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "How to format metadata", 
                            padx = 3*RclusTool.env$param$visu$size, pady = 8, relief = "groove")
    MetaAdviceText <- tk2label(MetaAdvice, text = "Data must be in a .txt file\n'Metadata name: value'\n\n", width = 30)
    tkconfigure(MetaAdvice,font = fontFrame)
    tkconfigure(MetaAdviceText,font = fontFrame)    
    tkgrid(MetaAdvice, columnspan = 1, column = 2, row = 2)
    tkgrid(MetaAdviceText)
    
    
}
    
#' function to initialize (and to create) the 'importTab'
#' @title import tab 
#' @description This function generates the import tab of the \code{\link{RclusToolGUI}}, in which the user can import files.
#' @param mainWindow : window in which the 'importTab' is created.
#' @param console : frame of the RclusTool interface in which messages should be displayed. 
#' @param graphicFrame : frame of the RclusTool interface in which graphics should be displayed.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @param reset : if TRUE the whole tab is reset, with default options
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
initImportTab <- function(mainWindow, console, graphicFrame, RclusTool.env, reset=F)
{
    if (is.null(RclusTool.env$gui$tabs.env$import) || !length(RclusTool.env$gui$tabs.env$import))
    {
        RclusTool.env$gui$tabs.env$import <- new.env()
        buildImportTab(mainWindow, console, graphicFrame, RclusTool.env)
        reset <- T
    }

    import.env <- RclusTool.env$gui$tabs.env$import

    if (reset)
    {
        import.env$featuresFile <- ""
        import.env$rdsFile <- ""
        import.env$metadataFile <- ""
        import.env$signalFile <- ""
        import.env$imageDir <- ""
        import.env$imageDirDefault <- getwd()
        import.env$DirDefault <- getwd()
        import.env$Dir <- ""
        if (!is.null(RclusTool.env$data.sample$files$images) && dir.exists(RclusTool.env$data.sample$files$images))
            import.env$imageDirDefault <- RclusTool.env$data.sample$files$images
    }

    ########## test only ##########
    if (RclusTool.env$gui$debug.mode == T) {
        rep <- system.file("extdata", package="RclusTool")
        import.env$featuresFile <- file.path(rep, "sample_example_features.csv")
        import.env$metadataFile <- file.path(rep, "sample_example_info.txt")
        import.env$signalFile <- file.path(rep, "sample_example_pulses.csv")
        import.env$imageDir <- file.path(rep, "img_example")
        import.env$OnCompute()
    }
    ######## end test only ########

    # refresh file names
    import.env$refreshFeaturesName()
    import.env$refreshSignalsName()
    import.env$refreshImagesName()
    import.env$refreshMetadataFileName()
    import.env$refreshDirectoryName()
}


