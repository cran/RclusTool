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

#' function to create the 'batchTab' for data clustering in batch process
#' @title Batch process tab
#' @description Generate the batch process tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the classification method to apply on several datasets.
#' @param mainWindow window in which the 'batchTab' is created.
#' @param console frame of the RclusTool interface in which messages should be displayed. 
#' @param graphicFrame frame of the RclusTool interface in which graphics should be dispayed.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @return None
#' @importFrom grDevices dev.new dev.off jpeg
#' @importFrom graphics layout par matplot legend axis
#' @importFrom utils alarm
#' @import tcltk tcltk2
#' @keywords internal
#' 
buildBatchTab <- function(mainWindow, console, graphicFrame, RclusTool.env) {
	#Reset working directory on exit 
    batch.env <- RclusTool.env$gui$tabs.env$batch

    fontFrame <- tkfont.create(family = "Arial", weight = "bold", size = 11)

    batch.env$tcl.supMethod.select <- tclVar("RF")
    batch.env$tcl.unsupMethod.select <- tclVar("K-means")
    batch.env$tcl.sampling.check <- tclVar("0")
    batch.env$tcl.scaling.check.unsup <- tclVar("0")

    label <- NULL
    x <- NULL
    cluster.summary <- NULL
    supMethod.names <- c("K-NN", "MLP", "SVM", "RF")
    supMethod.select <- supMethod.names[1] #no associated tcl variable
    unsupMethod.names <- c("K-means", "PAM", "HC", "Spectral", "EM")
    unsupMethod.select <- unsupMethod.names[1] #no associated tcl variable
    pca.nb.dims <- 0

    # Select separator, missing and decimal separator for csv (for features file)
    sepList <- c(",", ";", "\\s", "\\t")
    sepValues <- c(",", ";", "", "\t")
    names(sepValues) <- sepList
    decList <- c(".", ",")
    misList <- c("", "NA", "9999")
    sepSelect <- tclVar(",")
    decSelect <- tclVar(".")
    misSelect <- tclVar("")

    # Frames
    sup.method.title <- c("RF"="Random Forest (RF)",
                          "K-NN"="K-Nearest-Neighbor (K-NN)",
                          "MLP"="MultiLayer Perceptron (MLP)",
                          "SVM"="Support Vector Machine (SVM)")

    unsup.method.title <- c("K-means"="K-Means (KM)",
                            "PAM"="Partitioning Around Medoids (PAM)",
                            "HC"="Hierarchical Clustering (HC)",
                            "Spectral"="Spectral Clustering (SC)",
                            "EM"="Expectation-Maximization (EM)")

    win1.nb <- RclusTool.env$gui$win1$env$nb

    ## Supervised classification
    SupFrame <- tkwidget(win1.nb$env$batch, "labelframe", text = "SUPERVISED", font = fontFrame, pady=20, padx=120, relief = "groove")
    ## Unsupervised classification
    UnsupFrame <- tkwidget(win1.nb$env$batch, "labelframe", text = "UNSUPERVISED", font = fontFrame, pady=20, padx=198, relief = "groove")

    SupFrameExpert <- tkwidget(SupFrame, "labelframe", font = fontFrame)
    SupFrameStandard <- tkwidget(SupFrame, "labelframe", font = fontFrame)

    UnsupFrameExpert1 <- tkwidget(UnsupFrame, "labelframe", font = fontFrame)
    UnsupFrameExpert2 <- tkwidget(UnsupFrame, "labelframe", font = fontFrame)
    UnsupFrameStandard <- tkwidget(UnsupFrame, "labelframe", font = fontFrame)

    # method selection buttons supervised
    rb_sup.methods <- sapply(names(sup.method.title), function(name) {
                                 tkr <- tkradiobutton(SupFrameExpert, variable=batch.env$tcl.supMethod.select, value=name, text=sup.method.title[name])
                                 tkbind(tkr, "<ButtonRelease-1>")
                                 tkr
                            }, simplify=F)

    # method selection buttons unsupervised
    rb_unsup.methods <- sapply(names(unsup.method.title), function(name) {
                                   tkr <- tkradiobutton(UnsupFrameExpert1, variable=batch.env$tcl.unsupMethod.select, value=name, text=unsup.method.title[name])
                                   tkbind(tkr, "<ButtonRelease-1>")
                                   tkr
                            }, simplify=F)

    # Import config file
    onImport <- function() {
        batch.env$ProcessFile <- tclvalue(tkgetOpenFile(filetypes = "{{csv Files} {.csv}}"))
        batch.env$refreshPreprocessingName()
        if (!nchar(batch.env$ProcessFile)) {
            tkmessageBox(message = "No file selected!")
        } else {
            # Display informations in console
            tkinsert(console, "0.0", paste("----- Preprocessing -----\n",
                                           "Filename:  ", basename(batch.env$ProcessFile), "\n\n", sep = ""))
        }
    }

    ## Load training set
    OnLoadDir <- function() {
        protos.directory <- tk_choose.dir(default = getwd(), caption = "Select training set base dir.")
        if (is.na(protos.directory)||!nchar(protos.directory))
            return()
        batch.env$protos.directory <- protos.directory
        batch.env$refreshTrainingSetName()
    }

    ## Compute supervised classification

    OnSupCompute <- function() {
        if (!nchar(batch.env$dirFiles)) {
            tkmessageBox(message = "No data to import.\nPlease select a directory!", 
                         type = "ok", icon = "info", title = "Error")
        } else {
            if (!nchar(batch.env$protos.directory)) {
                utils::alarm()
                tkmessageBox(message="Please first select a training set.")
                OnLoadDir()
                return()
            }
            operations <- NULL
       		if (nchar(batch.env$ProcessFile)) {
        		# Call 'loadPreprocessFile' function
        		operations <- loadPreprocessFile(batch.env$ProcessFile)
        	}
        	batch.env$prototypes <- readTrainSet(traindir = batch.env$protos.directory, operations=operations)
        	batch.env$id.clean.proto <- 1:NROW(batch.env$prototypes)
			batch.env$model <- NULL
			
            # Default: Random Forest algorithm (for standard user)
            supMethod <- tclvalue(batch.env$tcl.supMethod.select)
            tkinsert(console, "0.0", paste("----- Supervised Classification -----\n", 
                                           "Batch process\n",
                                           "Datasets folder: ", basename(batch.env$WDir), "\n",
                                           "Training set: ", basename(batch.env$protos.directory), "\n",
                                           supMethod, " computing\n\n", sep = ""))

            allAbd <- data.frame(matrix(ncol = length(unique(batch.env$prototypes$Class[batch.env$id.clean.proto])), nrow = 0))
            colnames(allAbd) <- unique(batch.env$prototypes$Class[batch.env$id.clean.proto])

            cpt <- 1
            for (f in batch.env$files) {
                message("Processing:", f, "(", cpt, "of", length(batch.env$files), ")\n")
                RclusTool.env$gui$protos.dir <- batch.env$protos.directory
                RclusTool.env$data.sample <- NULL
                rdsfile <- ""
                if (any(grep(gsub(".csv", "", basename(f)), batch.env$rdsFiles)))
                    rdsfile <- file.path(batch.env$dirFiles, batch.env$rdsFiles[grep(gsub(".csv", "", basename(f)), batch.env$rdsFiles)])
                system.time(importSample(dir.save = batch.env$WDirDefault, file.features = file.path(batch.env$dirFiles, f),
                                         file.RDS = rdsfile, sepFeat = sepValues[tclvalue(sepSelect)], decFeat = tclvalue(decSelect), naFeat=tclvalue(misSelect)) 
                -> RclusTool.env$data.sample)
                if (nchar(batch.env$ProcessFile)) {
                    # Call 'loadPreprocessFile' function
                    operations <- loadPreprocessFile(batch.env$ProcessFile)
                    # Call 'applyPreprocessing' function
                    RclusTool.env$data.sample <- applyPreprocessing(RclusTool.env$data.sample, operations, RclusTool.env)
                }
                if (is.null(RclusTool.env$data.sample))
                    stop("Corrupted Configuration Files, please retry with another one.")

                res <- computeSupervised(RclusTool.env$data.sample, 
                                         prototypes = batch.env$prototypes[batch.env$id.clean.proto, ], 
                                         method.name = supMethod, model=batch.env$model, RclusTool.env=RclusTool.env)
                if (cpt==1){                        
                	batch.env$model <- res$model
                }
                
                #Keep only levels who are in clustering 
				if (!dir.exists(file.path(batch.env$WDirDefault,"batch_results")))
                	dir.create(file.path(batch.env$WDirDefault,"batch_results"))
                RclusTool.env$data.sample$files$results$batch <- file.path(batch.env$WDirDefault,"batch_results")
                # Save clustering and summary (csv files)
                fileClust.csv <- paste(RclusTool.env$data.sample$name, " clustering ", supMethod, ".csv", sep = "")
                saveClustering(fileClust.csv, res$label, RclusTool.env$data.sample$files$results$batch)
                fileSum.csv <- paste(RclusTool.env$data.sample$name, " results ", supMethod, ".csv", sep = "")
                saveSummary(fileSum.csv, res$summary, RclusTool.env$data.sample$files$results$batch)
				
                pref.features <- RclusTool.env$data.sample$config$defaultFeat
                if ( is.null(pref.features) || (length(pref.features)<2))
                    pref.features <- colnames(RclusTool.env$data.sample$features$preprocessed$x)[1:2]
                parH <- pref.features[1]
                parV <- pref.features[2]

                # Save scatterplot with pref.features
                grDevices::jpeg(file.path(RclusTool.env$data.sample$files$results$batch, paste(RclusTool.env$data.sample$name, " plot ", supMethod, ".jpg", sep = "")),
                                quality=100,  width = 800, height = 800)
                plotSampleFeatures(RclusTool.env$data.sample$features$preprocessed$x, label = droplevels(res$label), 
                                   parH = parH, parV = parV, figure.title=paste("Scatter-plot clustered by", supMethod))
                grDevices::dev.off()

                # Display abundances evolution per cluster
                abd <- as.data.frame.matrix(t(table(res$label)))
                row.names(abd) <- RclusTool.env$data.sample$name
                allAbd <- rbind(allAbd, abd)
                allAbd <- allAbd[, levels(res$label)]
                cpt <- cpt + 1
            }
        }
    grDevices::jpeg(file.path(RclusTool.env$data.sample$files$results$batch, paste(" plot abundances evolution ", supMethod, ".jpg", sep = "")),
                    quality=100,  width = 800, height = 800)
    graphics::matplot(allAbd, type = "l", col = RclusTool.env$param$visu$point.style$col[1:(dim(allAbd)[2])], 
                      pch = 1:(1+(dim(allAbd)[2]-1)), xlab = "Samples", ylab = "Abundances")
    graphics::legend("topleft", legend = names(allAbd), col = RclusTool.env$param$visu$point.style$col[1:(dim(allAbd)[2])], 
                      pch = 1:(1+(dim(allAbd)[2]-1)), bg = "transparent", cex = RclusTool.env$param$visu$cex)
    grDevices::dev.off()
    Abundances.csv <- paste("Abundances results ", supMethod, ".csv", sep = "")
   	saveSummary(Abundances.csv, allAbd, RclusTool.env$data.sample$files$results$batch)
    }

    # Compute unsupervised classification

    OnUnsupCompute <- function() {
    	if (RclusTool.env$gui$user.type=="expert"){
            batch.env$featSpace <- getSelectedSpace()
            if (is.null(batch.env$featSpace)) {
            	stop("No space selected")
            }
        }
        if (!nchar(batch.env$dirFiles)) {
            tkmessageBox(message = "No data to import.\nPlease select a directory!", 
                         type = "ok", icon = "info", title = "Error")
        } else {
            unsupMethod <- tclvalue(batch.env$tcl.unsupMethod.select)
            tkinsert(console, "0.0", paste("----- Unsupervised Clustering -----\n", 
                                           "Batch process\n",
                                           "Datasets folder: ", basename(batch.env$WDir), "\n",
                                           unsupMethod, " computing\n\n", sep = ""))

            cpt <- 1
            for (f in batch.env$files) {
                message("Processing:", f, "(", cpt, "of", length(batch.env$files), ")\n")
                RclusTool.env$data.sample <- NULL
                rdsfile <- ""
                if (any(grep(gsub(".csv", "", basename(f)), batch.env$rdsFiles)))
                    rdsfile <- file.path(batch.env$dirFiles, batch.env$rdsFiles[grep(gsub(".csv", "", basename(f)), batch.env$rdsFiles)])
                system.time(importSample(dir.save = batch.env$WDirDefault, file.features = file.path(batch.env$dirFiles, f),
                                         file.RDS = rdsfile, sepFeat = sepValues[tclvalue(sepSelect)], decFeat = tclvalue(decSelect), naFeat=tclvalue(misSelect)) 
                -> RclusTool.env$data.sample)

                operations <- NULL
                if (nchar(batch.env$ProcessFile)) {
                    # Call 'loadPreprocessFile' function
                    operations <- loadPreprocessFile(batch.env$ProcessFile)
                }

                # Call 'applyPreprocessing' function
                RclusTool.env$data.sample <- applyPreprocessing(RclusTool.env$data.sample, operations, RclusTool.env)
                if (is.null(RclusTool.env$data.sample))
                    stop("Corrupted Configuration Files, please retry with another one.")
                if (batch.env$featSpace == "Initial Features"){
                    batch.env$pca=F
                    batch.env$spec=F
                } else if (batch.env$featSpace == "Principal Components Analysis") {
                    batch.env$pca=T
                    batch.env$spec=F 
                } else if (batch.env$featSpace == "Spectral Embedding") { 
                    batch.env$pca=F
                    batch.env$spec=T
                }
                RclusTool.env$data.sample <- computeUnSupervised(RclusTool.env$data.sample, K=0, method.name=unsupMethod, pca=batch.env$pca, 
                                                                 use.sampling=tclvalue(batch.env$tcl.sampling.check)=="1", 
                                                                 spec=batch.env$spec,
                                                                 scaling=tclvalue(batch.env$tcl.scaling.check.unsup)=="1")
                #Just to have Space name to export
                operationsRes <- makeFeatureSpaceOperations(pca=batch.env$pca,
                											spectral=batch.env$spec,
                                    						sampling=tclvalue(batch.env$tcl.sampling.check)=="1", 
                                    						scaling=tclvalue(batch.env$tcl.scaling.check.unsup)=="1")
                                    						
                space <- operationsRes$space
                method.space.name.unsup <- paste(unsupMethod,space,sep="_")         
                label <- RclusTool.env$data.sample$clustering[[method.space.name.unsup]]$label
                cluster.summary <- RclusTool.env$data.sample$clustering[[method.space.name.unsup]]$summary
				if (!dir.exists(file.path(batch.env$WDirDefault,"batch_results")))
                	dir.create(file.path(batch.env$WDirDefault,"batch_results"))
                RclusTool.env$data.sample$files$results$batch <- file.path(batch.env$WDirDefault,"batch_results")
                # Save clustering and summary (csv files)
                fileClust.csv <- paste(RclusTool.env$data.sample$name, " clustering ", method.space.name.unsup, ".csv", sep = "")
                saveClustering(fileClust.csv, label, RclusTool.env$data.sample$files$results$batch)
                fileSum.csv <- paste(RclusTool.env$data.sample$name, " results ", method.space.name.unsup, ".csv", sep = "")
                saveSummary(fileSum.csv, cluster.summary, RclusTool.env$data.sample$files$results$batch)
				pref.features <- NULL
				if (!grepl('pca',space) && !grepl('spectral',space)){
                	pref.features <- RclusTool.env$data.sample$config$defaultFeat
               	}
                nom_espace=paste('RclusTool.env$data.sample$features$',space,'$x',sep="")
                if (is.null(pref.features) || (length(pref.features)<2)){
                    pref.features <- colnames(eval(parse(text = nom_espace)))[1:2]
                }
                parH <- pref.features[1]
                parV <- pref.features[2]

                # Save scatterplot with pref.features
                grDevices::jpeg(file.path(RclusTool.env$data.sample$files$results$batch, paste(RclusTool.env$data.sample$name, " plot ", method.space.name.unsup, ".jpg", sep = "")),
                                quality=100,  width = 800, height = 800)
                plotSampleFeatures(eval(parse(text = nom_espace)), label = label, 
                                   parH = parH, parV = parV, figure.title=paste("Scatter-plot clustered by", method.space.name.unsup))
                grDevices::dev.off()
                cpt <- cpt + 1
            }
        }
    }

    getSelectedSpace <- function()
    {
        space <- NULL
        selection <- tclvalue((tkcurselection(spaceList)))
        if (selection!="")
            space <- spaces[as.numeric(selection)+1] 
        space
    }

    tk.folder.but <- tk2button(SupFrame,text="Training set", image = "folder", compound = "left", width = 20, command=OnLoadDir)

    TrainingSetName <- tktext(SupFrame, bg="white", font="courier", width=75, height=2, font = fontFrame, state="disabled")

    batch.env$refreshTrainingSetName <- function()
    {
        tkconfigure(TrainingSetName, state="normal") 
        tkdelete(TrainingSetName, "1.0", "end")
        tkinsert(TrainingSetName,"end", batch.env$protos.directory)
        tkconfigure(TrainingSetName, state="disabled") 
    }

    ## Build the 'Required files' frame
    tkgrid(tklabel(win1.nb$env$batch, text="      "), row = 1, column = 1)
    RequiredDirFrame <- tkwidget(win1.nb$env$batch, "labelframe", text = "REQUIRED INPUT DATA FOLDER", pady= 10, font = fontFrame, relief = "groove")
    tkgrid(RequiredDirFrame, columnspan = 3, row = 2, sticky = "w")

    # Select separator and decimal separator for csv (for features file)
    opt1 <- tklabel(RequiredDirFrame, text="sep")
    opt2 <- tklabel(RequiredDirFrame, text="dec")
    opt3 <- tklabel(RequiredDirFrame, text="missing")
    combo.Sep <- ttkcombobox(RequiredDirFrame, values=sepList, textvariable=sepSelect, state="readonly", width = 2) 
    combo.Dec <- ttkcombobox(RequiredDirFrame, values=decList, textvariable=decSelect, state="readonly", width = 2) 
    combo.Mis <- ttkcombobox(RequiredDirFrame, values=misList, textvariable=misSelect, state="readonly", width = 2)

    tk.supCompute.but <- tkbutton(SupFrame, text="COMPUTE", width = 10, command=OnSupCompute)
    tk.unsupCompute.but <- tkbutton(UnsupFrame, text="COMPUTE", width = 10, command=OnUnsupCompute)

    # Positioning
    tkgrid(UnsupFrame, row = 4, sticky = "w")
    tkgrid(SupFrame, row = 5, sticky = "w")
    tkgrid(tk.folder.but, row = 2, column = 0, padx = 20)
    tkgrid(TrainingSetName, row = 2, column = 1)
    tkgrid(tk2label(SupFrame, text=""), row = 3, column = 1)
    
    ## Build the 'Classification' frame
    tkgrid(tklabel(win1.nb$env$batch, text="      "), row = 4, column = 1)
    ClassFrame <- tkwidget(win1.nb$env$batch, "labelframe", text = "BATCH CLASSIFICATION/CLUSTERING", font = fontFrame, relief = "groove")
    tkgrid(ClassFrame, columnspan = 3, row = 5, sticky = "w")

    DirFrameName <- tktext(RequiredDirFrame, bg="white", font="courier", width=75, height=2, font = fontFrame, state="disabled")

    batch.env$refreshDirFrameName <- function()
    {
        tkconfigure(DirFrameName, state="normal") 
        tkdelete(DirFrameName, "1.0", "end")
        tkinsert(DirFrameName,"end", batch.env$dirFiles)
        tkconfigure(DirFrameName, state="disabled") 
    }

    ## Select the directory containing files (.csv)
    dirButton <- tk2button(RequiredDirFrame, text = "DATASETS DIR", image = "folder", compound = "left", width = 28, command = function() {
                               dirFiles <- tk_choose.dir(default = batch.env$dirFilesDefault, caption = "Select folder for batch process.")
                               if (is.na(dirFiles)||!nchar(dirFiles))
                               {
                                   tkmessageBox(message = "No folder selected!")
                               } else
                               {
                                   batch.env$dirFiles <- dirFiles
                                   batch.env$dirFilesDefault <- batch.env$dirFiles
                                   batch.env$refreshDirFrameName()
                                   batch.env$WDir <- dirFiles 
                                   batch.env$WDirDefault <- batch.env$WDir 
                                   batch.env$refreshWDirectoryName()
                                   batch.env$rdsFiles <- list.files(batch.env$dirFiles, pattern = ".RDS", recursive = TRUE)
                                   # Delete results files in list
                                   if (any(grepl("_RclusTool", batch.env$rdsFiles)))
                                       batch.env$rdsFiles <- batch.env$rdsFiles[-grep("_RclusTool", batch.env$rdsFiles)]
                                   batch.env$files <- list.files(batch.env$dirFiles, pattern = ".csv", recursive = TRUE)
                                   # Delete results files in list
                                   if (any(grepl("_RclusTool", batch.env$files)))
                                       batch.env$files <- batch.env$files[-grep("_RclusTool", batch.env$files)]
                                   # Delete profiles files in list
                                   if (any(grepl("profiles_", batch.env$files)))
                                       batch.env$files <- batch.env$files[-grep("profiles_", batch.env$files)]
                               }
                                   })

    PreprocessingName <- tktext(RequiredDirFrame, bg="white", font="courier", width=75, height=2, font = fontFrame, state="disabled")

    batch.env$refreshPreprocessingName <- function()
    {
        tkconfigure(PreprocessingName, state="normal") 
        tkdelete(PreprocessingName, "1.0", "end")
        tkinsert(PreprocessingName,"end", batch.env$ProcessFile)
        tkconfigure(PreprocessingName, state="disabled") 
    }

    butImport <- tk2button(RequiredDirFrame, text = "Import preprocessing", image = "csvFile", compound = "left", width = 30, command = onImport)

 	## Build the 'Working Directory' frame
    WdFrame <- tkwidget(win1.nb$env$batch, "labelframe", text = "WORKING DIRECTORY", font = fontFrame, padx = 30, pady = 10, relief = "flat")
    tkgrid(WdFrame, columnspan = 3, row = 3, sticky = "w")
     
    # Select the working directory
    WDir <- tktext(WdFrame, bg="white", font="courier", width=75, height=2, font = fontFrame, state="disabled")

    batch.env$refreshWDirectoryName <- function()
    {
 		tkconfigure(WDir, state="normal") 
        tkdelete(WDir, "1.0", "end")
        tkinsert(WDir,"end", batch.env$WDir)
        tkconfigure(WDir, state="disabled") 
    }
    
    WDirButton <- tk2button(WdFrame, text = "DIRECTORY", image = "folder", compound = "left", width = 20, command = function() {
                                 WDir <- tk_choose.dir(default = batch.env$WDirDefault, caption = "Select folder.")
                                 if (is.na(WDir)) {
                                     tkmessageBox(message = "No folder selected!")
                                 } else {
                                     batch.env$WDir <- WDir
                                     batch.env$refreshWDirectoryName()
                                     batch.env$WDirDefault <- batch.env$WDir
                                 }
                            })
                                                  
    tkgrid(dirButton, row = 1, column = 0, padx = 20)
    tkgrid(DirFrameName, row = 1, column= 1)
    tkgrid(WDirButton, row = 2, column = 1, padx = 45, sticky = "w")
    tkgrid(WDir, row = 2, column = 2)
    tkgrid(opt1, row = 1, column = 3, sticky = "e")
    tkgrid(combo.Sep, row = 1, column = 4, sticky = "w")
    tkgrid(opt2, row = 1, column = 5, sticky = "e")
    tkgrid(combo.Dec, row = 1, column = 6, sticky = "w")
    tkgrid(opt3, row = 1, column = 7, sticky = "e")
    tkgrid(combo.Mis, row = 1, column = 8, sticky = "w")
    tkgrid(butImport, row = 2, column = 0, padx = 20)
    tkgrid(PreprocessingName, row = 2, column = 1)
    
    ## Unsupervised classification
    # expert method frame layout
    tk.sampling.check <- tkcheckbutton(UnsupFrameExpert1, text="", variable=batch.env$tcl.sampling.check)
    tk.scaling.check.unsup <- tkcheckbutton(UnsupFrameExpert1, text="", variable=batch.env$tcl.scaling.check.unsup)
	
    sapply(1:length(rb_unsup.methods), function(i) tkgrid(rb_unsup.methods[[i]], row=i+3, column=0, columnspan=2, sticky="w"))

    tkgrid(tk2label(UnsupFrameExpert1, text="Sampling particles set"), row=1, column=0, sticky="w")
    tkgrid(tk2label(UnsupFrameExpert1, text="Scaling particles set"), row=2, column=0, sticky="w")
    tkgrid(tk.sampling.check, row=1, column=1, sticky="e")
    tkgrid(tk.scaling.check.unsup, row=2, column=1, sticky="e")
    
    # Standard method layout
    tkgrid(tk2label(UnsupFrameStandard, text= "Standard parameters:\n     - PCA\n     - K estimation: method Elbow\n     - K-means"), column = 0)

    spaceFrame <- tkwidget(UnsupFrameExpert2, "labelframe", text = "FEATURE SPACE SELECTION", font = fontFrame, relief = "groove")

    spaceList <- tk2listbox(spaceFrame, selectmode = "single", activestyle = "dotbox",
                            height = 3, width = 27, autoscroll = "none", background = "white")

    spaces <- c("Initial Features", "Principal Components Analysis", "Spectral Embedding")
    
    for (space in spaces) {
        tkinsert(spaceList, "end", space)
    }
    tkselection.set(spaceList, 0)
    
    ## Supervised classification
    # expert method frame layout
    tkgrid(tk2label(SupFrameExpert, text="Scaling particles set"), row=1, column=0, sticky="w")
    sapply(1:length(rb_sup.methods), function(i) tkgrid(rb_sup.methods[[i]], row=i+1, column=0, columnspan=2, sticky="w"))
  
    # Standard method layout
    tkgrid(tk2label(SupFrameStandard, text= "Standard parameters:\n     - Random Forest\n     - Number of trees: 500"), column = 0)


    if (RclusTool.env$gui$user.type=="expert")
    {
        #Space Frame
        tkgrid(spaceFrame, row = 11, column = 0)                                          
        tkgrid(spaceList, row = 11, column = 0)           
        # secondary frames
        tkgrid(UnsupFrameExpert1, row = 1, column=1, padx=30, sticky = "w")
        tkgrid(UnsupFrameExpert2, row = 1, column=2, sticky = "w")
        tkgrid(SupFrameExpert, row = 3, column=1, sticky = "w")
    } else {
        tkgrid(UnsupFrameStandard, row = 1, sticky = "w")
        tkgrid(SupFrameStandard, row = 3, column=1, sticky = "w")
    }
    tkgrid(tk.supCompute.but, column = 0)
    tkgrid(tk.unsupCompute.but, column = 0)

    # Reset Batch Tab
    onReset <- function() {
        initBatchTab(mainWindow = mainWindow, console = console, 
                     graphicFrame = graphicFrame, RclusTool.env = RclusTool.env, reset=T)
    }
    butReset <- tk2button(win1.nb$env$batch, text = "Reset", image = "reset", compound = "left", width = -6, command = onReset)
    tkgrid(butReset, row = 8, column = 2)
}

#' function to initialize (and to create) the 'batchTab'
#' @title batch tab 
#' @description This function generates the batch tab  of the \code{\link{RclusToolGUI}}, in which the user can batch files.
#' @param mainWindow : window in which the 'batchTab' is created.
#' @param console : frame of the RclusTool interface in which messages should be displayed. 
#' @param graphicFrame : frame of the RclusTool interface in which graphics should be displayed.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @param reset : if TRUE the whole tab is reset, with default options
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
initBatchTab <- function(mainWindow, console, graphicFrame, RclusTool.env, reset=F)
{
    if (is.null(RclusTool.env$gui$tabs.env$batch) || !length(RclusTool.env$gui$tabs.env$batch))
    {
        RclusTool.env$gui$tabs.env$batch <- new.env()
        buildBatchTab(mainWindow, console, graphicFrame, RclusTool.env)
        reset <- T
    }

    batch.env <- RclusTool.env$gui$tabs.env$batch

    if (reset)
    {
        batch.env$files <- NULL
        batch.env$dirFilesDefault <- getwd()
        batch.env$dirFiles <- "" 
        batch.env$rdsFiles <- ""
        batch.env$ProcessFile <- ""
        batch.env$prototypes <- NULL
        batch.env$id.clean.proto <- NULL
        batch.env$protos.directory <- ""
        batch.env$selectedVar <- NULL
        batch.env$protoClean <- NULL
        batch.env$dateNames <- NULL
        batch.env$protoDateList <- NULL
        batch.env$classNames <- NULL
        batch.env$protoClassList <- NULL
        batch.env$tcl.upperThreshold <- NULL
        batch.env$tcl.lowerThreshold <- NULL
        batch.env$model <- NULL
        batch.env$WDirDefault <- getwd()
        batch.env$WDir <- ""
        batch.env$featSpace <- "Principal Components Analysis"
        batch.env$pca=F
        batch.env$spec=F
        tclvalue(batch.env$tcl.supMethod.select) <- "RF"
        tclvalue(batch.env$tcl.unsupMethod.select) <- "K-means"
        tclvalue(batch.env$tcl.sampling.check) <- "0"
        tclvalue(batch.env$tcl.scaling.check.unsup) <- "1"
        batch.env$refreshDirFrameName()
        batch.env$refreshPreprocessingName()
        batch.env$refreshTrainingSetName()
        batch.env$refreshWDirectoryName()
    }
}

