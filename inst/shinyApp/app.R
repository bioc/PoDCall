library(shiny)
library(shinyjs)
library(readr)
library(PoDCall)
library(ggplot2)
library(gridExtra)
library(DT)

#if (interactive()) {

    ui <- fluidPage(
        shinyjs::useShinyjs(),
        h1("PoDCall"),
        tabsetPanel(
          tabPanel("Import files",
              sidebarLayout(
                  sidebarPanel(
                  uiOutput("softwareOutput"),
                  selectInput("targetChannelInput", "Target channel",
                              choices=c(1,2,3,4,5,6), selected=1),
                  selectInput("controlChannelInput", "Control channel",
                              choices=c(1,2,3,4,5,6), selected=2),
                  fileInput("amplitudeFile",
                              "Select amplitude (.csv) file(s)",
                              accept=c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"),
                              multiple=TRUE),
                  fileInput("ssFile",
                              "Optional: select samplesheet file (.csv)",
                              accept=c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"),
                              multiple=FALSE),
                  tags$hr(),
                  uiOutput("refWellOutput"),
                  #uiOutput("targetChannelOutput"),
                  #uiOutput("controlChannelOutput"),
                  numericInput("BInput", "Number of permutations", 200),
                  numericInput("QInput", "Q (For determining outliers)", 9),
                  actionButton("goButton", "Set thresholds")
                ),
                mainPanel(h3("Selected files")
                            ,tableOutput("contents"))
                )
        ),tags$head(
        tags$style(
        HTML(".shiny-notification {height: 70px;
                                    width: 600px;
                                    position:fixed;
                                    top: calc(50% - 50px);;
                                    left: calc(50% - 400px);;
                                    }")
        )
        ),
        tabPanel("Total Droplets", br(),
                plotOutput("barPlot", width="95%", height="700px"),
                HTML('&nbsp;'), HTML('&nbsp;'),
                downloadButton('downloadBarPlot', 'Download Plot')),
        tabPanel("Thresholds", h3("PoDCall results"),
                fluidRow(DT::dataTableOutput("thresholdTable")
                ),
                fluidRow(downloadButton("thrOutput", "Download results"),
                        actionButton("resetTableEdit", "Undo changes"),
                        br(),br(),
                        fileInput("importResults", "Upload results file"),
                        style="margin-left: 10px;"
                )
        ),
        tabPanel("Visualization",
                sidebarLayout(
                sidebarPanel(
                uiOutput("wellOutput"),
                selectInput("channelInput", "Select channel",
                            choices=c("Target channel",
                                    "Control channel")),
                    uiOutput("thresholdOutput"),
                    actionButton("newThreshold",
                                "Set New Threshold"),br(),br(),
                    actionButton("resetWellSelection", "Reset Well Selection"),
                    downloadButton('downloadPlot', 'Download Plot'),
                    width=3,
                ),
                mainPanel(plotOutput("shinyPlot",
                                    width="100%",
                                    height="700px"),
                            width=7)
                )
        ),
        tabPanel("How to use",
                h3("File Import"),
                "Select correct software version and then select files with
                amplitude values for the wells that you",br(),"wish to set
                thresholds for.",br(),
                "There is a possibility to upload a sample sheet with sample IDs
                and array information.",br(),
                "Such a file can be exported from either 'QuantaSoft' or
                'QX Manager'. Make sure to select the",br(), "appropriate
                software version to ensure correct metadata.",br(),
                "Select an appropriate reference well, and If desired, number of
                permutations and Q, the parameter", br(), "for calling an
                outlier, can be changed. Click 'Set thresholds' to start
                calculating the thresholds,", br(), "pos/neg droplets,
                concentrations etc.", br(),
                br(), br(),
                h3("The Threshold Table"),
                "Under the tab 'Thresholds', the results from PoDCall will be
                presented as a table.",br(),
                "The thresholds can be manually edited by the user, and the
                other values will be updated.", br(),
                "To edit a threshold, double click on the cell you wish to edit
                and enter the new value.", br(),
                "The results table can be reset to the original values by
                clicking 'Undo changes'",br(),
                "The table can be saved as a .csv file, and it is the visual
                table presented, including any changes,", br(),
                "that is saved to file.", br(),
                "Click 'Download results' to save the results to file", br(),
                br(),
                h4("Upload threshold table"),
                "It is possible to upload a previously calculated results table,
                but individual data files corresponding",br(),"to the rows of
                the result table must also be uploaded under the 'Import files'
                tab to be able to make", br(), "any plots. To upload a result
                file (.csv-file downloaded from the PoDCall shiny app), click
                'Browse'", br(),"under 'Upload results file",
                br(), br(),
                h3("Visualization"),
                "To inspect the data and the set thresholds, the PoDCall shiny
                app provides a couple of options:", br(),
                "- Select a single well to view histogram and scatterplot",
                br(),
                "- Select multiple wells to view a comparison plot",
                br(),
                "- Manually edit a set threshold and automatically update table"
                , br(),
                h4("Select multiple wells"),
                "To select multiple wells, hold 'Ctrl' and click wells to select
                , or hold shift to select 'from - to'",
                br(),
                h4("Edit threshold"),
                "A threshold can be changed when viewing histogram and
                scatterplot for either channel of a well.", br(),
                "To change a threshold, enter the new threshold in the numeric
                input box and click 'Set New Threshold'.",
                br(),
                h4("Save plots to file"),
                "Plots can be saved to file by clicking 'Download Plot'", br(),
                br(),br(),br()
      )
    )
  )

server <- function(input, output, session) {

  ## Create software version selection output
  output$softwareOutput <- renderUI({

    selectInput("softwareInput", "Data exported from",
                choices=c("QuantaSoft", "QX Manager"),
                selected="QX Manager",
                multiple=FALSE,
                selectize=TRUE)
    })

    ## Read in data from selected files and store in a list
    plateList <- reactive ({

        ## Channels to analyse
        tarCh <- as.numeric(input$targetChannelInput)
        ctrlCh <- as.numeric(input$controlChannelInput)

        ## File object
        inFile <- input$amplitudeFile

        if(is.null(inFile)) # Don't do anything if no files are selected
            return(NULL)

        ## Create character vector with well ID
        well_id <- unlist(
            lapply(strsplit(inFile$name, split="_"),
                    function(x) x[length(x) -1]))
        ## Determine number of lines to skip in amplitude files
        if(input$softwareInput == "QuantaSoft")
            skipLines <- 0
        if(input$softwareInput == "QX Manager")
            skipLines <- 4

        ## Read files and store in list
        plateData <- mapply(function(x, i){
            wellData <- utils::read.csv(x, header=TRUE, sep=",", skip=skipLines,
                                        )[c(tarCh, ctrlCh)] #[seq_len(2)]
            colnames(wellData) <- c("Ch1", "Ch2", "Ch3",
                                    "Ch4", "Ch5", "Ch6")[c(tarCh, ctrlCh)]
            if(TRUE %in% is.nan(wellData[, 1]) |
                TRUE %in% is.na(wellData[, 1]))
                showNotification(paste0("Check channel ", tarCh,
                                        " values in file: ", i),
                                duration=NULL, type="warning")
            return(list(wellData))
        }, x=inFile$datapath, i=inFile$name)

        ## Warning if there is missing data in control channel
        ctrlChMissing <- lapply(plateData, function(x){TRUE %in% is.na(x[, 2])})
        if(TRUE %in% ctrlChMissing)
            showNotification(paste0("There is missing data for control channel
                                in one or more input files. Check input
                                files or disregard if channel is not used."),
                                duration=NULL, type="warning")

        ## Set well ID as names for data frames holding amplitude data
        names(plateData) <- well_id

        ## Remove empty elements (from empty files)
        plateList <- plateData[vapply(plateData, nrow, numeric(1)) > 0]
    })

    ## Output names of uploaded data
    output$contents <- renderTable({

        if(is.null(plateList()))
            return("No files selected...")

        ## Print names of amplitude files
        input$amplitudeFile$name
    })

    ssData <- reactive({

        ## File object
        inFile <- input$ssFile

        tarCh <- as.numeric(input$targetChannelInput)
        ctrlCh <- as.numeric(input$controlChannelInput)

        ## Import sample sheet data
        ssData <- importSampleSheet(sampleSheet=inFile$datapath[1],
                                    well_id=names(plateList()),
                                    software=input$softwareInput,
                                    targetChannel=tarCh,
                                    controlChannel=ctrlCh)

    })

    ## Create refWell selection output
    output$refWellOutput <- renderUI({
      selectInput("refWellInput", "Reference well",
                  choices=names(plateList()),
                  selected=TRUE,
                  multiple=FALSE,
                  selectize=TRUE)
    })

    # ## Create target channel selection output
    # output$targetChannelOutput <- renderUI({
    #   selectInput("targetChannelInput", "Target channel",
    #               choices=c(1,2,3,4,5,6),
    #               selected=FALSE,
    #               multiple=FALSE,
    #               selectize=TRUE)
    # })
    #
    # ## Create control channel selection output
    # output$controlChannelOutput <- renderUI({
    #   selectInput("controlChannelInput", "Control channel",
    #               choices=c(1,2,3,4,5,6),
    #               selected=FALSE,
    #               multiple=FALSE,
    #               selectize=TRUE)
    # })

    ## Calculate the results/thresholds
    thr <- eventReactive(input$goButton, {

        ## Avoid error messages when inputs are not yet available
        req(plateList())

        ## Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Calculating thresholds", value = 0)
        ## Close the progress when this reactive exits (even if error)
        on.exit(progress$close())

        ## Create a callback function to update progress.
        updateProgress <- function(value=NULL, detail=NULL){
            if (is.null(value)){
                value <- progress$getValue()
                value <- value + (progress$getMax() - value)/5
            }
        progress$set(value=value, detail=detail)
        }

        ## Get the amplitude data from uploaded files
        plateList <- plateList()
        sample_id <- ssData()[, "sample_id"]
        target_assay <- ssData()[, "target_assay"]
        ctrl_assay <- ssData()[, "ctrl_assay"]

        editedTarget <- character(length(plateList))
        editedControl <- character(length(plateList))
        q <- rep(input$QInput, length(plateList))
        referenceWell <- which(names(plateList) == input$refWellInput)

        ## Channels to analyse
        tarCh <- as.numeric(input$targetChannelInput)
        ctrlCh <- as.numeric(input$controlChannelInput)

        # Calculate threshold table
        data.frame(sample_id,
                    podcallThresholds(plateData=plateList,
                                    nrChannels=dim(plateList[[1]])[2],
                                    B=input$BInput,
                                    Q=input$QInput,
                                    refWell=referenceWell,
                                    targetChannel=tarCh,
                                    controlChannel=ctrlCh,
                                    updateProgress=updateProgress),
                    q,
                    target_assay, ctrl_assay,
                    ref_well=names(plateList)[referenceWell],
                    editedTarget, editedControl,
                    stringsAsFactors=FALSE)
    })

    ## Produce barplot output
    totDrBarplot <- reactive({
        req(plateList())

        ## Get data for bar plot
        plotData <- stack(lapply(plateList(), function(x) nrow(x)))
        colnames(plotData) <- c("totDroplets", "wellID")

        ## Make barplot
        barplot <- ggplot(data=plotData, aes(x=wellID, y=totDroplets))+
            geom_col(data=plotData, fill="gray40")+
            geom_hline(yintercept=10000)+
            labs(title="Total Droplets", x="Well", y="Number of droplets")+
            theme(plot.title=element_text(hjust=0.5, size=24),
                axis.title.x=element_text(size=18),
                axis.title.y=element_text(size=18),
                axis.text.x=element_text(angle=90))

        print(barplot)
    })

    shinyjs::disable("downloadBarPlot")
    output$barPlot <- renderPlot({
        totDrBarplot()
        shinyjs::enable("downloadBarPlot")
    })

    output$downloadBarPlot <- downloadHandler(
        filename=function() {
            paste("TotalDroplets_", Sys.Date(), ".tiff", sep="")},
        content=function(file) {
        ggsave(file, plot=totDrBarplot(), device="tiff", dpi=320,
                width=12, height=7, units="in")}
    )

    ## Create a reactive value to hold the threshold table
    thresholds <- reactiveValues(
        data=NULL,
        df=NULL
    )

    observe({
        thresholds$data <- thr()
        thresholds$df <- thresholds$data
    })

    proxy <- dataTableProxy(outputId="thresholdTable")

    ## Update values in table when threshold is changed by user
    observeEvent(input$thresholdTable_cell_edit, {

        ## Coordinates and value of clicked cell
        i <- input$thresholdTable_cell_edit$row
        j <- input$thresholdTable_cell_edit$col
        k <- input$thresholdTable_cell_edit$value

        isolate(
            if(j %in% match(c("thr_target", "thr_ctrl"), names(thresholds$df))){
            thresholds$df[i, j] <- DT::coerceValue(k, thresholds$df[i, j])

            if(j %in% match("thr_target", names(thresholds$df))) {
                data <- plateList()[[rownames(thresholds$df)[i]]]
                amp_tar <- data[[1]]
                amp_ctrl <- data[[2]]
                thr <- thresholds$df[i, j]
                tot_droplets <- length(amp_tar)
                neg_drop_tar <- length(amp_tar[amp_tar<thr])
                neg_drop_ctrl <-
                    length(amp_ctrl[amp_ctrl<thresholds$df$thr_ctrl[i]])
                pos_dr_tar <- tot_droplets-neg_drop_tar

                c_tar <- signif((-log(neg_drop_tar/tot_droplets))/0.000851,
                                digits=4)
                c_norm_4Plex <-
                    signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                        ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*400,
                        digits=4)

                c_norm_sg <-
                  signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                            ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*100,
                         digits=4)

                thresholds$df$pos_dr_target[i] <- pos_dr_tar
                thresholds$df$c_target[i] <- c_tar
                thresholds$df$c_norm_4Plex[i] <- c_norm_4Plex
                thresholds$df$c_norm_sg[i] <- c_norm_sg
                thresholds$df$editedTarget[i] <- "yes"

            }
            if(j %in% match("thr_ctrl", names(thresholds$df))){
                data <- plateList()[[rownames(thresholds$df)[i]]]
                amp_tar <- data[[1]]
                amp_ctrl <- data[[2]]
                thr <- thresholds$df[i, j]
                tot_droplets <- length(amp_tar)
                neg_drop_ctrl <- length(amp_ctrl[amp_ctrl<thr])
                neg_drop_tar <-
                    length(amp_tar[amp_tar<thresholds$df$thr_target[i]])
                pos_dr_ctrl <- tot_droplets-neg_drop_ctrl

                c_ctrl <- signif((-log(neg_drop_ctrl/tot_droplets))/0.000851,
                            digits=4)
                c_norm_4Plex <-
                    signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                        ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*400,
                        digits=4)

                c_norm_sg <-
                    signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                            ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*100,
                        digits=4)

                thresholds$df$pos_dr_ctrl[i] <- pos_dr_ctrl
                thresholds$df$c_ctrl[i] <- c_ctrl
                thresholds$df$c_norm_4Plex[i] <- c_norm_4Plex
                thresholds$df$c_norm_sg[i] <- c_norm_sg
                thresholds$df$editedControl[i] <- "yes"
            }
        }
        )
        replaceData(proxy=proxy,
                    data=thresholds$df,
                    resetPaging=FALSE) # replaces data in the updated table

    })

    ## Reset table changes
    observeEvent(input$resetTableEdit, {
        thresholds$df <- thresholds$data # original PodCall results
    })

    ## Import result file
    observeEvent(input$importResults, {
        resTable <- read.csv(input$importResults$datapath, row.names="Well_ID")
        resTable[is.na(resTable)] <- ""
        if(nrow(resTable) != length(plateList()))
            warning("Check that correct amplitude files have been uploaded")
        req(nrow(resTable) == length(plateList()))

        rownames(resTable) <- names(plateList())
        thresholds$data <- resTable
        thresholds$df <- thresholds$data
    })

    ## Disable downloadbutton for result table before the table is produced
    shinyjs::disable("thrOutput")

    ## Produce the threshold table output
    output$thresholdTable <- renderDT ({

        req(length(thresholds$df)>0)

        shinyjs::enable("thrOutput") #Enable downloadbutton for result table

        DT::datatable(data=thresholds$df,
                    selection="none",
                    colnames=c("well_id"=1),
                    rownames=names(plateList()),
                    editable=list(target="cell",
                                disable=list(columns=c(1,
                                                        4:ncol(thresholds$df)))
                    ))
    })

    ## Produce download button
    output$thrOutput <- downloadHandler(
        filename=function(){ # Make filename suggestion
            paste("PoDCall_results-", Sys.Date(), ".csv", sep="")
        },
        content=function(file){ # Create file contents
            write.csv(data.frame("Well_ID"=names(plateList()),thresholds$df),
                    file, row.names=FALSE)
        }
    )

    ## Create well selection output
    output$wellOutput <- renderUI({

        selectInput("wellInput", "Well name",
                    choices=names(plateList()),
                    selected=NULL,
                    multiple=TRUE,
                    width="100%",
                    size=12,
                    selectize=FALSE)
    })

    outputOptions(output, "wellOutput", suspendWhenHidden=FALSE)

    ## numericInput for threshold
    output$thresholdOutput <- renderUI({
        req(length(input$wellInput) == 1)
        chSel <- input$channelInput
        if(chSel == "Target channel"){
            numericInput("thrInput", "Threshold",
                        value=thresholds$df[input$wellInput, "thr_target"],
                        width="25%")
        }else{
            numericInput("thrInput", "Threshold",
                        value=thresholds$df[input$wellInput, "thr_ctrl"],
                        width="25%")
        }
    })

    shinyjs::disable("newThreshold")
    shinyjs::disable("thresholdOutput")

    observe({
        if(!is.null(input$wellInput) & length(input$wellInput) == 1){
            shinyjs::enable("newThreshold")
            shinyjs::enable("thresholdOutput")
        }
    })

    observe({
        if(!is.null(input$wellInput) & length(input$wellInput) > 1){
            shinyjs::disable("newThreshold")
            shinyjs::disable("thresholdOutput")
        }
    })

    ## Update table when new threshold is entered
    observeEvent(input$newThreshold,{
        req(length(input$wellInput) == 1)
        chSel <- input$channelInput
        new_thr <- input$thrInput
        well <- input$wellInput
        data <- plateList()[[well]]
        amp_ctrl <- data[[2]]
        amp_tar <- data[[1]]

        if(chSel == "Target channel"){
            thresholds$df[well, "thr_target"] <- new_thr
            thresholds$df[well, "pos_dr_target"] <-
                length(amp_tar[amp_tar>new_thr])
            tot_droplets <- length(amp_tar)
            neg_drop_tar <- tot_droplets-length(amp_tar[amp_tar>new_thr])
            neg_drop_ctrl <- tot_droplets-thresholds$df[well, "pos_dr_ctrl"]
            thresholds$df[well,"c_target"] <-
                signif((-log(neg_drop_tar/tot_droplets))/0.000851, digits=4)
            thresholds$df[well, "c_norm_4Plex"] <-
                signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*400, digits=4)
            thresholds$df[well, "c_norm_sg"] <-
                signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*100, digits=4)
            thresholds$df[well, "editedTarget"] <- "yes"
        }else{
            thresholds$df[well, "thr_ctrl"] <- new_thr
            thresholds$df[well, "pos_dr_ctrl"] <-
                length(amp_ctrl[amp_ctrl>new_thr])
            tot_droplets <- length(amp_ctrl)
            neg_drop_tar <- tot_droplets-thresholds$df[well, "pos_dr_target"]
            neg_drop_ctrl <- tot_droplets-length(amp_ctrl[amp_ctrl>new_thr])
            thresholds$df[well, "c_ctrl"] <-
                signif((-log(neg_drop_ctrl/tot_droplets))/0.000851, digits=4)
            thresholds$df[well, "c_norm_4Plex"] <-
                signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*400, digits=4)
            thresholds$df[well, "c_norm_sg"] <-
                signif((((-log(neg_drop_tar/tot_droplets))/0.000851)/
                ((-log(neg_drop_ctrl/tot_droplets))/0.000851))*100, digits=4)
            thresholds$df[well, "editedControl"] <- "yes"
        }
    })

    ## Reset well selection for plot
    observeEvent(input$resetWellSelection,{
        reset("wellOutput")
        shinyjs::disable("downloadPlot")
        shinyjs::disable("newThreshold")
    })

    podcallPlot <- reactive({
        req(thresholds$df)

        ## Channels to plot
        tarCh <- as.numeric(input$targetChannelInput)
        ctrlCh <- as.numeric(input$controlChannelInput)

        ## If single well selected, plot histogram and scatterplot
        if(length(input$wellInput) == 1){
            req(length(input$wellInput) == 1)

        ## Get channel from user selection
        chSel <- input$channelInput
        if(chSel == "Target channel"){
            channel <- 1
            colCh <- tarCh
            thr <- thresholds$df[input$wellInput, "thr_target"]
            plotTitle <- paste0(input$wellInput, ", Ch", tarCh, ", ",
                                thresholds$df[input$wellInput, "sample_id"],
                                ", ",
                                thresholds$df[input$wellInput, "target_assay"])
        }else if(chSel == "Control channel"){
            channel <- 2
            colCh <- ctrlCh
            thr <- thresholds$df[input$wellInput, "thr_ctrl"]
            plotTitle <- paste(input$wellInput,", ctrlCh, ",
                                thresholds$df[input$wellInput, "sample_id"],
                                ", ",
                                thresholds$df[input$wellInput, "ctrl_assay"])
        }

        # Get data for user selected well
        wellData <- plateList()[[input$wellInput]]

        # Make histogram for selected well and channel
        wellHist <- podcallHistogram(channelData=wellData[[channel]],
                                    thr=thr,
                                    channel=colCh)

        #Make scatterplot of selected well and channel
        wellScatter <- podcallScatterplot(channelData=wellData[[channel]],
                                            thr=thr,
                                            channel=colCh)

        grid.arrange(wellHist, wellScatter,
                    nrow=2,
                    top=plotTitle)
        }else{ # Multiple wells selected...
            req(length(input$wellInput) > 1)

        ## Set channel from user selection
        chSel <- input$channelInput
        if(chSel == "Target channel"){
            channel <- "target"
            ch <- tarCh
            if(any(is.na(thresholds$df[input$wellInput,"thr_target"])))
                showNotification("Missing thresholds for target channel",
                                duration=NULL, type="warning")
        }else if(chSel == "Control channel"){
            channel <- "control"
            ch <- ctrlCh
            if(any(is.na(thresholds$df[input$wellInput,"thr_ctrl"])))
                showNotification("Missing thresholds for control channel.Try a
                                 different reference well.",
                                duration=NULL, type="warning")
        }

        ## Make the comparison plot
        multiplot <- podcallMultiplot(plateData=plateList()[input$wellInput],
                                    thresholds=thresholds$df[input$wellInput,
                                                            c("thr_target",
                                                              "thr_ctrl")],
                                    channel=channel, colCh=ch)
        print(multiplot)
        }

    })

    shinyjs::disable("downloadPlot")

    output$shinyPlot <- renderPlot({
        podcallPlot()
        shinyjs::enable("downloadPlot")
    })

    output$downloadPlot <- downloadHandler(
        filename=function() {
            paste("PoDCall_results-", Sys.Date(), ".tiff", sep="")},
        content=function(file) {
            ggsave(file, plot=podcallPlot(), device="tiff", dpi=320,
                    width=10, height=10, units="in")}
    )

    }
shinyApp(ui, server)
#}
