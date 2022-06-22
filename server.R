server <- function(input, output, session) {
  
  # PART 1: APPLICATION START UP ###############################################
  
  options(shiny.maxRequestSize=512*1024^2)
  
  # Loads the correct html pages in "Data & Help" tab on start up
  output$aboutPanel <- renderUI({includeHTML("www/en-about.html")})
  output$movePanel <- renderUI({includeHTML("www/en-howToMove.html")})
  output$datasetPanel <- renderUI({includeHTML("www/en-changeDataset.html")})
  
  rv <<- reactiveValues()
  
  #' Indicates the `selectedGamesDF$winner` structure
  # @param v A single row from selectedGamesDF$winner, e.g.  selectedGamesDF$winner[1]
  #' @return TRUE if it's"white", "black" or "draw";
  #'         FALSE if it's "1-0", "0-1" or "1/2-1/2"  
  isVictoryBDW <- function(v) {
    return(
      grepl("black", v, ignore.case = TRUE) ||
        grepl("draw", v, ignore.case = TRUE) ||
        grepl("white", v, ignore.case = TRUE) 
    )
  }
  
  #' Indicates the `selectedGamesDF$moves` structure
  #' @param mv A single row from selectedGamesDF$moves, e.g.  selectedGamesDF$moves[1]
  #' @return TRUE if moves are recorded as for example "1. e4 e5 2. Nf3 Nc6 3. Bb5 ..."; 
  #'         FALSE if they are like: "e4 e5 Nf3 Nc6 Bb5 ..."
  isNumberedMoves <- function(mw) {
    return(startsWith(mw, "1."))
  }
  
  # initializeAll() ------------------------------------------------------------
  
  #' Initializes all reactive values of `rv` to the starting values.
  #' @param file the .csv file containing the initial dataset. It may not be "030k.csv" in the case of loading an external file 
  #' @return None
  initializeAll <- function(file="030k.csv"){
    rv$chss <- Chess$new()
    rv$allGamesDF <- rv$selectedGamesDF <- read.csv(file)
    rv$nTurn <- 0
    rv$allMovesVec <- c()
    rv$allMovesStr <- "" 
    rv$probabilitiesDF <- data.frame() 
    rv$whiteEloLS <- list()
    rv$blackEloLS <- list()
    addProbabilitiesRow(rv$selectedGamesDF$winner) |> isolate()
    observeEvent(rv, {
      rv$whiteEloLS[[1]] <- rv$selectedGamesDF$white_rating
      rv$blackEloLS[[1]] <- rv$selectedGamesDF$black_rating
    })
  }
  
  #' Appends a row at the end of `rv$probabilitiesDF`  containing the probability estimates of all possible outcomes
  #'  of a game together with the relative confidence intervals, sorted as follows: white, black and draw.
  #' @param win a vector which is the winner column in `rv$selectedGamesDF`
  #' @return None
   addProbabilitiesRow <- function(win) {
    # Dummy variables
    if (isVictoryBDW(win[1])){
      white <- ifelse(tolower(win) == "white", 1, 0)
      black <- ifelse(tolower(win) == "black", 1, 0)
      draw <- ifelse(tolower(win) == "draw", 1, 0)
    }
    if (!(isVictoryBDW(win[1]))) {
      white <- ifelse(tolower(win) == "1-0", 1, 0)
      black <- ifelse(tolower(win) == "0-1", 1, 0)
      draw <- ifelse(tolower(win) == "1/2-1/2", 1, 0)
    }
    getEstimations <- function(dummy) {
      if (sum(dummy) == 0) return(rep(0,3))
      if (sum(dummy) == length(dummy)) return(rep(1,3))
      fit <- glm(dummy ~ 1, family = "binomial")
      results <- c()
      results[1] <- predict.glm(fit, type = "response")[1]
      ci <- confint(fit) |> unname()
      # logit^{-1}(p)= exp(p) / [1+exp(p)]
      ci <- exp(ci)/(1+exp(ci))
      results[c(2,3)] <- ci
      return(results)
    }
    row <- c(rv$nTurn, getEstimations(white), getEstimations(black), getEstimations(draw))
    rv$probabilitiesDF <- rbind(rv$probabilitiesDF , row)
    names(rv$probabilitiesDF) <- c("Turn", "prWhite", "infWhite", "supWhite", "prBlack", "infBlack", "supBlack", "prDraw", "infDraw", "supDraw")
  }
  
  initializeAll() 
  
  # renderAll() ----------------------------------------------------------------
  
  #' Generic method that renders every element of the `output` list present in the ui.
  #' Except for the simplest cases, it calls specially defined functions for each `output` element
  #' which has to be rendered.
  #' @param None
  #' @return None
  renderAll <- function() {
    observeEvent(rv, {
      output$playedMain <- renderText({rv$allMovesStr})
      output$doItError <- renderText({ "" })
      output$allowedMoves <- renderUI({""})
      renderBoard()
      renderDetails()
      # Expected condition:
      if (nrow(rv$selectedGamesDF) != 0) {
        output$winPlotError <- renderText({""})
        output$eloPlotError <- renderText({""})
        output$commonContinuationsError <- renderText({""})
        output$nGamesInDF <- renderText({ nrow(rv$selectedGamesDF) })
        output$openingECO <- renderText({ getOpeningECO() })
        output$openingName <- renderText({ getOpeningName() })
        observeEvent(input$nCommon, renderCommonContinuation())
        renderWinnerPlot()
        observeEvent(input$eloPlotType, renderEloPlot()) 
      }
      # Error: no games in `rv$selectedGamesDF`
      if (nrow(rv$selectedGamesDF) == 0) {
        output$winPlotError <- renderText({"Error: no games found!"})
        output$eloPlotError <- renderText({"Error: no games found!"})
        output$commonContinuationsError <- renderText({"Error: no games found!"})
        output$nGamesInDF <- renderText({ "NA" })
        output$openingECO <- renderText({ "NA" })
        output$openingName <- renderText({ "NA" })
        output$commonContinuations <- renderTable({})
        output$winnerPlot <- renderPlot({})
        output$eloPlot <- renderPlot({})
      }
    })
  }
  
  #' Summarizes a vector of moves into a single string, including the numbering if it's needed.
  #' @param movesVec vector containing one move in each position
  #' @return a string with all the moves in sequence separated by a space
  genMoveStr <- function(movesVec) {
    str <- ""
    if (isNumberedMoves
(rv$allGamesDF$moves[1])) {
      for (i in 1:length(movesVec)) {
        if (i %% 2 == 1) str <- paste0(str, i/2+0.5, ". ")
        str <- paste0(str, movesVec[i], " ")
      }
    }
    if (!isNumberedMoves
(rv$allGamesDF$moves[1])) {
      for (i in 1:length(movesVec)) str <- paste0(str, movesVec[i], " ")
    }
    str <- substr(str, 1, nchar(str) - 1)
    return(str)
  }
  
  #' Try to guess the name of the current game opening. 
  #' When the number of moves increases, it should become more precise.
  #' @param None
  #' @return charachter: the highest absolute frequency mode in the `rv$selectedGamesDF$opening_name` column
  getOpeningName <- function() {
    return(
      sort(table(rv$selectedGamesDF$opening_name))[1] |> names()
    )
  }
  
  #' Try to guess the ECO code of the current game opening.
  #' When the number of moves increases, it should become more precise, with few moves it is more precise than getOpeningName().
  #' @param None
  #' @return charachter: the highest absolute frequency mode in the `rv$selectedGamesDF$opening_code` column
  getOpeningECO <- function() {
    return(
      sort(table(rv$selectedGamesDF$opening_code))[1] |> names()
    )
  }
  
  #' Returns the most frequently played moves in response to the last input one. 
  #' @param n the desired number of most popular continuation (note: bigger value of n requires more execution time)
  #' @return a table with the n most popular continuation and their relative frequencies found in `rv$selectedGamesDF`
  getPopularContinuation <- function(n) {
    nTurn <- rv$nTurn 
    if (isNumberedMoves
(rv$selectedGamesDF$moves[1])) nTurn <- floor((3*nTurn+2)/2) + 1
    if (!isNumberedMoves
(rv$selectedGamesDF$moves[1])) nTurn <- nTurn + 1
    movesMatrix <- str_split_fixed(rv$selectedGamesDF$moves, pattern = " ", n = nTurn + 1)
    nextMoves <- movesMatrix[ , nTurn]
    return(sort(table(nextMoves), decreasing = TRUE)[1:n])
  }
  
  #' Constructs a table containing the number of games found in `rv$selectedGamesDF`
  #' where the next move in `moves` column  is one of those returned by the
  #' `getPopularContinuation()` function. 
  #' Then render the built table in the UI in `output$commonContinuations`.
  #' @param None
  #' @return None
  renderCommonContinuation <- function() {
    names <- names(getPopularContinuation(input$nCommon))
    freq <- unname(getPopularContinuation(input$nCommon)) |>  as.vector()
    white <- black <- draw <- c()
    for (i in 1:input$nCommon) {
      tempMovesVect <- rv$allMovesVec
      tempMovesVect[length(tempMovesVect) + 1] <- names[i]
      tempMovesStr <- genMoveStr(tempMovesVect)
      tempDf <- rv$selectedGamesDF[startsWith(rv$selectedGamesDF$moves, tempMovesStr) , ]
      t <- table(tempDf$winner)
      if (isVictoryBDW(tempDf$winner[1])) {
        white[i] <- t[3]
        black[i] <- t[1]
        draw[i] <- t[2]
      }
      if (!isVictoryBDW(tempDf$winner[1])) {
        white[i] <- t[2]
        black[i] <- t[1]
        draw[i] <- t[3]
      }
    }
    df <- data.frame(names, freq, white, black, draw)
    names(df) <- c("Next move", "Games founded", "White wins", "Black wins", "Draw")
    output$commonContinuations <- renderTable(df)
  }
  
  #' Builds the plot displayed in `output$winnerPlor`
  #' This method benefits from the ggplot package, in order to do that it first builds
  #' a temporary data frame `dtfr` with a structure easier to manage in ggplot. 
  #' `dtfr`'s structure: 
  #'   +------+---------+-------------+----------------+----------------+
  #'   | Turn | Event   | Probability | lower bound CI | upper bound CI |
  #'   +======+=========+=============+================+================+
  #'   | 0    | 1-0     | Pr(w,0)     | InfPr(w,0)     | SupPr(w,0)     |
  #'   +------+---------+-------------+----------------+----------------+
  #'   | 0    | 0-1     | Pr(b,0)     | InfPr(b,0)     | SupPr(b,0)     |
  #'   +------+---------+-------------+----------------+----------------+
  #'   | 0    | 1/2-1/2 | Pr(d, 0)    | InfPr(d, 0)    | SupPr(d, 0)    |
  #'   +------+---------+-------------+----------------+----------------+
  #'   | ...  | ...     | ...         | ...            | ...            |
  #'   +------+---------+-------------+----------------+----------------+
  #'   | k    | 1-0     | Pr(w,k)     | InfPr(w,k)     | SupPr(w,k)     |
  #'   +------+---------+-------------+----------------+----------------+
  #'   | k    | 0-1     | Pr(b,k)     | InfPr(b,k)     | SupPr(b,k)     |
  #'   +------+---------+-------------+----------------+----------------+
  #'   | k    | 1/2-1/2 | Pr(d, k)    | InfPr(d, k)    | SupPr(d, k)    |
  #'   +------+---------+-------------+----------------+----------------+
  #' @param None
  #' @return None
  renderWinnerPlot <- function() {
    dtfr <- data.frame()
    for (i in 1:nrow(rv$probabilitiesDF)) {
      dtfr[i, 1] <- dtfr[i + nrow(rv$probabilitiesDF), 1] <- dtfr[i + 2*nrow(rv$probabilitiesDF), 1] <- rv$probabilitiesDF$Turn[i]
      dtfr[i, 2] <- "White"
      dtfr[i + nrow(rv$probabilitiesDF), 2] <- "Black"
      dtfr[i + 2*nrow(rv$probabilitiesDF), 2] <- "Draw"
      dtfr[i, 3] <- rv$probabilitiesDF$prWhite[i]
      dtfr[i, 4] <- rv$probabilitiesDF$infWhite[i]
      dtfr[i, 5] <- rv$probabilitiesDF$supWhite[i]
      dtfr[i + nrow(rv$probabilitiesDF), 3] <- rv$probabilitiesDF$prBlack[i]
      dtfr[i + nrow(rv$probabilitiesDF), 4] <- rv$probabilitiesDF$infBlack[i]
      dtfr[i + nrow(rv$probabilitiesDF), 5] <- rv$probabilitiesDF$supBlack[i]
      dtfr[i + 2*nrow(rv$probabilitiesDF), 3] <- rv$probabilitiesDF$prDraw[i]
      dtfr[i + 2*nrow(rv$probabilitiesDF), 4] <- rv$probabilitiesDF$infDraw[i]
      dtfr[i + 2*nrow(rv$probabilitiesDF), 5] <- rv$probabilitiesDF$supDraw[i]
    }
    names(dtfr) <- c("turn", "result", "prob", "inf", "sup")
    output$winnerPlot <- renderPlot({
      g <- ggplot(dtfr, aes(x=turn, fill=result, color=result)) +
        geom_point(aes(y=prob),colour="black", pch=23, size=2) +
        geom_line(aes(y=prob)) +
        geom_ribbon(aes(ymin=inf, ymax=sup), alpha=0.2) + 
        my_theme()
      g + scale_fill_manual(values = my_cols) + scale_color_manual(values = my_cols)
    }) 
  }
  
  #' Builds the plot displayed in `output$eloPlot`
  #' This method handles three possible different cases:
  #' - plt1: a set of box plot diagram pairs that show in a general way the trend of the distributions of the elo scores
  #' - plt2: a pair of histograms showing the two elo distributions at the current turn
  #' - plt3: Trend of the average score of the black/white player during the rounds
  #' In each case, before building the rendered plot, the function builds a data frame conveniently 
  #' structured to proceed with the graphical representation: 
  #'     plt1:                                plt2:                      plt3: 
  #'       +------+-------+---------------+      vector contained in        +------+-------------+-------+
  #'       | Trun | Color | ELO           |       rv$<color>EloLS           | Turn | Average ELO | Color |
  #'       +======+=======+===============+    +-------+     +-------+      +======+=============+=======+
  #'       | 0    | white | w0[1]         |    | white |     | black |      | 0    | avg(w,0)    | white |
  #'       +------+-------+---------------+    +=======+     +=======+      +------+-------------+-------+
  #'       | 0    | white | w0[2]         |    |   w1  |     |   b1  |      | 0    | avg(b,0)    | black |
  #'       +------+-------+---------------+    +-------+     +-------+      +------+-------------+-------+
  #'       | ...  | ...   | ...           |    |  ...  |     |  ...  |      | ...  | ...         | ...   |
  #'       +------+-------+---------------+    +-------+     +-------+      +------+-------------+-------+
  #'       | 0    | white | w0[lengt(w0)] |    |   wn  |     |  wn   |      | n    | avg(w,n)    | white |
  #'       +------+-------+---------------+    +-------+     +-------+      +------+-------------+-------+
  #'       | 0    | black | b0[1]         |                                 | n    | avg(b,n)    | black |
  #'       +------+-------+---------------+                                 +------+-------------+-------+
  #'       | ...  | ...   | ...           |
  #'       +------+-------+---------------+
  #'       | 0    | black | b0[lengt(b0)] |
  #'       +------+-------+---------------+
  #'       | 1    | white | w1[1]         |
  #'       +------+-------+---------------+
  #'       | ...  | ...   | ...           |
  #'       +------+-------+---------------+   
  #' @param None
  #' @return None
  renderEloPlot <- function() {
    switch (input$eloPlotType,
      "plt1" = {
        dtfr <- data.frame()
        for (i in 1:length(rv$blackEloLS)) {
          temp1 <- data.frame(
            "turn" = rep(i -1, length(rv$whiteEloLS[[i]])),
            "color" = rep("white", length(rv$whiteEloLS[[i]])),
            "elo" = rv$whiteEloLS[[i]]
          ) # temp1
          temp2 <- data.frame(
            "turn" = rep(i-1, length(rv$blackEloLS[[i]])),
            "color" = rep("black", length(rv$blackEloLS[[i]])),
            "elo" = rv$blackEloLS[[i]]
          ) # temp2
          dtfr <- rbind(dtfr, temp1, temp2)
        }
        output$eloPlot <- renderPlot({
          g <- ggplot(dtfr, aes(x = factor(turn), y = elo, fill = color)) +
            geom_boxplot(col = my_cols[4], outlier.shape = NA) +
            my_theme()
          g + scale_fill_manual(values = my_cols) + scale_color_manual(values = my_cols)
        })
      }, # plt1 
      "plt2" = {
        output$eloPlot <- renderPlot({
          plt1 <- ggplot(data.frame("elo" = rv$whiteEloLS[[rv$nTurn + 1]]), aes(x = elo)) +
            geom_histogram(binwidth = 25, col = my_cols[4], fill = my_cols[2]) + 
            labs(title = "White score") + 
            my_theme()
          plt2 <- ggplot(data.frame("elo" = rv$blackEloLS[[rv$nTurn + 1]]), aes(x = elo)) +
            geom_histogram(binwidth = 25, col = my_cols[4], fill = my_cols[1]) + 
            labs(title = "Black score") + 
            my_theme() 
          gridExtra::grid.arrange(plt1, plt2, ncol = 1, nrow = 2)
        })
      }, # plt2
      "plt3" = {
        turnColumn <- rep(0:rv$nTurn) |> rep(2)
        colorColumn <- c(rep("white", rv$nTurn + 1), rep("black", rv$nTurn + 1))
        whiteAvg <- blackAvg <- c()
        for (i in 0:rv$nTurn) {
          whiteAvg[i+1] <- mean(rv$whiteEloLS[[i+1]])
          blackAvg[i+1] <- mean(rv$blackEloLS[[i+1]])
        }
        df<- data.frame("turn" = turnColumn, "color" = colorColumn, "elo" = c(whiteAvg, blackAvg))
        output$eloPlot <- renderPlot({
          g <- ggplot(df, aes(x = turn, y = elo, col = color, fill = color)) +
            geom_point(colour = "black", pch=23, size=2) +
            geom_line() +
            my_theme()
          g + scale_fill_manual(values = my_cols) + scale_color_manual(values = my_cols)
        })
      } # plt3
    )
  }
  
  #' Render the chessboard with the pieces in their right squares.
  #' @param None
  #' @return None
  renderBoard <- function() {
    output$board <- renderChessboardjs({chessboardjs(rv$chss$fen())})
  }
  
  #' Render all the tables displayed in the "details" tab.
  #' @param None
  #' @return None
  renderDetails <- function() {
    output$playedDetails <- renderText({rv$allMovesStr})
    prDf <- rv$probabilitiesDF
    prDf[ , 1] <- as.integer(prDf[ , 1])
    output$winningOddsDetails <- renderTable({prDf}, digits = 6)
    buildEloDf <- function(ls) {
      row <- c()
      df <- data.frame()
      for (i in 1:length(ls)) {
        row <- c(i-1, length(ls[[i]]), summary(ls[[i]]))
        df <- rbind(df, row)
      }
      names(df) <- c("Tunr", "Games found" ,"Minimum value", "1st quartile", "Median", "Mean",  "3rd quartile", "Maximum value")
      return(df)
    } # buildEloDf function
    output$whiteEloDetails <- buildEloDf(rv$whiteEloLS) |> renderTable(digits = 0)
    output$blackEloDetails <- buildEloDf(rv$blackEloLS) |> renderTable(digits = 0)
  }
  
  renderAll()
  
  # PART 2: APPLICATION MAIN LOGIC #############################################
  
  # "Move" button --------------------------------------------------------------
  
  #' If a correct move has been entered, update all reactive values contained in `rv`
  #' and recalculate all outputs with the new data set contained in `rv$selectedGamesDF`.
  #' Otherwise it displays an error message along with a list of acceptable inputs.
  observeEvent(input$doIt, {
    disable("doIt")
    updateTextInput(session, inputId = "nextMove", value = "")
    if (input$nextMove %in% rv$chss$moves()) {
      rv$nTurn <- rv$nTurn + 1
      rv$allMovesVec[rv$nTurn] <- isolate(input$nextMove)
      rv$allMovesStr <- genMoveStr(rv$allMovesVec)
      rv$chss$move(rv$allMovesVec[rv$nTurn])
      rv$selectedGamesDF <- rv$selectedGamesDF[startsWith(rv$selectedGamesDF$moves, rv$allMovesStr) , ]
      addProbabilitiesRow(rv$selectedGamesDF$winner) |> isolate()
      rv$whiteEloLS[[rv$nTurn + 1]] <- rv$selectedGamesDF$white_rating
      rv$blackEloLS[[rv$nTurn + 1]] <- rv$selectedGamesDF$black_rating
      renderAll()
    }
    if (!(input$nextMove %in% rv$chss$moves())) {
      output$doItError <- renderText({"ERROR: illegal move"})
      str <- "<p><span class = 'infoValues'> Allowed moves are:</span> <br>"
      str <- paste0(str, "<span id='allowedMoves'>" )
      for (i in 1:length(rv$chss$moves())) { str<- paste0(str, rv$chss$moves()[i], ", ") }
      str<- paste0(str, "</span></p>")
      output$allowedMoves <- renderUI({HTML(str)})
    }
    enable("doIt")
  }) # input$doIt
  
  
  # "Undo" button --------------------------------------------------------------
  
  #' Check if at least one move has been entered.
  #' In this case it reports the program status as the moment before the "move" button was pressed.
  observeEvent(input$undo, {
    if(rv$nTurn > 0) {
      rv$whiteEloLS[[rv$nTurn + 1]] <- c()
      rv$blackEloLS[[rv$nTurn + 1]] <- c()
      rv$probabilitiesDF <- head(rv$probabilitiesDF, -1)
      rv$allMovesVec <- head(rv$allMovesVec, -1)
      rv$allMovesStr <- genMoveStr(rv$allMovesVec)
      rv$chss$undo()
      rv$nTurn <- rv$nTurn - 1
      rv$selectedGamesDF <- rv$allGamesDF[startsWith(rv$allGamesDF$moves, rv$allMovesStr) , ]
      if (nrow(rv$selectedGamesDF) == 0) {rv$selectedGamesDF <- rv$allGamesDF}
      renderAll()
    }
  }) # input$undo
  
  # "Reset" button -------------------------------------------------------------
  
  #' Brings the app to its initial state
  observeEvent(input$reset, {
    disable("reset")
    initializeAll()
    renderAll()
    enable("reset")
  }) # input$reset
  
  # "Change data set" action ----------------------------------------------------
  
  #' Checks the file uploaded by the user. 
  #' If everything is ok, initializes the program with the new dataset
  observeEvent(input$inputFile, {
    req(input$inputFile)
    check1 <- file_ext(input$inputFile$datapath) == "csv"
    if (!check1) output$inputFileError <- renderText({"ERROR: Please, select a .csv file"})
    if (check1) {
      header <- colnames(read.csv2(input$inputFile$datapath))
      check2 <- grepl("moves", header) && grepl("white_rating", header) && 
        grepl("black_rating", header) && grepl("opening_name", header) &&
        grepl("opening_code", header) && grepl("winner", header)
      if (!check2) output$inputFileError <- renderText({"ERROR: The column requirements are not satisfied"})
      if (check2) {
        output$inputFileError <- renderText({""})
        initializeAll(file = input$inputFile$datapath)
        renderAll()
        updateTabsetPanel(session, inputId = "menu", selected = "Main")
      }
    }
  }) #input$inputFile
  
  #' When the browser is closed, terminates the program and resets the default value of 
  #' options(shiny.maxRequestSize).
  session$onSessionEnded(function(){
    stopApp()
    options(shiny.maxRequestSize=5*1024^2) # R default value
  })
  
  # PART 3: Extras not strictly necessary ######################################
  
  # Color turn indicator  ------------------------------------------------------
  
  #' Dynamically shows and hides an icon for white or black turn depending on 
  #' who has to enter the current following move.
  observeEvent(rv$nTurn, {
    if (rv$nTurn %% 2 == 0) {
      hide("blackIndicator")
      show("whiteIndicator")
    }
    if (rv$nTurn %% 2 == 1) {
      hide("whiteIndicator")
      show("blackIndicator")
    }  
  })
  
  # Language support -----------------------------------------------------------
  
  #' Shows the language buttons only in the appropriate tab
  observeEvent(input$menu, {
    if(input$menu == "?About") {
      show("it")
      show("en")
      show("sourceCodeButton")
    } else {
      hide("it")
      hide("en")
      hide("sourceCodeButton")
    }
  })
  
  #' Select the required language
  observeEvent(input$it, {
    output$aboutPanel <- renderUI({includeHTML("www/it-about.html")})
    output$movePanel <- renderUI({includeHTML("www/it-howToMove.html")})
    output$datasetPanel <- renderUI({includeHTML("www/it-changeDataset.html")})
  })
  observeEvent(input$en, {
    output$aboutPanel <- renderUI({includeHTML("www/en-about.html")})
    output$movePanel <- renderUI({includeHTML("www/en-howToMove.html")})
    output$datasetPanel <- renderUI({includeHTML("www/en-changeDataset.html")})
  })
  
  # Mouse and keyboard support -------------------------------------------------
  
  #' Checks for each click made in the app if it was made on the board.
  #' If it is:
  #'  - identifies the name of the clicked square
  #'  - checks if there is at least one valid move in that square
  #'  - if there's one: update the value of `input$nextMove` with that.
  #'  - if there are more: uses the value considered most plausible
  observeEvent(input$mouseClicked, {
    checkX <- input$mouseClicked[3] - input$mouseClicked[1] >= 0  && input$mouseClicked[3] - input$mouseClicked[1] <= input$mouseClicked[3]
    checkY <- input$mouseClicked[3] - input$mouseClicked[2] >= 0  && input$mouseClicked[3] - input$mouseClicked[2] <= input$mouseClicked[3]
    if (checkX && checkY) {
      numb <- c("8", "7", "6", "5", "4", "3", "2", "1")
      lett <- c("a", "b", "c", "d", "e", "f", "g", "h")
      squareSize <- input$mouseClicked[3]/8
      squareName <- paste0(lett[ceiling(input$mouseClicked[1]/squareSize)], numb[ceiling(input$mouseClicked[2]/squareSize)])
      possibileMoves <- rv$chss$moves()[grepl(squareName, rv$chss$moves())]
      if ("O-O" %in% rv$chss$moves()) {possibileMoves <- append(possibileMoves,"O-O")}
      if ("O-O-O" %in% rv$chss$moves()) {possibileMoves <- append(possibileMoves,"O-O-O")}
      plausible <- possibileMoves[1]
      for (i in 2:length(possibileMoves)) {
        isBetter <- nrow(rv$selectedGamesDF[startsWith(rv$selectedGamesDF$moves, genMoveStr(c(rv$allMovesVec, possibileMoves[i-1]))) , ]) < 
          nrow(rv$selectedGamesDF[startsWith(rv$selectedGamesDF$moves, genMoveStr(c(rv$allMovesVec, possibileMoves[i]))) , ])
        if(isBetter) {plausible <- possibileMoves[i]}
      }
      updateTextInput(session, inputId = "nextMove", value = plausible)
    }
  })
  
  observeEvent(input$dblClick, {
    if(input$menu == "Main") click("doIt")
  })
  
  # keyboard events: clicks an actionButton depending on the key combination
  
  observeEvent(input$enterPressed, {click("doIt")} )
  
  observeEvent(input$ctrlzPressed, {click("undo")} )
  
  observeEvent(input$rKeyPressed, {click("reset") })
  
  # Shortcut to the "change dataset" section: 
  observeEvent(input$changeDataset, {
    updateTabsetPanel(session, inputId = "menu", selected = "?About")
    updateNavlistPanel(session, inputId = "helpNavlist", selected = "Change dataset")
  })
  
}

shinyServer(server)
