server <- function(input, output, session) {
  
  # PART 1: APPLICATION START UP ###############################################
  
  options(shiny.maxRequestSize=512*1024^2)
  
  # Carica le pagine html corrette nel tab "Data & Help" all'avvio
  output$aboutPanel <- renderUI({includeHTML("www/en-about.html")})
  output$movePanel <- renderUI({includeHTML("www/en-howToMove.html")})
  output$datasetPanel <- renderUI({includeHTML("www/en-changeDataset.html")})
  
  rv <<- reactiveValues()
  
  #' Indica come è codificata la variabile `selectedGamesDF$winner`
  #' @param v una singola riga del dataframe selectedGamesDF$winner, per es. selectedGamesDF$winner[1]
  #' @return TRUE se il vincitore è indicato con: "white", "black" or "draw";
  #'         FALSE se il vincitore è indicato con: "1-0", "0-1" or "1/2-1/2"  
  isVictoryBDW <- function(v) {
    return(
      grepl("black", v, ignore.case = TRUE) ||
        grepl("draw", v, ignore.case = TRUE) ||
        grepl("white", v, ignore.case = TRUE) 
    )
  }
  
  #' Indica come sono salvate le sequenze di mosse in `selectedGamesDF$moves`
  #' @param mv una singola riga del dataframe selectedGamesDF$moves, per es. selectedGamesDF$moves[1]
  #' @return TRUE Se le mosse sono registrate con la numerazione,    per es: "1. e4 e5 2. Nf3 Nc6 3. Bb5 ..."; 
  #'         FALSE Se non sono indicati i numeri delle mosse,        per es: "e4 e5 Nf3 Nc6 Bb5 ..."
  isNumeredMoves <- function(mw) {
    return(startsWith(mw, "1."))
  }
  
  # initializeAll() ------------------------------------------------------------
  
  #' Inizializza ai valori di partenza tutti i reacriveValues di`rv` 
  #' @param file il file in .csv contenente i dati da utilizzare come dataset iniziale. Può differire da "030k.csv" nel caso venga caricato un file esterno 
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
    observeEvent(rv, {rv$whiteEloLS[[1]] <- rv$selectedGamesDF$white_rating})
    observeEvent(rv, {rv$blackEloLS[[1]] <- rv$selectedGamesDF$black_rating}) 
  }
  
  #' Aggiunge una riga alla fine di `rv$probabilitiesDF` con le stime delle probabilità dei possibili esiti di 
  #'  una partita, seguite dagli estremi dei relativi intervalli di confidenza. L'ordinazione è la seguente: bianco, nero, pareggio.
  #' @param win un vettore contenente la colonna `winner` della tabella `rv$selectedGamesDF`
  #' @return None
  addProbabilitiesRow <- function(win) {
    N <- length(win)
    t <- rv$nTurn
    freq <- unname(table(win)/N)
    getInf <- function(p, N) {
      infs <- p + qnorm(0.025) * sqrt( (p*(1-p)) / N )
      for (i in 1:length(infs)) { infs[i] <- max(0, infs[i]) }
      return(infs)
    }
    getSup <- function(p, N) {
      sups <- p + qnorm(0.975) * sqrt( (p*(1-p)) / N) 
      for (i in 1:length(sups)) { sups[i] <- min(1, sups[i]) }
      return(sups)
    }  
    Infs <- getInf(freq, N)
    Sups <- getSup(freq, N)
    row <- c()
    if (isVictoryBDW(win[1])) { # B-D-W
      row <- c(t, freq[3], Infs[3], Sups[3], freq[1], Infs[1], Sups[1], freq[2], Infs[2], Sups[2])
    }
    if (!isVictoryBDW(win[1])) { # 0-1*1-0*1/2-1/2
      row <- c(t, freq[2], Infs[2], Sups[2], freq[1], Infs[1], Sups[1], freq[3], Infs[3], Sups[3])
    }
    rv$probabilitiesDF <- rbind(rv$probabilitiesDF , row)
    names(rv$probabilitiesDF) <- c("Turn", "prWhite", "infWhite", "supWhite", "prBlack", "infBlack", "supBlack", "prDraw", "infDraw", "supDraw")
  }
  
  initializeAll() 
  
  # renderAll() ----------------------------------------------------------------
  
  #' Metodo generico che si occupa della renderizzazione di tutti gli elementi della lista `output` definiti nella ui
  #' Tranne che per i casi più semplici, richiama metodi e funzioni specifiche per ogni elemento di `output`
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
  
  #' Riassume tutti gli elementi contenuti in un vettore di mosse in un'unica stringa aggiungedo, se necessario, la numerazione delle mossse.
  #' @param movesVec vector containing one move in each position
  #' @return a string with all the moves in sequence separated by a space
  genMoveStr <- function(movesVec) {
    str <- ""
    if (isNumeredMoves(rv$allGamesDF$moves[1])) {
      for (i in 1:length(movesVec)) {
        if (i %% 2 == 1) str <- paste0(str, i/2+0.5, ". ")
        str <- paste0(str, movesVec[i], " ")
      }
    }
    if (!isNumeredMoves(rv$allGamesDF$moves[1])) {
      for (i in 1:length(movesVec)) str <- paste0(str, movesVec[i], " ")
    }
    str <- substr(str, 1, nchar(str) - 1)
    return(str)
  }
  
  #' Prova ad identificare come si chiama l'apertura.
  #' Aumentando il numero di mosse inserite, dovrebbe diventare sempre più affidabile.
  #' @param None
  #' @return charachter: il nome dell'apertura che compare con maggiore frequenza nel dataset in uso.
  getOpeningName <- function() {
    return(
      sort(table(rv$selectedGamesDF$opening_name))[1] |> names()
    )
  }
  
  #' Prova a idenfiticare il codice ECO dell'apertura giocata.
  #' Aumentando il numero di mosse inserite, dovrebbe diventare sempre più affidabile. 
  #' Con poche mosse inserite, dovrebbe essere più preciso di getOpeningName()
  #' @param None
  #' @return charachter: il codice ECO dell'apertura che compare con maggiore frequenza nel dataset in uso.
  getOpeningECO <- function() {
    return(
      sort(table(rv$selectedGamesDF$opening_code))[1] |> names()
    )
  }
  
  #' Ritorna le risposte più frequentemente giocate contro l'ultima mossa inserita in input dall'utente.  
  #' @param n Il numero di continuazioni popolari che si vuole siano ritornate
  #' @return table: tabella con le `n` continuazioni più giocate e quante volte queste compaiono nel dataset in uso.
  getPopularContinuation <- function(n) {
    nTurn <- rv$nTurn 
    if (isNumeredMoves(rv$selectedGamesDF$moves[1])) nTurn <- floor((3*nTurn+2)/2) + 1
    if (!isNumeredMoves(rv$selectedGamesDF$moves[1])) nTurn <- nTurn + 1
    movesMatrix <- str_split_fixed(rv$selectedGamesDF$moves, pattern = " ", n = nTurn + 1)
    nextMoves <- movesMatrix[ , nTurn]
    return(sort(table(nextMoves), decreasing = TRUE)[1:n])
  }
  
  #' Per ciascuno dei risultati ottenuti richiamando la funzione `getPopularContinuation()`
  #' aggiunge una riga ad una tabella contenente il conteggio dei possibili esiti della partita 
  #' nell'ipotesi che la prossima mossa sia una di quelle ritornate dalla funzione.
  #' Infine renderizza la tablla costruita nell' user interface in `output$commonContinuations` 
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
  
  #' Costruisce il grafico visualizzato in `output$winnerPlor`
  #' Questo metodo sfrutta la libreria ggplot2. Per fare questo, risulta conveniente prima costruire
  #' un dataframe `dftfr` temporaneo con una struttura più agevole da utilizzare. 
  #' La struttura di `dtfr` è la seguente:
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
  
  #' Costruisce e renderizza il grafico visualizzato in `output$eloPlot`.
  #' Questo metodo deve gestire tre differenti casi:
  #' - plt1: una sequenza di coppie di boxplot che forniscono una rappresentazione generale dell'andamento delle distribuzioni dei punteggi ELO.
  #' - plt2: una coppia di istogrammi che rappresenta più dettagliatamente le distribuzioni dei punteggi ELO al turno corrente.
  #' - plt3: andamento del punteggio ELO medio dei due giocatori e la sua evoluzione col susseguirsi dei turni.
  #' In ciascuno di questi casi, prima di procedere con la rappresentazione, conviene costruirsi un dataframe con una struttura che ne 
  #' agevoli le successive elaborazioni con il pacchetto ggplot. In particolare: 
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
            labs(title = "White ELO") + 
            my_theme()
          plt2 <- ggplot(data.frame("elo" = rv$blackEloLS[[rv$nTurn + 1]]), aes(x = elo)) +
            geom_histogram(binwidth = 25, col = my_cols[4], fill = my_cols[1]) + 
            labs(title = "Black ELO") + 
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
  
  #' Renderizza la scacchiera con i pezzi nelle loro corrette case.
  #' @param None
  #' @return None
  renderBoard <- function() {
    observe(
      output$board <- renderChessboardjs({chessboardjs(rv$chss$fen(),  )})
    )
  }
  
  #' Renderizza tutte le tabelle con le informazioni numeriche visualizzate nel tab "details".
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
  
  #' Se è stata inserita una mossa legale, aggiorna tutti i reactive values contenuti in `rv`
  #' e ricalcola tutti gli output sulla base del nuovo dataset contenuto in `rv$selectedGamesDF`.
  #' Altrimenti, visualizza a schermo un messaggio d'errore insieme ad una lista con tutti gli input accettati.
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
  
  #' Controlla se è stata inserita almeno una mossa.
  #' In questo caso riporta il programma allo stato precedente all'ultima pressione del tasto "move".
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
  })# input$undo
  
  # "Reset" button -------------------------------------------------------------
  
  #' Riporta l'applicazione al suo stato iniziale.
  observeEvent(input$reset, {
    disable("reset")
    initializeAll()
    renderAll()
    enable("reset")
  }) # input$reset
  
  # "Change data set" action ----------------------------------------------------
  
  #' Controlla il file caricato in input dall'utente. 
  #' Se è tutto ok, inizializza il programma con il nuovo dataset.
  observeEvent(input$inputFile, {
    req(input$inputFile)
    check1 <- file_ext(input$inputFile$datapath) == "csv"
    header <- colnames(read.csv2(input$inputFile$datapath))
    check2 <- grepl("moves", header) && grepl("white_rating", header) && 
      grepl("black_rating", header) && grepl("opening_name", header) &&
      grepl("opening_code", header) && grepl("winner", header)
    if (!check1) output$inputFileError <- renderText({"ERROR: Please, select a .csv file"})
    if (!check2) output$inputFileError <- renderText({"ERROR: The column requirements are not satisfied"})
    if (check1 && check2) {
      output$inputFileError <- renderText({""})
      initializeAll(file = input$inputFile$datapath)
      renderAll()
    }
  })#input$inputFile
  
  #' Quando si chiude la finestra del browser, ripristina il valore di default di R per la dimensione massima dei file caricati.
  session$onSessionEnded(function(){
    stopApp()
    options(shiny.maxRequestSize=5*1024^2) # R default value
  })
  
  # PART 3: Extras not strictly necessary ######################################
  
  # Color turn indicator  ------------------------------------------------------
  
  #' Nascone e mostra un icona per indicare il turno del giocatore, in base al colore a cui 
  #' spetta la prossima mossa.
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
  
  #' Mostra i tasti per cambiare lingua solo nel tab appropriato.
  observeEvent(input$menu, {
    if(input$menu == "Data & help") {
      show("it")
      show("en")
    } else {
      hide("it")
      hide("en")
    }
  })
  
  #' Seleziona la lingua richiesta.
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
  
  #' Controlla per ogni click registrato, se è stato effettuato nell'aria della scacchiera.
  #' Nel caso lo fosse:
  #'  - identifica il nome della casa dove è stato effettuato il click.
  #'  - controlla se la casa cliccata è destinazione del movimento di un pezzo che rappresenta una mossa lecita.
  #'  - Se c'è una sola mossa valida per quella casa: aggiorna il valore visualizzato in `input$nextMove`.
  #'  - Se ce ne sono più di una, aggiorna con la mossa ritenuta più plausibile.
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
  
  observeEvent(input$dblClick, {click("doIt")} )
  
  # shortcuts con la tastiera: clicca il tasto desiderato alla pressione di determinate combinazioni di tasti.
  
  observeEvent(input$enterPressed, {click("doIt")} )
  
  observeEvent(input$ctrlzPressed, {click("undo")} )
  
  observeEvent(input$rKeyPressed, {click("reset")} )
  
}

shinyServer(server)
