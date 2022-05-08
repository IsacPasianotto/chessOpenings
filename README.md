(English version below)

# Shiny app per l'analisi delle aperture nel gioco degli scacchi

Questa è una Shiny app scritta principalmente con il linguaggio di programmazione R per la rappresentazione di alcune semplici analisi durante le prime mosse di una partita di scacchi. 

## Come usarla: 

Scaricato la cartella con il progetto, è sufficiente aprirlo con Rstudio ed selezionare "Run App".

In alternativa su una console in *R*, digitare: 

`shiny::runApp('<pathToTheDirectory>')`


## Librerie necessarie al funzionamento

Per funzionare correttamente, il programma richiede le seguenti librerie: 

- shiny
- shinyjs
- stringr
- ggplot2
- gridExtra
- rchess

Qualora una o più non fossero installate, prima di eseguire l'applicazione è necessario inserire in una console di *R*:

`install.package('missingPackageName')`

---

# Chess opening explorer


This is a shiny App written in R which shows some simple analysis of a chess game during the first moves of the opening.

All the explanations on how the application works and how to use it are on the "Data & Help" section.

## How to use?

Download this project, Then open it in *Rstudio*, or run the following command in an R console:
        
`shiny::runApp('<pathToTheDirectory>')`

The program requires the following libraries (and all their dependencies) to work:

- shiny
- shinyjs
- stringr
- ggplot2
- gridExtra
- rchess

If one (or more) is missing, use the following command:

`install.package('missingPackageName')`
