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
