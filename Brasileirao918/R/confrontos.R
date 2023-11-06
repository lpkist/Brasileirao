confrontos <- function(){
  tab_confrontos <- column(12,
                      h1("Confrontos"),
                      selectInput(inputId = "time1",
                                  label = "Escolha o 1º time",
                                  choices = unique(brasileirao$time_visitante),
                                  "Grêmio"),
                      selectInput(inputId = "time2",
                                  label = "Escolha o 2º time",
                                  choices = unique(brasileirao$time_mandante),
                                  "Internacional")


)

# server <- function(input, output, session) {
#   #
#   #   observe({updateSelectInput(session, inputId = "species",
#   #                             choices = unique(brasileirao$time_mandante)[-input$specie]),
#   #           updateSelectInput(session, inputId = "specie",
#   #                             choices = unique(brasileirao$time_mandante)[-input$species])})
# }
# shinyApp(ui, server)
return(tab_confrontos)
}
