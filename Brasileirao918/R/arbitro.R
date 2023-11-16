arbitro <- function(){
tab_arbitro <- fluidRow(column(2,
                    h1("Árbitro"),
                    p("Nesta aba exploraremos estatísticas de cada arbitro, com os dados históricos e por temporada"),
                    selectInput(
                      inputId = "arbitro",
                      label = "Escolha o Arbitro:",
                      choices = sort(unique(brasileirao$arbitro))
                    ),
                    sliderInput(inputId = "Ano",
                    label = "Escolha um Ano:",
                    min = 2003, max = 2023, value = 2013, step = 1)),
                column(7,
                       h3("Estádios Mais apitados(2003 - 2023)"),
                       plotOutput("Top5EstAp", width = 700, height = 500)),
                column(3,
                       h3("Hitórico Geral(2003 - 2023)"),
                       tableOutput("GolsAp"),
                       tableOutput("FaltasAp"),
                       h3("Análise Por Temporada Escolhida"),
                       tableOutput("GolsApTemp"),
                       tableOutput("FaltasApTemp"),
                       tableOutput("TimesApTemp"))
)

return(list(tab_arbitro = tab_arbitro))
}
