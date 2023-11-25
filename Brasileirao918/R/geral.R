geral <- function(){
tab_geral <- fluidRow(column(3,
                           h1("Geral"),
                           tableOutput('Pub_total'),
                           h3('Estatística de Gols'),
                           tableOutput('Gols_total'),
                           tableOutput('Ano_gols_max'),
                           tableOutput('Ano_gols_min')),
                    column(6,
                           h4("Valor do Time ao Longo do Tempo"),
                           plotlyOutput("valor_times", width = 500, height = 370)),
                    column(3,
                           h3("Menor Valor de Time"),
                           tableOutput('Time_valor_min'),
                           h3('Maior Valor de Time'),
                           tableOutput('Time_valor_max'))
)
}
