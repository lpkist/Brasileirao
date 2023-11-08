arbitro <- function(){
tab_arbitro <- fluidRow(column(2,
                    h1("Árbitro"),
                    p("Nesta aba exploraremos estatísticas de cada arbitro, com os dados históricos e por temporada"),
                    selectInput(
                      inputId = "arbitro",
                      label = "Escolha o Arbitro:",
                      choices = unique(brasileirao$arbitro)
                    ),
                    sliderInput(inputId = "AnoArbitro",
                    label = "Escolha um Ano:",
                    min = 2003, max = 2023, value = 2013, step = 1)),
                column(7,
                       plotOutput("Top5EstAp", width = 700, height = 500)),
                column(3,
                       h3("Hitórico Geral(2003 - 2023)"),
                       tableOutput("GolsAp"),
                       tableOutput("FaltasAp"),
                       h3("Análise Por Temporada Escolhida"),
                       tableOutput("GolsApTemp"),
                       tableOutput("FaltasApTemp"))
)

return(list(tab_arbitro = tab_arbitro))
}




###########################
# dados <- brasileirao
# dados$estadio <- trimws(dados$estadio)
#
# dados <- dados %>% mutate(resulM = ifelse(gols_mandante>gols_visitante, "vence",
#                                  ifelse(gols_mandante<gols_visitante, "perde", "empate")),
#                  resulV = ifelse(gols_mandante<gols_visitante, "vence",
#                                  ifelse(gols_mandante>gols_visitante, "perde", "empate")))
#
# brasileirao %>%
#   filter(arbitro == "Leandro Pedro Vuaden") %>%
#   count(estadio) %>% arrange(-n) %>% top_n(n, n = 5) %>%
#   ggplot(aes(x = estadio, y = n))+
#   geom_bar(stat = "identity")+
#   labs(x = "Estadio", y = "Jogos Apitados")+theme_bw()
#
#
# GolsAp <- brasileirao %>% filter(arbitro == "Alexandre Vargas Tavares") %>%
#   summarise(gols = sum(gols_mandante+gols_visitante))

# FaltasAp <- brasileirao %>% filter(arbitro == "Leandro Pedro Vuaden") %>%
#   drop_na() %>%
#   summarise(faltasTot = sum(faltas_mandante+faltas_visitante),
#             faltasMedia = mean(faltas_mandante+faltas_visitante))
#
# dados %>% count(resulM)
# dados %>% count(resulV)
#
# res <- brasileirao %>% filter(ano_campeonato == 2011) %>%
#   mutate(resultado =
#            ifelse(gols_mandante > gols_visitante,
#                   "Vitória",
#                   ifelse(gols_mandante == gols_visitante,
#                          "Empate", "Derrota"))) %>%
#   select(ano_campeonato, time_mandante, time_visitante,
#          rodada, resultado, gols_mandante, gols_visitante)
# M <- res %>% select(-time_visitante) %>% mutate(time = time_mandante, gols = gols_mandante, gols_sof = gols_visitante) %>%
#   select(-time_mandante, -gols_mandante, -gols_visitante)
# V <- res %>% select(-time_mandante) %>%
#   mutate(resultado = ifelse(resultado == "Vitória", "Derrota",
#                             ifelse(resultado == "Derrota", "Vitória", "Empate")),
#          time = time_visitante, gols = gols_visitante,
#          gols_sof = gols_mandante) %>% select(-time_visitante, -gols_visitante, -gols_mandante)
# jogos <- rbind(M,V)
# final <- jogos %>% mutate(pontos = ifelse(resultado == "Vitória", 3,
#                                           ifelse(resultado == "Empate", 1, 0))) %>%
#   group_by(time) %>% arrange(rodada) %>% mutate(total = cumsum(pontos), vitorias = sum(resultado == "Vitória"), gols_marcados = cumsum(gols), gols_sof = cumsum(gols_sof), sg = gols_marcados - gols_sof)
#
# fim <- c()
# for(i in 1:38){
#   rod <- final %>% filter(rodada == i) %>% arrange(-total,-vitorias,-sg,gols_marcados) %>% cbind(colocacao = c(1:20))
#   fim <- rbind(fim, rod)
# }
# final <- fim
#
#
# timesVitAno <- final %>% filter(arbitro == "Leandro Pedro Vuaden") %>%
#   group_by(time) %>%
#   count(resultado) %>% arrange(-n)
