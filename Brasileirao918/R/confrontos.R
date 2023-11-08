#' @import tidyverse
confrontos <- function(time1 = "Grêmio", time2 = "Internacional", periodo= c(2003,2023)){
  library(tidyverse)
  tab_confrontos <- column(12,
                      h1("Confrontos"),
                      selectInput(inputId = "time1",
                                  label = "Escolha o 1º time",
                                  choices = unique(brasileirao$time_visitante),
                                  "Grêmio"),
                      selectInput(inputId = "time2",
                                  label = "Escolha o 2º time",
                                  choices = unique(brasileirao$time_mandante),
                                  "Internacional"),
                      sliderInput("periodo_conf",
                                  "Selecione o período do confronto",
                                  min = 2003,
                                  max = max(brasileirao$ano_campeonato),
                                  value = c(2003,2023),
                                  step = 1, sep = ""),
                      tableOutput("historico_conf"),
                      tableOutput("gols_conf"),
                      tableOutput("jogos_s_gols")


)
  # res <- brasileirao %>% mutate(resultado =
  #                                 ifelse(gols_mandante > gols_visitante,
  #                                        "Vitória",
  #                                        ifelse(gols_mandante == gols_visitante,
  #                                               "Empate", "Derrota"))) %>%
  #   select(ano_campeonato, time_mandante, time_visitante,
  #          gols_mandante, gols_visitante, resultado)
  #
  # MV <- res %>% filter(ano_campeonato %in% periodo[1]:periodo[2],
  #                              time_mandante == time1,
  #                              time_visitante == time2)
  # VM <- res %>% filter(ano_campeonato %in% periodo[1]:periodo[2],
  #                              time_mandante == time2,
  #                              time_visitante == time1) %>%
  #   mutate(resultado = ifelse(resultado == "Vitória", "Derrota",
  #                             ifelse(resultado == "Derrota", "Vitória", "Empate")))
  #
  # conf_df <- rbind(MV, VM)
  # hist_conf <- conf_df %>% mutate(resultado = factor(resultado,
  #                                     levels = c("Vitória", "Empate", "Derrota"),
  #                                     labels = c(paste("Vitórias do", time1), "Empates", paste("Vitórias do", time2)), ordered = T)) %>%
  # group_by(resultado) %>% summarise(n = n(),.groups = "drop") #%>%
  #ggplot(aes(x = 0, y = n, fill = resultado))#+
  #geom_bar(stat = "identity", show.legend = F)+
  # coord_flip()+
  # xlim(-1,1)+
  # theme_void()+
  # geom_text(aes(y = cumsum(n),label = paste0(resultado, " (",n, ")")), vjust = -2)


  #gols_conf <- hist_conf

# server <- function(input, output, session) {
#   #
#   #   observe({updateSelectInput(session, inputId = "species",
#   #                             choices = unique(brasileirao$time_mandante)[-input$specie]),
#   #           updateSelectInput(session, inputId = "specie",
#   #                             choices = unique(brasileirao$time_mandante)[-input$species])})
# }
# shinyApp(ui, server)
return(list(tab_confrontos = tab_confrontos))
}
