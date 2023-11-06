library(tidyverse)
library(Brasileirao918)
tecnico <- "Renato Portaluppi"
data("brasileirao")
res <- brasileirao %>% mutate(resultado =
                         ifelse(gols_mandante > gols_visitante,
                                "Vitória",
                                ifelse(gols_mandante == gols_visitante,
                                       "Empate", "Derrota"))) %>%
  select(ano_campeonato, time_mandante, time_visitante,
         tecnico_mandante, tecnico_visitante)

tec_man <- res %>% filter(tecnico_mandante == tecnico)
tec_vis <- res %>% filter(tecnico_visitante == tecnico)


tab_tecnico <- column(12,
                      h1("Técnico"),
                      selectInput(inputId = "tecnico",
                                  label = "Selecione o técnico",
                                  choices = unique(rbind(brasileirao$time_mandante, brasileirao$tecnico_visitante)), selected = tecnico)
                      # Conteúdo do Slide de Modelos aqui
)
