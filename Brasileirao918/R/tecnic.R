#' @import tidyverse
tecnic <- function(tecnico = "Renato Portaluppi"){
tab_tecnico <- column(12,
                      h1("Técnico"),
                      selectInput(inputId = "tecnico",
                                  label = "Selecione o técnico",
                                  choices = sort(unique(rbind(brasileirao$tecnico_mandante, brasileirao$tecnico_visitante))),
                                  selected = tecnico, multiple = T),
                      plotOutput("top_pontos"),
                      plotOutput("top_aproveitamentos")
                      # Conteúdo do Slide de Modelos aqui
)

res <- brasileirao %>% mutate(resultado =
                                ifelse(gols_mandante > gols_visitante,
                                       "Vitória",
                                       ifelse(gols_mandante == gols_visitante,
                                              "Empate", "Derrota"))) %>%
  select(ano_campeonato, time_mandante, time_visitante,
         tecnico_mandante, tecnico_visitante, resultado)

tec_man <- res %>% filter(tecnico_mandante %in% tecnico) %>%
  mutate(pontos = ifelse(resultado ==  "Vitória", 3,
                         ifelse(resultado ==  "Empate", 1,0)),
         time = time_mandante,
         adv = time_visitante) %>%
  select(-c(tecnico_mandante, tecnico_visitante,
            time_mandante, time_visitante))

tec_vis <- res %>% filter(tecnico_visitante %in% tecnico) %>%
  mutate(pontos = ifelse(resultado ==  "Vitória", 0,
                         ifelse(resultado ==  "Empate", 1,3)),
         time = time_visitante,
         adv = time_mandante) %>%
  select(-c(tecnico_mandante, tecnico_visitante,
            time_mandante, time_visitante))

df_treinador <- rbind(tec_man, tec_vis)
aproveitamento <- df_treinador %>% group_by(time) %>%
  summarise(total_pontos = sum(pontos),
            aprov = total_pontos/(3*n()))

plot_aprov <- aproveitamento %>%
  ggplot(aes(x = reorder(time, aprov), y = aprov, fill = time))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  theme_bw()+
  labs(y = "Aproveitamento", x = "Time")+
  scale_y_continuous(breaks = seq(0, 1, by = .2),
                     labels = paste0(100*seq(0, 1, by = .2),"%"))

plot_pontos <- aproveitamento %>%
  ggplot(aes(x = reorder(time, total_pontos), y = total_pontos, fill = time))+
  geom_bar(stat = "identity", show.legend = F)+
  coord_flip()+
  theme_bw()+
  labs(y = "Pontos", x = "Time")

return(list(tab_tecnico = tab_tecnico,
            plot_aprov = plot_aprov,
            plot_pontos = plot_pontos))
}
