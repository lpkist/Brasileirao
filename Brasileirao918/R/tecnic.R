#' @import shiny
#' @import shinydashboard
#' @import tidyverse
tecnic <- function(tecnico = c("Renato Portaluppi","Renato Gaúcho")){
  suppressPackageStartupMessages(library(tidyverse))
  tab_tecnico <- fluidRow(column(5,
                                 h1("Técnico"),
                                 selectInput(inputId = "tecnico",
                                             label = "Selecione todos os nomes que se aplicam ao técnico",
                                             choices = sort(
                                               unique(rbind(brasileirao$tecnico_mandante,
                                                            brasileirao$tecnico_visitante))),
                                             selected = tecnico, multiple = T),
                                 h4("Análise de Gols como Mandante"),
                                 tableOutput("tmn"),
                                 h4("Análise de Gols como Visitante"),
                                 tableOutput("tvs")),
                          column(7,
                                 h4("Total de Pontos por Time Treinado"),
                                 plotOutput("top_pontos"),
                                 h4("Aproveitamento por Time Treinado"),
                                 plotOutput("top_aproveitamentos")
                          )
  )

  corBr <- c(`América-MG` = "#007f4e",
             `América-RN` = "#E41920",
             `Athletico-PR` = "#E5050F",
             `Atlético-GO` = "#DC1212",
             `Atlético-MG` = "black",
             `Atlético-PR` = "#E5050F",
             `Avaí FC` = "#00679A",
             `Barueri` = "#FECE02",
             `Botafogo` = "black",
             `Brasiliense-DF` = "#FFDE00",
             `Ceará SC` = "black",
             `Chapecoense` = "#00813A",
             `Corinthians` = "#231f20",
             `Coritiba FC` = "#00534C",
             `Criciúma EC` = "#FDD116",
             `Cruzeiro` = "#2F529E",
             `CSA` = "#0A275D",
             `Cuiabá-MT` = "#FBD800",
             `EC Bahia` = "#006CB5",
             `EC Vitória` = "#ff1100",
             `Figueirense FC` = "black",
             `Flamengo` = "#C52613",
             `Fluminense` = "#870a28",
             `Fortaleza` = "#006cb5",
             `Goiás` = "#01481E",
             `Goiás EC` = "#01481E",
             `Grêmio` = "#0d80bf",
             `Guarani` = "#006C51",
             `Internacional` = "#c20c18",
             `Ipatinga FC` = "#48887B",
             `Joinville-SC` = "#343232",
             `Juventude` = "#00922E",
             `Náutico` = "#ED1C24",
             `Palmeiras` = "#006437",
             `Paraná` = "#1C4189",
             `Paysandu SC` = "#0075C9",
             `Ponte Preta` = "black",
             `Portuguesa` = "#EE221C",
             `RB Bragantino` = "#D61E3F",
             `Santa Cruz` = "#B11C18",
             `Santo André` = "#0A2572",
             `Santos` = "black",
             `Santos FC` = "black",
             `São Caetano` = "#222568",
             `São Paulo` = "#fe0000",
             `Sport Recife` = "#ffd900",
             `Vasco da Gama` = "black")

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
    scale_fill_manual(values = corBr)+
    theme_bw()+
    theme(text = element_text(size=17))+
    labs(y = "Aproveitamento", x = "Time")+
    scale_y_continuous(breaks = seq(0, 1, by = .2),
                       labels = paste0(100*seq(0, 1, by = .2),"%"))

  plot_pontos <- aproveitamento %>%
    ggplot(aes(x = reorder(time, total_pontos), y = total_pontos, fill = time))+
    geom_bar(stat = "identity", show.legend = F)+
    coord_flip()+
    scale_fill_manual(values = corBr)+
    theme_bw()+
    theme(text = element_text(size=17))+
    labs(y = "Pontos", x = "Time")

  return(list(tab_tecnico = tab_tecnico,
              plot_aprov = plot_aprov,
              plot_pontos = plot_pontos))
}
