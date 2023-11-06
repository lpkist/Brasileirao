#' @import ggplot2
#' @title Criador de gráficos com ggplot2
#' @description
#' Esta função cria gráficos usando o pacote `ggplot2` de uma maneira mais
#' simples. Além disso, é possível escolher o tema de interesse.
#' @param dados Objeto do tipo `data.frame` que será utilizado para a criação do gráfico
#' @param x Variável que ficará no eixo x
#' @param y Variável que ficará no eixo y
#' @param color Variável que será utilizada na estética `color`
#' @param fill Variável que será utilizada na estética `fill`
#' @param group Variável que será utilizada na separação dos gráficos na função `facet_wrap`
#' @param tipo Tipo de gráfico que se deseja criar. Estão implementados os seguintes:
#' \describe{
#'   \item{pontos}{Gráfico de pontos}
#'   \item{hist}{Histograma}
#'   \item{dens}{Gráfico de densidade}
#'   \item{bar}{Gráfico de barras}
#' }
#' @export

grafico <- function(dados, x=NULL, y = NULL, color = NULL, fill = NULL, group = NULL, tipo = "hist", tema = 1){
  if(!(x%in%colnames(dados)|is.null(x))) stop("x não é uma coluna de dados")
  if(!(is.null(y)||y%in%colnames(dados))) stop("y não é uma coluna de dados")
  if(!(is.null(color)||color%in%colnames(dados))) stop("color não é uma coluna de dados")
  if(!(is.null(fill)||fill%in%colnames(dados))) stop("fill não é uma coluna de dados")
  if(!(is.null(group)||group%in%colnames(dados))) stop("group não é uma coluna de dados")
  tipos <- c("pontos", "hist", "dens", "bar")
  if(!(tipo%in%tipos)) stop('tipo de gráfico inválido. Deve ser um entre c("pontos", "hist", "dens", "bar)')
  if(!(is.numeric(tema))) stop("tema deve ser um inteiro")
  base <- ggplot(dados)
  if(!is.null(x)) base <- base +aes(x=.data[[x]])
  if(!is.null(y)) base <- base +aes(y=.data[[y]])
  if(!is.null(color)) base <- base +aes(color=.data[[color]])
  if(!is.null(fill)) base <- base +aes(fill=.data[[fill]])
  if(tipo == "pontos") base <- base+geom_point()
  if(tipo == "hist") base <- base+geom_histogram()
  if(tipo == "dens") base <- base+geom_density()
  if(tipo == "bar") base <- base+geom_bar()
  if(!is.null(group)) base <- base+facet_wrap(~.data[[group]])
  temas <- list(c(),theme_bw()+
                  theme(text = element_text(size=17)),
                theme_void(),
                theme_dark())
  tema <- tema%%length(temas)
  tema <- ifelse(tema == 0, length(temas), tema)
  if(tema != 1){
  base <- base+temas[[tema]]
  }
  base
}
