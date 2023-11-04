#' @import ggplot2
#' @export

grafico <- function(dados, x=NULL, y = NULL, color = NULL, fill = NULL, group = NULL, tipo = "hist"){
  if(!(x%in%colnames(dados)|is.null(x))) stop("x não é uma coluna de dados")
  if(!(is.null(y)||y%in%colnames(dados))) stop("y não é uma coluna de dados")
  if(!(is.null(color)||color%in%colnames(dados))) stop("color não é uma coluna de dados")
  if(!(is.null(fill)||fill%in%colnames(dados))) stop("fill não é uma coluna de dados")
  if(!(is.null(group)||group%in%colnames(dados))) stop("group não é uma coluna de dados")
  tipos <- c("pontos", "hist", "dens", "bar")
  if(!(tipo%in%tipos)) stop('tipo de gráfico inválido. Deve ser um entre c("pontos", "hist", "dens", "bar)')
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
  base

}
