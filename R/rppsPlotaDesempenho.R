#' Plota o desempenho da carteira de investimentos em fundos de um RPPS.
#'
#' Esta funcao recebe um objeto do tipo xts_dair, contendo o retorno da carteira de um RPPS, bem como
#' dos retornos dos fundos que a compoem, e retorna um grafico do tipo ggplot por meio de um
#' objeto gtable
#'
#' @param retornos um objeto do tipo xts_dair contendo os retornos do portfolio das aplicacoes financeiras do RPPS.
#' @param opcao objeto do tipo inteiro especificando um dentre as seguintes opcoes de retorno:
#' \itemize{
#'   \item \code{opcao = 0} (padrao) objeto do tipo gtable contendo um grafico em que os retornos acumulados sao exibidos acima de uma legenda.
#'   \item \code{opcao = 1} environment contendo dois objetos: legenda (do tipo \code{grob}) e grafico (do tipo \code{grob})
#' }
#' @param numColLeg numero de colunas na legenda (por padrao, sao 3)
#' @return um objeto conforme especificado pelo parametro \code{opcao}
#' @author Bruno M. S. S. Melo
#'
#' @examples
#' \dontrun{
#' tsRetornos <- rppsDesempenho(dtCvm, dtDair)
#' rppsPlotaDesempenho(retornos = tsRetornos, opcao = 0, numColLeg = 3)
#' }
#' @seealso \code{PerformanceAnalytics}, \code{xts}, \code{ggplot2}, \code{gridExtra}
#' @export
rppsPlotaDesempenho <- function(retornos, opcao = 0, numColLeg = 3) {

  if (prod(dim(retornos)) > 0){

    #extract legend
    #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
    g_legend <- function(a.gplot){
      tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}

    dfData <- data.frame(
      DATA=zoo::index(retornos),
      as.data.frame(cumsum(retornos))
    )

    dfLong <- reshape2::melt(dfData, id = "DATA")

    perfPlot <- ggplot2::ggplot(
      dfLong,ggplot2::aes(x=DATA,y=value,group=variable,color=variable) ) +
      ggplot2::geom_line() +
      ggplot2::scale_x_date() +
      ggplot2::ylab("RETORNO ACUMULADO") +
      ggplot2::xlab("") +
      ggplot2::theme(legend.title=ggplot2::element_blank()) +
      ggplot2::guides(color=guide_legend(ncol=numColLeg))

    perfLegend <- g_legend(perfPlot)

    if (opcao == 0){
      perfPlot <- gridExtra::arrangeGrob(
        ggplot2::ggplotGrob(
          perfPlot + ggplot2::theme(legend.position="none")
        ),
        perfLegend,
        nrow=2
      )
      result <- perfPlot
    } else{
      result <- new.env(parent = emptyenv())
      result$legenda <- perfLegend
      result$grafico <- ggplot2::ggplotGrob(perfPlot + ggplot2::theme(legend.position="none"))
    }
    return(result)
  }
}
