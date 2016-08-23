#' Plota o desempenho da carteira de investimentos em fundos de um RPPS.
#'
#' Esta funcao recebe um objeto do tipo xts_dair, contendo o retorno da carteira de um RPPS, bem como
#' dos retornos dos fundos que a compoem, e retorna um grafico do tipo ggplot por meio de um
#' objeto gtable
#'
#' @param rtRpps um objeto do tipo xts_dair contendo os retornos do portfolio das aplicacoes financeiras do RPPS.
#' @return um objeto do tipo gtable contendo um grafico produzido com o pacote ggplot2
#' @author Bruno M. S. S. Melo
#'
#' @examples
#' \dontrun{
#' tsRetornos <- rppsDesempenho(dtCvm, dtDair)
#' rppsPlotaDesempenho(tsRetornos)
#' }
#' @seealso \code{PerformanceAnalytics}, \code{xts}, \code{ggplot2}, \code{gridExtra}
#' @export
rppsPlotaDesempenho <- function(rtRpps) {

  if (prod(dim(rtRpps)) > 0) {

    #extract legend
    #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
    g_legend<-function(a.gplot){
      tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}

    dfData <- data.frame(
      DATA=zoo::index(rtRpps),
      as.data.frame(cumsum(rtRpps))
    )

    dfLong <- reshape2::melt(dfData, id = "DATA")

    perfPlot <- ggplot2::ggplot(dfLong,ggplot2::aes(x=DATA,y=value,group=variable,color=variable) ) +
      ggplot2::geom_line() +
      ggplot2::scale_x_date() +
      ggplot2::ylab("RETORNO ACUMULADO") +
      ggplot2::xlab("") +
      ggplot2::theme(legend.title=ggplot2::element_blank())

    perfLegend <- g_legend(perfPlot)

    perfPlot <- gridExtra::grid.arrange(
      gridExtra::arrangeGrob(
        perfPlot + ggplot2::theme(legend.position="none"), nrow = 1
      ),
      perfLegend,
      nrow=2
    )
  }
}
