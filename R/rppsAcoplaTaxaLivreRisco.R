#' Acopla uma taxa livre de risco a serie temporal de retornos dos investimentos em cotas de fundos de um RPPS.
#'
#' Esta funcao acopla uma taxa livre de risco a serie temporal de retornos da carteiras de investimentos
#' em cotas de fundos obtida a partir da funcao \code{rppsDesempenho}.
#'
#' Obs.: A serie temporal de retornos so podera conter uma taxa livre de risco. Se chamada mais de uma
#'       vez para uma mesma serie historica de retornos, nova taxa sendo incluida ira substituir outra
#'       que esteja presente.
#'
#' @param retornos objeto xts contendo os retornos mensais da carteira de investimentos em fundos do RPPS, conforme
#'        gerado pela funcao \code{rppsDesempenho}
#' @param taxa data.frame contendo a serie historica da taxa livre de risco brasileira. Assume-se que
#'        o data.frame possui as seguintes colunas DATA e VALOR, nesta ordem.
#' @param nome.taxa nome do titulo a ser utilizado como taxa livre de risco.
#' @return um objeto do tipo data.frame contendo os retornos do portfolio das aplicacoes financeiras do RPPS, acrescidos
#'         da taxa livre de risco da economia
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' tsRetornos <- rppsDesempenho(dtCvm, dtDair)
#' tsRetornos <- rppsAcoplaTaxaLivreRisco(retornos = tsRetornos,
#'                                        taxa = dfLft2021,
#'                                        nome.taxa = "LFT 2021")
#' }
#' @seealso \code{RppsWrangler} e \code{RcvmWrangler}
#' @export
rppsAcoplaTaxaLivreRisco <- function(retornos, taxa, nome.taxa) {

  rtData <- retornos

  # Taxa livre de risco
  dtLivreRisco <- taxa[order(taxa$DATA),]
  dtLivreRisco <- dtLivreRisco[which(dtLivreRisco$DATA >= min(zoo::index(retornos))),]
  tsTlr <- xts::xts(x = dtLivreRisco$VALOR, order.by = dtLivreRisco$DATA)
  tsTlr <- zoo::na.spline(tsTlr)
  tsTlr <- xts::to.period(tsTlr, OHLC = F, period = 'months')

  retTlr <- PerformanceAnalytics::Return.calculate(tsTlr, method="discrete")

  rtData <- cbind(rtData, retTlr)
  names(rtData)[ncol(rtData)] <- nome.taxa

  rtData <- zoo::na.locf(rtData, maxgap = Inf)
  rtData <- zoo::na.locf(rtData, maxgap = Inf, fromLast = T)

  # zera o primeiro elemento da serie
  rtData[1,ncol(rtData)] <- 0
  attr(x = rtData, which = "col_taxa_livre_risco") <- ncol(rtData)

  rtData
}
