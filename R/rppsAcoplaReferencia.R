#' Acopla indice de referencia a serie temporal de retornos dos investimentos em cotas de fundos de um RPPS.
#'
#' Esta funcao acopla um indice de referencia a serie temporal de retornos da carteiras de investimentos
#' em cotas de fundos obtida a partir da funcao \code{rppsDesempenho}.
#'
#' Obs.: Mais de um indice de referencia podera ser acoplado a uma serie temporal de retornos. No entanto, so podera
#' ser incluido um indice a cada chamada desta funcao.
#'
#' @param retornos objeto xts contendo os retornos mensais da carteira de investimentos em fundos do RPPS, conforme
#'        gerado pela funcao \code{rppsDesempenho}
#' @param referencia data.frame contendo a serie historica da variacao do indice de referencia. Assume-se que
#'        o data.frame possui as seguintes colunas: ANO, MES e VALOR. Faz-se necessario que a ordem das colunas
#'        seja obedecida.
#' @param nome.ref nome do indice de referencia.
#' @param taxa taxa a ser acrescentada ao indice de referencia.
#' @param periodo.taxa periodo a ser considerado para a taxa. As opcoes disponiveis sao: "ANO" e "MES". Caso o periodo
#'        da taxa nao coincida com o periodo da serie historica do indice, sera calculada taxa equivalente a ser
#'        adicionada a mesma.
#' @return um objeto do tipo data.frame contendo os retornos do portfolio das aplicacoes financeiras do RPPS.
#' @author Bruno M. S. S. Melo
#' @examples
#' \dontrun{
#' tsRetornos <- rppsDesempenho(dtCvm, dtDair)
#' tsRetornos <- rppsAcoplaReferencia(retornos = tsRetornos,
#'                                    referencia = dfIpca,
#'                                    nome.ref = "IPCA + 6% a.a.",
#'                                    taxa = 6,
#'                                    periodo.taxa = "ANO")  # IPCA + 6% a.a.
#' }
#' @seealso \code{RppsWrangler} e \code{RcvmWrangler}
#' @export
rppsAcoplaReferencia <- function(retornos, referencia, nome.ref, taxa, periodo.taxa) {

  names(referencia) <- c("ANO", "MES", "VALOR")

  # funcao que recebe uma linha da serie historica do indice de referencia bem como a coluna que
  # indexa a serie temporal relativa aos retornos da carteira do RPPS e retorna a posicao, na serie
  # temporal de retornos, onde devera ser "encaixada" a linha do indice de referencia.
  melhorEncaixe <- function(linhaIndice, tsDatas) {

    id <- which(lubridate::year(tsDatas)==linhaIndice[1] &
                            lubridate::month(tsDatas)==linhaIndice[2])[1]

    if (length(id)==1) {
      return(id)
    } else {
      return(NA)
    }
  }

  # Calculo da taxa equivalente
  taxa <- taxa/100
  if (periodo.taxa == "ANO") {
    taxa <- (1+taxa)^(1/12)-1
  }

  posEncaixe <- apply(X = referencia, MARGIN = 1, FUN = melhorEncaixe, zoo::index(retornos))
  rtData <- cbind(retornos, rep(NA, nrow(retornos)))
  rtData[posEncaixe[which(!is.na(posEncaixe))],ncol(rtData)] <-
    referencia[which(!is.na(posEncaixe)),]$VALOR/100 + taxa
  colnames(rtData) <- c(unlist(dimnames(rtData))[1:(ncol(rtData)-1)], nome.ref)

  # zera o primeiro elemento da serie
  rtData[1,ncol(rtData)] <- 0

  attr(x = rtData, which = "col_indices_ref") <- c(attr(x = rtData, which = "col_indices_ref"),ncol(rtData))

  rtData
}
