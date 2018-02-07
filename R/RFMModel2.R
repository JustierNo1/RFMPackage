#' calculateRFM
#'
#' Description
#' Calclate the weighted RMF score: recency, frequency, and monetary.
#
# Arguments
#'@param data - A data.table containing the transaction record details for every customer.
#'@param re - Weight of recency
#'@param fr - Weight of frequency
#'@param mo - Weight of monetary
#'
#'@details
#'\code{data} contains the transactional data. The dataset must contain a column labeled "Customer" that allows unique customer identification and a column labeled "TransDate", indicating the purchase date. The column "PurchAmount" specifies the total spending per purchase.
#
#'Return Value
#'@return Returns a data.data containing the recency, frequency and monetary core as well as the weighted final score and the group membership.

calculaterfm <-function(x,re,fr,mo){
  max.Date <- max(transactions$TransDate)

  rfm <- x[, list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = sum(PurchAmount)),
    by="Customer" #this automatically adds customer ID
    ]

  rfm_scores <- rfm[,list(
    Customer,
    recency = as.numeric(cut2(-recency, g=3)),
    frequency = as.numeric(cut2(frequency, g=3)),
    monetary = as.numeric(cut2(monetary, g=3))
  )]

  rfm_scores[,overall:=mean(c(recency,frequency,monetary)), by=Customer]

  rfm_scores[,overall:= recency*re+frequency*fr+monetary*mo, by=Customer]

  rfm_scores$rounded <- round(rfm_scores$overall)

  rfm_scores[overall == max(overall),]
}
