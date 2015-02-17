\name{make_row_cluster-Heatmap-method}
\alias{make_row_cluster,Heatmap-method}
\alias{make_row_cluster}
\title{
Make cluster and adjust order in each component  


}
\description{
Make cluster and adjust order in each component  


}
\usage{
\S4method{make_row_cluster}{Heatmap}(object, order = "hclust", km = object@matrix_param$km, split = object@matrix_param$split,
    distance = object@row_hclust_param$distance,
    method = object@row_hclust_param$method)
}
\arguments{

}
\details{
Following components may be adjusted: - row title - row cluster - order of column annotation - column cluster  


}
