% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_related_functions.R
\name{get_cluster_elem}
\alias{get_cluster_elem}
\title{Get indices of individuals that are part of a cluster}
\usage{
get_cluster_elem(mytree, i)
}
\arguments{
\item{mytree}{the hierarchical classification tree}

\item{i}{the ID of the cluster}
}
\value{
a vector of indices
}
\description{
Get indices of individuals that are part of a cluster
}
\examples{
hc <- hclust(dist(USArrests), "ave")
get_cluster_elem(hc, 30)
}
