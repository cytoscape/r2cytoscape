% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectNodes.R
\name{selectNodes}
\alias{selectNodes}
\title{Select nodes}
\usage{
selectNodes(nodes, by.col = "name", keep.selected = FALSE,
  network = "current", base.url = "http://localhost:1234/v1")
}
\arguments{
\item{nodes}{a list of nodes that you want to select}

\item{by.col}{column by which to perform selection (e.g., SUID, name); default is "name"}

\item{keep.selected}{(boolean) whether to add to existing selection (TRUE) or clear prior selection (FALSE); default is FALSE}

\item{network}{name or suid of the network; default is "current" network}

\item{base.url}{cyrest base url for communicating with cytoscape}
}
\value{
list of SUIDs for nodes selected, excluding those that did not match
provided list and those previously selected (even if keep.selected=TRUE)
}
\description{
Select nodes based on name, SUID or any other column.
}
\examples{
\donttest{
example(createNetwork)

selectNodes(c('node 0','node 1'))
selectNodes('B', by.col='group')
}
}
