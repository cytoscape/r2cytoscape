#' Create an igraph network from a Cytoscape network
#'
#' @description Takes a Cytoscape network and generates data frames for vertices and edges to
#' send to the graph_from_data_frame function.
#' Returns the network.suid and applies the perferred layout set in Cytoscape preferences.
#' @details Nodes and edges from the Cytoscape network will be translated into vertices and edges
#' in igraph. Associated table columns will also be passed to igraph as vertiex and edge attributes.
#' @param network name or suid of the network; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return (igraph) an igraph network
#' @export
#' @import igraph
#' @examples
#' \donttest{
#' createNetworkFromIgraph(g)
#' }
#' @seealso createNetwork, createIgraphFromNetwork

createIgraphFromNetwork <- function(network='current',base.url='http://localhost:1234/v1') {

    if(class(network)=='character') # if name...
        network = getNetworkSuid(network.name=network,base.url=base.url) # then get SUID

    #get dataframes
    cyedges <- getTableColumns('edge',network = network,base.url = base.url)
    cynodes <- getTableColumns('node',network = network,base.url = base.url)

    #check for source and target columns
    if(!"source" %in% colnames(cyedges)||(!"target" %in% colnames(cyedges))){
        st=data.frame(do.call('rbind',strsplit(cyedges$name,"\ \\(.*\\)\ ")))
        colnames(st) <- c("source","target")
        cyedges <- cbind(st,cyedges)
    }

    #setup columns for igraph construction
    colnames(cyedges)[colnames(cyedges)=="source"]<-"from"
    colnames(cyedges)[colnames(cyedges)=="target"]<-"to"
    cyedges2=cbind(cyedges[c("from","to")], cyedges[ ,!(names(cyedges) %in% c("from","to"))])
    cynodes2=cbind(cynodes["name"], cynodes[ ,!(names(cynodes)=="name")])

    #ship
    graph_from_data_frame(cyedges2, directed=TRUE, vertices=cynodes2)
}
