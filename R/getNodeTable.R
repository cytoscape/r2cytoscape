#' @title Get node table
#' @description Retrieve the entire node table as a matrix object
#'
#' @details If there are missing values in the Node Table, then a warning message about the rbind function
#' and the "number of columns of result is not a multiple of vector length". This indicates that values were
#' shifted (and repeated) to fill in missing columns. This could result in corrupted data for certain nodes.
#' In these cases, consider using getTableColumns instead.
#' @param network name or suid of the network that you want the node table; default is "current" network
#' @param base.url cyrest base url for communicating with cytoscape
#' @return matrix node table
#' @export
#' @import RJSONIO
#' @import httr
#' @seealso getTableColumns

getNodeTable <- function(network='current',base.url='http://localhost:1234/v1'){

    if(class(network)=='character') # if name...
        network = getNetworkSuid(network.name=network,base.url=base.url) # then get SUID

    get.node.column.url <- paste(base.url,"networks",network,"tables/defaultnode/rows/",sep="/")

    response <- GET(url=get.node.column.url)

    json_file <- fromJSON(rawToChar(response$content))
    json_file <- lapply(json_file, function(x) {
        x[sapply(x, is.null)] <- NA
        if(length(x) > 1) { x } else{ unlist(x) }
    })

    nodetable <- do.call("rbind", json_file)
    return(nodetable)
}
