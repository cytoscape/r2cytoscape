library(r2cytoscape)
context("Tests performed on galfiltered.cys")

# Check functions to be used to skip or prep for tests
check_cytoscape <- function() {
    tryCatch({checkCytoscapeVersion()},
             error = function(e) {skip("Cytoscape is not running. Must launch Cytoscape in order to perform test.")}
    )
}
check_galfiltered<-function() {
    if(!listNetworks() == "galFiltered.sif")
        commandRun('session open file="sampleData/galFiltered.cys"')
}

## Skip all tests if Cytoscape is not running
check_cytoscape()

## Open galFiltered.cys if not already
check_galfiltered()

## Run tests
test_that("applyLayout",{
    expect_equal(length(applyLayout("circular")),0) #no response
    expect_equal(length(applyLayout("asdfgh")),1) #error response
    expect_error(applyLayout(),"is missing")
})

test_that("applyStyle",{
    expect_equal(length(applyStyle("default")),2) #success response
    expect_equal(length(applyStyle("asdfgh")),1) #failed response
    expect_error(applyStyle(),"is missing")
})

test_that("bundleEdges",{
    expect_equal(length(bundleEdges()),1) #success response
})

test_that("checkCytoscapeVersion", {
    expect_true(is.character(checkCytoscapeVersion()["apiVersion"]))
    expect_true(is.character(checkCytoscapeVersion()["cytoscapeVersion"]))
})

test_that("selectNodes, selectFirstNeighbors, clearSelection",{
    expect_equal(selectNodes("MCM1"),NULL)
    expect_equal(length(selectNodes("YIL070C")),1)
    expect_equal(length(selectFirstNeighbors()),2)
    expect_equal(clearSelection(),NULL)
    expect_equal(length(selectNodes("YIL070C")),1)
    expect_equal(length(selectNodes("MCM1","COMMON")),1)
    expect_equal(length(selectNodes("YIL070C",keep.selected = T)),1) #tested by next line...
    expect_equal(length(selectFirstNeighbors()),21) #tests keep.selected
})

test_that("getNetwork*",{
    expect_match(getNetworkName(),"galFiltered.sif")
    expect_equal(length(getNetworkSuid()),1)
    expect_match(getNetworkName(getNetworkSuid()),"galFiltered.sif")
    expect_equal(length(getNetworkViewId()),1)
})

test_that("get*Table*",{
    expect_warning(getNodeTable()) #due to missing data for "?" node
    #expect_equal(nrow(getNodeTable()),331) #warning
    #expect_equal(ncol(getNodeTable()),28)  #warning
    expect_equal(nrow(getNodeViewTable()),331)
    expect_equal(ncol(getNodeViewTable()),4)
    expect_equal(nrow(getTableColumns('node','COMMON')),331)
    expect_equal(nrow(getTableColumns('edge','interaction')),362)
})

test_that("list*", {
    expect_match(listNetworks(),"galFiltered.sif")
    expect_true(length(listStyles())>15)
    expect_equal(length(listTableColumns()),28)
    expect_true(length(listTableColumns('edge'))>5)
    expect_equal(length(listTableColumns('network')),6)
    expect_true(length(listVisualProperties())>100)
    expect_equal(length(listVisualProperties('NODE_FILL_COLOR')),4)
})
