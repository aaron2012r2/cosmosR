#' Set up initial parameters for Cosmos DB querying
#'
#' @param key Access key, primary or secondary, for interacting with Cosmos DB collection
#' @param uri HTTPS URI to the Cosmos DB
#' @param dbName String name of CosmosDB Database target
#' @param collName String name of CosmosDB Collection target
#' @return Environment variable is created for usage in all other functions
#' @keywords environment cosmos key uri
#' @export
#' @examples
#' cosmosAuth("KeY123","https://yourcosmosdb.documents.azure.com","SampleDB","SampleCollection")


cosmosAuth <- function(key, uri, dbName, collName) {

    # Access key assignment
    assign("keyCosmos", key, envir = envCosmosDB)

    # URI for the CosmosDB
    assign("uri", uri, envir = envCosmosDB)

    # Name of the target db
    assign("dbName", dbName, envir = envCosmosDB)

    # Name of the target collection
    assign("collName", collName, envir = envCosmosDB)

}

envCosmosDB <- new.env(parent = emptyenv())
