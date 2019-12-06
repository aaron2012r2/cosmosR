#' POST a Document Create to the REST API for Cosmos DB.
#' 
#' @param sql.doc List of key/values to add to document. Required.
#' @param sql.partitionkey_value The value of the partitionkey. Must also be part of sql.doc. Required. 
#' @param debug.auth Logical value for getting verbose output of auth header being constructed. Defaults to false.
#' @param debug.query Logical value for getting verbose output of HTTP response, printing all headers. Defaults to false.
#' @param content.response Logical value to determine whether to retrieve full response or just the documents
#' @return Prints status code of HTTP POST, and returns full HTTP response or just the content
#' @keywords create cosmosdb post
#' @export
#' @examples
#' cosmosCreate(sql.doc = list(id="uuid", code="code1"), sql.partitionkey_value = "code1")

cosmosCreate <- function(sql.doc = "", sql.partitionkey_value = "", debug.auth = FALSE, debug.query = FALSE, content.response = FALSE) {

    require(digest)
    require(base64enc)
    require(httr)
    require(jsonlite)
    require(stringr)

    # Use the current time to create a proper auth header
    current.time <- Sys.time()

    # Coerce current time to proper format
    ms.date.string <- tolower(format(current.time, "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"))

    # Create POST URI for posting query to collection
    post.uri <- paste(envCosmosDB$uri, "/dbs/", envCosmosDB$dbName, "/colls/", envCosmosDB$collName, "/docs", sep = "")

    # Create the resource link and type from the environment variables
    res.link <- paste("dbs/", envCosmosDB$dbName, "/colls/", envCosmosDB$collName, sep = "")
    res.type <- "docs"

    # JSON body as list
    json.body <- sql.doc

    # Set partition key (Note: must be the value, not the key!)
    partitionkey <- paste('["', sql.partitionkey_value, '"]', sep="")

    # Debug print
    if (debug.query == TRUE) {
        print(paste('sql.doc:', sql.doc, sep=" "))
        print(paste('partitionkey:', partitionkey, sep=" "))
    }
    
    # Generate auth header using specifications
    auth.header <- genHeader(verb = "POST", resource.type = res.type, resource.link = res.link, stored.time = ms.date.string, debug = debug.auth)
    
    # Geneerate POST request
    raw.response <- POST(post.uri, add_headers(.headers = c("Authorization" = auth.header, "x-ms-version" = "2017-02-22", "x-ms-date" = ms.date.string, "Content-Type" = "application/json", "x-ms-documentdb-partitionkey" = partitionkey, "x-ms-documentdb-isupsert" = "true", "Accept" = "application/json" )), body = json.body, encode = "json", verbose())

    # Send the status code of the POST to the console
    if (debug.query == TRUE) {
        print(paste("Status Code is", raw.response$status_code, sep = " "))
    }
    # Debug flag for viewing headers upon troubleshooting
    if (debug.query == TRUE) {
        print("*** Headers of Response ***")
        print(raw.response$headers)
        print(readBin(raw.response$content, "character"))
    }

    # Check content response flag; act accordingly
    if (content.response == FALSE) {
        raw.response
    } else if (content.response == TRUE) {
        char.response <- readContent(raw.response)
        char.response$Documents
    } else {
        print("Invalid content response option specified. Logical value required.")
    }

}
