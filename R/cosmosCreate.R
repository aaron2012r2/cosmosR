#' POST a full query to the REST API for Cosmos DB.
#' 
#' @param sql.what String for specifying what fields to retrieve. Typically called select condition. Defaults to *
#' @param sql.where String for specifying what filter to use on data. Typically called search condition. Defaults to empty.
#' @param debug.auth Logical value for getting verbose output of auth header being constructed. Defaults to false.
#' @param debug.query Logical value for getting verbose output of HTTP response, printing all headers. Defaults to false.
#' @param content.response Logical value to determine whether to retrieve full response or just the documents
#' @return Prints status code of HTTP POST, and returns full HTTP response or just the content
#' @keywords query cosmosdb post
#' @export
#' @examples
#' cosmosQuery(sql.what = "c.contact.eloquaId", sql.where = "c.contact.eloquaId != null")

cosmosQuery <- function(sql.json = "", max.items = 100, debug.auth = FALSE, debug.query = FALSE, content.response = FALSE) {

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

    # Create full query with function
    # full.query <- constructQuery(sql.what, sql.where)

    # Convert full query to JSON for HTTP POST
    # json.query <- toJSON(list(query = full.query, parameters = list()))
    json.query <- sql.json

    # First set of brackets break the operation; remove them
    json.query <- str_replace(json.query, fixed("["), "")
    json.query <- str_replace(json.query, fixed("]"), "")

    # Generate auth header using specifications
    auth.header <- genHeader(verb = "POST", resource.type = res.type, resource.link = res.link, stored.time = ms.date.string, debug = debug.auth)
    raw.response <- POST(post.uri, add_headers(.headers = c("Authorization" = auth.header, "x-ms-version" = "2017-02-22", "x-ms-date" = ms.date.string, "Content-Type" = "application/json", "x-ms-documentdb-isupsert" = "true")), body = json.query)

    # Send the status code of the POST to the console
    print(paste("Status Code is", raw.response$status_code, sep = " "))

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
