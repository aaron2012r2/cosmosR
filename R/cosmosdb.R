#################################
#                               #
# Cosmos DB querying functions  #
#                               #
#################################
#                               #
#     Create simple SELECT      #
#    statements against any     #
#      CosmosDB REST API        #
#                               #
#################################


# Load the required packages

library(digest)
library(base64enc)
library(httr)
library(jsonlite)
library(stringr)

# Establish the CosmosDB environment for key storage
envCosmosDB <- new.env(parent = emptyenv())

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


genHeader <- function(verb, resource.type = "docs", resource.link, stored.time, master.key = envCosmosDB, debug = FALSE) {

    # Check if the key exists; if not, tell the user to authorize!
    if (length(envCosmosDB) == 0) {

        print("Please authorize using the cosmosAuth function")
        stop

    } else {

        # Set the break space character
        break.space <- "\n"

        # consistent casing of parameters for auth header
        verb <- tolower(verb)

        # Create the fully-formed string
        string.to.hash <- paste(verb, break.space, resource.type, break.space, resource.link, break.space, stored.time, break.space, "", break.space, sep = "")

        # The master key comes in base64 format; decode this first
        master.key <- base64decode(master.key$keyCosmos)

        # Necessary to UTF-8 encode the string being hashed; it may otherwise contain chars that break the operation
        body <- enc2utf8(string.to.hash)

        # Base64 encode the entire string, using the Master Key as the base, algorithm SHA256, using raw flag
        sig.string <- base64encode(hmac(master.key, body, algo = "sha256", raw = T))

        # Adding the required prefix to the auth header
        auth.header <- URLencode(paste("type=master&ver=1.0&sig=", sig.string, sep = ""), reserved = TRUE)

        # Debug flag for when things just don't quite work out
        if (debug == TRUE) {
            print(paste("Verb is", verb))
            print(paste("Resource type is", resource.type))
            print(paste("Resource link is", resource.link))
            print(paste("Stored time is", stored.time))
            print(paste("The full string being hashed is", string.to.hash))
            print(paste("Auth header is", auth.header))
        }

        auth.header
    }

}

constructQuery <- function(sql.what, sql.where) {

    # Create the query using predicate if it exists
    if (sql.where == "") {
        full.query <- paste("SELECT", sql.what, "FROM c", sep = " ")
    } else {
        full.query <- paste("SELECT ", sql.what, " FROM c WHERE (", sql.where, ")", sep = "")
    }

    # Return the query
    full.query
}

cosmosQuery <- function(sql.what = "*", sql.where = "", debug.auth = FALSE, debug.query = FALSE, content.response = FALSE) {

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
    full.query <- constructQuery(sql.what, sql.where)

    # Convert full query to JSON for HTTP POST
    json.query <- toJSON(list(query = full.query, parameters = list()))

    # First set of brackets break the operation; remove them
    json.query <- str_replace(json.query, fixed("["), "")
    json.query <- str_replace(json.query, fixed("]"), "")

    # Generate auth header using specifications
    auth.header <- genHeader(verb = "POST", resource.type = res.type, resource.link = res.link, stored.time = ms.date.string, debug = debug.auth)
    raw.response <- POST(post.uri, add_headers(.headers = c("Authorization" = auth.header, "x-ms-version" = "2017-02-22", "x-ms-date" = ms.date.string, "Content-Type" = "application/query+json", "x-ms-documentdb-isquery" = "true", "x-ms-documentdb-query-enablecrosspartition" = "true")), body = json.query)

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

readContent <- function(binary.stream) {

    # Parse binary response into character
    char.response <- readBin(binary.stream$content, "character")

    # Convert the character response to json
    json.response <- fromJSON(char.response)

    # Return the json
    json.response

}