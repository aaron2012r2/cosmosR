#################################
#                               #
# Cosmos DB querying functions  #
#                               #
#################################
#                               #
# Author: Aaron Schroeder       #
#                               #
#                               #
#################################


# Load the required packages

library(digest)
library(base64enc)
library(httr)
library(jsonlite)

# Establish the CosmosDB environment for key storage
envCosmosDB <- new.env(parent = emptyenv())

cosmosAuth <- function(key) {

    assign("keyCosmos", key, envir = envCosmosDB)

}


generate.header <- function(verb = "GET", resource.type = "docs", resource.link = "dbs/nhlstats-db/colls/nhlstats-coll", stored.time, master.key = envCosmosDB, debug = FALSE) {

    # Check if the key exists; if not, tell the user to authorize!
    if (length(envCosmosDB) == 0) {

        print("Please authorize using the cosmosAuth function")
        stop

    } else {

        # Set the break space character
        break.space <- "\n"

        # consistent casing of parameters for auth header
        verb <- tolower(verb)
        resource.type <- tolower(resource.type)
        resource.link <- tolower(resource.link)

        # DEPRECATED / *sometimes necessary* function to coerce date to proper format
        #stored.time <- tolower(format(stored.time, "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"))

        # Create the fully-formed string
        string.to.hash <- paste(verb, break.space, resource.type, break.space, resource.link, break.space, stored.time, break.space, "", break.space, sep = "")

        # createHmac using sha256 & master.key, base64 digest

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


cosmosQuery <- function(post.uri = "https://prototype-cdmc.documents.azure.com/dbs/nhlstats-db/colls/nhlstats-coll/docs", res.type = "docs", res.link = "dbs/nhlstats-db/colls/nhlstats-coll", debug.auth = FALSE, debug.query = FALSE) {

    # Use the current time to create a proper auth header
    current.time <- Sys.time()

    # Coerce current time to proper format
    ms.date.string <- tolower(format(current.time, "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"))

    # Generate auth header using specifications
    auth.header <- generate.header(verb = "POST", resource.type = res.type, resource.link = res.link, stored.time = ms.date.string, debug = debug.auth)
    raw.response <- POST(post.uri, add_headers(.headers = c("Authorization" = auth.header, "x-ms-version" = "2017-02-22", "x-ms-date" = ms.date.string, "Content-Type" = "application/query+json", "x-ms-documentdb-isquery" = "true", "x-ms-query-enable-crosspartition" = "true")), body = "{\"query\": \"SELECT * FROM c\", \"parameters\": [] }")

    # Send the status code of the POST to the console
    print(paste("Status Code is", raw.response$status_code, sep = " "))

    # Debug flag for viewing headers upon troubleshooting
    if (debug.query == TRUE) {

        print("*** Headers of Response ***")
        print(rcr$headers)

    }

    # Return the full response
    raw.response

}

readContent <- function(binary.stream) {

    # Parse binary response into character
    char.response <- readBin(binary.stream$content, "character")

    # Convert the character response to json
    json.response <- fromJSON(char.response)

    # Return the json
    json.response

}