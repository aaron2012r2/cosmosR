# cosmosR
Basic test functions for interacting with a CosmosDB REST API using the DocumentDB protocols in R

### Cosmos DB Intro
More information can be found at [Microsoft's website] [https://docs.microsoft.com/en-us/azure/cosmos-db/introduction]
I wrote this specifically for a Cosmos DB using the Document DB protocol via the REST API.

### Getting Started
First, source the R file to load all functions.
Next, you'll need the key to a Cosmos DB. If using all my default parameters, follow the steps below to 'select all' from the database
```
cosmosAuth("KeyGoesHere")
listAllDocuments <- cosmosQuery()
readContent(listAllDocuments)
```
This provides a list named listAllDocuments which contains the full raw response of the POST to the REST API, and then reads the content portion as a character response into the console.

### Query Target
By default, this points to a URI, db, and collection I created for testing purposes within my personal Azure subscription. It contains 3 NHL game stats, plus 1 mock object to showcase the flexible document format of Cosmos DB

### Custom parameters
The parameters for each function can take in different URI's, collections, etc; this enables the functions to be re-pointed to a different Cosmos DB

