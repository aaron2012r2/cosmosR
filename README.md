# cosmosR
Basic functions in R for simple interactions with a CosmosDB REST API using Document-based storage.

### Cosmos DB Intro
More information can be found at [Microsoft's website](https://docs.microsoft.com/en-us/azure/cosmos-db/introduction).<br />
I wrote this specifically for a Cosmos DB using the Document DB protocol via the REST API.

### Getting Started
First, source the R file to load all functions.
Next, you'll need the key to a Cosmos DB. If using all my default parameters, follow the steps below to perform "SELECT * FROM c" from the database
```
cosmosAuth("KeyGoesHere", "uri", "dbName", "collName")
list.all.documents <- cosmosQuery(content.response = TRUE)
```
This provides a list named list.all.documents which contains the full contents of all documents retrieved, and only the documents. No metadata about the HTTP response is stored.

### Query Target
As of 10-Jul-2017 the cosmosQuery function accepts basic parameters to target any db and collection in a Cosmos DB to which you have access. No guarantees are made, however, since this feature is new.

### Custom parameters
Queries can be constructed and will always SELECT from "c," the full Cosmos DB, at this time. These are built using the constructQuery function.
