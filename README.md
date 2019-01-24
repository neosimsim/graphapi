-	send
-	receive
-	edit

JSON objects accross the network.

Communication accross the network is done with a HTTP Rest API.

# Architecture

## graphserver - API Server

The server may run on a different device.

This server stores and reads objects from storage. Also the server shall
do data processing, graphsearch, calculation etc.

The following endoints shall be provided

### /elements

#### GET

#### POST

### /elements/{uuid}

#### GET

#### PUT

### /links/

#### GET

#### POST

### /links/{uuid}

#### GET

#### PUT

### /properties/{type}

#### GET

## graphsearch - Graph processing server

### file api
`graphsearch` communicates over files. The application watches for files to be written
to update the graph and listen for queries. It responses to queries by writing files.

The base directory (repository) for an application instance can be specified by the `-r` option.

	graphsearch -r /var/graphapi

The following files and directories are relevant:

### elements/

Files in this directory are expected to be JSON files. Each element-file
is treated as node in the graph.

`graphsearch` watches files to be

- created
- modified
- deleted

and update the graph accordingly.

### links/

Files in this directory are treated as edges in the graph. `graphsearch` watches files
to be

- created
- modified
- deleted

and update the graph accordingly.

### queries

To execute a query one creates a subdirectory withing the queries directory.
Once a subdirectory is created `graphsearch` locks the query by writing its
process id to the file *pid*. Therefore only one running instance of
`graphsearch` exectutes one particulat query, making `graphsearch` scalable.
After the lock file is written `graphsearch` waits for the file *query* to be
written, see query syntax. Once the query file has been written, `graphsearch`
executes the query and write the result to *result*.

### Example

An example of *graphapi* directory may look like this

	graphapi
	├── elements
	│   └── b03475a9-fd66-45b4-9fb8-1db02bd66b44
	├── links
	│   └── e24a97db-bdcc-4cb0-a713-ac42804852dc
	├── queries
	│   └── cb86ab3c-19b5-4c2b-ae58-5ea4603b9972
	│       ├── pid
	│       ├── query
	│       └── result
	└── schemas
		└── order


## Client

The client is a command line tool, that runs on a user's
computer. It sends and receives JSON object from and to the server.

The follwing subcommand shall be provided.

### elements

-	sends to the server from file or stdin
	-	all
	-	id
-	fetches from server to stdout
	-	all
	-	id

### links

### properties

The client is used by GUIs, like tcl/tk.

# Open Tasks
- Access restriction

## Tree View
Der Tree View zeigt eine Menge von Teil Graphen an. Die Graphen werden bestimmt durch eine Menge Wurzeln und eines Typs von
Verknupfungen, z.B. `isParentOf`.

Bei Zykeln wird die Traversierung abgebrochen.

