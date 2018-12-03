-	send
-	receive
-	edit

JSON objects accross the network.

Communication accross the network is done with a HTTP Rest API.

# Architecture

## Server

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

