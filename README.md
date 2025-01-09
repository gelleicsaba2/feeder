# Feeder
Lazarus project \
Written in: Free pascal \
Feeder is a content share service. Public, Not encrypted & websocket communication \
You can get content from server with websocket, and also you can manage contents with manager program.

## Projects
There are 3 projects:

1. Server (src/chatServer.lpi)
2. Manager client (src/management.lpi)
3. Client test program (src/chatClient.lpi)

## Settings
There are 3 settings (.ini) file in the bin folder. \
You can set the host the client port, the manager client port and the database path.

## Content
You can get the contents through websocket by giving a Guid.

## Manager program operations
1. upload text content
2. list all guids
3. show content
4. show last uploaded GUID
5. remove content
6. export content to file

(To show options, run ./FeederMngmt without parameters)

## Examples
```
(Run the ./FeederServe in another terminal)
./FeederMngmt upload content.txt

./FeederMngmt list

./FeederMngmt show (content guid)
```

# For the security
There isn't any authentication method to connect to the server. \
In the firewall settings you can set the manager client port (outgoing/ip addresses, etc..).

