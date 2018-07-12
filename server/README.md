# Chat Server

A Java Spark based chat server.
The server is using a combined REST and WebSocket API.

## Startup

Install maven (>= v. 3.5.0)

On a command line execute

```shell
mvn compile exec:java
```

## API

### JSON data types

#### Participant

```json
{
  "id" : "101",
  "name" : "Marge"
}
```

#### MessageLog

```json
{
  "messageLog" : ""
}
```

#### Message

#### Chat room

```json
{
  "id" : "1",
  "title" : "Room 1"
}
```

### REST endpoints

The server will listen on port 4567.

```URI
/chatRoom
```

`GET` -> get a list of all chat rooms
`POST` -> add a chat room 

```URI
/chatRoom/<roomId>
```

`GET` -> get the chat history for room <roomId>
`DELETE` -> delete room <roomId>

```URI
/participant/<participantName>
```

`GET´ -> get the id of <participantName>


