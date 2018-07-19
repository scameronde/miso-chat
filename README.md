# Chat - a small showcase for a web client written in Haskell using Miso

This project contains a simple chat server, written in Java, and a chat client written in Haskell and compiled to JavaScript.

The server offers a REST- and a WebSocket-API. The REST-API is used for login, room discovery, creation and deletion. The WebSocket-API is used for passing the chat messages.

The client is a single page web application. As said, it is completely written in Haskell using the fantastic Miso framework (https://haskell-miso.org), but runs in the browser like an Angular or ReactJS application.

Please consult the readme files in the server and client folder for further information.

