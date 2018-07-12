package de.scameronde.chat;

import static de.scameronde.chat.JsonUtils.dataToJson;
import static de.scameronde.chat.JsonUtils.jsonToData;
import static spark.Spark.delete;
import static spark.Spark.get;
import static spark.Spark.options;
import static spark.Spark.post;
import static spark.Spark.webSocket;

import java.util.List;
import java.util.Optional;

import de.scameronde.chat.businesstypes.ChatRoom;
import de.scameronde.chat.businesstypes.MessageLog;
import de.scameronde.chat.businesstypes.Participant;

import javaslang.control.Either;
import spark.Request;
import spark.Response;

public class Server {
  static Repository repository = new InMemoryRepository();

  public static void main(String[] args) {
    // register web socket endpoint for all running chats
    ChatHandler.setRepository(repository);
    webSocket("/chat", ChatHandler.class);

    // this is only necessary for cross site development
    options("/*", ((request, response) -> allowCrossOriginRequests(response)));

    // get all known chat rooms
    get("/chatRoom", ((request, response) -> {
      List<ChatRoom> chatRooms = repository.getChatRooms();
      Either<Exception, String> jsonChatRooms = dataToJson(chatRooms);
      return createResponse(response, jsonChatRooms);
    }));

    // get chat history for a chat room
    get("/chatRoom/:chatRoomId", ((request, response) -> {
      String id = getParameter(request, ":chatRoomId");
      Optional<ChatRoom> chatRoom = findChatRoom(id);
      Optional<MessageLog> messageLog = chatRoom.map(cr -> repository.getMessageLog(cr));
      Either<Exception, Optional<String>> jsonMessageLog = dataToJson(messageLog);
      return createOptionalResponse(response, jsonMessageLog);
    }));

    // delete chat room including history
    delete("/chatRoom/:chatRoomId", ((request, response) -> {
      String id = getParameter(request, ":chatRoomId");
      Optional<ChatRoom> chatRoom = findChatRoom(id);
      chatRoom.ifPresent(cr -> repository.deleteChatRoom(cr));
      Either<Exception, Optional<String>> result = Either.right(chatRoom.map(x -> ""));
      return createOptionalResponse(response, result);
    }));

    // make a participant known to the system and return its id back to the caller
    post("/participant", ((request, response) -> {
      Either<Exception, Participant> participant = jsonToData(request.body(), Participant.class);
      if (participant.isRight()) {
        String id = repository.addParticipant(participant.get());
        Either<Exception, String> jsonId = participant.map(p -> '"' + id + '"');
        return createResponse(response, jsonId);
      }
      else {
        return createResponse(response, participant.map(p -> ""));
      }
    }));

    // gets the Id of an existing participant. A poor mans login.
    get("/participant/:participantName", ((request, response) -> {
      String participantName = getParameter(request, ":participantName");
      Optional<Participant> knownParticipant = repository.login(participantName);
      Either<Exception, Optional<String>> result = dataToJson(knownParticipant);
      return createOptionalResponse(response, result);
    }));

    // make a chat room known to the system and return its id back to the caller
    post("/chatRoom", ((request, response) -> {
      Either<Exception, ChatRoom> chatRoom = jsonToData(request.body(), ChatRoom.class);
      if (chatRoom.isRight()) {
        String id = repository.addChatRoom(chatRoom.get());
        Either<Exception, String> jsonId = chatRoom.map(c -> '"' + id + '"');
        return createResponse(response, jsonId);
      }
      else {
        return createResponse(response, chatRoom.map(c -> ""));
      }
    }));
  }

  private static Optional<ChatRoom> findChatRoom(String id) {
    return repository.getChatRooms()
                     .stream()
                     .filter(room -> room.getId().equals(id))
                     .findFirst();
  }

  private static Object allowCrossOriginRequests(Response response) {
    response.header("Access-Control-Allow-Origin", "*");
    response.header("Access-Control-Allow-Methods", "POST, GET, PUT, DELETE, OPTIONS");
    response.header("Access-Control-Allow-Credentials", "false");
    response.header("Access-Control-Max-Age", "86400"); // 24 hours
    response.header("Access-Control-Allow-Headers", "X-Requested-With, X-HTTP-Method-Override, Content-Type, Accept");

    response.status(204);
    return "";
  }

  private static String createResponse(Response response, Either<Exception, String> payload) {
    if (payload.isRight()) {
      String payloadData = payload.get();
      setResponseOK(response);
      return payloadData;
    }
    else {
      setResponseError(response);
      return payload.left().get().getMessage();
    }
  }

  private static String createOptionalResponse(Response response,
                                               Either<Exception, Optional<String>> payload) {
    if (payload.isRight()) {
      Optional<String> payloadData = payload.get();
      if (payloadData.isPresent()) {
        setResponseOK(response);
        return payloadData.get();
      }
      else {
        setResponseNotFound(response);
        return "";
      }
    }
    else {
      setResponseError(response);
      return payload.left().get().getMessage();
    }
  }

  private static void setResponseError(Response response) {
    response.status(500);
    response.type("application/text");
    response.header("Access-Control-Allow-Origin", "*");
  }

  private static void setResponseOK(Response response) {
    response.status(200);
    response.type("application/json");
    response.header("Access-Control-Allow-Origin", "*");
  }

  private static void setResponseNotFound(Response response) {
    response.status(404);
    response.type("application/text");
    response.header("Access-Control-Allow-Origin", "*");
  }


  static String getParameter(Request request, String parameter) {
    return request.params(parameter);
  }

}
