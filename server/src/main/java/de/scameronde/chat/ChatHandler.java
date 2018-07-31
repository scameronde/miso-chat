package de.scameronde.chat;

import static de.scameronde.chat.JsonUtils.jsonToData;
import static de.scameronde.chat.JsonUtils.dataToJson;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

import de.scameronde.chat.businesstypes.ChatCommand;
import de.scameronde.chat.businesstypes.ChatRoom;
import de.scameronde.chat.businesstypes.ChatMessage;
import de.scameronde.chat.businesstypes.Participant;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketClose;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketConnect;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketMessage;
import org.eclipse.jetty.websocket.api.annotations.WebSocket;

import javaslang.control.Either;

@WebSocket
public class ChatHandler {
  private static Repository repository;
  HashMap<Session, SessionInfo> sessions = new HashMap<>();

  @OnWebSocketConnect
  public void connected(Session session) {
    System.out.println("WebSocketConnection connected");
    sessions.put(session, new SessionInfo());
  }

  @OnWebSocketClose
  public void closed(Session session, int statusCode, String reason) {
    System.out.println("WebSocketConnection closed");
    sessions.remove(session);
  }

  @OnWebSocketMessage
  public void message(Session session, String message) throws IOException {
    Either<Exception, ChatCommand> decodedJson = jsonToData(message, ChatCommand.class);
    if (decodedJson.isRight()) {
      ChatCommand chatCommand = decodedJson.get();
      if (chatCommand.getCommand().equals("register")) {
        System.out.println("Registering for ChatRoom");
        System.out.println("  Participant: " + chatCommand.getRegistration().getParticipant());
        System.out.println("  ChatRoom   : " + chatCommand.getRegistration().getChatRoom());
        sessions.get(session).participant = chatCommand.getRegistration().getParticipant();
        sessions.get(session).chatRoom = chatCommand.getRegistration().getChatRoom();
      }
      if (chatCommand.getCommand().equals("message")) {
        System.out.println("Received Message");
        System.out.println("  ChatMessage: " + chatCommand.getChatMessage().getMessage());
        ChatRoom chatRoom = sessions.get(session).chatRoom;
        Participant participant = sessions.get(session).participant;

        String newMessage = participant.getName() + " > " + chatCommand.getChatMessage().getMessage() + "\n";
        ChatMessage newChatMessage = new ChatMessage(newMessage);
        String newJsonMessage = dataToJson(newChatMessage).get();
        repository.addChatMessage(chatRoom, newMessage, participant);
        List<Session> recipients = sessions.entrySet()
                                           .stream()
                                           .filter(entry -> entry.getValue().chatRoom != null && entry.getValue().chatRoom.equals(chatRoom))
                                           .map(entry -> entry.getKey())
                                           .collect(Collectors.toList());

        for (Session otherSession : recipients) {
          System.out.println("Sending message back");
          otherSession.getRemote().sendString(newJsonMessage); // and send it back
        }
      }
    }
  }


  private class SessionInfo {
    public Participant participant;
    public ChatRoom chatRoom;
  }

  public static void setRepository(Repository repository) {
    ChatHandler.repository = repository;
  }
}
