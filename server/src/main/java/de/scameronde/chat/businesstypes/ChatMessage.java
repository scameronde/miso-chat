package de.scameronde.chat.businesstypes;

public class ChatMessage {
  String chatMessage;

  public ChatMessage() {

  }

  public ChatMessage(String message) {
    this.chatMessage = message;
  }


  public String getChatMessage() {
    return chatMessage;
  }

  public void setChatMessage(String message) {
    this.chatMessage = message;
  }
}
