package de.scameronde.chat.businesstypes;

public class ChatMessage {
  String message;

  public ChatMessage() {

  }

  public ChatMessage(String message) {
    this.message = message;
  }


  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }
}
