package de.scameronde.chat.businesstypes;

public class Message {
  String message;

  public Message() {

  }

  public Message(String message) {
    this.message = message;
  }


  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }
}
