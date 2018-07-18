package de.scameronde.chat.businesstypes;

public class ChatMessageLog {
  String chatMessageLog;

  public ChatMessageLog() {

  }

  public ChatMessageLog(String messageLog) {
    this.chatMessageLog = messageLog;
  }

  public String getChatMessageLog() {
    return chatMessageLog;
  }

  public void setChatMessageLog(String messageLog) {
    this.chatMessageLog = messageLog;
  }
}
