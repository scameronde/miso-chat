package de.scameronde.chat.businesstypes;

public class ChatCommand {
  String command;
  ChatRegistration registration;
  ChatMessage message;

  public ChatCommand() {

  }

  public String getCommand() {
    return command;
  }

  public void setCommand(String command) {
    this.command = command;
  }

  public ChatRegistration getRegistration() {
    return registration;
  }

  public void setRegistration(ChatRegistration registration) {
    this.registration = registration;
  }

  public ChatMessage getChatMessage() {
    return message;
  }

  public void setChatMessage(ChatMessage message) {
    this.message = message;
  }
}
