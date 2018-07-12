package de.scameronde.chat.businesstypes;

public class ChatCommand {
  String command;
  ChatRegistration registration;
  Message message;

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

  public Message getMessage() {
    return message;
  }

  public void setMessage(Message message) {
    this.message = message;
  }
}
