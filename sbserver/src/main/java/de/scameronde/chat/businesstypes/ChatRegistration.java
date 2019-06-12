package de.scameronde.chat.businesstypes;

public class ChatRegistration {
  Participant participant;
  ChatRoom chatRoom;

  public ChatRegistration() {

  }

  public ChatRegistration(Participant participant, ChatRoom chatRoom) {
    this.participant = participant;
    this.chatRoom = chatRoom;
  }

  public Participant getParticipant() {
    return participant;
  }

  public void setParticipant(Participant participant) {
    this.participant = participant;
  }

  public ChatRoom getChatRoom() {
    return chatRoom;
  }

  public void setChatRoom(ChatRoom chatRoom) {
    this.chatRoom = chatRoom;
  }
}
