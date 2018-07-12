package de.scameronde.chat.businesstypes;

public class ChatRoom {
  String id;
  String title;

  public ChatRoom() {
    this.id = "";
  }

  public ChatRoom(String id, String title) {
    this.id = id;
    this.title = title;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    ChatRoom chatRoom = (ChatRoom) o;

    return id.equals(chatRoom.id);
  }

  @Override
  public int hashCode() {
    return id.hashCode();
  }
}
