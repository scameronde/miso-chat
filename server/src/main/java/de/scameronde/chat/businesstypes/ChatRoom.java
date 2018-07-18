package de.scameronde.chat.businesstypes;

public class ChatRoom {
  String rid;
  String title;

  public ChatRoom() {
    this.rid = "";
  }

  public ChatRoom(String rid, String title) {
    this.rid = rid;
    this.title = title;
  }

  public String getTitle() {
    return title;
  }

  public void setTitle(String title) {
    this.title = title;
  }

  public String getRid() {
    return rid;
  }

  public void setRid(String rid) {
    this.rid = rid;
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

    return rid.equals(chatRoom.rid);
  }

  @Override
  public int hashCode() {
    return rid.hashCode();
  }
}
