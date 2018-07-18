package de.scameronde.chat.businesstypes;

public class Participant {
  String pid;
  String name;

  public Participant() {
    this.pid = "";
  }

  public Participant(String pid, String name) {
    this.pid = pid;
    this.name = name;
  }

  public String getPid() {
    return pid;
  }

  public void setPid(String pid) {
    this.pid = pid;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  @Override
  public String toString() {
    return name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }

    Participant that = (Participant) o;

    return pid.equals(that.pid);
  }

  @Override
  public int hashCode() {
    return pid.hashCode();
  }
}
