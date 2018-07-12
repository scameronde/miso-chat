package de.scameronde.chat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import de.scameronde.chat.businesstypes.ChatRoom;
import de.scameronde.chat.businesstypes.MessageLog;
import de.scameronde.chat.businesstypes.Participant;

public class InMemoryRepository implements Repository {
  static Integer idcounter = 100;

  List<Participant> participants = new ArrayList<>();
  List<ChatRoom> chatRooms = new ArrayList<>();
  Map<ChatRoom, String> logs = new HashMap<>();

  public InMemoryRepository() {
    ChatRoom chatRoom1 = new ChatRoom("1", "Room 1");
    ChatRoom chatRoom2 = new ChatRoom("2", "Room 2");
    chatRooms.add(chatRoom1);
    chatRooms.add(chatRoom2);
    logs.put(chatRoom1, "");
    logs.put(chatRoom2, "");
    participants.add(new Participant("", "Homer"));
    participants.add(new Participant("", "Marge"));
    participants.add(new Participant("", "Maggie"));
    participants.add(new Participant("", "Bart"));
    participants.add(new Participant("", "Lisa"));
    participants.add(new Participant("", "Burns"));
    participants.add(new Participant("", "Smithers"));
    participants.add(new Participant("", "Ned"));
    participants.add(new Participant("", "Rod"));
    participants.add(new Participant("", "Todd"));
    participants.add(new Participant("", "Leny"));
    participants.add(new Participant("", "Carl"));
  }

  @Override
  public String addParticipant(Participant participant) {
    String id = String.valueOf(idcounter++);
    participant.setId(id);
    participants.add(participant);
    return id;
  }

  @Override
  public Optional<Participant> login(String participantName) {
    return participants.stream()
                       .filter(p -> p.getName().equals(participantName))
                       .findFirst();
  }

  @Override
  public List<ChatRoom> getChatRooms() {
    throttle(2);
    return chatRooms;
  }

  @Override
  public String addChatRoom(ChatRoom chatRoom) {
    String id = String.valueOf(idcounter++);
    chatRoom.setId(id);
    chatRooms.add(chatRoom);
    logs.put(chatRoom, "");
    return id;
  }

  @Override
  public void deleteChatRoom(ChatRoom chatRoom) {
    chatRooms.remove(chatRoom);
    logs.remove(chatRoom);
  }

  @Override
  public void addMessage(ChatRoom chatRoom, String message, Participant participant) {
    logs.merge(chatRoom, message, String::concat);
  }

  @Override
  public MessageLog getMessageLog(ChatRoom chatRoom) {
    return new MessageLog(logs.get(chatRoom));
  }

  private void throttle(long millis) {
    try {
      Thread.sleep(millis);
    }
    catch (InterruptedException e) {
    }
  }
}
