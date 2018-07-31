package de.scameronde.chat;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import de.scameronde.chat.businesstypes.ChatRoom;
import de.scameronde.chat.businesstypes.ChatMessageLog;
import de.scameronde.chat.businesstypes.Participant;

public class InMemoryRepository implements Repository {
  static Integer idcounter = 100;

  List<Participant> participants = new ArrayList<>();
  List<ChatRoom> chatRooms = new ArrayList<>();
  Map<ChatRoom, String> logs = new HashMap<>();

  public InMemoryRepository() {
    ChatRoom chatRoom1 = new ChatRoom("1", "Room 1");
    ChatRoom chatRoom2 = new ChatRoom("2", "Room 2");
    addChatRoom(chatRoom1);
    addChatRoom(chatRoom2);
    logs.put(chatRoom1, "");
    logs.put(chatRoom2, "");
    addParticipant(new Participant("1", "Homer"));
    addParticipant(new Participant("2", "Marge"));
    addParticipant(new Participant("3", "Maggie"));
    addParticipant(new Participant("4", "Bart"));
    addParticipant(new Participant("5", "Lisa"));
    addParticipant(new Participant("6", "Burns"));
    addParticipant(new Participant("7", "Smithers"));
    addParticipant(new Participant("8", "Ned"));
    addParticipant(new Participant("9", "Rod"));
    addParticipant(new Participant("10", "Todd"));
    addParticipant(new Participant("11", "Leny"));
    addParticipant(new Participant("12", "Carl"));
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
  public void addChatMessage(ChatRoom chatRoom, String message, Participant participant) {
    logs.merge(chatRoom, message, String::concat);
  }

  @Override
  public ChatMessageLog getChatMessageLog(ChatRoom chatRoom) {
    return new ChatMessageLog(logs.get(chatRoom));
  }

  private void throttle(long millis) {
    try {
      Thread.sleep(millis);
    }
    catch (InterruptedException e) {
    }
  }
}
