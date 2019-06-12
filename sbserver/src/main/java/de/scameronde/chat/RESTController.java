package de.scameronde.chat;

import java.util.List;

import de.scameronde.chat.businesstypes.ChatMessageLog;
import de.scameronde.chat.businesstypes.ChatRoom;
import de.scameronde.chat.businesstypes.Participant;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@CrossOrigin
@RestController
public class RESTController {
  private final Repository repository;

  public RESTController(Repository repository) {
    this.repository = repository;
  }


  @GetMapping("/chatRoom")
  public List<ChatRoom> getChatRooms() {
    return repository.getChatRooms();
  }


  @GetMapping("/chatRoom/{chatRoomId}")
  public ChatMessageLog getChatRoomHistory(@PathVariable String chatRoomId) {
    ChatRoom foundRoom = repository.getChatRooms()
                                   .stream()
                                   .filter(room -> room.getId().equals(chatRoomId))
                                   .findFirst()
                                   .orElseThrow(IllegalArgumentException::new);
    return repository.getChatMessageLog(foundRoom);
  }


  @DeleteMapping("/chatRoom/{chatRoomId}")
  public void deleteChatRoom(@PathVariable String chatRoomId) {
    ChatRoom foundRoom = repository.getChatRooms()
                                   .stream()
                                   .filter(room -> room.getId().equals(chatRoomId))
                                   .findFirst()
                                   .orElseThrow(NotFoundException::new);
    repository.deleteChatRoom(foundRoom);
  }


  @PostMapping("/chatRoom")
  public void addChatRoom(@RequestBody ChatRoom chatRoom) {
    repository.addChatRoom(chatRoom);
  }


  @GetMapping("/participant/{participantName}")
  public Participant getParticipantId(@PathVariable String participantName) {
    return repository.login(participantName)
                     .orElseThrow(NotFoundException::new);
  }
}
