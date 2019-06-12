package de.scameronde.chat;

import de.scameronde.chat.businesstypes.ChatCommand;
import de.scameronde.chat.businesstypes.ChatMessage;

import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;

@CrossOrigin
@Controller
public class WSController {

  public WSController() {
    super();
  }

  @MessageMapping("/command")
  @SendTo("/topic/chat")
  public ChatMessage receiveChatCommand(ChatCommand chatCommand) {
    System.out.println("Command received");
    return new ChatMessage("Hello World!");
  }

}
