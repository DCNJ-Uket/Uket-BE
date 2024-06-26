package com.uket.app.ticket.api.service;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.ticket.service.TicketService;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.repository.UserRepository;
import com.uket.domain.user.service.UserService;
import jakarta.transaction.Transactional;
import java.util.UUID;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@Transactional
class TicketServiceTest {
    @Autowired
    TicketService ticketService;

    @Autowired
    TicketRepository ticketRepository;

    @Autowired
    UserRepository userRepository;
    @Autowired
    UserService userService;

    private Users testUser;

    @BeforeEach
    void setUp() {
        CreateUserDto createUserDto = CreateUserDto.builder()
            .name("test")
            .role(UserRole.ROLE_USER)
            .platform(Platform.KAKAO)
            .platformId("1234")
            .email("abc@naver.com")
            .build();

        testUser = userService.saveUser(createUserDto);
    }

    @Test
    void 티켓이_성공적으로_삭제된다() {
        Ticket ticket = ticketRepository.save(Ticket.builder()
            .user(testUser)
            .status(TicketStatus.BEFORE_ENTER)
            .ticketNo(UUID.randomUUID().toString())
            .build());

        ticketService.cancelTicketByUserIdAndId(testUser.getId(), ticket.getId());

        assertNull(ticketRepository.findByUserIdAndId(testUser.getId(), ticket.getId()));
    }

    @Test
    void 잘못된_티켓_Id를_입력할경우_예외처리를_진행한다() {
        Ticket ticket = ticketRepository.save(Ticket.builder()
            .user(testUser)
            .status(TicketStatus.BEFORE_ENTER)
            .ticketNo(UUID.randomUUID().toString())
            .build());

        Long wrongTicketNumber = ticket.getId()+1;

        assertThrows(TicketException.class, () -> {
            ticketService.cancelTicketByUserIdAndId(testUser.getId(), wrongTicketNumber);
        });
    }

}
