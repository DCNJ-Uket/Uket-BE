package com.uket.app.admin.api.service;

import static org.junit.jupiter.api.Assertions.*;

import com.uket.domain.ticket.repository.TicketRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class EnterShowServiceTest {

    @InjectMocks
    EnterShowService enterShowService;

    @Mock
    TicketRepository ticketRepository;

    @Test
    void 입금이_완료되지_않은_티켓은_예외처리_한다() {


    }
}
