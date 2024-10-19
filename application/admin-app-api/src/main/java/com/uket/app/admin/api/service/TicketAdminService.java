package com.uket.app.admin.api.service;

import com.uket.app.admin.api.repository.TicketAdminRepository;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TicketAdminService {

    private final TicketAdminRepository ticketAdminRepository;

    public Page<LiveEnterUserDto> searchLiveEnterUsers(Pageable pageable){

        Page<Ticket> tickets = ticketAdminRepository.findByStatusOrderByModifiedAtDesc(TicketStatus.FINISH_ENTER, pageable);
        return tickets.map(LiveEnterUserDto::from);
    }
}
