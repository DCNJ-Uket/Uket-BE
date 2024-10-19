package com.uket.app.admin.api.repository;

import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.repository.TicketRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface TicketAdminRepository extends TicketRepository{

    Page<Ticket> findByStatusOrderByModifiedAtDesc(TicketStatus status, Pageable pageable);
}
