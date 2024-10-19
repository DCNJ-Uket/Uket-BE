package com.uket.app.admin.api.service.search;

import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.repository.TicketRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
public class TicketSearcherByStatus extends TicketSearcher{

    public TicketSearcherByStatus(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.STATUS;
    }

    @Override
    public Page<CheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        Page<Ticket> tickets = ticketRepository.findByStatus(searchRequest.status(), pageable);
        return tickets.map(CheckTicketDto::from);
    }
}
