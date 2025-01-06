package com.uket.app.admin.api.service.search;

import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.repository.TicketRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

@RequiredArgsConstructor
public abstract class TicketSearcher implements SearchTicket {

    protected final TicketRepository ticketRepository;

    public abstract Boolean isSupport(TicketSearchType searchType);
}

interface SearchTicket {
    Page<CheckTicketDto> search(SearchRequest searchRequest, Pageable pageable);
}
