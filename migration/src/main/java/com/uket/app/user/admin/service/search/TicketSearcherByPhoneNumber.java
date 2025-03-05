package com.uket.app.user.admin.service.search;

import com.uket.app.user.admin.dto.request.SearchRequest;
import com.uket.app.user.admin.enums.TicketSearchType;
import com.uket.domain.ticket.dto.AdminCheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.repository.TicketRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


@Service
public class TicketSearcherByPhoneNumber extends TicketSearcher {

    public TicketSearcherByPhoneNumber(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.PHONE_NUMBER;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<AdminCheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        Page<Ticket> tickets = ticketRepository.findByPhoneNumberEndingWith(searchRequest.phoneNumberLastFourDigits(), pageable);
        return tickets.map(AdminCheckTicketDto::from);
    }
}
