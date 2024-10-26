package com.uket.app.admin.api.service.search;

import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.repository.TicketRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


@Service
public class TicketSearcherByPhoneNumber extends TicketSearcher{

    public TicketSearcherByPhoneNumber(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.PHONE_NUMBER;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<CheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        return ticketRepository.findByUserUserDetailsPhoneNumber(searchRequest.phoneNumber(), pageable);
    }
}
