package com.uket.app.admin.api.service.search;

import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.repository.TicketRepository;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class TicketSearcherByCreatedAt extends TicketSearcher{

    public TicketSearcherByCreatedAt(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.CREATED_AT;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<CheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        LocalDate createdAtLocal = searchRequest.createdAt();
        if(createdAtLocal == null){
            throw new IllegalStateException("createdAt이 null일 수 없습니다.");
        }
        LocalDateTime createStart = createdAtLocal.atStartOfDay();
        LocalDateTime createEnd = createdAtLocal.atTime(LocalTime.MAX);
        Timestamp createStartTimestamp = Timestamp.valueOf(createStart);
        Timestamp createEndTimestamp = Timestamp.valueOf(createEnd);

        Page<Ticket> tickets = ticketRepository.findByCreatedAtBetween(createStartTimestamp, createEndTimestamp,pageable);
        return tickets.map(CheckTicketDto::from);
    }
}
