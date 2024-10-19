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
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
public class TicketSearcherByModifiedAt extends TicketSearcher{

    public TicketSearcherByModifiedAt(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.MODIFIED_AT;
    }

    @Override
    public Page<CheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        LocalDate modifiedAtLocal = searchRequest.modifiedAt();
        if(modifiedAtLocal == null){
            throw new IllegalStateException("createdAt이 null일 수 없습니다.");
        }
        LocalDateTime modifyStart = modifiedAtLocal.atStartOfDay();
        LocalDateTime modifyEnd = modifiedAtLocal.atTime(LocalTime.MAX);
        Timestamp modifyStartTimestamp = Timestamp.valueOf(modifyStart);
        Timestamp modifyEndTimestamp = Timestamp.valueOf(modifyEnd);

        Page<Ticket> tickets = ticketRepository.findByModifiedAtBetween(modifyStartTimestamp, modifyEndTimestamp,pageable);
        return tickets.map(CheckTicketDto::from);
    }
}
