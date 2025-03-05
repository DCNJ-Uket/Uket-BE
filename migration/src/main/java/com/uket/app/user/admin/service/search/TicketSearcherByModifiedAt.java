package com.uket.app.user.admin.service.search;

import com.uket.app.user.admin.dto.request.SearchRequest;
import com.uket.app.user.admin.enums.TicketSearchType;
import com.uket.domain.ticket.dto.AdminCheckTicketDto;
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
public class TicketSearcherByModifiedAt extends TicketSearcher {

    public TicketSearcherByModifiedAt(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.MODIFIED_AT;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<AdminCheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        LocalDate modifiedAtLocal = searchRequest.modifiedAt();
        if(modifiedAtLocal == null) {
            throw new IllegalStateException("createdAt이 null일 수 없습니다.");
        }
        LocalDateTime modifyStart = modifiedAtLocal.atStartOfDay();
        LocalDateTime modifyEnd = modifiedAtLocal.atTime(LocalTime.MAX);
        Timestamp modifyStartTimestamp = Timestamp.valueOf(modifyStart);
        Timestamp modifyEndTimestamp = Timestamp.valueOf(modifyEnd);

        Page<Ticket> tickets = ticketRepository.findByModifiedAtBetween(modifyStartTimestamp, modifyEndTimestamp,pageable);

        return tickets.map(AdminCheckTicketDto::from);
    }
}
