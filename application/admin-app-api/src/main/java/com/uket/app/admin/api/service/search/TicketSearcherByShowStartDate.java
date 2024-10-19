package com.uket.app.admin.api.service.search;

import com.uket.app.admin.api.dto.request.SearchRequest;
import com.uket.app.admin.api.enums.TicketSearchType;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.repository.TicketRepository;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
public class TicketSearcherByShowStartDate extends TicketSearcher{

    public TicketSearcherByShowStartDate(TicketRepository ticketRepository) {
        super(ticketRepository);
    }

    @Override
    public Boolean isSupport(TicketSearchType searchType) {
        return searchType == TicketSearchType.SHOW_DATE;
    }

    @Override
    public Page<CheckTicketDto> search(SearchRequest searchRequest, Pageable pageable) {
        LocalDate showDateLocal = searchRequest.showDate();
        if(showDateLocal == null){
            throw new IllegalStateException("showDate가 null일 수 없습니다.");
        }
        LocalDateTime showStart = showDateLocal.atStartOfDay();
        LocalDateTime showEnd = showDateLocal.atTime(LocalTime.MAX);

        Page<Ticket> tickets = ticketRepository.findByShowStartDateBetween(showStart, showEnd,pageable);
        return tickets.map(CheckTicketDto::from);
    }
}
