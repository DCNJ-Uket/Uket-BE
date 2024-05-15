package com.uket.domain.event.service;

import com.uket.domain.event.dto.TicketingDto;
import com.uket.domain.event.repository.TicketingRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TicketingService {

    private final TicketingRepository ticketingRepository;

    public List<TicketingDto> findByShowId(Long showId) {
        return ticketingRepository.findByShowId(showId, TicketingDto.class);
    }
}
