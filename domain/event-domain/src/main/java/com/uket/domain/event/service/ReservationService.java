package com.uket.domain.event.service;

import com.uket.domain.event.dto.ReservationDto;
import com.uket.domain.event.repository.ReservationRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ReservationService {

    private final ReservationRepository reservationRepository;

    public List<ReservationDto> findByShowId(Long showId) {
        return reservationRepository.findByShowId(showId, ReservationDto.class);
    }
}
