package com.uket.domain.event.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.dto.ReservationDto;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.event.exception.EventException;
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

    public List<ReservationDto> findByShowIdAndReservationUserType(Long showId, ReservationUserType reservationUserType) {
        if (reservationUserType.equals(ReservationUserType.TICKETING_STUDENT)) {
            return reservationRepository.findByShowId(showId, ReservationDto.class);
        }
        return reservationRepository.findByShowIdAndType(showId, reservationUserType, ReservationDto.class);
    }

    public Reservation findById(Long reservationId) {
        return reservationRepository.findById(reservationId)
                .orElseThrow(() -> new EventException(ErrorCode.NOT_FOUND_RESERVATION));
    }
}
