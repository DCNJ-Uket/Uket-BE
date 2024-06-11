package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Reservation;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ReservationRepository extends JpaRepository<Reservation,Long> {

    <T> List<T> findByShowId(Long showId, Class<T> type);

}
