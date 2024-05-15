package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Shows;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShowRepository extends JpaRepository<Shows, Long> {

    <T> List<T> findByEventId(Long eventId, Class<T> type);

    <T> Optional<T> findNameById(Long showId, Class<T> type);
}
