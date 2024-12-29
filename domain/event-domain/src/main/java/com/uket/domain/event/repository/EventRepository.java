package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Events;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface EventRepository extends JpaRepository<Events, Long>, EventRepositoryCustom {
    @Query("SELECT e FROM Events e JOIN FETCH e.survey WHERE e.id = :eventId")
    Optional<Events> findEventWithSurvey(@Param("eventId") Long eventId);
}
