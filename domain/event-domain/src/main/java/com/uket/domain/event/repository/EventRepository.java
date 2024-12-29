package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Events;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface EventRepository extends JpaRepository<Events, Long>, EventRepositoryCustom {
}
