package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Events;
import org.springframework.data.jpa.repository.JpaRepository;

public interface EventRepository extends JpaRepository<Events, Long>, EventRepositoryCustom {
    Events findByName(String name);
}
