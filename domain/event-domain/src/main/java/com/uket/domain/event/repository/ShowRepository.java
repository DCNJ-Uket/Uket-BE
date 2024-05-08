package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Shows;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShowRepository extends JpaRepository<Shows, Long> {

    List<Shows> findByEvent(Events event);
}
