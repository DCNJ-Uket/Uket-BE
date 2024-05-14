package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Ticketing;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TicketingRepository extends JpaRepository<Ticketing,Long> {

    List<Ticketing> findByShowId(Long showId);
}
