package com.uket.domain.event.repository;

import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.dto.ShowNameDto;
import com.uket.domain.event.entity.Shows;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShowRepository extends JpaRepository<Shows, Long> {

    List<ShowDto> findByEventId(Long eventId);

    Optional<ShowNameDto> findNameById(Long showId);
}
