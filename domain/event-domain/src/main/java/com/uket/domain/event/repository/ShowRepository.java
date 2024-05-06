package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Shows;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ShowRepository extends JpaRepository<Shows, Long> {

}
