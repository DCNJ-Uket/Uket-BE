package com.uket.domain.university.repository;

import com.uket.domain.university.entity.University;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UniversityRepository extends JpaRepository<University, Long> {

    Optional<University> findByName(String name);

    List<University> findAllByNameNot(String name);
}
