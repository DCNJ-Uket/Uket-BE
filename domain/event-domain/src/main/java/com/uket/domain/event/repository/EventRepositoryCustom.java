package com.uket.domain.event.repository;

import com.uket.domain.university.dto.UniversityDto;
import java.time.LocalDate;
import java.util.List;

public interface EventRepositoryCustom {

    List<UniversityDto> searchUniversitiesByDate(LocalDate date);

}
