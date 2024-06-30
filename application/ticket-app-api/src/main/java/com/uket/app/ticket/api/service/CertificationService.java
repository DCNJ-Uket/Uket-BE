package com.uket.app.ticket.api.service;

import com.uket.domain.university.dto.UniversityDto;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.service.UniversityService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class CertificationService {

    private final UniversityService universityService;

    public List<UniversityDto> getCertifiableOfUniversity() {
        List<University> certifiableUniversities = universityService.getCertifiableUniversities();

        return certifiableUniversities.stream().map(UniversityDto::from).toList();
    }
}
