package com.uket.domain.university.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.exception.UniversityException;
import com.uket.domain.university.repository.UniversityRepository;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UniversityService {

    private static final String DEFAULT_UNIVERSITY_NAME = "일반인";

    private final UniversityRepository universityRepository;

    public University findById(Long universityId) {
        return universityRepository.findById(universityId)
                .orElseThrow(() -> new UniversityException(ErrorCode.NOT_FOUND_UNIVERSITY));
    }

    public Optional<University> findByName(String name) {
        if (DEFAULT_UNIVERSITY_NAME.equals(name)) {
            return Optional.empty();
        }
        return universityRepository.findByName(name);
    }

    public University getDefault() {
        return universityRepository.findByName(DEFAULT_UNIVERSITY_NAME)
                .orElseThrow(() -> new UniversityException(ErrorCode.NOT_FOUND_UNIVERSITY));
    }

    public Boolean isDefault(University university) {
        return university.getName().equals(DEFAULT_UNIVERSITY_NAME);
    }

    public Optional<Long> getCurrentEvent(Long universityId) {
        University university = universityRepository.findById(universityId)
                .orElseThrow(() -> new UniversityException(ErrorCode.NOT_FOUND_UNIVERSITY));

        if (university.getName().equals(DEFAULT_UNIVERSITY_NAME)) {
            throw new UniversityException(ErrorCode.NOT_FOUND_UNIVERSITY);
        }
        return Optional.ofNullable(university.getCurrentEvent());
    }
}
