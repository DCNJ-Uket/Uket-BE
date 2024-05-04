package com.uket.domain.university.service;


import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.university.exception.UniversityException;
import com.uket.domain.university.repository.UniversityRepository;
import java.util.Optional;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class UniversityServiceTest {
    @InjectMocks
    UniversityService universityService;

    @Mock
    UniversityRepository universityRepository;

    @Test
    void 기본값이_존재하지_않을_경우_예외를_발생시킨다() {
        when(universityRepository.findByName(any())).thenReturn(Optional.empty());

        Assertions.assertThatThrownBy(() -> universityService.getDefault())
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage())
                .isInstanceOf(UniversityException.class);
    }
}
