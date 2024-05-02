package com.uket.domain.university.service;


import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.university.entity.University;
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

        assertThatThrownBy(() -> universityService.getDefault())
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage())
                .isInstanceOf(UniversityException.class);
    }

    @Test
    void 요청한_대학이_존재하지_않는_경우_예외를_발생시킨다() {

        when(universityRepository.findByName(any())).thenReturn(Optional.empty());

        assertThatThrownBy(() -> universityService.getCurrentEvent(""))
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage())
                .isInstanceOf(UniversityException.class);
    }

    @Test
    void 요청한_대학이_일반인인_경우_예외를_발생시킨다() {

        assertThatThrownBy(() -> universityService.getCurrentEvent("일반인"))
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage())
                .isInstanceOf(UniversityException.class);
    }

    @Test
    void 요청한_대학이_진행중인_축제가_없는_경우_null을_반환한다() {

        when(universityRepository.findByName(any())).thenReturn(
                Optional.of(University.builder().name("건국대학교").build())
        );

        Assertions.assertThat(universityService.getCurrentEvent("건국대학교")).isEmpty();
    }

    @Test
    void 요청한_대학이_진행중인_축제가_있는_경우_축제의_id를_반환한다() {

        when(universityRepository.findByName(any())).thenReturn(
                Optional.of(University.builder().name("건국대학교").currentEvent(1L).build())
        );

        Assertions.assertThat(universityService.getCurrentEvent("건국대학교")).isNotEmpty();
    }
}
