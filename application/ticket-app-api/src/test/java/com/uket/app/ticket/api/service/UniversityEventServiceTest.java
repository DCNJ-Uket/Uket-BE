package com.uket.app.ticket.api.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.exception.UniversityException;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import jakarta.transaction.Transactional;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
@Transactional
class UniversityEventServiceTest {

    private static final String UNIVERSITY_OUTSIDER = "일반인";
    private static final String UNIVERSITY_KONKUK = "건국대학교";
    private static final String EVENT_KONKUK = "녹색지대";


    @Autowired
    UniversityEventService universityEventService;
    @Autowired
    UniversityRepository universityRepository;
    @Autowired
    EventRepository eventRepository;

    @BeforeEach
    void beforeEach() {
        CreateUserDto createUserDto = CreateUserDto.builder()
                .name("test")
                .role(UserRole.ROLE_USER)
                .platform(Platform.KAKAO)
                .platformId("1234")
                .email("abc@naver.com")
                .build();

        universityRepository.save(University.builder().name(UNIVERSITY_OUTSIDER).build());
    }

    @Test
    void 축제를_진행중인_대학의_경우_축제_id를_반환한다() {
        Events event = eventRepository.save(
                Events.builder()
                        .name(EVENT_KONKUK)
                        .build()
        );
        universityRepository.save(University.builder().name(UNIVERSITY_KONKUK).currentEvent(event.getId()).build());

        Assertions.assertThat(universityEventService.getCurrentEventOfUniversity(UNIVERSITY_KONKUK).getId())
                .isEqualTo(event.getId());
    }

    @Test
    void 축제를_진행중이지_않은_대학의_경우_예외를_반환한다() {
        universityRepository.save(University.builder().name(UNIVERSITY_KONKUK).build());

        Assertions.assertThatThrownBy(() -> universityEventService.getCurrentEventOfUniversity(UNIVERSITY_KONKUK))
                .isInstanceOf(EventException.class)
                .hasMessage(ErrorCode.NOT_FOUND_CURRENT_EVENT.getMessage());
    }

    @Test
    void 존재하지_않는_대학의_경우_예외를_반환한다() {
        universityRepository.save(University.builder().name(UNIVERSITY_KONKUK).build());

        Assertions.assertThatThrownBy(() -> universityEventService.getCurrentEventOfUniversity(""))
                .isInstanceOf(UniversityException.class)
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage());
    }

    @Test
    void 대학이름을_일반인으로_요청한_경우_예외를_반환한다() {
        universityRepository.save(University.builder().name(UNIVERSITY_KONKUK).build());

        Assertions.assertThatThrownBy(() -> universityEventService.getCurrentEventOfUniversity(UNIVERSITY_OUTSIDER))
                .isInstanceOf(UniversityException.class)
                .hasMessage(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage());
    }
}
