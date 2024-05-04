package com.uket.app.ticket.api.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.uket.app.ticket.api.service.UserRegisterService;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.auth.constants.JwtValues;
import jakarta.transaction.Transactional;
import java.time.LocalDate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

@SpringBootTest
@Transactional
@AutoConfigureMockMvc
class UniversityControllerTest {

    private static final String BASE_URL = "/api/v1/universities";
    private static final String UNIVERSITY_OUTSIDER = "일반인";
    private static final String UNIVERSITY_KONKUK = "건국대학교";
    private static final String EVENT_KONKUK = "녹색지대";
    private static final String QUERY_STRING_UNIVERSITY = "university";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    UserService userService;
    @Autowired
    UserRegisterService userRegisterService;
    @Autowired
    UniversityRepository universityRepository;
    @Autowired
    EventRepository eventRepository;

    Users user;
    University konkuk;
    String accessToken;

    @BeforeEach
    void beforeEach() {
        CreateUserDto createUserDto = CreateUserDto.builder()
                .name("test")
                .role(UserRole.ROLE_USER)
                .platform(Platform.KAKAO)
                .platformId("1234")
                .email("abc@naver.com")
                .build();

        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .universityEmail("abc@konkuk.ac.kr")
                .phoneNumber("01012341234")
                .studentMajor("컴퓨터공학부")
                .studentCode("202411032")
                .build();

        user = userService.saveUser(createUserDto);

        Events event = eventRepository.save(
                Events.builder()
                        .name(EVENT_KONKUK)
                        .startDate(LocalDate.now())
                        .endDate(LocalDate.now())
                        .build()
        );
        universityRepository.save(
                University.builder()
                        .name(UNIVERSITY_OUTSIDER)
                        .build()
        );
        konkuk = universityRepository.save(
                University.builder()
                        .name(UNIVERSITY_KONKUK)
                        .emailPostFix("@konkuk.ac.kr")
                        .currentEvent(event.getId())
                        .build()
        );

        AuthToken authToken = userRegisterService.register(user.getId(), createUserDetailsDto, UNIVERSITY_KONKUK);
        accessToken = String.join("", JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX, authToken.accessToken());
    }

    @Test
    void 존재하는_대학교의_현재_진행중인_축제를_조회할_수_있다() throws Exception {

        ResultActions perform = mockMvc.perform(
                get(BASE_URL + "/" + konkuk.getId() + "/event")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
                        .param(QUERY_STRING_UNIVERSITY, UNIVERSITY_KONKUK)
        );

        perform.andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value(EVENT_KONKUK))
                .andExpect(jsonPath("$.startDate").exists())
                .andExpect(jsonPath("$.endDate").exists());
    }

    @Test
    void 존재하지_않는_대학의_경우_예외를_반환한다() throws Exception {

        ResultActions perform = mockMvc.perform(
                get(BASE_URL + "/" + 0 + "/event")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
                        .param(QUERY_STRING_UNIVERSITY, "")
        );

        perform.andExpect(status().is4xxClientError())
                .andExpect(jsonPath("$.code").value(ErrorCode.NOT_FOUND_UNIVERSITY.getCode()))
                .andExpect(jsonPath("$.message").value(ErrorCode.NOT_FOUND_UNIVERSITY.getMessage()));
    }

    @Test
    void 진행중인_이벤트가_없는_경우_예외를_반환한다() throws Exception {
        String UNIV_SEJONG = "세종대학교";

        University sejong = universityRepository.save(
                University.builder()
                        .name(UNIV_SEJONG)
                        .build()
        );

        ResultActions perform = mockMvc.perform(
                get(BASE_URL + "/" + sejong.getId() + "/event")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
                        .param(QUERY_STRING_UNIVERSITY, UNIV_SEJONG)
        );

        perform.andExpect(status().is4xxClientError())
                .andExpect(jsonPath("$.code").value(ErrorCode.NOT_FOUND_CURRENT_EVENT.getCode()))
                .andExpect(jsonPath("$.message").value(ErrorCode.NOT_FOUND_CURRENT_EVENT.getMessage()));
    }
}
