package com.uket.app.ticket.api.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.uket.app.ticket.api.service.UserRegisterService;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.constants.JwtValues;
import jakarta.transaction.Transactional;
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
class TicketControllerTest {

    private static final String BASE_URL = "/api/v1/tickets";
    private static final String UNIVERSITY_OUTSIDER = "일반인";
    private static final String UNIVERSITY_KONKUK = "건국대학교";

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    UserService userService;
    @Autowired
    UserRegisterService userRegisterService;
    @Autowired
    UniversityRepository universityRepository;

    Users user;
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

        universityRepository.save(
                University.builder()
                        .name(UNIVERSITY_OUTSIDER)
                        .build()
        );

        AuthToken authToken = userRegisterService.register(user.getId(), createUserDetailsDto, UNIVERSITY_KONKUK);
        accessToken = String.join("", JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX, authToken.accessToken());
    }

    @Test
    void 해당_사용자가_소유하지_않은_티켓의_경우_예외를_반환한다() throws Exception {

        ResultActions perform = mockMvc.perform(
                get(BASE_URL + "/1/qrcode")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
        );

        perform.andExpect(status().is4xxClientError())
                .andExpect(jsonPath("$.code").value(ErrorCode.INVALID_ACCESS_TICKET.getCode()))
                .andExpect(jsonPath("$.message").value(ErrorCode.INVALID_ACCESS_TICKET.getMessage()));
    }
}
