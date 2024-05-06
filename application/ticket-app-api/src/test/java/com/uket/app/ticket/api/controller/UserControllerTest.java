package com.uket.app.ticket.api.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.uket.app.ticket.api.dto.request.UserRegisterRequest;
import com.uket.app.ticket.api.dto.response.TokenResponse;
import com.uket.app.ticket.api.util.AuthTokenGenerator;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import com.uket.modules.jwt.auth.constants.JwtValues;
import jakarta.transaction.Transactional;
import java.util.Optional;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

@SpringBootTest
@Transactional
@AutoConfigureMockMvc
class UserControllerTest {

    private static final String BASE_URL = "/api/v1/users";

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private UserService userService;
    @Autowired
    private UniversityRepository universityRepository;
    @Autowired
    private AuthTokenGenerator authTokenGenerator;
    @Autowired
    private JwtAuthTokenUtil jwtAuthTokenUtil;

    private Users user;

    @BeforeEach
    void beforeEach() {
        CreateUserDto createUserDto = CreateUserDto.builder()
                .name("test")
                .role(UserRole.ROLE_USER)
                .platform(Platform.KAKAO)
                .platformId("1234")
                .email("abc@naver.com")
                .build();

        user = userService.saveUser(createUserDto);

        universityRepository.save(University.builder().name("일반인").build());
        universityRepository.save(University.builder().name("건국대학교").emailPostFix("@konkuk.ac.kr").build());
    }

    @Test
    void 회원가입시_토큰이_재발급된다() throws Exception {

        UserRegisterRequest request = new UserRegisterRequest("홍길동",
                "01012341234", "건국대학교", "abc123@konkuk.ac.kr","컴퓨터공학부", "12341234");
        AuthToken authToken = authTokenGenerator.generateAuthToken(user);
        String accessToken = String.join("", JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX, authToken.accessToken());

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/register")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        perform.andExpect(status().isOk())
                .andExpect(jsonPath("$.accessToken").exists())
                .andExpect(jsonPath("$.refreshToken").exists())
                .andExpect(jsonPath("$.isRegistered").value(true));
    }


    @Test
    void 재발급된_토큰은_회원가입된_상태여야_한다() throws Exception {

        UserRegisterRequest request = new UserRegisterRequest("홍길동",
                "01012341234", "건국대학교", "abc123@konkuk.ac.kr","컴퓨터공학부", "12341234");
        AuthToken authToken = authTokenGenerator.generateAuthToken(user);
        String accessToken = String.join("", JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX, authToken.accessToken());

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/register")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        String response = perform.andReturn().getResponse().getContentAsString();
        TokenResponse tokenResponse = objectMapper.readValue(response, TokenResponse.class);

        Assertions.assertThat(jwtAuthTokenUtil.isRegistered(tokenResponse.accessToken())).isTrue();
    }

    @ParameterizedTest
    @CsvSource(value = {"홍길동,null","null,01012341234","null,null"},delimiter = ',')
    void 입금자명_또는_전화번호는_null이면_안된다(String depositorName, String phoneNumber) throws Exception{

        depositorName = Optional.ofNullable(depositorName).filter(s -> !"null".equals(s)).orElse(null);
        phoneNumber = Optional.ofNullable(phoneNumber).filter(s -> !"null".equals(s)).orElse(null);

        UserRegisterRequest request = UserRegisterRequest.builder()
                .depositorName(depositorName)
                .phoneNumber(phoneNumber)
                .university("건국대학교")
                .studentMajor("컴퓨터공학부")
                .studentCode("12341234")
                .build();
        AuthToken authToken = authTokenGenerator.generateAuthToken(user);
        String accessToken = String.join("", JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX, authToken.accessToken());

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/register")
                        .header(HttpHeaders.AUTHORIZATION, accessToken)
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        perform.andExpect(status().is4xxClientError());
    }
}
