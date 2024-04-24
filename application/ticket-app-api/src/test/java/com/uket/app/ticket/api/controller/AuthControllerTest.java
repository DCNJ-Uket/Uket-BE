package com.uket.app.ticket.api.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.uket.app.ticket.api.dto.request.TokenReissueRequest;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

@SpringBootTest
@Transactional
@AutoConfigureMockMvc
class AuthControllerTest {

    private static final String BASE_URL = "/api/v1/auth";

    @Autowired
    private MockMvc mockMvc;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private JwtAuthTokenUtil jwtAuthTokenUtil;

    @Autowired
    private UserService userService;

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
    }

    @Test
    void 토큰을_재발급_할_수_있다() throws Exception {

        //expired token
        String accessToken = jwtAuthTokenUtil.createAccessToken(user.getId(), user.getName(),
                String.valueOf(user.getRole()), user.getIsRegistered(), System.currentTimeMillis());
        String refreshToken = jwtAuthTokenUtil.createRefreshToken(user.getId());
        TokenReissueRequest request = new TokenReissueRequest(accessToken, refreshToken);

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/reissue")
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        perform.andExpect(status().isOk())
                .andExpect(jsonPath("$.accessToken").exists())
                .andExpect(jsonPath("$.refreshToken").exists())
                .andExpect(jsonPath("$.isRegistered").exists());
    }

    @Test
    void accessToken이_만료되지_않은_경우_재발급이_불가능하다() throws Exception {
        String accessToken = jwtAuthTokenUtil.createAccessToken(user.getId(), user.getName(),
                String.valueOf(user.getRole()), user.getIsRegistered());
        String refreshToken = jwtAuthTokenUtil.createRefreshToken(user.getId());
        TokenReissueRequest request = new TokenReissueRequest(accessToken, refreshToken);

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/reissue")
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        perform.andExpect(status().is4xxClientError());
    }

    @Test
    void refreshToken이_만료된_경우_재발급이_불가능하다() throws Exception {
        String accessToken = jwtAuthTokenUtil.createAccessToken(user.getId(), user.getName(),
                String.valueOf(user.getRole()), user.getIsRegistered());
        String refreshToken = jwtAuthTokenUtil.createRefreshToken(user.getId(), System.currentTimeMillis());
        TokenReissueRequest request = new TokenReissueRequest(accessToken, refreshToken);

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/reissue")
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        perform.andExpect(status().is4xxClientError());
    }

    @Test
    void 존재하지_않은_유저의_아이디인_경우_예외처리한다() throws Exception {
        //TODO
        String accessToken = jwtAuthTokenUtil.createAccessToken(0L, user.getName(),
                String.valueOf(user.getRole()), user.getIsRegistered(),System.currentTimeMillis());
        String refreshToken = jwtAuthTokenUtil.createRefreshToken(0L);
        TokenReissueRequest request = new TokenReissueRequest(accessToken, refreshToken);

        ResultActions perform = mockMvc.perform(
                post(BASE_URL + "/reissue")
                        .content(objectMapper.writeValueAsString(request))
                        .contentType(MediaType.APPLICATION_JSON)
        );

        perform.andExpect(status().is4xxClientError());
    }
}
