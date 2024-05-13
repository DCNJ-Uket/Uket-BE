package com.uket.app.ticket.api.service;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
import com.uket.modules.redis.service.TokenService;
import jakarta.transaction.Transactional;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import static org.junit.jupiter.api.Assertions.*;


@SpringBootTest
@Transactional
@ActiveProfiles("test")
class TokenServiceTest {
    @Autowired
    private TokenService tokenService;

    @Autowired
    private AuthService authService;

    @Autowired
    private JwtAuthTokenUtil jwtAuthTokenUtil;

    @Autowired
    private UserService userService;



    private Users testUser;

    private String accessToken;
    private String refreshToken;


    @BeforeEach
    void beforeEach() {
        CreateUserDto createUserDto = CreateUserDto.builder()
            .name("test_redis")
            .role(UserRole.ROLE_USER)
            .platform(Platform.KAKAO)
            .platformId("4052")
            .email("redis@naver.com")
            .build();

        testUser = userService.saveUser(createUserDto);

        Users serviceUser = userService.findById(testUser.getId());

        accessToken = jwtAuthTokenUtil.createAccessToken(testUser.getId(),testUser.getName(), testUser.getRole().toString(),testUser.getIsRegistered(), 3_000L);
        refreshToken = jwtAuthTokenUtil.createRefreshToken();
        tokenService.storeToken(refreshToken, accessToken, testUser.getId());
    }

    @Test
    void Redis에_token관련_정보가_잘_저장된다() {
        Set<String> keys = tokenService.allKeys();
        assertNotNull(keys);
        assertFalse(keys.isEmpty());
        assertTrue(keys.contains("refreshToken:" + refreshToken));
    }


    @Test
    void Reissue를_진행할경우_Redis에_저장되는_refreshToken은_바뀐다() {
        AuthToken authToken = authService.reissue(accessToken, refreshToken);

        assertNotNull(authToken.accessToken());
        assertNotEquals(refreshToken, authToken.refreshToken(), "The old and new refresh tokens should not be the same");
    }

    @Test
    void RefreshToken_만료시간이_지난경우_Redis에서_삭제되어_Reissue가_진행되지_않는다() throws InterruptedException {
        //TokenService.java의 redis 만료시간을 2초로 변경 후 진행
        Thread.sleep(3000);

        Exception exception = assertThrows(RuntimeException.class, () -> {
            authService.reissue(accessToken, refreshToken);
        });

        assertEquals("Refresh token is invalid or expired", exception.getMessage());
    }

}
