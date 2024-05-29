package com.uket.app.ticket.api.service;

import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.service.UserService;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
import com.uket.modules.redis.service.RotateTokenService;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.junit.jupiter.api.Assertions.*;


@SpringBootTest
@Transactional
@TestMethodOrder(OrderAnnotation.class)
class RotateTokenServiceTest {
    @Autowired
    private RotateTokenService rotateTokenService;

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

        accessToken = jwtAuthTokenUtil.createAccessToken(testUser.getId(),testUser.getName(), testUser.getRole().toString(),testUser.getIsRegistered(), 3_000L);
        refreshToken = jwtAuthTokenUtil.createRefreshToken(6_000L);
        rotateTokenService.storeToken(refreshToken, accessToken, testUser.getId());
    }

    @Order(1)
    @Test
    void Redis에_token관련_정보가_잘_저장된다() {
        assertDoesNotThrow(() -> rotateTokenService.validateRefreshToken(refreshToken));
    }


    @Order(2)
    @Test
    void Reissue를_진행할경우_Redis에_저장되는_refreshToken은_바뀐다() {
        AuthToken authToken = authService.reissue(accessToken, refreshToken);

        assertNotNull(authToken.accessToken());
        assertNotEquals(refreshToken, authToken.refreshToken(), "The old and new refresh tokens should not be the same");
    }

    @Order(3)
    @Test
    void RefreshToken_만료시간이_지난경우_Redis에서_삭제되어_Reissue가_진행되지_않는다() throws InterruptedException {
        Thread.sleep(7000);

        Exception exception = assertThrows(RuntimeException.class, () -> {
            authService.reissue(accessToken, refreshToken);
        });

        assertEquals("만료된 토큰입니다.", exception.getMessage());
    }

}
