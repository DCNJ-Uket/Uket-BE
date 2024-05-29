package com.uket.modules.redis.service;

import static com.uket.modules.redis.constants.RedisValues.REDIS_KEY_ACCESS_TOKEN;
import static com.uket.modules.redis.constants.RedisValues.REDIS_KEY_USER_ID;

import com.uket.modules.redis.exception.ErrorCode;
import com.uket.modules.redis.exception.RedisException;
import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class RotateTokenService {
    private final RedisTemplate<String, String> redisTemplate;

    public void storeToken(String refreshToken, String accessToken, Long userId) {
        final String refreshTokenKey = "refreshToken:" + refreshToken;
        deleteTokenIfExist(refreshToken);

        Map<String, String> tokenDetails = new HashMap<>();
        tokenDetails.put(REDIS_KEY_ACCESS_TOKEN, accessToken);
        tokenDetails.put(REDIS_KEY_USER_ID, userId.toString());
        redisTemplate.opsForHash().putAll(refreshTokenKey, tokenDetails);
        redisTemplate.expire(refreshTokenKey, Duration.ofHours(2));
    }

    public Long getUserIdForToken(String refreshToken) {
        String refreshTokenKey = "refreshToken:" + refreshToken;
        String userIdAsString = (String) redisTemplate.opsForHash().get(refreshTokenKey, "userId");

        if (userIdAsString == null) {
            throw new RedisException(ErrorCode.USER_ID_NOT_FOUND);  // 적절한 ErrorCode 필요
        }

        return Long.parseLong(userIdAsString);

    }

    public String getAccessTokenForToken(String refreshToken) {
        String refreshTokenKey = "refreshToken:" + refreshToken;
        return (String) redisTemplate.opsForHash().get(refreshTokenKey, "accessToken");
    }

    private void deleteTokenIfExist(String refreshToken) {
        final String refreshTokenKey = "refreshToken:" + refreshToken;
        redisTemplate.delete(refreshTokenKey);
    }

    public void validateRefreshToken(String refreshToken) {
        final String refreshTokenKey = "refreshToken:" + refreshToken;
        if (!Boolean.TRUE.equals(redisTemplate.hasKey(refreshTokenKey))) {
            throw new RedisException(ErrorCode.INVALID_OR_EXPIRED_REFRESH_TOKEN);
        }
    }
}
