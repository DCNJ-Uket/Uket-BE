package com.uket.modules.redis.service;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class TokenService {
    private final RedisTemplate<String, String> redisTemplate;

    public void storeToken(String refreshToken, String accessToken, Long userId) {
        final String refreshTokenKey = "refreshToken:" + refreshToken;

        if (Boolean.TRUE.equals(redisTemplate.hasKey(refreshTokenKey))) {
            redisTemplate.delete(refreshTokenKey);
        }

        Map<String, String> tokenDetails = new HashMap<>();
        tokenDetails.put("accessToken", accessToken);
        tokenDetails.put("userId", userId.toString());
        redisTemplate.opsForHash().putAll(refreshTokenKey, tokenDetails);

        redisTemplate.expire(refreshTokenKey, Duration.ofHours(2));
        //redisTemplate.expire(refreshTokenKey, Duration.ofSeconds(2));
    }

    public Long getUserIdForToken(String refreshToken) {
        String refreshTokenKey = "refreshToken:" + refreshToken;
        String userIdAsString = (String) redisTemplate.opsForHash().get(refreshTokenKey, "userId");
        return userIdAsString != null ? Long.parseLong(userIdAsString) : null;

    }

    public String getAccessTokenForToken(String refreshToken) {
        String refreshTokenKey = "refreshToken:" + refreshToken;
        return (String) redisTemplate.opsForHash().get(refreshTokenKey, "accessToken");
    }

    public void deleteTokenIfExist(String refreshToken) {
        final String refreshTokenKey = "refreshToken:" + refreshToken;
        if (Boolean.TRUE.equals(redisTemplate.hasKey(refreshTokenKey))) {
            redisTemplate.delete(refreshTokenKey);
        }
    }

    public void validateRefreshToken(String refreshToken) {
        final String refreshTokenKey = "refreshToken:" + refreshToken;
        if (!Boolean.TRUE.equals(redisTemplate.hasKey(refreshTokenKey))) {
            throw new IllegalStateException("Refresh token is invalid or expired");
        }
    }

    public Set<String> allKeys() {
        return redisTemplate.keys("*");
    }
}
