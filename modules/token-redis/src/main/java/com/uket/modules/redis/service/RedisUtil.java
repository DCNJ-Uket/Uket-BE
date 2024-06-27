package com.uket.modules.redis.service;

import java.time.Duration;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class RedisUtil {

    private final ValueOperations<String, String> valueOperations;

    public void setDataExpire(String key, String value, Long duration) {
        Duration expireDuration = Duration.ofMillis(duration);
        valueOperations.set(key, value, expireDuration);
    }

    public Optional<String> getData(String key) {
        String value = valueOperations.get(key);

        if (value == null) {
            return Optional.empty();
        }
        return Optional.of(value);
    }
}
