package com.uket.modules.redis.lock.config;

import com.uket.modules.redis.lock.properties.RedisLockProperties;
import lombok.RequiredArgsConstructor;
import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.Config;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@RequiredArgsConstructor
public class RedissonConfig {

    private static final String REDISSON_HOST_PREFIX = "redis://";

    private final RedisLockProperties redisLockProperties;

    @Bean
    public RedissonClient redissonClient() {

        Config config = new Config();
        String redisAddress = REDISSON_HOST_PREFIX + redisLockProperties.host() + ":" + redisLockProperties.port();
        config.useSingleServer().setAddress(redisAddress);

        return Redisson.create(config);
    }
}
