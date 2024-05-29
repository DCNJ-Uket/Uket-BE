package com.uket.modules.redis.lock.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "spring.data.redis.lock")
public record RedisLockProperties(
    String host,
    int port
) {

}
