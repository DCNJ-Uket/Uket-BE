package com.uket.modules.redis.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;


@ConfigurationProperties(prefix = "spring.data.redis")
@ConfigurationPropertiesBinding
public record RedisProperties (
    String host,
    int port
) {

}
