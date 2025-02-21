package com.uket.app.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

@ConfigurationProperties(prefix = "spring.mail")
@ConfigurationPropertiesBinding
public record EmailProperties(
        String host,
        Integer port,
        String username,
        String password,
        @NestedConfigurationProperty EmailDetailProperties properties
) {
    @ConfigurationPropertiesBinding
    public record EmailDetailProperties(
            Long authCodeExpirationMillis
    ) {

    }
}
