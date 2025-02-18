package com.uket.modules.jwt.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

@ConfigurationProperties(prefix = "app.ticket-token")
public record TicketTokenProperties(
        String secretKey,
        @NestedConfigurationProperty TokenExpirationProperties expiration
) implements TokenProperties { }
