package com.uket.app.ticket.api.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;

@ConfigurationProperties(prefix = "app")
@ConfigurationPropertiesBinding
public record AppProperties(
        String redirectUrl
) {

}
