package com.uket.app.ticket.api.properties;

import com.uket.app.ticket.api.properties.EmailProperties.EmailDetailProperties;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;
import org.springframework.boot.context.properties.NestedConfigurationProperty;

@ConfigurationProperties(prefix = "spring.mail.properties.mail.smtp")
@ConfigurationPropertiesBinding
public record SmtpProperties(
        Boolean auth,
        Long connectiontimeout,
        Long timeout,
        Long writetimeout,
        @NestedConfigurationProperty StarttlsProperties starttls
) {
    @ConfigurationPropertiesBinding
    public record StarttlsProperties(
            Boolean enable,
            Boolean required
    ) {

    }
}
