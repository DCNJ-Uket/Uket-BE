package com.uket.domain.auth.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;

@ConfigurationProperties(prefix = "app")
@ConfigurationPropertiesBinding
public record AppProperties(
        KaKaoProperties kakao,
        GoogleProperties google
) {
    @ConfigurationPropertiesBinding
    public record KaKaoProperties(
            String tokenUri,
            String userInfoUri,
            String clientId,
            String clientSecret
    ) { }

    @ConfigurationPropertiesBinding
    public record GoogleProperties(
            String tokenUri,
            String userInfoUri,
            String clientId,
            String clientSecret
    ) { }
}
