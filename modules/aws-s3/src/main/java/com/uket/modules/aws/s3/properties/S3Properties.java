package com.uket.modules.aws.s3.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConfigurationPropertiesBinding;

@ConfigurationProperties(prefix = "spring.cloud.aws.s3")
@ConfigurationPropertiesBinding
public record S3Properties(
    String accessKey,
    String secretKey,
    String bucket
) {

}
