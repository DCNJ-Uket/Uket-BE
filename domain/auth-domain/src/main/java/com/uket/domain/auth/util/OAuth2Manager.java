package com.uket.domain.auth.util;

import java.nio.charset.StandardCharsets;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.web.client.RestClient;

@Slf4j
public class OAuth2Manager {
    protected static final String MEDIA_TYPE = new MediaType(MediaType.APPLICATION_FORM_URLENCODED,
            StandardCharsets.UTF_8).toString();

    public RestClient createRestClient(String baseUrl) {
        return RestClient.builder()
                .baseUrl(baseUrl)
                .defaultStatusHandler(
                        HttpStatusCode::is4xxClientError,
                        (request, response) -> {
                            log.error("Client Error Code={}", response.getStatusCode());
                            log.error("Client Error Message={}",
                                    new String(response.getBody().readAllBytes()));
                        })
                .defaultStatusHandler(
                        HttpStatusCode::is5xxServerError,
                        (request, response) -> {
                            log.error("Server Error Code={}", response.getStatusCode());
                            log.error("Server Error Message={}",
                                    new String(response.getBody().readAllBytes()));
                        })
                .build();
    }
}
