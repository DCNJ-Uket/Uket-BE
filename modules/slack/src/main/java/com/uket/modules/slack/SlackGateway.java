package com.uket.modules.slack;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.List;
import lombok.Builder;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
@RequiredArgsConstructor
public class SlackGateway {

    private final ObjectMapper objectMapper;

    @Value("${app.slack.webhook.url}")
    private String url;

    @SneakyThrows
    public void sendErrorMessage(SlackBotDto dto) {
        String body = objectMapper.writeValueAsString(dto);
        RestTemplate restTemplate = new RestTemplate();
        RequestEntity<String> requestEntity = RequestEntity
                .post(url)
                .contentType(MediaType.APPLICATION_JSON)
                .body(body);

        restTemplate.exchange(requestEntity, Void.class);
    }

    @Builder
    public record SlackBotDto (
        @JsonProperty("attachments")
        List<SlackBotAttachmentDto> attachments
    ){

    }

    @Builder
    public record SlackBotAttachmentDto (
        @JsonProperty("color")
        String color,
        @JsonProperty("author_name")
        String authorName,
        @JsonProperty("title")
        String title,
        @JsonProperty("text")
        String text,
        @JsonProperty("fields")
        List<SlackBotFieldDto> fields
    ){

    }

    @Builder
    public record SlackBotFieldDto (
        @JsonProperty("title")
        String title,
        @JsonProperty("value")
        String value,
        @JsonProperty("short")
        boolean shortField
    ){

    }
}
