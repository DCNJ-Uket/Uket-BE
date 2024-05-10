package com.uket.modules.slack;

import com.uket.modules.slack.dto.ErrorReportDto;
import java.util.Arrays;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.core.env.Environment;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@RequiredArgsConstructor
@Component
public class ApplicationListener {

    private final SlackGateway slackGateway;
    private final Environment environment;

    private boolean isLocalProfile() {
        List<String> activeProfiles = Arrays.asList(environment.getActiveProfiles());
        return activeProfiles.contains("local");
    }

    @Async
    @EventListener
    public void onErrorReport(ErrorReportDto errorReportDto) {
        if (isLocalProfile()) {
            return;
        }

        String errorMessage = errorReportDto.errorMessage() == null ? "알 수 없는 에러" : errorReportDto.errorMessage();
        SlackGateway.SlackBotDto dto = SlackGateway.SlackBotDto.builder()
                .attachments(List.of(
                        SlackGateway.SlackBotAttachmentDto.builder()
                                .authorName("uket-server")
                                .color("#ff2400")
                                .title("백엔드 서비스 에러 리포트")
                                .text("백엔드 서버에서 핸들링되지 않은 오류가 발생하였습니다")
                                .fields(List.of(
                                        SlackGateway.SlackBotFieldDto.builder()
                                                .title("에러 메시지")
                                                .value(errorMessage)
                                                .shortField(false)
                                                .build(),
                                        SlackGateway.SlackBotFieldDto.builder()
                                                .title("에러 페이로드")
                                                .value(errorReportDto.payload())
                                                .shortField(false)
                                                .build()
                                ))
                                .build()
                ))
                .build();

        slackGateway.sendErrorMessage(dto);
    }
}
