package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.SurveyResponseRequest;
import com.uket.app.ticket.api.dto.request.TicketingRequest;
import com.uket.app.ticket.api.dto.response.SurveyAnswerResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "질의 응답 API", description = "질의 응답 관련 API")
@RestController
@RequestMapping("/api/v1/survey")
@SecurityRequirement(name = "JWT")
@ApiResponse(responseCode = "200", description = "OK")
public interface SurveyApi {

    @PostMapping
    @Operation(summary = "질의응답 사용자 응답 제출 API", description = "질의응답 문항에 대해 사용자가 응답한 내용을 등록합니다.")
    ResponseEntity<SurveyAnswerResponse> makeSurveyResponse(
        @Parameter(hidden = true)
        @LoginUserId
        Long userId,

        @RequestBody
        SurveyResponseRequest request
    );
}
