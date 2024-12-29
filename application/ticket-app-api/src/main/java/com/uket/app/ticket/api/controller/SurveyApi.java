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
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
        mediaType = "application/json",
        examples = {
            @ExampleObject(name = "FO0001", description = "질문묶음를 찾을 수 없는경우에 발생하는 오류입니다.",
                value = """
                                    {"code": "FO0001", "message": "해당 질문지를 찾을 수 없습니다. 질문지 아이디를 다시 확인해주세요."}
                                    """
            ),
            @ExampleObject(name = "US0001", description = "해당 유저를 찾을 수 없는 경우에 발생하는 오류입니다.",
                value = """
                                    {"code": "US0001", "message": "해당 사용자를 찾을 수 없습니다."}
                                    """
            ),
            @ExampleObject(name = "FO0002", description = "질문 묶음에 있는 질문을 찾을 수 없는 경우에 발생하는 오류입니다.",
                value = """
                                    {"code": "FO0002", "message": "해당 질문을 찾을 수 없습니다. 질문 아이디를 다시 확인해주세요."}
                                    """
            ),
            @ExampleObject(name = "FO0003", description = "질문에 대한 응답이 없는경우에 발생합니다.",
                value = """
                                    {"code": "FO0003", "message": "해당 질문에 대한 응답이 없습니다."}
                                    """
            )
        }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
        mediaType = "application/json",
        examples = {
            @ExampleObject(name = "FO0006",
                description = "질문에 대한 응답 목록이 존재하지 않을 경우 발생합니다.",
                value = """
                                    {"code": "FO0006", "message": "질문에 대한 응답이 목록에 없습니다."}
                                    """
            )
        }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<SurveyAnswerResponse> makeSurveyResponse(
        @Parameter(hidden = true)
        @LoginUserId
        Long userId,

        @RequestBody
        SurveyResponseRequest request
    );
}
