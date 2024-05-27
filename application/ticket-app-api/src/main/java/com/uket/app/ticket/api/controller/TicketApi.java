package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.TicketingRequest;
import com.uket.app.ticket.api.dto.response.TicketingResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "티켓 API", description = "티켓 관련 API")
@RestController
@RequestMapping("/api/v1/tickets")
@SecurityRequirement(name = "JWT")
@ApiResponse(responseCode = "200", description = "OK")
public interface TicketApi {

    @PostMapping
    @Operation(summary = "티켓 예매 API", description = "티켓을 예매할 수 있습니다.")
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
            }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<TicketingResponse> ticketing(
            @Parameter(hidden = true)
            @LoginUserId
            Long userId,

            @RequestBody
            TicketingRequest request
    );

}
