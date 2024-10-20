package com.uket.app.admin.api.controller;

import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.app.admin.api.dto.response.UpdateTicketStatusResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.ticket.enums.TicketStatus;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "어드민용 티켓 관리 API", description = "어드민용 티켓 관리 API")
@RestController
@RequestMapping("/admin/v1/ticket")
@SecurityRequirement(name = "JWT")
@ApiResponse(responseCode = "200", description = "OK")
public interface TicketApi {

    @Operation(summary = "입장 확인 API", description = "QR code를 통한 Token값으로 입장 확인을 할 수 있습니다.")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
        mediaType = "application/json",
        examples = {
            @ExampleObject(name = "QR0001", description = "QR code와 관련되지 않은 토큰을 입력할 시 발생합니다.",
                value = """
                                    {"code": "QR0001", "message": "QR과 관련없는 다른 유형의 토큰이 입력되었습니다."}
                                    """
            ),
            @ExampleObject(name = "TI0012", description = "입금이 완료되지않은 티켓으로 입장할 시 발생합니다.",
                value = """
                                    {"code": "TI0012", "message": "입금이 완료되지 않은 티켓입니다."}
                                    """
            ),
            @ExampleObject(name = "TI0013", description = "이미 입장 처리된 티켓으로 재입장을 시도할 경우 발생합니다.",
                value = """
                                    {"code": "TI0013", "message": "이미 입장이 완료된 티켓입니다."}
                                    """
            )
        }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "401", description = "UNAUTHORIZED", content = @Content(
        mediaType = "application/json",
        examples = {
            @ExampleObject(name = "QR0003", description = "QR코드의 토큰이 유효하지 않은 경우 발생합니다.",
                value = """
                                    {"code": "QR0003", "message": "QR code의 토큰이 유효하지않습니다. 변조되었을 가능성이 있으니 개발자에게 문의 부탁드립니다."}
                                    """
            )
        }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "403", description = "FORBIDDEN", content = @Content(
        mediaType = "application/json",
        examples = {
            @ExampleObject(name = "QR0002", description = "QR code의 token 유효기간이 만료된 경우 발생합니다.",
                value = """
                                    {"code": "QR0002", "message": "QR code의 유효 기간이 만료되었습니다. 재발급 부탁드립니다."}
                                    """
            ),
            @ExampleObject(name = "AU0008", description = "admin계정의 accessToken이 만료되었을 때 발생합니다.",
                value = """
                                    {"code": "AU0008", "message": "만료된 토큰입니다."}
                                    """
            )
        }, schema = @Schema(implementation = ErrorResponse.class)))
    @GetMapping("/{token}/enter")
    ResponseEntity<EnterShowResponse> enterShow(
        @PathVariable("token") String ticketToken
    );

    @Operation(summary = "티켓 상태 변경 API", description = "어드민용 티켓 상태를 변경합니다.")
    @PatchMapping("/{ticketId}/status/{ticketStatus}")
    ResponseEntity<UpdateTicketStatusResponse> updateTicketStatus(
            @PathVariable("ticketId") Long ticketId,
            @PathVariable("ticketStatus") TicketStatus ticketStatus
    );
}
