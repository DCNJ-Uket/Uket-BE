package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.request.TicketingRequest;
import com.uket.app.ticket.api.dto.response.CancelTicketResponse;
import com.uket.app.ticket.api.dto.response.TicketingResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import com.uket.domain.ticket.dto.CancelTicketDto;
import com.uket.domain.ticket.dto.CheckTicketDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
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
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "TI0002", description = "티켓 예매 가능 수량이 부족할 경우 발생합니다.",
                            value = """
                                    {"code": "TI0002", "message": "티켓 예매 가능 인원이 없습니다."}
                                    """
                    ),
                    @ExampleObject(name = "TI0003", description = "이미 예약된 티켓을 발행하려는 경우 발생합니다.",
                            value = """
                                    {"code": "TI0003", "message": "이미 예약된 티켓입니다."}
                                    """
                    ),
                    @ExampleObject(name = "TI0004", description = "재학생 예매 시간에 일반인 요청이 들어온 경우 발생합니다.",
                            value = """
                                    {"code": "TI0004", "message": "예매가 불가능한 사용자 구분입니다."}
                                    """
                    ),
                    @ExampleObject(name = "TI0005", description = "동일한 공연에 다른 시간대 혹은 같은 시간대 예약 요청이 들어온 경우 발생합니다.",
                            value = """
                                    {"code": "TI0005", "message": "해당 공연에 이미 예약이 되어있습니다."}
                                    """
                    ),
                    @ExampleObject(name = "TI0006", description = "해당 공연의 입장이 이미 시작된 경우 발생합니다.",
                            value = """
                                    {"code": "TI0006", "message": "예매 가능 시각이 지났습니다."}
                                    """
                    ),
                    @ExampleObject(name = "TI0007", description = "예매 시작 시간 이전에 요청이 들어온 경우 발생합니다.",
                            value = """
                                    {"code": "TI0007", "message": "예매 시작 전입니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "US0001", description = "토큰에 담긴 UserId에 대한 사용자를 찾을 수 없을 때 발생합니다.",
                            value = """
                                    {"code": "US0001", "message": "해당 사용자를 찾을 수 없습니다."}
                                    """
                    ),
                    @ExampleObject(name = "EV0004", description = "요청한 reservationId에 대한 정보를 찾을 수 없을 때 발생합니다.",
                            value = """
                                    {"code": "EV0004", "message": "해당 예매 정보를 찾을 수 없습니다."}
                                    """
                    ),
                    @ExampleObject(name = "UN0001", description = "요청한 universityId 대한 정보를 찾을 수 없을 때 발생합니다.",
                            value = """
                                    {"code": "UN0001", "message": "해당 대학을 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<TicketingResponse> ticketing(
            @Parameter(hidden = true)
            @LoginUserId
            Long userId,

            @RequestBody
            TicketingRequest request
    );

    @GetMapping("/{id}/qrcode")
    @Operation(summary = "티켓 QR 코드 발급 API", description = "티켓의 QR 코드를 발급할 수 있습니다.")
    @ApiResponse(responseCode = "400", description = "BAD REQUEST", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "TI0008", description = "티켓의 소유주가 아닌 사용자가 티켓 QR 코드를 발급하려고 할 때 발생합니다.",
                            value = """
                                    {"code": "TI0008", "message": "해당 티켓을 소유하지 않은 사용자입니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<byte[]> getQRCode(
            @Parameter(hidden = true)
            @LoginUserId
            Long userId,

            @PathVariable("id")
            Long ticketId
    );


    @DeleteMapping("/{id}/cancel")
    @Operation(summary = "티켓 취소 API", description = "티켓을 취소할 수 있습니다.")
    @ApiResponse(responseCode = "404", description = "BAD REQUEST", content = @Content(
        mediaType = "application/json",
        examples = {
            @ExampleObject(name = "TI0009", description = "티켓의 아이디가 잘못된 경우 발생합니다.",
                value = """
                                    {"code": "TI0009", "message": "해당 티켓을 찾을 수 없습니다. 티켓 아이디를 다시 확인해주세요."}
                                    """
            )
        }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<CancelTicketResponse> cancelTicket(
        @Parameter(hidden = true)
        @LoginUserId
        Long userId,

        @PathVariable("id")
        Long ticketId
    );
}
