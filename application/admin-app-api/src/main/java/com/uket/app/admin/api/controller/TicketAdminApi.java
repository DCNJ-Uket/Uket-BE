package com.uket.app.admin.api.controller;

import com.uket.app.admin.api.dto.request.EnterShowRequest;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "어드민용 티켓 관리 API", description = "어드민용 티켓 관리 API")
@RestController
@RequestMapping("/admin/v1/ticket")
@SecurityRequirement(name = "JWT")
@ApiResponse(responseCode = "200", description = "OK")
public interface TicketAdminApi {

    @PostMapping("/enter")
    @Operation(summary = "입장 확인 API", description = "QR code를 통한 Token값으로 입장 확인을 할 수 있습니다.")
    ResponseEntity<EnterShowResponse> enterShow(
            @RequestBody EnterShowRequest request
    );

}
