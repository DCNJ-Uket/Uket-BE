package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.response.ShowResponse;
import com.uket.core.dto.response.ErrorResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "축제 API", description = "축제 관련 API")
@RestController
@SecurityRequirement(name = "JWT")
@RequestMapping("/api/v1/events")
public interface EventApi {

    @GetMapping("/{id}/shows")
    @Operation(summary = "공연 조회 API", description = "축제별 공연을 조회할 수 있습니다.")
    @ApiResponse(responseCode = "200", description = "OK")
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(
                    name = "EV0001",
                    description = "해당 축제를 찾을 수 없습니다.",
                    value = """
                            {"code": "EV0001", "message": "해당 축제를 찾을 수 없습니다."}
                            """
                    ),
                    @ExampleObject(
                            name = "UN0001",
                            description = "해당 대학을 찾을 수 없습니다.",
                            value = """
                            {"code": "UN0001", "message": "해당 대학을 찾을 수 없습니다."}
                            """
                    )
            },
            schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<ShowResponse> getShows(
            @PathVariable("id")
            Long showId
    );
}
