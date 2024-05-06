package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.response.ShowResponse;
import io.swagger.v3.oas.annotations.Operation;
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
    ResponseEntity<ShowResponse> getShows(
            @PathVariable("id")
            Long showId
    );
}
