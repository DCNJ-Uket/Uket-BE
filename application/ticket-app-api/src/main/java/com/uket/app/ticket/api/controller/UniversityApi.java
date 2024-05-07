package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.domain.university.dto.UniversityDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "대학 API", description = "대학 관련 API")
@RestController
@RequestMapping("/api/v1/universities")
public interface UniversityApi {

    @GetMapping
    @Operation(summary = "전체 대학 조회 API", description = "현재 진행중인 축제가 있는 모든 대학을 조회합니다.")
    ResponseEntity<ListResponse<UniversityDto>> getUniversities();

    @GetMapping(value = "/{id}/event")
    @Operation(summary = "대학별 진행중인 축제 조회 API", description = "대학별 진행중인 축제를 조회합니다.")
    ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(
            @PathVariable("id")
            @Parameter(description = "대학 id")
            Long universityId
    );
}
