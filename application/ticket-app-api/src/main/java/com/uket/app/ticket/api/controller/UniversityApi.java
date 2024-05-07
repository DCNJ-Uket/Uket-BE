package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.core.dto.response.ErrorResponse;
import com.uket.domain.university.dto.UniversityDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "대학 API", description = "대학 관련 API")
@RestController
@RequestMapping("/api/v1/universities")
@ApiResponse(responseCode = "200", description = "OK")
public interface UniversityApi {

    @GetMapping
    @Operation(summary = "전체 대학 조회 API", description = "현재 진행중인 축제가 있는 모든 대학을 조회합니다.")
    ResponseEntity<ListResponse<UniversityDto>> getUniversities();

    @GetMapping(value = "/{id}/event")
    @Operation(summary = "대학별 진행중인 축제 조회 API", description = "대학별 진행중인 축제를 조회합니다.")
    @ApiResponse(responseCode = "404", description = "NOT FOUND", content = @Content(
            mediaType = "application/json",
            examples = {
                    @ExampleObject(name = "EV0001", description = "진행중인 축제가 DB에 존재하지 않을 경우 발생합니다.",
                            value = """
                                    {"code": "EV0001", "message": "해당 축제를 찾을 수 없습니다."}
                                    """
                    ),
                    @ExampleObject(name = "EV0002", description = "해당 대학이 진행중인 축제가 존재하지 않을 경우 발생합니다.",
                            value = """
                                    {"code": "EV0002", "message": "진행중인 축제를 찾을 수 없습니다."}
                                    """
                    )
            }, schema = @Schema(implementation = ErrorResponse.class)))
    ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(
            @PathVariable("id")
            @Parameter(description = "대학 id")
            Long universityId
    );
}
