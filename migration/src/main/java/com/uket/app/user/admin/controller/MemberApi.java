package com.uket.app.user.admin.controller;

import com.uket.app.user.admin.dto.response.SearchAdminsResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "어드민 사용자 관리 API", description = "어드민 사용자 관리 API")
@RestController
@RequestMapping("/admin/v1/member")
@ApiResponse(responseCode = "200", description = "OK")
public interface MemberApi {

    @Operation(summary = "어드민 멤버 전체 조회", description = "어드민 전체 멤버 목록을 페이지별로 조회합니다. 페이지는 1부터 시작합니다.")
    @GetMapping("/search/all")
    ResponseEntity<SearchAdminsResponse> searchAllAdmins(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int size
    );

}
