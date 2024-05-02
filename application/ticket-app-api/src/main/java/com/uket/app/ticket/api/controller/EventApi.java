package com.uket.app.ticket.api.controller;

import com.uket.app.ticket.api.dto.response.CurrentEventResponse;
import com.uket.domain.auth.config.userid.LoginUserId;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Tag(name = "축제 API", description = "축제 관련 API")
@RestController
@SecurityRequirement(name = "JWT")
@RequestMapping("/api/v1/events")
public interface EventApi {

    @GetMapping(value = "/current", params = {"university"})
    ResponseEntity<CurrentEventResponse> getCurrentEventOfUniversity(
            @Parameter(hidden = true)
            @LoginUserId Long userId,

            @Parameter(example = "건국대학교", description = "사용자가 선택한 학교 이름")
            @RequestParam("university")
            String university
    );
}
