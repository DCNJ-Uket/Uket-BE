package com.uket.app.ticket.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/dev")
public interface DevApi {

    @GetMapping
    @Operation(summary = "테스트용 api", description = "테스트를 진행합니다.")
    ResponseEntity<String> test();
}
