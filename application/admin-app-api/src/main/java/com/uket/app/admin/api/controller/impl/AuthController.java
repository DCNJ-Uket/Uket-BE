package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.AuthApi;
import com.uket.app.admin.api.dto.request.EmailLoginRequest;
import com.uket.app.admin.api.service.AuthService;
import com.uket.domain.auth.admin.dto.AdminAuthToken;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class AuthController implements AuthApi {

    private final AuthService authService;

    @Override
    public ResponseEntity<AdminAuthToken> login(EmailLoginRequest request) {
        AdminAuthToken adminAuthToken = authService.login(request.email(), request.password());
        return ResponseEntity.ok(adminAuthToken);
    }
}
