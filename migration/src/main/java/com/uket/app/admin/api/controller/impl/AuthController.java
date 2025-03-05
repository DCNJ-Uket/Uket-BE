package com.uket.app.admin.api.controller.impl;

import com.uket.app.admin.api.controller.AuthApi;
import com.uket.app.admin.api.dto.request.AdministratorRegisterRequest;
import com.uket.app.admin.api.dto.request.EmailLoginRequest;
import com.uket.app.admin.api.dto.request.EmailRegisterRequest;
import com.uket.app.admin.api.dto.response.ActiveOrganizationsResponse;
import com.uket.app.admin.api.dto.response.AdminRegisterResponse;
import com.uket.app.admin.api.dto.response.ListResponse;
import com.uket.app.admin.api.service.AuthService;
import com.uket.app.admin.api.service.UniversityEventService;
import com.uket.app.admin.auth.dto.AdminAuthToken;
import com.uket.app.admin.auth.service.AdminService;
import com.uket.app.domain.user.Admin;
import jakarta.mail.MessagingException;
import java.util.List;
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

    @Override
    public ResponseEntity<AdminRegisterResponse> register(EmailRegisterRequest request) {
        Admin admin = authService.registerToEmail(request.email(), request.password(), request.name());

        AdminRegisterResponse response = AdminRegisterResponse.of(admin);
        return ResponseEntity.ok(response);
    }
}
