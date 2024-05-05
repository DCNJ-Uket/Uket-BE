package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.AuthApi;
import com.uket.app.ticket.api.dto.request.LoginRequest;
import com.uket.app.ticket.api.dto.request.TokenReissueRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
import com.uket.app.ticket.api.dto.response.TokenResponse;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.app.ticket.api.service.AuthService;
import com.uket.domain.user.enums.Platform;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class AuthController implements AuthApi {

    private final AuthService authService;
    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    @Override
    public ResponseEntity<AuthResponse> login(LoginRequest request, String provider) {
        Platform platform = Platform.fromString(provider);
        AuthToken authToken = authService.login(platform, request.redirectUri(), request.code());

        Long userId = jwtAuthTokenUtil.getId(authToken.accessToken());
        String userName = jwtAuthTokenUtil.getName(authToken.accessToken());

        AuthResponse response = AuthResponse.of(userId,userName,authToken);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<TokenResponse> reissue(TokenReissueRequest request) {
        AuthToken authToken = authService.reissue(request.accessToken(), request.refreshToken());

        TokenResponse response = TokenResponse.from(authToken);
        return ResponseEntity.ok(response);
    }
}
