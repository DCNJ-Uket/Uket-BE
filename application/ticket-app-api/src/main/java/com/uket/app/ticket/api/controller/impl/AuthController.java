package com.uket.app.ticket.api.controller.impl;

import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_AUTHORIZATION_HEADER;
import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX;

import com.uket.app.ticket.api.controller.AuthApi;
import com.uket.app.ticket.api.dto.request.LoginRequest;
import com.uket.app.ticket.api.dto.response.LoginResponse;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.auth.service.AuthService;
import com.uket.domain.user.enums.Platform;
import com.uket.jwtprovider.auth.properties.TokenProperties;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class AuthController implements AuthApi {

    private final AuthService authService;
    private final TokenProperties tokenProperties;

    @Override
    public ResponseEntity<LoginResponse> login(LoginRequest request, String provider) {
        Platform platform = Platform.fromString(provider);

        AuthToken authToken = authService.login(platform, request.redirectUri(), request.code());

        LoginResponse response = LoginResponse.from(authToken);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<Void> reissue(HttpServletRequest request, HttpServletResponse response) {
        AuthToken token = authService.reissue(request.getCookies());

        int maxAge = Integer.parseInt(tokenProperties.expiration().refreshTokenExpiration());
        response.setHeader(JWT_AUTHORIZATION_HEADER, JWT_AUTHORIZATION_VALUE_PREFIX + token.accessToken());

        return new ResponseEntity<>(HttpStatus.OK);
    }
}
