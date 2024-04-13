package com.uket.app.ticket.api.controller.impl;

import static com.uket.app.ticket.api.util.CookieGenerator.createCookie;
import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_AUTHORIZATION_HEADER;
import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_AUTHORIZATION_VALUE_PREFIX;
import static com.uket.jwtprovider.auth.constants.JwtValues.JWT_PAYLOAD_VALUE_REFRESH;

import com.uket.app.ticket.api.controller.AuthApi;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.auth.service.AuthService;
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
    public ResponseEntity<Void> reissue(HttpServletRequest request, HttpServletResponse response) {
        AuthToken token = authService.reissue(request.getCookies());

        int maxAge = Integer.parseInt(tokenProperties.expiration().refreshTokenExpiration());
        response.setHeader(JWT_AUTHORIZATION_HEADER, JWT_AUTHORIZATION_VALUE_PREFIX + token.accessToken());
        response.addCookie(createCookie(JWT_PAYLOAD_VALUE_REFRESH, token.refreshToken(), maxAge));

        return new ResponseEntity<>(HttpStatus.OK);
    }
}
