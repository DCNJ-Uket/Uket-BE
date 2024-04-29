package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UserApi;
import com.uket.app.ticket.api.dto.request.UserRegisterRequest;
import com.uket.app.ticket.api.dto.response.TokenResponse;
import com.uket.domain.auth.dto.response.AuthToken;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
@RequiredArgsConstructor
public class UserController implements UserApi {

    @Override
    public ResponseEntity<TokenResponse> register(Long userId, Boolean isRegistered, String type, UserRegisterRequest request) {

        TokenResponse response = TokenResponse.from(AuthToken.of(null,null,true));
        return ResponseEntity.ok(response);
    }
}
