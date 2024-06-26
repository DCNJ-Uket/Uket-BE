package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EmailApi;
import com.uket.app.ticket.api.dto.request.AuthCodeRequest;
import com.uket.app.ticket.api.dto.request.AuthEmailRequest;
import com.uket.app.ticket.api.dto.response.AuthEmailResponse;
import com.uket.app.ticket.api.properties.EmailProperties;
import com.uket.app.ticket.api.service.UserAuthEmailService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EmailController implements EmailApi {

    private final EmailProperties emailProperties;
    private final UserAuthEmailService userAuthEmailService;

    @Override
    public ResponseEntity<AuthEmailResponse> sendEmail(AuthEmailRequest request) {
        String email = request.email();
        Long universityId = request.universityId();
        Long expiration = emailProperties.properties().authCodeExpirationMillis();

        userAuthEmailService.sendAuthEmail(email, universityId);

        return ResponseEntity.ok(AuthEmailResponse.of(email, expiration));
    }

    @Override
    public ResponseEntity<Void> verifyEmail(AuthCodeRequest request) {

        userAuthEmailService.verifyAuthEmail(request.email(), request.universityId(), request.authCode());

        return ResponseEntity.ok().build();
    }
}
