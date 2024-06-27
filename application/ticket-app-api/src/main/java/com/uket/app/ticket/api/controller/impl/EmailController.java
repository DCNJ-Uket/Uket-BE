package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EmailApi;
import com.uket.app.ticket.api.dto.request.EmailRequest;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EmailController implements EmailApi {

    @Override
    public ResponseEntity<?> sendEmail(Long userId, EmailRequest request) {
        return null;
    }
}
