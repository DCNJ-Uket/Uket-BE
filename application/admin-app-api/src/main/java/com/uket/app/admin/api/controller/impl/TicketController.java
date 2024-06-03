package com.uket.app.admin.api.controller.impl;

import static com.uket.modules.jwt.constants.JwtValues.JWT_PAYLOAD_VALUE_TICKET;

import com.uket.app.admin.api.controller.TicketApi;
import com.uket.app.admin.api.dto.request.EnterShowRequest;
import com.uket.app.admin.api.dto.response.EnterShowResponse;
import com.uket.domain.auth.admin.validator.TokenValidator;
import com.uket.modules.jwt.util.JwtTicketUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class TicketController implements TicketApi {

    private final JwtTicketUtil jwtTicketUtil;
    private final TokenValidator tokenValidator;

    @Override
    public ResponseEntity<EnterShowResponse> enterShow(EnterShowRequest request) {

        String ticketToken = request.ticketToken();

        tokenValidator.validateExpiredToken(ticketToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_TICKET, ticketToken);
        tokenValidator.validateTokenSignature(ticketToken);

        Long ticketId = jwtTicketUtil.getTicketId(ticketToken);

        EnterShowResponse response = EnterShowResponse.of(ticketId);
        return ResponseEntity.ok(response);
    }
}
