package com.uket.app.admin.api.service;

import static com.uket.modules.jwt.constants.JwtValues.JWT_PAYLOAD_VALUE_TICKET;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.validator.TokenValidator;
import com.uket.domain.ticket.dto.TicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.exception.TicketException;
import com.uket.domain.ticket.service.TicketService;
import com.uket.modules.jwt.util.JwtTicketUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class EnterShowService {

    private final TokenValidator tokenValidator;
    private final JwtTicketUtil jwtTicketUtil;
    private final TicketService ticketService;

    public TicketDto enterShow(String ticketToken) {
        tokenValidator.validateExpiredToken(ticketToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_TICKET, ticketToken);
        tokenValidator.validateTokenSignature(ticketToken);

        Long ticketId = jwtTicketUtil.getTicketId(ticketToken);
        Ticket ticket = ticketService.findById(ticketId);

        validateBeforePaymentTicket(ticket.getStatus());
        validateAlreadyEnterTicket(ticket.getStatus());

        return TicketDto.from(ticket);
    }

    private void validateBeforePaymentTicket(TicketStatus status) {

        if (status.equals(TicketStatus.BEFORE_PAYMENT)) {
            throw new TicketException(ErrorCode.BEFORE_PAYMENT_TICKET);
        }
    }

    private void validateAlreadyEnterTicket(TicketStatus status) {

        if (status.equals(TicketStatus.FINISH_ENTER)) {
            throw new TicketException(ErrorCode.ALREADY_ENTER_TICKET);
        }
    }
}
