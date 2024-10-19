package com.uket.app.admin.api.service;

import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import java.time.LocalDateTime;

public record LiveEnterUserDto(
        LocalDateTime enterTime,
        String name,
        LocalDateTime ticketDate,
        String phoneNumber,
        TicketStatus ticketStatus
) {

        public static LiveEnterUserDto from(Ticket ticket) {
            return new LiveEnterUserDto(
                    ticket.getEnterAt(),
                    ticket.getUser().getUserDetails().getDepositorName(),
                    ticket.getShow().getStartDate(),
                    ticket.getUser().getUserDetails().getPhoneNumber(),
                    ticket.getStatus()
            );
        }
}
