package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.user.entity.Users;
import lombok.Builder;

@Builder
public record CreateTicketDto(
        Users user,
        Events event,
        Shows show,
        Reservation reservation,
        TicketStatus status
) {

}
