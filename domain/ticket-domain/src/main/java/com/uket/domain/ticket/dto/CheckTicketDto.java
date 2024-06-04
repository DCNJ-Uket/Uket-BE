package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.user.entity.Users;
import java.time.LocalDateTime;

public record CheckTicketDto(
    String userName,
    LocalDateTime showDate,
    LocalDateTime enterStartTime,
    LocalDateTime enterEndTime,
    String showLocation,
    String universityName,
    TicketStatus ticketStatus,
    String ticketSerialNumber,
    ReservationUserType userType,
    String showName
) {
    public static CheckTicketDto from(Ticket ticket) {
        Users user = ticket.getUser();
        Events event = ticket.getEvent();
        Shows show = ticket.getShow();
        Reservation reservation = ticket.getReservation();
        String showName = event.getName() + " " + show.getName();

        return new CheckTicketDto(
            user.getName(),
            show.getStartDate(),
            reservation.getStartTime(),
            reservation.getEndTime(),
            show.getLocation(),
            event.getUniversity().getName(),
            ticket.getStatus(),
            ticket.getTicketSerialNumber(),
            reservation.getType(),
            showName
        );
    }
}
