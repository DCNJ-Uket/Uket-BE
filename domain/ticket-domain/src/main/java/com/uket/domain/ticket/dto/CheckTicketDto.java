package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.user.entity.Users;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record CheckTicketDto(
    String userName,
    LocalDateTime showDate,
    LocalDateTime enterStartTime,
    LocalDateTime enterEndTime,
    String showLocation,
    String universityName,
    TicketStatus ticketStatus,
    String ticketNo,
    ReservationUserType userType,
    String showName
) {
    public static CheckTicketDto from(Ticket ticket) {
        Users user = ticket.getUser();
        Events event = ticket.getEvent();
        Shows show = ticket.getShow();
        Reservation reservation = ticket.getReservation();
        String showName = event.getName() + " " + show.getName();

        return CheckTicketDto.builder()
            .userName(user.getName())
            .showDate(show.getStartDate())
            .enterStartTime(reservation.getStartTime())
            .enterEndTime(reservation.getEndTime())
            .showLocation(show.getLocation())
            .universityName(event.getUniversity().getName())
            .ticketStatus(ticket.getStatus())
            .ticketNo(ticket.getTicketNo())
            .userType(reservation.getType())
            .showName(showName)
            .build();
    }
}
