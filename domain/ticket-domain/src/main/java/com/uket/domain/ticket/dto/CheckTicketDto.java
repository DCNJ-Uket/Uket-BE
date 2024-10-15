package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.user.entity.Users;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import lombok.Builder;

@Builder
public record CheckTicketDto(
    String userName,
    String telephone,
    LocalDateTime showStartDate,
    LocalDateTime enterStartTime,
    LocalDateTime enterEndTime,
    String showLocation,
    String universityName,
    String ticketStatus,
    String ticketNo,
    String userType,
    String showName,

    String eventName,

    Long ticketId,

    LocalDateTime createdAt,

    LocalDateTime updatedAt
) {
    public static CheckTicketDto from(Ticket ticket) {
        Users user = ticket.getUser();
        Events event = ticket.getEvent();
        Shows show = ticket.getShow();
        Reservation reservation = ticket.getReservation();

        return CheckTicketDto.builder()
            .userName(user.getName())
            .telephone(user.getUserDetails().getPhoneNumber())
            .showStartDate(show.getStartDate())
            .enterStartTime(reservation.getStartTime())
            .enterEndTime(reservation.getEndTime())
            .showLocation(show.getLocation())
            .universityName(event.getUniversity().getName())
            .ticketStatus(ticket.getStatus().getValue())
            .ticketNo(ticket.getTicketNo())
            .userType(reservation.getType().getValue())
            .showName(show.getName())
            .eventName(event.getName())
            .ticketId(ticket.getId())
            .createdAt(ticket.getCreatedAt().toLocalDateTime())
            .updatedAt(ticket.getModifiedAt().toLocalDateTime())
            .build();
    }
}
