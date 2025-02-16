package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.user.entity.Users;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record AdminCheckTicketDto(
    String userName,
    String phoneNumber,
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
    Long eventId,
    Long userId,

    LocalDateTime createdAt,

    LocalDateTime updatedAt
) {
    public static AdminCheckTicketDto from(Ticket ticket) {
        Users user = ticket.getUser();
        Events event = ticket.getEvent();
        Shows show = ticket.getShow();
        Reservation reservation = ticket.getReservation();

        return AdminCheckTicketDto.builder()
            .userName(user.getUserDetails().getDepositorName())
            .phoneNumber(user.getUserDetails().getPhoneNumber())
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
            .eventId(ticket.getEvent().getId())
            .userId(user.getId())
            .createdAt(ticket.getCreatedAt().toLocalDateTime())
            .updatedAt(ticket.getModifiedAt().toLocalDateTime())
            .build();
    }
}
