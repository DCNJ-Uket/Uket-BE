package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.user.entity.Users;
import java.sql.Timestamp;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import lombok.Builder;

@Builder
public record CheckTicketDto(
    String userName,
    ZonedDateTime showDate,
    ZonedDateTime enterStartTime,
    ZonedDateTime enterEndTime,
    String showLocation,
    String universityName,
    String ticketStatus,
    String ticketNo,
    String userType,
    String showName,

    String eventName,

    Long ticketId,
    Long eventId,

    Timestamp createdAt,
    String backgroundImageUrl
) {
    private static final String zoneId = "Asia/Seoul";

    public static CheckTicketDto of(Ticket ticket, String backgroundImageUrl) {
        Users user = ticket.getUser();
        Events event = ticket.getEvent();
        Shows show = ticket.getShow();
        Reservation reservation = ticket.getReservation();

        return CheckTicketDto.builder()
            .userName(user.getName())
            .showDate(show.getStartDate().atZone(ZoneId.of(zoneId)))
            .enterStartTime(reservation.getStartTime().atZone(ZoneId.of(zoneId)))
            .enterEndTime(reservation.getEndTime().atZone(ZoneId.of(zoneId)))
            .showLocation(show.getLocation())
            .universityName(event.getUniversity().getName())
            .ticketStatus(ticket.getStatus().getValue())
            .ticketNo(ticket.getTicketNo())
            .userType(reservation.getType().getValue())
            .showName(show.getName())
            .eventName(event.getName())
            .ticketId(ticket.getId())
            .eventId(ticket.getEvent().getId())
            .createdAt(ticket.getCreatedAt())
            .backgroundImageUrl(backgroundImageUrl)
            .build();
    }
}
