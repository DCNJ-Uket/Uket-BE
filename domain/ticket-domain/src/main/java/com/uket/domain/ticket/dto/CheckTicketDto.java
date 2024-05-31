package com.uket.domain.ticket.dto;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import java.sql.Timestamp;
import java.time.LocalDateTime;

public record CheckTicketDto(
    String userName,
    LocalDateTime showDate,
    LocalDateTime EnterStartTime,
    LocalDateTime EnterEndTime,
    String showLocation,
    String universityName,
    TicketStatus ticketStatus,
    String ticketSerialNumber,
    Timestamp purchaseTime,
    String userType
) {
    public static CheckTicketDto from(Ticket ticket) {
        Users user = ticket.getUser();
        Events event = ticket.getEvent();
        Shows show = ticket.getShow();
        Reservation reservation = ticket.getReservation();
        UserDetails userDetails = user.getUserDetails();

        return new CheckTicketDto(
            user.getName(),
            show.getTicketingDate(),
            reservation.getStartTime(),
            reservation.getEndTime(),
            show.getLocation(),
            event.getUniversity().getName(),
            ticket.getStatus(),
            ticket.getTicketSerialNumber(),
            ticket.getCreatedAt(),
            userDetails.getStudentMajor() // 학생 전공 정보를 회원 구분으로 사용, null일경우 외부인
        );
    }
}
