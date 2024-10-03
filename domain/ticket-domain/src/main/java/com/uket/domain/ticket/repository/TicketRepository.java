package com.uket.domain.ticket.repository;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.user.entity.Users;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface TicketRepository extends JpaRepository<Ticket,Long> {

    Boolean existsByUserAndReservation(Users user, Reservation reservation);

    Boolean existsByUserAndReservationAndStatusNot(Users user, Reservation reservation, TicketStatus status);

    Boolean existsByUserAndShow(Users user, Shows show);

    Boolean existsByUserAndShowAndStatusNot(Users user, Shows show, TicketStatus status);

    Boolean existsByUserIdAndId(Long userId, Long ticketId);

    Optional<Ticket> findByUserIdAndId(Long userId, Long ticketId);

    List<Ticket> findAllByUserId(Long userId);

    List<Ticket> findAllByUserIdAndStatusNot(Long userId, TicketStatus status);

    Page<Ticket> findByStatus(TicketStatus status, Pageable pageable);

    Page<Ticket> findByUserName(String userName, Pageable pageable);

    Page<Ticket> findByUserUserDetailsPhoneNumber(String phoneNumber, Pageable pageable);

    Page<Ticket> findByShowStartDate(LocalDateTime startDate, Pageable pageable);

    Page<Ticket> findByReservationType(ReservationUserType userType, Pageable pageable);

    Page<Ticket> findByCreatedAt(Timestamp createdAt, Pageable pageable);

    Page<Ticket> findByCreatedAtBetween(Timestamp createdAt,Timestamp endTimestamp, Pageable pageable);
    Page<Ticket> findByModifiedAt(Timestamp modifiedAt, Pageable pageable);

    Page<Ticket> findByModifiedAtBetween(Timestamp modifiedAt,Timestamp endTimestamp, Pageable pageable);
}

