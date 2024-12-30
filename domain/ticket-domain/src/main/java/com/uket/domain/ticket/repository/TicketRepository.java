package com.uket.domain.ticket.repository;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.user.entity.Users;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
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

    void deleteAllByUserId(Long userId);

    @Query("SELECT t FROM Ticket t " +
        "WHERE t.user.id = :userId " +
        "AND t.status NOT IN (:cancelled, :expired) " +
        "AND (t.event.endDate > :now)")
    List<Ticket> findValidTicketsByUserId(@Param("userId") Long userId,
        @Param("cancelled") TicketStatus cancelled,
        @Param("expired") TicketStatus expired,
        @Param("now") LocalDate now);

    @Query("SELECT t FROM Ticket t JOIN FETCH t.reservation r WHERE t.user.id = :userId AND t.status <> :status")
    List<Ticket> findAllByUserIdAndStatusNotWithReservation(@Param("userId") Long userId, @Param("status") TicketStatus status);

}

