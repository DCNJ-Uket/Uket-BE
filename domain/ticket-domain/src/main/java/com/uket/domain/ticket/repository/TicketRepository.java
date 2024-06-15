package com.uket.domain.ticket.repository;

import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.user.entity.Users;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TicketRepository extends JpaRepository<Ticket,Long> {

    Boolean existsByUserAndReservation(Users user, Reservation reservation);

    Boolean existsByUserAndShow(Users user, Shows show);

    Boolean existsByUserIdAndId(Long userId, Long ticketId);

    Ticket findByUserIdAndId(Long userId, Long ticketId);

    List<Ticket> findAllByUserId(Long userId);
}

