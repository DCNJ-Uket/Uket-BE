package com.uket.app.ticket.api.service;

import static org.assertj.core.api.Assertions.assertThat;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.event.repository.ReservationRepository;
import com.uket.domain.event.repository.ShowRepository;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.ticket.entity.Ticket;
import com.uket.domain.ticket.enums.TicketStatus;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.repository.UserRepository;
import jakarta.transaction.Transactional;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class TicketingServiceTest {

    @Autowired
    TicketingService ticketingService;
    @Autowired
    ReservationRepository reservationRepository;
    @Autowired
    TicketRepository ticketRepository;
    @Autowired
    UserRepository userRepository;
    @Autowired
    EventRepository eventRepository;
    @Autowired
    ShowRepository showRepository;
    @Autowired
    UniversityRepository universityRepository;

    @AfterEach
    void afterEach() {
        reservationRepository.deleteAll();
        ticketRepository.deleteAll();
    }

    @Test
    void 티켓_예매시_정상적으로_예매_횟수가_올라간다() {

        Reservation reservation = reservationRepository.save(Reservation.builder()
                .reservedCount(0)
                .totalCount(1)
                .build());

        ticketingService.increaseReservedCount(reservation.getId());

        Reservation persistReservation = reservationRepository.findById(reservation.getId())
                .orElseThrow(IllegalArgumentException::new);

        assertThat(persistReservation.getReservedCount()).isEqualTo(1);
    }

    @Test
    void 동시에_100명이_예매해도_정상적으로_동작한다() throws InterruptedException {

        Reservation reservation = reservationRepository.save(Reservation.builder()
                .reservedCount(0)
                .totalCount(100)
                .build());

        int numberOfThreads = 100;
        ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreads);
        CountDownLatch latch = new CountDownLatch(numberOfThreads);

        for (int i = 0; i < numberOfThreads; i++) {
            executorService.submit(() -> {
                try {
                    ticketingService.increaseReservedCount(reservation.getId());
                } finally {
                    latch.countDown();
                }
            });
        }
        latch.await();

        Reservation persistReservation = reservationRepository.findById(reservation.getId())
                .orElseThrow(IllegalArgumentException::new);

        assertThat(persistReservation.getReservedCount()).isEqualTo(reservation.getTotalCount());
    }
}
