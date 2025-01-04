package com.uket.app.ticket.api.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.uket.domain.event.entity.Events;
import com.uket.domain.event.entity.Reservation;
import com.uket.domain.event.entity.Shows;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.event.repository.ReservationRepository;
import com.uket.domain.event.repository.ShowRepository;
import com.uket.domain.ticket.repository.TicketRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.repository.UserRepository;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Disabled;
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
        ticketRepository.deleteAll();
        reservationRepository.deleteAll();
        showRepository.deleteAll();
        eventRepository.deleteAll();
        userRepository.deleteAll();
        universityRepository.deleteAll();
    }

    @Test
    @Disabled
    void 티켓_예매시_정상적으로_예매_횟수가_올라간다() {

        University university = universityRepository.save(University.builder().name("1").build());
        University university2 = universityRepository.save(University.builder().name("2").build());

        Users user = userRepository.save(Users.builder()
                .name("test")
                .university(university)
                .build());

        Events event = eventRepository.save(Events.builder().build());

        Shows show = showRepository.save(Shows.builder()
                .event(event)
                .build());

        Reservation reservation = reservationRepository.save(Reservation.builder()
                .reservedCount(0)
                .show(show)
                .totalCount(1)
                .build());

        ticketingService.ticketing(reservation.getId(), user.getId(), university2.getId());

        Reservation persistReservation = reservationRepository.findById(reservation.getId())
                .orElseThrow(IllegalArgumentException::new);

        assertThat(persistReservation.getReservedCount()).isEqualTo(1);
    }

    @Test
    @Disabled
    void 동시에_100명이_예매해도_정상적으로_동작한다() throws InterruptedException {

        University university = universityRepository.save(University.builder().name("1").build());
        University university2 = universityRepository.save(University.builder().name("2").build());

        Events event = eventRepository.save(Events.builder().build());

        Shows show = showRepository.save(Shows.builder()
                .event(event)
                .build());

        Reservation reservation = reservationRepository.save(Reservation.builder()
                .reservedCount(0)
                .show(show)
                .totalCount(100)
                .build());

        int numberOfThreads = 100;
        ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreads);
        CountDownLatch latch = new CountDownLatch(numberOfThreads);

        for (long i = 0; i < numberOfThreads; i++ ){
            userRepository.save(Users.builder()
                    .name("test")
                    .university(university)
                    .build());
        }

        for (int i = 1; i <= numberOfThreads; i++) {
            long finalI = i;
            executorService.submit(() -> {
                try {
                    ticketingService.ticketing(reservation.getId(), finalI, university2.getId());
                } catch (Exception e) {
                    System.out.println(e.getMessage());
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
