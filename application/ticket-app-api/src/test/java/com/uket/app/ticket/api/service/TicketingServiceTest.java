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

    @Test
    @Transactional
    void 유저가_발급한_티켓이_전부_반환된다() {
        Users user = userRepository.save(Users.builder()
            .name("test")
            .role(UserRole.ROLE_USER)
            .platform(Platform.KAKAO)
            .platformId("1234")
            .email("abc@naver.com")
            .build()
        );

        University university = universityRepository.save(University.builder()
            .currentEvent(1L)
            .emailPostFix("@konkuk.ac.kr")
            .logoPath("konkuk.png")
            .name("건국대")
            .build());

        Events konkuk = eventRepository.save(Events.builder()
            .university(university)
            .startDate(LocalDate.now())
            .endDate(LocalDate.now())
            .location("건국대 노천극장")
            .build()
        );

        Shows konkukFirstShow = showRepository.save(Shows.builder()
            .totalTicketCount(0)
            .startDate(LocalDateTime.now())
            .endDate(LocalDateTime.now())
            .location("노천극장 건국대")
            .event(konkuk)
            .ticketingDate(LocalDateTime.now())
            .build());

        Shows konkukSecondShow = showRepository.save(Shows.builder()
            .totalTicketCount(0)
            .startDate(LocalDateTime.now().plusDays(1))
            .endDate(LocalDateTime.now().plusDays(1))
            .location("노천극장 건국대")
            .event(konkuk)
            .ticketingDate(LocalDateTime.now())
            .build());

        Reservation konkukFirstShowReserve = reservationRepository.save(Reservation.builder()
            .totalCount(10)
            .reservedCount(0)
            .show(konkukFirstShow)
            .type(ReservationUserType.TICKETING_STUDENT)
            .startTime(LocalDateTime.now())
            .endTime(LocalDateTime.now())
            .build());

        Reservation konkukSecondShowReserve = reservationRepository.save(Reservation.builder()
            .totalCount(10)
            .reservedCount(0)
            .show(konkukFirstShow)
            .type(ReservationUserType.TICKETING_STUDENT)
            .startTime(LocalDateTime.now())
            .endTime(LocalDateTime.now())
            .build());

        Ticket ticket1 = ticketRepository.save(Ticket.builder()
            .user(user)
            .event(konkuk)
            .show(konkukFirstShow)
            .reservation(konkukFirstShowReserve)
            .status(TicketStatus.BEFORE_ENTER)
            .ticketNo(UUID.randomUUID().toString())
            .build());

        Ticket ticket2 = ticketRepository.save(Ticket.builder()
            .user(user)
            .event(konkuk)
            .show(konkukSecondShow)
            .reservation(konkukSecondShowReserve)
            .status(TicketStatus.BEFORE_ENTER)
            .ticketNo(UUID.randomUUID().toString())
            .build());

        List<CheckTicketDto> tickets = ticketingService.checkUserTickets(user.getId());

        assertThat(tickets).hasSize(2);
        assertThat(tickets.get(0).userName()).isEqualTo(user.getName());
        assertThat(tickets.get(1).userName()).isEqualTo(user.getName());
    }
}
