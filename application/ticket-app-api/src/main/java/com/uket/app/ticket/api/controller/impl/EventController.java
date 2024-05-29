package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.ShowResponse;
import com.uket.app.ticket.api.dto.response.ReservationResponse;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.dto.ReservationDto;
import com.uket.domain.event.service.EventService;
import com.uket.domain.event.service.ShowService;
import com.uket.domain.event.service.ReservationService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class EventController implements EventApi {

    private final EventService eventService;
    private final ShowService showService;
    private final ReservationService reservationService;

    @Override
    public ResponseEntity<ShowResponse> getShows(Long eventId) {

        String universityName = eventService.findUniversityNameByEventId(eventId);
        List<ShowDto> shows = showService.findByEventId(eventId);

        ShowResponse response = ShowResponse.of(universityName, shows);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<ReservationResponse> getPossibleReservations(Long showId) {

        String showName = showService.findNameById(showId);
        List<ReservationDto> reservations = reservationService.findByShowId(showId);

        ReservationResponse response = ReservationResponse.of(showName, reservations);
        return ResponseEntity.ok(response);
    }
}
