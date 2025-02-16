package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.EventApi;
import com.uket.app.ticket.api.dto.response.AccountResponse;
import com.uket.app.ticket.api.dto.response.ShowResponse;
import com.uket.app.ticket.api.dto.response.ReservationResponse;
import com.uket.app.ticket.api.dto.response.SurveyResponse;
import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.dto.ReservationDto;
import com.uket.domain.event.entity.Account;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.event.service.EventService;
import com.uket.domain.event.service.ShowService;
import com.uket.domain.event.service.ReservationService;
import com.uket.domain.form.dto.FormDto;
import com.uket.domain.form.dto.OptionDto;
import com.uket.domain.form.entity.Form;
import com.uket.domain.form.entity.Survey;
import com.uket.domain.form.service.FormService;
import java.util.ArrayList;
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
    private final FormService formService;

    @Override
    public ResponseEntity<ShowResponse> getShows(Long userId, Long eventId) {

        Events event = eventService.findById(eventId);
        String eventName = event.getName();

        String universityName = eventService.findUniversityNameByEventId(eventId);
        ReservationUserType reservationUserType = eventService.getReservationUserTypeByUniversityName(userId, universityName);

        List<ShowDto> shows = showService.findByEventId(eventId);

        ShowResponse response = ShowResponse.of(reservationUserType, universityName, eventName, shows);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<SurveyResponse> getSurveys(Long eventId) {
        Survey survey = eventService.findSurveyById(eventId);
        List<FormDto> formDtos = new ArrayList<>();
        List<Form> forms = formService.findFormsBySurveyId(survey.getId());
        for(Form form : forms) {
            List<OptionDto> options = formService.findOptionsByFormId(form.getId());
            formDtos.add(FormDto.from(form, options));
        }
        SurveyResponse response = SurveyResponse.from(survey, formDtos);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<SurveyResponse> getSurveys(Long eventId) {
        Survey survey = eventService.findSurveyById(eventId);
        List<FormDto> formDtos = new ArrayList<>();
        List<Form> forms = formService.findFormsBySurveyId(survey.getId());
        for(Form form : forms) {
            List<OptionDto> options = formService.findOptionsByFormId(form.getId());
            formDtos.add(FormDto.from(form, options));
        }
        SurveyResponse response = SurveyResponse.from(survey, formDtos);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<ReservationResponse> getPossibleReservations(Long showId, String userType) {

        String showName = showService.findNameById(showId);
        ReservationUserType reservationUserType = ReservationUserType.fromString(userType);
        List<ReservationDto> reservations = reservationService.findByShowIdAndReservationUserType(showId,reservationUserType);

        ReservationResponse response = ReservationResponse.of(showName, reservations);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<AccountResponse> getAccount(Long userId, Long eventId) {
        Account account = eventService.findAccountByEventId(eventId);
        AccountResponse accountResponse = AccountResponse.from(account);
        return ResponseEntity.ok(accountResponse);
    }

}
