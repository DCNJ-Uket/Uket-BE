package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.dto.ReservationDto;
import java.util.List;

public record ReservationResponse(
        String showName,
        List<ReservationDto> reservations
) {

    public static ReservationResponse of(String showName, List<ReservationDto> reservations) {
        return new ReservationResponse(showName, reservations);
    }
}
