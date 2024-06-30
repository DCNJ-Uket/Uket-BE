package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.dto.ShowDto;
import com.uket.domain.event.enums.ReservationUserType;
import java.util.List;

public record ShowResponse(
        String reservationUserType,
        String universityName,
        List<ShowDto> shows
) {

    public static ShowResponse of(ReservationUserType reservationUserType, String university, List<ShowDto> shows) {
        return new ShowResponse(reservationUserType.getValue(), university, shows);
    }
}
