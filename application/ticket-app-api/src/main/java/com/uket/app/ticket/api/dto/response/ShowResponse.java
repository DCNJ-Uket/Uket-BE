package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.dto.ShowDto;
import java.util.List;

public record ShowResponse(
        String universityName,
        List<ShowDto> shows
) {

    public static ShowResponse of(String university, List<ShowDto> shows) {
        return new ShowResponse(university, shows);
    }
}
