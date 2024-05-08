package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.dto.TicketingDto;
import java.util.List;

public record TicketingResponse(
        String showName,
        List<TicketingDto> ticketings
) {

    public static TicketingResponse of(String showName, List<TicketingDto> ticketings) {
        return new TicketingResponse(showName, ticketings);
    }
}
