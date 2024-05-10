package com.uket.app.ticket.api.dto.response;

import java.util.List;

public record ListResponse<T>(
        List<T> items
) {

    public static <T> ListResponse<T> from(List<T> items) {
        return new ListResponse<>(items);
    }
}
