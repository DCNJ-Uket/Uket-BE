package com.uket.app.admin.api.dto.response;

import org.springframework.data.domain.Page;
import java.util.List;

public record CustomPageResponse<T>(
    List<T> content         ,
    int pageNumber,
    int pageSize,
    boolean first,
    boolean last,
    long totalElements,
    int totalPages,
    int numberOfElements,
    boolean empty
) {
    public CustomPageResponse(Page<T> page) {
        this(
            page.getContent(),
            page.getNumber() + 1,
            page.getSize(),
            page.isFirst(),
            page.isLast(),
            page.getTotalElements(),
            page.getTotalPages(),
            page.getNumberOfElements(),
            page.isEmpty()
        );
    }
}

