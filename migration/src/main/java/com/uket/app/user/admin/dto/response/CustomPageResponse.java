package com.uket.app.user.admin.dto.response;

import java.util.List;
import org.springframework.data.domain.Page;

public record CustomPageResponse<T>(
    List<T> content,
    int pageNumber,
    int pageSize,
    boolean first,
    boolean last,
    long totalElements,
    int totalPages,
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
            page.isEmpty()
        );
    }
}

