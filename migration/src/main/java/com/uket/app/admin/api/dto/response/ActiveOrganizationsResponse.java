package com.uket.app.admin.api.dto.response;

import lombok.Builder;

@Builder
public record ActiveOrganizationsResponse(
    Long id,
    String name
) {
}

