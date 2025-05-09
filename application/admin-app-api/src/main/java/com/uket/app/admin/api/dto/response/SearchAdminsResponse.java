package com.uket.app.admin.api.dto.response;

import com.uket.domain.auth.admin.dto.SearchAdminDto;
import java.util.List;
import lombok.Builder;

@Builder
public record SearchAdminsResponse(
        List<SearchAdminDto> memberList
) {
    public static SearchAdminsResponse from(List<SearchAdminDto> dtoList) {
        return SearchAdminsResponse.builder()
                .memberList(dtoList)
                .build();
    }
}
