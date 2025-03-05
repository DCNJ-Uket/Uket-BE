package com.uket.app.user.admin.dto.response;

import com.uket.app.user.admin.dto.SearchAdminDto;
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
