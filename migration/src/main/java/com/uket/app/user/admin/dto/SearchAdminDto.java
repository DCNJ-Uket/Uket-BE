package com.uket.app.user.admin.dto;

import com.uket.app.user.admin.entity.Admin;
import com.uket.app.user.admin.enums.AdminRole;
import lombok.Builder;

@Builder
public record SearchAdminDto(
        Long adminId,
        AdminRole role,
        String universityName,
        String email,
        String name
) {
    public static SearchAdminDto from(Admin admin) {
        return SearchAdminDto.builder()
                .adminId(admin.getId())
                .role(admin.getRole())
                .universityName(admin.getUniversity().getName())
                .email(admin.getEmail())
                .name(admin.getName())
                .build();
    }

}
