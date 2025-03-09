package com.uket.domain.auth.admin.dto;

import com.uket.domain.auth.admin.entity.Admin;
import com.uket.domain.auth.admin.entity.AdminRole;
import com.uket.domain.university.entity.University;
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
        University university = admin.getUniversity();
        return SearchAdminDto.builder()
                .adminId(admin.getId())
                .role(admin.getRole())
                .universityName(university.getName())
                .email(admin.getEmail())
                .name(admin.getName())
                .build();
    }

}
