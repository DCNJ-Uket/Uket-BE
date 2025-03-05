package com.uket.app.admin.api.dto.response;

import com.uket.app.domain.user.Admin;

public record AdminRegisterResponse(
        Long adminId,
        String name
) {

    public static AdminRegisterResponse of(Admin admin) {
        return new AdminRegisterResponse(admin.getId(), admin.getName());
    }
}
