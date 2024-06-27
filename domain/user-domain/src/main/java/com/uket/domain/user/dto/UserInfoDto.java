package com.uket.domain.user.dto;

import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import lombok.Builder;

@Builder
public record UserInfoDto(
        String name,
        String depositorName,
        String profileImage,
        Boolean isRegistered,
        String phoneNumber,
        String universityName,
        String studentMajor,
        String studentCode,
        String universityEmail
) {

    public static UserInfoDto of(Users user, UserDetails userDetails, String universityName) {
        return UserInfoDto.builder()
                .name(user.getName())
                .depositorName(userDetails.getDepositorName())
                .profileImage(user.getProfileImage())
                .isRegistered(user.getIsRegistered())
                .phoneNumber(userDetails.getPhoneNumber())
                .universityName(universityName)
                .studentMajor(userDetails.getStudentMajor())
                .studentCode(userDetails.getStudentCode())
                .universityEmail(userDetails.getUniversityEmail())
                .build();
    }
}
