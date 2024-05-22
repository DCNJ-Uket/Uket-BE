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
        Long universityId,
        String studentMajor,
        String studentCode,
        String universityEmail
) {

    public static UserInfoDto of(Users user, UserDetails userDetails, Long universityId) {
        return UserInfoDto.builder()
                .name(user.getName())
                .depositorName(userDetails.getDepositorName())
                .profileImage(user.getProfileImage())
                .isRegistered(user.getIsRegistered())
                .phoneNumber(userDetails.getPhoneNumber())
                .universityId(universityId)
                .studentMajor(userDetails.getStudentMajor())
                .studentCode(userDetails.getStudentCode())
                .universityEmail(userDetails.getUniversityEmail())
                .build();
    }
}
