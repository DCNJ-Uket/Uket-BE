package com.uket.domain.user.service;

import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.dto.UserInfoDto;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.repository.UserDetailsRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UserDetailsService {

    private final UserService userService;
    private final UserDetailsRepository userDetailsRepository;

    @Transactional
    public UserDetails saveUserDetails(CreateUserDetailsDto createUserDetailsDto) {

        UserDetails userDetails = UserDetails.builder()
                .depositorName(createUserDetailsDto.depositorName())
                .phoneNumber(createUserDetailsDto.phoneNumber())
                .universityEmail(createUserDetailsDto.universityEmail())
                .studentMajor(createUserDetailsDto.studentMajor())
                .studentCode(createUserDetailsDto.studentCode())
                .build();

        return userDetailsRepository.save(userDetails);
    }

    @Transactional
    public UserInfoDto updateUserInfo(Long userId, String depositorName, String phoneNumber) {
        Users findUser = userService.findById(userId);
        UserDetails userDetails = findUser.getUserDetails();
        userDetails.updateUserInfo(depositorName, phoneNumber);

        userDetailsRepository.save(userDetails);
        return UserInfoDto.of(findUser, userDetails, findUser.getUniversity().getName());
    }
}
