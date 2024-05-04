package com.uket.domain.user.service;

import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.repository.UserDetailsRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UserDetailsService {

    private final UserDetailsRepository userDetailsRepository;

    @Transactional
    public UserDetails saveUserDetails(CreateUserDetailsDto createUserDetailsDto) {

        UserDetails userDetails = UserDetails.builder()
                .depositorName(createUserDetailsDto.depositorName())
                .phoneNumber(createUserDetailsDto.phoneNumber())
                .studentMajor(createUserDetailsDto.studentMajor())
                .studentCode(createUserDetailsDto.studentCode())
                .build();

        return userDetailsRepository.save(userDetails);
    }
}
