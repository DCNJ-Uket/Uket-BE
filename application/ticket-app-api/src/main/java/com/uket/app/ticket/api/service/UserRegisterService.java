package com.uket.app.ticket.api.service;

import com.uket.app.ticket.api.util.AuthTokenGenerator;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.service.UserDetailsService;
import com.uket.domain.user.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UserRegisterService {

    private final UserService userService;
    private final UniversityService universityService;
    private final UserDetailsService userDetailsService;
    private final AuthTokenGenerator authTokenGenerator;

    @Transactional
    public AuthToken register(Long userId, CreateUserDetailsDto createUserDetailsDto, String university) {
        Users findUser = userService.findById(userId);

        University findUniversity = universityService.findByName(university)
                .orElse(universityService.getDefault());

        String universityEmail = createUserDetailsDto.universityEmail();
        if (Boolean.FALSE.equals(universityService.isDefault(findUniversity))) {
            findUniversity.validateUniversityEmail(universityEmail);
        }

        UserDetails userDetails = userDetailsService.saveUserDetails(createUserDetailsDto);
        findUser.register(userDetails, findUniversity);
        return authTokenGenerator.generateAuthToken(findUser);
    }
}
