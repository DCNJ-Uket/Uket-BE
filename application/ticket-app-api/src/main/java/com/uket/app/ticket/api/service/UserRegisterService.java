package com.uket.app.ticket.api.service;

import com.uket.app.ticket.api.util.AuthTokenGenerator;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.exception.UserException;
import com.uket.domain.user.service.UserDetailsService;
import com.uket.domain.user.service.UserService;
import com.uket.modules.redis.service.RotateTokenService;
import java.util.Optional;
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
    private final RotateTokenService rotateTokenService;

    @Transactional
    public AuthToken register(Long userId, CreateUserDetailsDto createUserDetailsDto, String university) {
        Users findUser = userService.findById(userId);

        Optional<University> findUniversity = universityService.findByName(university);

        String universityEmail = createUserDetailsDto.universityEmail();
        findUniversity.ifPresent(univ -> {
            if(universityEmail == null || !universityEmail.contains(univ.getEmailPostFix())){
                throw new UserException(ErrorCode.NOT_MATCH_UNIVERSITY_EMAIL);
            }
        });

        UserDetails userDetails = userDetailsService.saveUserDetails(createUserDetailsDto);
        findUser.register(userDetails, findUniversity.orElse(universityService.getDefault()));
        AuthToken authToken = authTokenGenerator.generateAuthToken(findUser);

        rotateTokenService.storeToken(authToken.refreshToken(), authToken.accessToken(), userId);
        return authToken;
    }
}
