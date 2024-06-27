package com.uket.app.ticket.api.service;

import com.uket.app.ticket.api.properties.EmailProperties;
import com.uket.app.ticket.api.util.RandomCodeGenerator;
import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.exception.AuthException;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.service.UserService;
import com.uket.modules.redis.service.RedisUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UserAuthEmailService {

    private static final String AUTH_CODE_PREFIX = "AuthCode:";

    private final MailService mailService;
    private final UserService userService;
    private final UniversityService universityService;

    private final EmailProperties emailProperties;
    private final RedisUtil redisUtil;

    public void sendAuthEmail(String email, Long universityId) {
        validateEmail(email, universityId);

        String subject = "Uket 회원가입 인증 코드입니다.";
        String authCode = RandomCodeGenerator.generateRandomCode();
        Long authCodeExpirationMillis = emailProperties.properties().authCodeExpirationMillis();

        mailService.sendEmail(email, subject, authCode);
        redisUtil.setDataExpire(AUTH_CODE_PREFIX + email, authCode, authCodeExpirationMillis);
    }

    public void verifyAuthEmail(String email, Long universityId, String authCode) {
        validateEmail(email, universityId);

        String savedAuthCode = redisUtil.getData(AUTH_CODE_PREFIX + email)
                .orElseThrow(() -> new AuthException(ErrorCode.INVALID_AUTH_CODE));

        if (!authCode.equals(savedAuthCode)) {
            throw new AuthException(ErrorCode.NOT_MATCHED_AUTH_CODE);
        }
    }

    private void validateEmail(String email, Long universityId) {
        userService.checkDuplicateEmail(email);
        universityService.checkEmailPrefix(email, universityId);
    }
}
