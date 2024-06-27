package com.uket.app.ticket.api.service;

import com.uket.app.ticket.api.properties.EmailProperties;
import com.uket.app.ticket.api.util.RandomCodeGenerator;
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
    private final EmailProperties emailProperties;
    private final RedisUtil redisUtil;

    public void sendAuthEmail(String email) {
        userService.checkDuplicateEmail(email);

        String subject = "Uket 회원가입 인증 코드입니다.";
        String authCode = RandomCodeGenerator.generateRandomCode();
        Long authCodeExpirationMillis = emailProperties.properties().authCodeExpirationMillis();

        mailService.sendEmail(email, subject, authCode);
        redisUtil.setDataExpire(AUTH_CODE_PREFIX + email, authCode, authCodeExpirationMillis);
    }

}
