package com.uket.app.admin.api.service;

import com.uket.app.exception.AuthException;
import com.uket.app.exception.ErrorCode;
import com.uket.app.properties.EmailProperties;
import com.uket.app.util.RandomCodeGenerator;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.service.UserService;
import com.uket.modules.redis.util.RedisUtil;
import jakarta.mail.MessagingException;
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

    public void sendAdminAuthEmail(String email) throws MessagingException {
        validateOnlyEmail(email);

        //TODO: 샘플 이미지 변경 및 링크 변수화 필요
        String subject = "[Uket admin] 회원가입 링크 안내";
        String content = "<html>" +
            "<body>" +
            "<img src='https://fastly.picsum.photos/id/10/2500/1667.jpg?hmac=J04WWC_ebchx3WwzbM-Z4_KC_LeLBWr5LZMaAkWkF68' " +
            "alt='Uket Logo' " +
            "style='width: 200px; height: auto; margin-bottom: 20px;'>" +
            "<p>안녕하세요, Uket팀입니다.</p>" +
            "<p>사용자 추가가 완료되어 하단의 회원가입 링크를 전달드립니다.</p>" +
            "<br><p>회원가입을 완료한 후, 어드민 서비스를 이용해주세요.</p>" +
            "<br><p style = 'font-weight: bold;'>Uket admin 회원가입 링크 : " +
            "<a href='https://uket.site'>회원가입 바로가기</a>" +
            "</p>" +
            "<br><p>감사합니다.</p>" +
            "</body>" +
            "</html>";

        mailService.sendEmailWithHtmlContent(email, subject, content);
    }

    private void validateEmail(String email, Long universityId) {
        userService.checkDuplicateEmail(email);
        universityService.checkEmailPrefix(email, universityId);
    }

    private void validateOnlyEmail(String email) {
        userService.checkDuplicateEmail(email);
    }
}
