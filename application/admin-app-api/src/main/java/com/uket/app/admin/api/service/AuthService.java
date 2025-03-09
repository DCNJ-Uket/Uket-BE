package com.uket.app.admin.api.service;


import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.dto.AdminAuthToken;
import com.uket.domain.auth.admin.entity.Admin;
import com.uket.domain.auth.admin.entity.AdminRole;
import com.uket.domain.auth.admin.exception.AuthException;
import com.uket.domain.auth.admin.service.AdminService;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.exception.UniversityException;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.enums.UserRole;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
import jakarta.mail.MessagingException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class AuthService {

    private final AdminService adminService;
    private final UniversityService universityService;
    private final UserAuthEmailService userAuthEmailService;
    private final BCryptPasswordEncoder bCryptPasswordEncoder;
    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    public AdminAuthToken login(String email, String password) {
        Admin admin = adminService.findByEmail(email);

        validateRegistered(admin);
        validatePassword(admin, password);

        String accessToken = jwtAuthTokenUtil.createAccessToken(admin.getId(), admin.getName(),
                String.valueOf(UserRole.ROLE_ADMIN), true);

        return AdminAuthToken.from(accessToken, admin.getName());
    }

    @Transactional
    public Admin registerToEmail(String email, String password, String name) {

        String encodedPassword = bCryptPasswordEncoder.encode(password);

        return adminService.save(email, encodedPassword, name);
    }

    @Transactional
    public Admin registerWithoutPassword(String name, String email, String organization, AdminRole role)
        throws MessagingException {
        University university = universityService.findByName(organization).orElseThrow(() -> new UniversityException(ErrorCode.NOT_FOUND_UNIVERSITY));
        Admin admin = adminService.saveWithoutPassword(name,email, university, role);
        userAuthEmailService.sendAdminAuthEmail(email);
        return admin;
    }

    private void validateRegistered(Admin admin) {
        if (Boolean.FALSE.equals(admin.getIsRegistered())) {
            throw new AuthException(ErrorCode.NOT_REGISTERED_ADMIN);
        }
    }

    private void validatePassword(final Admin admin, final String password) {
        if (!bCryptPasswordEncoder.matches(password, admin.getPassword())) {
            throw new AuthException(ErrorCode.NOT_MATCH_PASSWORD);
        }
    }
}
