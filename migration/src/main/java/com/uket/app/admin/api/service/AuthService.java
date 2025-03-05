package com.uket.app.admin.api.service;


import com.uket.app.admin.api.exception.AdminException;
import com.uket.app.domain.user.AdminRole;
import com.uket.app.exception.ErrorCode;
import com.uket.app.admin.auth.dto.AdminAuthToken;
import com.uket.app.domain.user.Admin;
import com.uket.app.exception.AuthException;
import com.uket.app.admin.auth.service.AdminService;
import com.uket.domain.university.entity.University;
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
    public Admin registerWithoutPassword(Long userId, String name, String email, University university, AdminRole role)
        throws MessagingException {
        if(Boolean.FALSE.equals(adminService.checkAdministratorUser(userId))) {
            throw new AdminException(ErrorCode.NOT_ADMINISTRATOR_REGISTER);
        }
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
