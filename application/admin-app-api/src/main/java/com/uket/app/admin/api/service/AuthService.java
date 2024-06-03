package com.uket.app.admin.api.service;


import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.dto.AdminAuthToken;
import com.uket.domain.auth.admin.entity.Admin;
import com.uket.domain.auth.admin.exception.AuthException;
import com.uket.domain.auth.admin.service.AdminService;
import com.uket.domain.user.enums.UserRole;
import com.uket.modules.jwt.util.JwtAuthTokenUtil;
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
    private final BCryptPasswordEncoder bCryptPasswordEncoder;
    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    public AdminAuthToken login(String email, String password) {
        Admin admin = adminService.findByEmail(email);

        validatePassword(admin, password);
        String accessToken = jwtAuthTokenUtil.createAccessToken(admin.getId(), admin.getName(),
                String.valueOf(UserRole.ROLE_ADMIN), true);

        return AdminAuthToken.from(accessToken);
    }

    private void validatePassword(final Admin admin, final String password) {
        if (!bCryptPasswordEncoder.matches(password, admin.getPassword())) {
            throw new AuthException(ErrorCode.NOT_MATCH_PASSWORD);
        }
    }
}
