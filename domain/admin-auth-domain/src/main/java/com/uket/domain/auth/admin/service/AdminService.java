package com.uket.domain.auth.admin.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.admin.entity.Admin;
import com.uket.domain.auth.admin.exception.AuthException;
import com.uket.domain.auth.admin.repository.AdminRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AdminService {

    private final AdminRepository adminRepository;

    public Admin findByEmail(String email) {
        return adminRepository.findByEmail(email)
                .orElseThrow(() -> new AuthException(ErrorCode.NOT_FOUND_EMAIL_OF_ADMIN));
    }

    @Transactional
    public Admin save(String email, String password, String name) {

        if (Boolean.TRUE.equals(adminRepository.existsByEmail(email))) {
            throw new AuthException(ErrorCode.ALREADY_EXIST_USER);
        }

        Admin admin = Admin.builder()
                .email(email)
                .password(password)
                .name(name)
                .isRegistered(false)
                .build();

        return adminRepository.save(admin);
    }
}
