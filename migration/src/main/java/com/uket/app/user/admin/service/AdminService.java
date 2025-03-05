package com.uket.app.user.admin.service;

import com.uket.app.global.exception.ErrorCode;
import com.uket.app.user.admin.dto.SearchAdminDto;
import com.uket.app.user.admin.entity.Admin;
import com.uket.app.user.admin.repository.AdminRepository;
import com.uket.domain.auth.admin.exception.AuthException;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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
            throw new AuthException(ErrorCode.ALREADY_EXIST_ADMIN);
        }

        Admin admin = Admin.builder()
                .email(email)
                .password(password)
                .name(name)
                .isRegistered(false)
                .build();

        return adminRepository.save(admin);
    }

    @Transactional
    public Page<SearchAdminDto> searchAllAdmins(Pageable pageable) {
        Page<Admin> admins = adminRepository.findAll(pageable);
        return admins.map(SearchAdminDto::from);
    }
}
