package com.uket.app.admin.auth.repository;

import com.uket.app.domain.user.Admin;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AdminRepository extends JpaRepository<Admin, Long> {

    Optional<Admin> findByEmail(String email);
    Boolean existsByEmail(String email);
}
