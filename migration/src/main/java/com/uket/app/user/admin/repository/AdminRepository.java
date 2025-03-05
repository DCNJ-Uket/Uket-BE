package com.uket.app.user.admin.repository;

import com.uket.app.user.admin.entity.Admin;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AdminRepository extends JpaRepository<Admin, Long> {

    Optional<Admin> findByEmail(String email);
    Boolean existsByEmail(String email);
}
