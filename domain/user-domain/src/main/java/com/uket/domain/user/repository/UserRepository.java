package com.uket.domain.user.repository;

import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.entity.Users;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRepository extends JpaRepository<Users,Long> {

    Optional<Users> findByPlatformAndPlatformId(Platform platform, String platformId);
}
