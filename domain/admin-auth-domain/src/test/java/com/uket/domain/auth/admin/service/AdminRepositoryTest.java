package com.uket.domain.auth.admin.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.uket.domain.auth.admin.entity.Admin;
import com.uket.domain.auth.admin.repository.AdminRepository;
import com.uket.domain.university.entity.University;
import jakarta.persistence.EntityManager;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@DataJpaTest
public class AdminRepositoryTest {
    @Autowired private AdminRepository adminRepository;
    @Autowired private EntityManager em;

    @Test
    @DisplayName("어드민 findAll(Pageable) 테스트")
    void testFindAllAdmin() {
        // given
        University university = University.builder()
                .name("universityA")
                .build();
        em.persist(university);
        Admin admin1 = Admin.builder()
                .university(university)
                .build();
        Admin admin2 = Admin.builder()
                .university(university)
                .build();

        adminRepository.save(admin1);
        adminRepository.save(admin2);

        // when
        Page<Admin> findAdmins = adminRepository.findAll(PageRequest.of(0, 2));

        // then
        List<Admin> admins = findAdmins.getContent();
        assertThat(admins.size()).isEqualTo(2);
        assertThat(admins.stream().anyMatch(a -> a.getId() == 1L)).isEqualTo(true);
        assertThat(admins.stream().anyMatch(a -> a.getId() == 2L)).isEqualTo(true);
        assertThat(admins.get(0).getUniversity().getName()).isEqualTo("universityA");
        System.out.println("admins = " + admins);
    }

    @Configuration
    @EnableAutoConfiguration
    @EntityScan(basePackages = { "com.uket.domain.auth.admin", "com.uket.domain.university" })
    @EnableJpaRepositories(basePackages = "com.uket.domain.auth.admin")
    static class RepositoryConfigure {
    }
}
