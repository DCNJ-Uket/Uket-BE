package com.uket.domain.auth.admin.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import com.uket.domain.auth.admin.dto.SearchAdminDto;
import com.uket.domain.auth.admin.entity.Admin;
import com.uket.domain.auth.admin.repository.AdminRepository;
import com.uket.domain.university.entity.University;
import java.util.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

@ExtendWith(MockitoExtension.class)
class AdminServiceTest {

    @Mock
    private AdminRepository adminRepository;

    @InjectMocks
    private AdminService adminService;

    @Test
    @DisplayName("어드민 전체 조회 테스트")
    void testSearchAdmin() {
        // given
        University university = University.builder()
                .id(1L)
                .name("universityA")
                .build();
        Admin admin1 = Admin.builder()
                .id(1L)
                .university(university)
                .build();
        Admin admin2 = Admin.builder()
                .id(2L)
                .university(university)
                .build();
        Admin admin3 = Admin.builder()
                .id(3L)
                .university(university)
                .build();

        PageRequest pageRequest = PageRequest.of(0, 2);
        when(adminRepository.findAll(pageRequest)).thenReturn(
                new PageImpl<>(List.of(admin1, admin2))
        );

        // when
        Page<SearchAdminDto> searchAdminDtos = adminService.searchAllAdmins(pageRequest);

        // then
        List<SearchAdminDto> adminDtos = searchAdminDtos.getContent();
        assertThat(adminDtos.size()).isEqualTo(2);
        assertThat(adminDtos.get(0).universityName()).isEqualTo("universityA");
        System.out.println("adminDtos = " + adminDtos);
    }

}