package com.uket.app.ticket.api.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.repository.UniversityRepository;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.enums.Platform;
import com.uket.domain.user.enums.UserRole;
import com.uket.domain.user.exception.UserException;
import com.uket.domain.user.service.UserService;
import jakarta.transaction.Transactional;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

@SpringBootTest
@Transactional
class UserRegisterServiceTest {

    private static final String UNIVERSITY_OUTSIDER = "일반인";
    private static final String UNIVERSITY_KONKUK = "건국대학교";

    @Autowired
    UserRegisterService userRegisterService;
    @Autowired
    private UserService userService;
    @Autowired
    private UniversityRepository universityRepository;

    private Users user;

    @BeforeEach
    void beforeEach() {
        CreateUserDto createUserDto = CreateUserDto.builder()
                .name("test")
                .role(UserRole.ROLE_USER)
                .platform(Platform.KAKAO)
                .platformId("1234")
                .email("abc@naver.com")
                .build();

        user = userService.saveUser(createUserDto);

        universityRepository.save(University.builder().name(UNIVERSITY_OUTSIDER).build());
        universityRepository.save(University.builder().name(UNIVERSITY_KONKUK).emailPostFix("@konkuk.ac.kr").build());
    }

    @Test
    void 회원가입시_토큰을_반환한다() {

        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .phoneNumber("01012341234")
                .build();

        AuthToken authToken = userRegisterService.register(user.getId(), createUserDetailsDto, null);

        assertThat(authToken.accessToken()).isNotNull();
        assertThat(authToken.refreshToken()).isNotNull();
        assertThat(authToken.isRegistered()).isTrue();
    }

    @Test
    void 대학을_입력할_시_해당_대학으로_처리된다() {
        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .phoneNumber("01012341234")
                .universityEmail("abc123@konkuk.ac.kr")
                .studentMajor("컴퓨터 공학부")
                .studentCode("1234")
                .build();

        userRegisterService.register(user.getId(), createUserDetailsDto, UNIVERSITY_KONKUK);
        Users findUser = userService.findById(user.getId());

        assertThat(findUser.getUniversity().getName()).isEqualTo(UNIVERSITY_KONKUK);
    }

    @Test
    void 대학을_입력하지_않을_시_외부인으로_처리된다() {
        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .phoneNumber("01012341234")
                .build();

        userRegisterService.register(user.getId(), createUserDetailsDto, null);
        Users findUser = userService.findById(user.getId());

        assertThat(findUser.getUniversity().getName()).isEqualTo(UNIVERSITY_OUTSIDER);
    }

    @Test
    void 회원가입이_완료되면_회원의_가입여부가_true가_된다() {
        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .phoneNumber("01012341234")
                .build();

        userRegisterService.register(user.getId(), createUserDetailsDto, null);
        Users findUser = userService.findById(user.getId());

        assertThat(findUser.getIsRegistered()).isTrue();
    }

    @Test
    void 회원가입이_완료되면_회원의_세부정보가_저장된다() {
        String depositorName = "홍길동";
        String phoneNumber = "01012341234";
        String email = "abc123@konkuk.ac.kr";
        String major = "컴퓨터 공학부";
        String code = "1234";

        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName(depositorName)
                .phoneNumber(phoneNumber)
                .universityEmail(email)
                .studentMajor(major)
                .studentCode(code)
                .build();

        userRegisterService.register(user.getId(), createUserDetailsDto, UNIVERSITY_KONKUK);
        Users findUser = userService.findById(user.getId());
        UserDetails userDetails = findUser.getUserDetails();

        assertThat(userDetails.getDepositorName()).isEqualTo(depositorName);
        assertThat(userDetails.getPhoneNumber()).isEqualTo(phoneNumber);
        assertThat(userDetails.getUniversityEmail()).isEqualTo(email);
        assertThat(userDetails.getStudentMajor()).isEqualTo(major);
        assertThat(userDetails.getStudentCode()).isEqualTo(code);
    }

    @Test
    void 대학_이메일과_대학이_다를_경우_예외를_반환한다() {
        CreateUserDetailsDto createUserDetailsDto = CreateUserDetailsDto.builder()
                .depositorName("홍길동")
                .phoneNumber("01012341234")
                .universityEmail("abc123@naver.com")
                .studentMajor("컴퓨터 공학부")
                .studentCode("1234")
                .build();

        Long userId = user.getId();
        Assertions.assertThatThrownBy(
                        () -> userRegisterService.register(userId, createUserDetailsDto, UNIVERSITY_KONKUK))
                .isInstanceOf(UserException.class)
                .hasMessage(ErrorCode.NOT_MATCH_UNIVERSITY_EMAIL.getMessage());
    }
}
