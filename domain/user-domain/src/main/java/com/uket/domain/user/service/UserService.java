package com.uket.domain.user.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.university.entity.University;
import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.dto.UserInfoDto;
import com.uket.domain.user.entity.UserDetails;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.exception.UserException;
import com.uket.domain.user.repository.UserRepository;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class UserService {

    private final UserRepository userRepository;

    @Transactional
    public Users saveUser(CreateUserDto createUserDto) {

        Optional<Users> existUser = userRepository.findByPlatformAndPlatformId(
                createUserDto.platform(),
                createUserDto.platformId());

        if (existUser.isPresent()) {
            return updateProfileOfExistUser(createUserDto, existUser.get());
        }

        Users newUser = Users.builder()
                .name(createUserDto.name())
                .email(createUserDto.email())
                .profileImage(createUserDto.profileImage())
                .platform(createUserDto.platform())
                .platformId(createUserDto.platformId())
                .role(createUserDto.role())
                .isRegistered(false)
                .build();

        return userRepository.save(newUser);
    }

    public Users findById(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new UserException(ErrorCode.NOT_FOUND_USER));
    }

    public UserInfoDto getUserInfo(Long userId) {
        Users findUser = findById(userId);

        UserDetails userDetails = findUser.getUserDetails();
        University university = findUser.getUniversity();

        return UserInfoDto.of(findUser, userDetails, university.getName());
    }

    private Users updateProfileOfExistUser(CreateUserDto createUserDto, Users existUser) {
        existUser.updateProfile(createUserDto.email(), createUserDto.name(), createUserDto.profileImage());
        userRepository.save(existUser);
        return existUser;
    }
}
