package com.uket.domain.user.service;

import com.uket.domain.user.dto.CreateUserDto;
import com.uket.domain.user.entity.Users;
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
            return updateEmailAndNameOfExistUser(createUserDto, existUser.get());
        }

        Users newUser = Users.builder()
                .name(createUserDto.name())
                .email(createUserDto.email())
                .platform(createUserDto.platform())
                .platformId(createUserDto.platformId())
                .role(createUserDto.role())
                .build();

        return userRepository.save(newUser);
    }

    private Users updateEmailAndNameOfExistUser(CreateUserDto createUserDto, Users existUser) {
        existUser.updateEmailAndName(createUserDto.email(), createUserDto.name());
        userRepository.save(existUser);
        return existUser;
    }
}
