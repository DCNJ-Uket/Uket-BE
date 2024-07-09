package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.UserApi;
import com.uket.app.ticket.api.dto.request.UserInfoUpdateRequest;
import com.uket.app.ticket.api.dto.request.UserRegisterRequest;
import com.uket.app.ticket.api.dto.response.AuthResponse;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.service.TicketInfoService;
import com.uket.app.ticket.api.service.TicketingService;
import com.uket.app.ticket.api.service.UserRegisterService;
import com.uket.domain.auth.dto.response.AuthToken;
import com.uket.domain.ticket.dto.CheckTicketDto;
import com.uket.domain.user.dto.CreateUserDetailsDto;
import com.uket.domain.user.dto.UserInfoDto;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.service.UserDetailsService;
import com.uket.domain.user.service.UserService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
@RequiredArgsConstructor
public class UserController implements UserApi {

    private final UserService userService;
    private final UserDetailsService userDetailsService;
    private final UserRegisterService userRegisterService;
    private final TicketInfoService ticketInfoService;

    @Override
    public ResponseEntity<AuthResponse> register(Long userId, UserRegisterRequest request) {

        CreateUserDetailsDto createUserDetailsDto = generateCreateUserDetailsDto(request);

        AuthToken authToken = userRegisterService.register(userId, createUserDetailsDto, request.university());
        Users findUser = userService.findById(userId);

        AuthResponse response = AuthResponse.of(findUser,authToken);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<UserInfoDto> getUserInfo(Long userId) {
         UserInfoDto userInfoDto = userService.getUserInfo(userId);

        return ResponseEntity.ok(userInfoDto);
    }

    @Override
    public ResponseEntity<ListResponse<CheckTicketDto>> getUserTickets(Long userId) {
        List<CheckTicketDto> tickets = ticketInfoService.getUserTickets(userId);
        ListResponse<CheckTicketDto> response = ListResponse.from(tickets);
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<UserInfoDto> updateUserInfo(Long userId, UserInfoUpdateRequest request) {
        UserInfoDto userInfoDto = userDetailsService.updateUserInfo(userId, request.depositorName(), request.phoneNumber());

        return ResponseEntity.ok(userInfoDto);
    }

    private CreateUserDetailsDto generateCreateUserDetailsDto(UserRegisterRequest request) {
        return CreateUserDetailsDto.builder()
                .depositorName(request.depositorName())
                .phoneNumber(request.phoneNumber())
                .universityEmail(request.universityEmail())
                .studentMajor(request.studentMajor())
                .studentCode(request.studentCode())
                .build();
    }
}
