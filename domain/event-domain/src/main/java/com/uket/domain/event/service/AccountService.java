package com.uket.domain.event.service;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.event.entity.Account;
import com.uket.domain.event.entity.Events;
import com.uket.domain.event.enums.ReservationUserType;
import com.uket.domain.event.exception.EventException;
import com.uket.domain.event.repository.AccountRepository;
import com.uket.domain.event.repository.EventRepository;
import com.uket.domain.university.entity.University;
import com.uket.domain.university.exception.UniversityException;
import com.uket.domain.university.service.UniversityService;
import com.uket.domain.user.entity.Users;
import com.uket.domain.user.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AccountService {

    private final AccountRepository accountRepository;

    public Account findById(Long accountId) {
        return accountRepository.findById(accountId)
                .orElseThrow(() -> new EventException(ErrorCode.UNKNOWN_SERVER_ERROR));
    }

}
