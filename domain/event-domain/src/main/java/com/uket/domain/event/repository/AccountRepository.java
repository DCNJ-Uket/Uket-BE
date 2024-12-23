package com.uket.domain.event.repository;

import com.uket.domain.event.entity.Account;
import com.uket.domain.event.entity.Events;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AccountRepository extends JpaRepository<Account, Long> {

}
