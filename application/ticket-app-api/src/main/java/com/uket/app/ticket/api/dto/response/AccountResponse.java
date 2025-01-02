package com.uket.app.ticket.api.dto.response;

import com.uket.domain.event.entity.Account;
import lombok.Builder;

@Builder
public record AccountResponse(
        String accountNumber,
        String accountOwner,
        String depositUrl,
        Integer ticketPrice
) {
    public static AccountResponse from(Account account) {
        return AccountResponse.builder()
                .accountNumber(account.getAccountNumber())
                .accountOwner(account.getAccountOwner())
                .depositUrl(account.getDepositUrl())
                .ticketPrice(account.getTicketPrice())
                .build();
    }
}
