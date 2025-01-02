package com.uket.app.ticket.api.dto.response;

import com.uket.domain.ticket.dto.AccountInfoDto;

public record AccountInfoResponse(
        String accountNumber,
        String accountOwner,
        String depositUrl
) {
    public static AccountInfoResponse of(AccountInfoDto accountInfoDto) {
        String aN = accountInfoDto.accountNumber();
        String aO = accountInfoDto.accountOwner();
        String dU = accountInfoDto.depositUrl();
        return new AccountInfoResponse(aN, aO, dU);
    }
}
