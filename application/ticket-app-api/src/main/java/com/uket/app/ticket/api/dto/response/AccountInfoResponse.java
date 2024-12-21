package com.uket.app.ticket.api.dto.response;

public record AccountInfoResponse(
        String accountNumber,
        String accountOwner,
        String depositUrl
) {
    public static AccountInfoResponse of(String accountNumber, String accountOwner, String depositUrl) {
        return new AccountInfoResponse(accountNumber, accountOwner, depositUrl);
    }
}
