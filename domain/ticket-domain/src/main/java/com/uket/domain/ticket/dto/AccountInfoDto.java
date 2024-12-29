package com.uket.domain.ticket.dto;

public record AccountInfoDto(
        String accountNumber,
        String accountOwner,
        String depositUrl
) {
    public static AccountInfoDto of(String accountNumber, String accountOwner, String depositUrl) {
        return new AccountInfoDto(accountNumber, accountOwner, depositUrl);
    }
}
