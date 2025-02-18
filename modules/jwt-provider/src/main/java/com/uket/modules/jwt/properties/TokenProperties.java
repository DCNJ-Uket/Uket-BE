package com.uket.modules.jwt.properties;

public interface TokenProperties {
    String secretKey();
    TokenExpirationProperties expiration();

    record TokenExpirationProperties(
            Long ticketExpiration,
            Long accessTokenExpiration,
            Long refreshTokenExpiration
    ) {
        public TokenExpirationProperties {
            if (ticketExpiration == null) {
                throw new IllegalArgumentException("ticketExpiration이 null일 수 없습니다.");
            }
            if (accessTokenExpiration == null) {
                throw new IllegalArgumentException("accessTokenExpiration이 null일 수 없습니다.");
            }
            if (refreshTokenExpiration == null){
                throw new IllegalArgumentException("refreshTokenExpiration이 null일 수 없습니다.");
            }
        }
    }
}
