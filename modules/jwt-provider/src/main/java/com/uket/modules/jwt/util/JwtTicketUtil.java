package com.uket.modules.jwt.util;

import com.uket.modules.jwt.properties.TokenProperties;
import io.jsonwebtoken.Jwts;
import java.util.Date;
import javax.crypto.SecretKey;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class JwtTicketUtil {

    private final TokenProperties tokenProperties;
    private final SecretKey secretKey;

    public String createTicketInfo() {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .issuedAt(new Date(now))
                .expiration(getTicketInfoExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    private Date getTicketInfoExpiration(long now) {
        return new Date(now + tokenProperties.expiration().ticketExpiration());
    }
}
