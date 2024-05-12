package com.uket.modules.jwt.util;

import static com.uket.modules.jwt.constants.JwtValues.JWT_PAYLOAD_KEY_CATEGORY;
import static com.uket.modules.jwt.constants.JwtValues.JWT_PAYLOAD_KEY_ID;
import static com.uket.modules.jwt.constants.JwtValues.JWT_PAYLOAD_VALUE_TICKET;

import com.uket.modules.jwt.constants.JwtValues;
import com.uket.modules.jwt.properties.TokenProperties;
import io.jsonwebtoken.Jwts;
import java.util.Date;
import javax.crypto.SecretKey;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class JwtTicketUtil {

    private final TokenProperties tokenProperties;
    private final SecretKey secretKey;

    public Long getTicketId(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JwtValues.JWT_PAYLOAD_KEY_ID, Long.class);
    }

    public String createTicketToken(Long ticketId) {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .claim(JWT_PAYLOAD_KEY_CATEGORY, JWT_PAYLOAD_VALUE_TICKET)
                .claim(JWT_PAYLOAD_KEY_ID, ticketId)
                .issuedAt(new Date(now))
                .expiration(getTicketInfoExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    private Date getTicketInfoExpiration(long now) {
        return new Date(now + tokenProperties.expiration().ticketExpiration());
    }
}
