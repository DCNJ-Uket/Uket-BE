package com.uket.modules.jwt.util;

import com.uket.modules.jwt.constants.JwtValues;
import com.uket.modules.jwt.properties.TokenProperties;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.SignatureException;
import java.util.Date;
import java.util.UUID;
import javax.crypto.SecretKey;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class JwtAuthTokenUtil {

    private final TokenProperties tokenProperties;
    private final SecretKey secretKey;

    public String getCategory(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, String.class);
    }

    public Long getId(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JwtValues.JWT_PAYLOAD_KEY_ID, Long.class);
    }

    public String getName(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JwtValues.JWT_PAYLOAD_KEY_NAME, String.class);
    }

    public String getRole(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JwtValues.JWT_PAYLOAD_KEY_ROLE, String.class);
    }

    public Boolean isRegistered(String token) {
        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JwtValues.JWT_PAYLOAD_KEY_REGISTERED, Boolean.class);
    }

    public Boolean isExpired(String token) {
        try {
            Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token)
                    .getPayload()
                    .getExpiration();
        } catch (ExpiredJwtException e) {
            return true;
        }
        return false;
    }

    public Boolean isValidToken(String token) {
        try {
            Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token);
        } catch (SignatureException e) {
            return false;
        }
        return true;
    }

    public String createAccessToken(Long userId, String name, String role, Boolean isRegistered) {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .claim(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, JwtValues.JWT_PAYLOAD_VALUE_ACCESS)
                .claim(JwtValues.JWT_PAYLOAD_KEY_ID, userId)
                .claim(JwtValues.JWT_PAYLOAD_KEY_NAME, name)
                .claim(JwtValues.JWT_PAYLOAD_KEY_ROLE, role)
                .claim(JwtValues.JWT_PAYLOAD_KEY_REGISTERED, isRegistered)
                .issuedAt(new Date(now))
                .expiration(getAccessTokenExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    public String createAccessToken(Long userId, String name, String role, Boolean isRegistered, Long expiration) {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .claim(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, JwtValues.JWT_PAYLOAD_VALUE_ACCESS)
                .claim(JwtValues.JWT_PAYLOAD_KEY_ID, userId)
                .claim(JwtValues.JWT_PAYLOAD_KEY_NAME, name)
                .claim(JwtValues.JWT_PAYLOAD_KEY_ROLE, role)
                .claim(JwtValues.JWT_PAYLOAD_KEY_REGISTERED, isRegistered)
                .issuedAt(new Date(now))
                .expiration(new Date(expiration))
                .signWith(secretKey)
                .compact();
    }

    public String createRefreshToken() {
        long now = System.currentTimeMillis();
        UUID uuid = UUID.randomUUID();

        return Jwts.builder()
                .claim(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, JwtValues.JWT_PAYLOAD_VALUE_REFRESH)
                .claim(JwtValues.JWT_PAYLOAD_KEY_UUID, uuid)
                .issuedAt(new Date(now))
                .expiration(getRefreshTokenExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    public String createRefreshToken(Long expiration) {
        long now = System.currentTimeMillis();
        UUID uuid = UUID.randomUUID();

        return Jwts.builder()
                .claim(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, JwtValues.JWT_PAYLOAD_VALUE_REFRESH)
                .claim(JwtValues.JWT_PAYLOAD_KEY_UUID, uuid)
                .issuedAt(new Date(now))
                .expiration(new Date(expiration))
                .signWith(secretKey)
                .compact();
    }

    private Date getAccessTokenExpiration(long now) {
        return new Date(now + tokenProperties.expiration().accessTokenExpiration());
    }

    private Date getRefreshTokenExpiration(long now) {
        return new Date(now + tokenProperties.expiration().refreshTokenExpiration());
    }
}
