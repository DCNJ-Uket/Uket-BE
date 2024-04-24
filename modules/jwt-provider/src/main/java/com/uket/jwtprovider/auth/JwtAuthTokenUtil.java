package com.uket.jwtprovider.auth;

import static com.uket.jwtprovider.auth.constants.JwtValues.*;

import com.uket.jwtprovider.auth.properties.TokenProperties;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.UUID;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import org.springframework.stereotype.Component;

@Component
public class JwtAuthTokenUtil {

    private final TokenProperties tokenProperties;
    private final SecretKey secretKey;

    public JwtAuthTokenUtil(TokenProperties tokenProperties) {
        this.tokenProperties = tokenProperties;

        this.secretKey = new SecretKeySpec(
                tokenProperties.secretKey().getBytes(StandardCharsets.UTF_8),
                Jwts.SIG.HS256.key().build().getAlgorithm());
    }

    public String getCategory(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JWT_PAYLOAD_KEY_CATEGORY, String.class);
    }

    public Long getId(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JWT_PAYLOAD_KEY_ID, Long.class);
    }

    public String getName(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JWT_PAYLOAD_KEY_NAME, String.class);
    }

    public String getRole(String token) {

        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JWT_PAYLOAD_KEY_ROLE, String.class);
    }

    public Boolean isRegistered(String token) {
        return Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token).getPayload()
                .get(JWT_PAYLOAD_KEY_REGISTERED, Boolean.class);
    }

    public Boolean isExpired(String token) {
        long now = System.currentTimeMillis();
        try {
            Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token)
                    .getPayload()
                    .getExpiration().before(new Date(now));
        } catch (ExpiredJwtException e) {
            return true;
        }
        return false;
    }

    public Boolean isValidToken(String token) {
        try {
            Jwts.parser().verifyWith(secretKey).build().parseSignedClaims(token);
        } catch (Exception e) {
            return false;
        }
        return true;
    }

    public String createAccessToken(Long userId, String name, String role, Boolean isRegistered) {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .claim(JWT_PAYLOAD_KEY_CATEGORY, JWT_PAYLOAD_VALUE_ACCESS)
                .claim(JWT_PAYLOAD_KEY_ID, userId)
                .claim(JWT_PAYLOAD_KEY_NAME, name)
                .claim(JWT_PAYLOAD_KEY_ROLE, role)
                .claim(JWT_PAYLOAD_KEY_REGISTERED, isRegistered)
                .issuedAt(new Date(now))
                .expiration(getAccessTokenExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    public String createRefreshToken() {
        long now = System.currentTimeMillis();
        UUID uuid = UUID.randomUUID();

        return Jwts.builder()
                .claim(JWT_PAYLOAD_KEY_CATEGORY, JWT_PAYLOAD_VALUE_REFRESH)
                .claim(JWT_PAYLOAD_KEY_UUID, uuid)
                .issuedAt(new Date(now))
                .expiration(getRefreshTokenExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    private Date getAccessTokenExpiration(long now) {
        long accessTokenExpiration = Long.parseLong(
                tokenProperties.expiration().accessTokenExpiration());

        return new Date(now + accessTokenExpiration);
    }

    private Date getRefreshTokenExpiration(long now) {
        long accessTokenExpiration = Long.parseLong(
                tokenProperties.expiration().refreshTokenExpiration());

        return new Date(now + accessTokenExpiration);
    }
}
