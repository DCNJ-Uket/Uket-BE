package com.uket.modules.jwt.auth;

import com.uket.modules.jwt.auth.constants.JwtValues;
import com.uket.modules.jwt.auth.properties.TokenProperties;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.SignatureException;
import java.nio.charset.StandardCharsets;
import java.util.Date;
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

    public String createRefreshToken(Long userId) {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .claim(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, JwtValues.JWT_PAYLOAD_VALUE_REFRESH)
                .claim(JwtValues.JWT_PAYLOAD_KEY_ID, userId)
                .issuedAt(new Date(now))
                .expiration(getRefreshTokenExpiration(now))
                .signWith(secretKey)
                .compact();
    }

    public String createRefreshToken(Long userId, Long expiration) {
        long now = System.currentTimeMillis();

        return Jwts.builder()
                .claim(JwtValues.JWT_PAYLOAD_KEY_CATEGORY, JwtValues.JWT_PAYLOAD_VALUE_REFRESH)
                .claim(JwtValues.JWT_PAYLOAD_KEY_ID, userId)
                .issuedAt(new Date(now))
                .expiration(new Date(expiration))
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
