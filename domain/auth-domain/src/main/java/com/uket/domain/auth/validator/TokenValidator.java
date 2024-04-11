package com.uket.domain.auth.validator;


import com.uket.domain.auth.exception.ExpiredTokenException;
import com.uket.domain.auth.exception.InvalidTokenException;
import com.uket.jwtprovider.auth.JwtAuthTokenUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class TokenValidator {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;

    public void validateExpiredToken(String token) {
        if (Boolean.TRUE.equals(jwtAuthTokenUtil.isExpired(token))) {
            throw new ExpiredTokenException();
        }
    }

    public void validateTokenCategory(String category, String token) {
        String tokenCategory = jwtAuthTokenUtil.getCategory(token);
        if (Boolean.FALSE.equals(tokenCategory.equals(category))) {
            throw new InvalidTokenException();
        }
    }
}
