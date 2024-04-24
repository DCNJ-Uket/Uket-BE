package com.uket.domain.auth.filter;

import static com.uket.modules.jwt.auth.constants.JwtValues.*;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.exception.AuthException;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.domain.user.dto.UserDto;
import com.uket.modules.jwt.auth.JwtAuthTokenUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

@Component
@Slf4j
@RequiredArgsConstructor
public class JwtFilter extends OncePerRequestFilter {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;
    private final TokenValidator tokenValidator;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        String accessToken = request.getHeader(JWT_AUTHORIZATION_HEADER);

        if (accessToken == null) {
            filterChain.doFilter(request, response);
            return;
        }
        accessToken = accessToken.replace(JWT_AUTHORIZATION_VALUE_PREFIX, "");

        if (validateAccessToken(accessToken)) {
            Authentication authentication = new JWTTokenAuthentication(accessToken, generateUserDto(accessToken));
            SecurityContextHolder.getContext().setAuthentication(authentication);
        }
        filterChain.doFilter(request, response);
    }

    private boolean validateAccessToken(String accessToken) {
        try {
            tokenValidator.validateExpiredToken(accessToken);
            tokenValidator.validateTokenSignature(accessToken);
            tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_ACCESS, accessToken);
        } catch (AuthException exception) {
            ErrorCode errorCode =  exception.getErrorCode();
            log.warn("[AuthException] {}: {}", errorCode.getCode(), errorCode.getMessage(), exception);
            return false;
        }
        return true;
    }

    private UserDto generateUserDto(String accessToken) {
        Long userId = jwtAuthTokenUtil.getId(accessToken);
        String name = jwtAuthTokenUtil.getName(accessToken);
        String role = jwtAuthTokenUtil.getRole(accessToken);

        return UserDto.builder()
                .userId(userId)
                .name(name)
                .role(role)
                .build();
    }
}
