package com.uket.app.ticket.api.filter;

import static com.uket.jwtprovider.auth.constants.JwtValues.*;

import com.uket.domain.auth.domain.CustomOAuth2User;
import com.uket.domain.auth.validator.TokenValidator;
import com.uket.domain.user.dto.UserDto;
import com.uket.jwtprovider.auth.JwtAuthTokenUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

@RequiredArgsConstructor
public class JwtFilter extends OncePerRequestFilter {

    public final JwtAuthTokenUtil jwtAuthTokenUtil;
    public final TokenValidator tokenValidator;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        String accessToken = request.getHeader(JWT_AUTHORIZATION_HEADER);

        if (accessToken == null) {
            filterChain.doFilter(request, response);
            return;
        }
        accessToken = accessToken.replace(JWT_AUTHORIZATION_VALUE_PREFIX, "");

        tokenValidator.validateExpiredToken(accessToken);
        tokenValidator.validateTokenCategory(JWT_PAYLOAD_VALUE_ACCESS, accessToken);

        CustomOAuth2User customOAuth2User = new CustomOAuth2User(generateUserDto(accessToken));
        Authentication authToken = new UsernamePasswordAuthenticationToken(customOAuth2User, null, customOAuth2User.getAuthorities());
        SecurityContextHolder.getContext().setAuthentication(authToken);

        filterChain.doFilter(request, response);
    }

    private UserDto generateUserDto(String accessToken) {
        Long memberId = jwtAuthTokenUtil.getId(accessToken);
        String name = jwtAuthTokenUtil.getName(accessToken);
        String role = jwtAuthTokenUtil.getRole(accessToken);

        return UserDto.builder()
                .memberId(memberId)
                .name(name)
                .role(role)
                .build();
    }
}
