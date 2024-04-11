package com.uket.app.ticket.api.filter;

import static com.uket.jwtprovider.auth.constants.JwtValues.*;

import com.uket.core.exception.ErrorCode;
import com.uket.domain.auth.domain.CustomOAuth2User;
import com.uket.domain.user.dto.UserDto;
import com.uket.jwtprovider.auth.JwtAuthTokenUtil;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

@RequiredArgsConstructor
public class JwtFilter extends OncePerRequestFilter {

    public final JwtAuthTokenUtil jwtAuthTokenUtil;

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response,
            FilterChain filterChain) throws ServletException, IOException {
        String accessToken = request.getHeader(JWT_AUTHORIZATION_HEADER);

        if (accessToken == null) {
            filterChain.doFilter(request, response);
            return;
        }
        accessToken = accessToken.replace(JWT_AUTHORIZATION_VALUE_PREFIX, "");

        if(Boolean.TRUE.equals(jwtAuthTokenUtil.isExpired(accessToken))){
            PrintWriter writer = response.getWriter();
            writer.print(ErrorCode.TOKEN_EXPIRED);

            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

        String category = jwtAuthTokenUtil.getCategory(accessToken);
        if (!category.equals(JWT_PAYLOAD_VALUE_ACCESS)) {

            PrintWriter writer = response.getWriter();
            writer.print(ErrorCode.INVALID_TOKEN);

            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

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
