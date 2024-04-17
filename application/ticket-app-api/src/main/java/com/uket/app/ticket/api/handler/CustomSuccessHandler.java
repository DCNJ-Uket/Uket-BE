package com.uket.app.ticket.api.handler;

import static com.uket.jwtprovider.auth.constants.JwtValues.*;

import com.uket.app.ticket.api.properties.AppProperties;
import com.uket.app.ticket.api.util.CookieGenerator;
import com.uket.domain.auth.domain.CustomOAuth2User;
import com.uket.jwtprovider.auth.JwtAuthTokenUtil;
import com.uket.jwtprovider.auth.properties.TokenProperties;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.web.authentication.SimpleUrlAuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class CustomSuccessHandler extends SimpleUrlAuthenticationSuccessHandler {

    private final JwtAuthTokenUtil jwtAuthTokenUtil;
    private final TokenProperties tokenProperties;
    private final AppProperties appProperties;
    private final CookieGenerator cookieGenerator;

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response,
            Authentication authentication) throws IOException {
        CustomOAuth2User customUserDetails = (CustomOAuth2User) authentication.getPrincipal();

        Collection<? extends GrantedAuthority> authorities = authentication.getAuthorities();
        Iterator<? extends GrantedAuthority> iterator = authorities.iterator();
        GrantedAuthority auth = iterator.next();

        String role = auth.getAuthority();
        Long memberId = customUserDetails.getMemberId();
        String name = customUserDetails.getName();

        String accessToken = jwtAuthTokenUtil.createAccessToken(memberId, name, role);
        String refreshToken = jwtAuthTokenUtil.createRefreshToken(memberId, name, role);

        int maxAgeOfAccess = Integer.parseInt(tokenProperties.expiration().accessTokenExpiration());
        int maxAgeOfRefresh = Integer.parseInt(tokenProperties.expiration().refreshTokenExpiration());

        response.setHeader(HttpHeaders.SET_COOKIE,cookieGenerator.createCookie(JWT_PAYLOAD_VALUE_ACCESS, accessToken, maxAgeOfAccess).toString());
        response.addHeader(HttpHeaders.SET_COOKIE,cookieGenerator.createCookie(JWT_PAYLOAD_VALUE_REFRESH, refreshToken, maxAgeOfRefresh).toString());
        response.setHeader(HttpHeaders.ACCESS_CONTROL_ALLOW_ORIGIN, "https://localhost:5173");
        response.setHeader(HttpHeaders.ACCESS_CONTROL_ALLOW_CREDENTIALS, "true");
        response.setStatus(HttpStatus.OK.value());
        response.sendRedirect(appProperties.redirectUrl());
    }
}
