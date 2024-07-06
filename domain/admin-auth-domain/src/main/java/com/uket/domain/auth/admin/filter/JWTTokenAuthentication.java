package com.uket.domain.auth.admin.filter;

import com.uket.domain.user.dto.UserDto;
import java.util.ArrayList;
import java.util.Collection;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;

@RequiredArgsConstructor
public class JWTTokenAuthentication implements Authentication {

    private final String accessToken;
    private final UserDto userDto;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        Collection<GrantedAuthority> collection = new ArrayList<>();
        collection.add((GrantedAuthority) userDto::role);

        return collection;
    }

    @Override
    public String getName() {
        return String.valueOf(userDto.userId());
    }

    @Override
    public Object getCredentials() {
        return accessToken;
    }

    @Override
    public Object getDetails() {
        return null;
    }

    @Override
    public Object getPrincipal() {
        return "";
    }

    @Override
    public boolean isAuthenticated() {
        return true;
    }

    @Override
    public void setAuthenticated(boolean isAuthenticated) throws IllegalArgumentException {

    }

}
