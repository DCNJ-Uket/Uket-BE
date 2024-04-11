package com.uket.domain.auth.domain;

import com.uket.domain.user.dto.UserDto;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.oauth2.core.user.OAuth2User;

@RequiredArgsConstructor
public class CustomOAuth2User implements OAuth2User {

    private final UserDto userDto;

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        Collection<GrantedAuthority> collection = new ArrayList<>();
        collection.add((GrantedAuthority) userDto::role);

        return collection;
    }

    @Override
    public String getName() {
        return userDto.name();
    }

    public Long getMemberId() {
        return userDto.memberId();
    }

    @Override
    @Deprecated
    public Map<String, Object> getAttributes() {
        return null;
    }
}
