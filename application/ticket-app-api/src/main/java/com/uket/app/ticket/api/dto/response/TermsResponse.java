package com.uket.app.ticket.api.dto.response;

import com.uket.domain.terms.entity.Terms;
import com.uket.domain.terms.entity.TermsType;
import lombok.Builder;

@Builder
public record TermsResponse(
        Long termsId,
        String name,
        TermsType type,
        String link,
        Boolean isAgreed
) {

    public static TermsResponse of(Terms terms, Boolean isAgreed, String link){
        return TermsResponse.builder()
                .termsId(terms.getId())
                .name(terms.getName())
                .type(terms.getType())
                .link(link)
                .isAgreed(isAgreed)
                .build();
    }
}
