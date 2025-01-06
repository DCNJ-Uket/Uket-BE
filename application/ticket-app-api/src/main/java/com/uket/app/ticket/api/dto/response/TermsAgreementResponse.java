package com.uket.app.ticket.api.dto.response;

import com.uket.domain.terms.entity.TermsSign;

public record TermsAgreementResponse(
        Long termId,
        Boolean isAgreed
) {
    public static TermsAgreementResponse from(TermsSign termsSign){
        return new TermsAgreementResponse(termsSign.getTermsId(), termsSign.getIsAgreed());
    }
}
