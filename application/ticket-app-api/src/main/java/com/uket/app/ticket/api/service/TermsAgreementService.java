package com.uket.app.ticket.api.service;

import com.uket.app.ticket.api.dto.request.TermsAgreementRequest;
import com.uket.domain.terms.entity.Terms;
import com.uket.domain.terms.entity.TermsSign;
import com.uket.domain.terms.service.TermsService;
import com.uket.domain.terms.service.TermsSignService;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TermsAgreementService {

    private final TermsService termsService;
    private final TermsSignService termsSignService;

    @Transactional
    public List<TermsSign> agreeTerms(Long userId, List<TermsAgreementRequest> requests){
        List<TermsSign> termsSigns = requests.stream().map(request -> {
            Long termsId = request.termId();
            Boolean isAgreed = request.isAgreed();

            Terms term = termsService.findById(termsId);
            term.checkMandatory(isAgreed);

            return isAgreed ? TermsSign.agree(userId, termsId) : TermsSign.agreeNot(userId, termsId);
        }).toList();

        return termsSignService.saveAll(termsSigns);
    }
}
