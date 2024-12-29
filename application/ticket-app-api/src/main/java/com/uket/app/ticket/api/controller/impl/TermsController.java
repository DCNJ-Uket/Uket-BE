package com.uket.app.ticket.api.controller.impl;

import com.uket.app.ticket.api.controller.TermsApi;
import com.uket.app.ticket.api.dto.response.ListResponse;
import com.uket.app.ticket.api.dto.response.TermsResponse;
import com.uket.domain.terms.entity.Terms;
import com.uket.domain.terms.service.DocumentService;
import com.uket.domain.terms.service.TermsService;
import com.uket.domain.terms.service.TermsSignService;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

@Controller
@RequiredArgsConstructor
public class TermsController implements TermsApi {

    private final TermsService termsService;
    private final TermsSignService termsSignService;
    private final DocumentService documentService;

    @Override
    public ResponseEntity<ListResponse<TermsResponse>> getTerms(Long userId) {
        List<Terms> activeTerms = termsService.findAllActive();

        Map<Long, Boolean> termsSignMap = termsSignService.getTermsSignMap(userId, getTermsIds(activeTerms));
        Map<Long, String> linkMap = documentService.getLinkMap(getDocumentNos(activeTerms));
        List<TermsResponse> termsResponses = getTermsResponse(activeTerms, termsSignMap, linkMap);

        return ResponseEntity.ok(ListResponse.from(termsResponses));
    }

    private List<Long> getTermsIds(List<Terms> activeTerms) {
        return activeTerms.stream().map(Terms::getId).toList();
    }

    private List<Long> getDocumentNos(List<Terms> activeTerms) {
        return activeTerms.stream().map(Terms::getDocumentNo).toList();
    }

    private List<TermsResponse> getTermsResponse(
            List<Terms> activeTerms,
            Map<Long, Boolean> termsSignMap,
            Map<Long, String> termsLinkMap
    ) {
        return activeTerms.stream()
                .map(terms -> TermsResponse.of(
                        terms,
                        termsSignMap.getOrDefault(terms.getId(), false),
                        termsLinkMap.get(terms.getDocumentNo())
                ))
                .toList();
    }
}
