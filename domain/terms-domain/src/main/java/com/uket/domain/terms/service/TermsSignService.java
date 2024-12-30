package com.uket.domain.terms.service;

import com.uket.domain.terms.entity.TermsSign;
import com.uket.domain.terms.repository.TermsSignRepository;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TermsSignService {

    private final TermsSignRepository termsSignRepository;

    @Transactional(readOnly = true)
    public Map<Long, Boolean> getTermsSignMap(Long userId, List<Long> termsIds){

        return termsSignRepository.findLatestByUserIdAndTermsIds(userId, termsIds).stream()
                .collect(Collectors.toMap(TermsSign::getTermsId, TermsSign::getIsAgreed));
    }

    @Transactional
    public List<TermsSign> saveAll(List<TermsSign> termsSigns){
        return termsSignRepository.saveAll(termsSigns);
    }
}
