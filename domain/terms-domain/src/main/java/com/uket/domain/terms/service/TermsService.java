package com.uket.domain.terms.service;

import com.uket.domain.terms.entity.Terms;
import com.uket.domain.terms.repository.TermsRepository;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TermsService {

    private final TermsRepository termsRepository;

    @Transactional(readOnly = true)
    public List<Terms> findAllActive() {
        return termsRepository.findAllByIsActiveTrue();
    }
}
